fit_full_model_new <- function(
  df,
  response = "Cluster",
  label = "all_clusters",
  date_col = "Date",
  site_col = "Site",
  time_prop = 0.8,
  class_weights = c("1" = 0.2, "2" = 0.3, "3" = 0.5),
  time_folds = 5,
  tune_initial = 20,
  tune_iter = 500,
  validate_sites = TRUE,
  seed = 42L
) {
  suppressPackageStartupMessages({
    library(tidyverse)
    library(tidymodels)
    library(themis)
    library(colino)
    library(ranger)
    library(caret)
    library(future)
    library(doFuture)
    library(here)
    library(cli)
  })

  if (
    !is.numeric(seed) ||
      length(seed) != 1L ||
      is.na(seed) ||
      seed < 1 ||
      seed > .Machine$integer.max
  ) {
    cli::cli_abort("{.arg seed} must be one positive integer.")
  }
  seed <- as.integer(seed)
  set.seed(seed)
  registerDoFuture()
  # `detectCores()` reports every physical core on the host, not the CPUs
  # assigned to this Slurm job.  `availableCores()` respects the scheduler's
  # CPU allocation (for example, `SLURM_CPUS_PER_TASK`), and reserving one
  # CPU keeps the main R process responsive.
  workers <- parallelly::availableCores(omit = 1L)
  cli::cli_alert_info("Using {.val {workers}} parallel worker(s).")
  plan(multisession, workers = workers)
  on.exit(plan(sequential), add = TRUE)

  if (!response %in% names(df)) {
    cli::cli_abort("Response column {.field {response}} was not found.")
  }
  if (!site_col %in% names(df)) {
    cli::cli_abort("Site column {.field {site_col}} was not found.")
  }
  if (
    !is.numeric(time_prop) ||
      length(time_prop) != 1L ||
      is.na(time_prop) ||
      time_prop <= 0 ||
      time_prop >= 1
  ) {
    cli::cli_abort(
      "{.arg time_prop} must be one number strictly between 0 and 1."
    )
  }
  if (
    !is.numeric(time_folds) ||
      length(time_folds) != 1L ||
      is.na(time_folds) ||
      time_folds < 1
  ) {
    cli::cli_abort("{.arg time_folds} must be a positive integer.")
  }

  if (is.null(date_col)) {
    date_candidates <- names(df)[vapply(
      df,
      function(x) inherits(x, c("Date", "POSIXt")),
      logical(1)
    )]
    if (length(date_candidates) != 1L) {
      cli::cli_abort(c(
        "Could not identify one unambiguous sampling-date column.",
        "i" = paste(
          "Supply {.arg date_col} explicitly; detected:",
          "{paste(date_candidates, collapse = ', ')}."
        )
      ))
    }
    date_col <- date_candidates
  }
  if (!date_col %in% names(df)) {
    cli::cli_abort("Date column {.field {date_col}} was not found.")
  }
  if (anyNA(df[[date_col]])) {
    cli::cli_abort("Date column {.field {date_col}} contains missing values.")
  }
  if (anyNA(df[[site_col]])) {
    cli::cli_abort("Site column {.field {site_col}} contains missing values.")
  }

  df[[response]] <- factor(df[[response]])
  observed_levels <- levels(df[[response]])
  missing_weights <- setdiff(observed_levels, names(class_weights))
  if (length(missing_weights) > 0L) {
    cli::cli_abort(
      paste(
        "{.arg class_weights} has no value for outcome level(s):",
        "{paste(missing_weights, collapse = ', ')}."
      )
    )
  }

  add_case_weights <- function(data) {
    raw_weights <- unname(class_weights[as.character(data[[response]])])
    dplyr::mutate(data, case_wt = hardhat::importance_weights(raw_weights))
  }

  make_time_split <- function(data, prop) {
    sites <- sort(unique(data[[site_col]]))
    site_timestamp_counts <- vapply(
      sites,
      function(site) {
        length(unique(data[data[[site_col]] == site, date_col, drop = TRUE]))
      },
      integer(1)
    )
    invalid_sites <- as.character(sites[site_timestamp_counts < 2L])
    if (length(invalid_sites) > 0L) {
      cli::cli_abort(
        paste(
          "Temporal validation requires at least two unique timestamps",
          "at every site. Invalid site(s): {paste(invalid_sites, collapse = ', ')}."
        )
      )
    }

    train_rows <- integer()
    test_rows <- integer()
    cutoff_records <- vector("list", length(sites))

    for (i in seq_along(sites)) {
      site <- sites[[i]]
      site_rows <- which(data[[site_col]] == site)
      timestamps <- sort(unique(data[[date_col]][site_rows]))
      cutoff_index <- floor(length(timestamps) * prop)
      cutoff_index <- max(1L, min(cutoff_index, length(timestamps) - 1L))
      cutoff <- timestamps[[cutoff_index]]
      site_train_rows <- site_rows[data[[date_col]][site_rows] <= cutoff]
      site_test_rows <- site_rows[data[[date_col]][site_rows] > cutoff]

      train_rows <- c(train_rows, site_train_rows)
      test_rows <- c(test_rows, site_test_rows)
      cutoff_records[[i]] <- tibble(
        site = as.character(site),
        cutoff = cutoff,
        training_rows = length(site_train_rows),
        assessment_rows = length(site_test_rows),
        training_timestamps = cutoff_index,
        assessment_timestamps = length(timestamps) - cutoff_index
      )
    }

    list(
      train = data[sort(train_rows), , drop = FALSE],
      test = data[sort(test_rows), , drop = FALSE],
      cutoffs = bind_rows(cutoff_records)
    )
  }

  make_time_folds <- function(data, v = 5L) {
    site_rows <- split(
      seq_len(nrow(data)),
      as.character(data[[site_col]]),
      drop = TRUE
    )
    site_timestamps <- lapply(
      site_rows,
      function(rows) sort(unique(data[[date_col]][rows]))
    )
    invalid_sites <- names(site_timestamps)[lengths(site_timestamps) < 3L]
    if (length(invalid_sites) > 0L) {
      cli::cli_abort(
        paste(
          "Time-blocked tuning requires at least three unique timestamps",
          "at every site. Invalid site(s):",
          "{paste(invalid_sites, collapse = ', ')}."
        )
      )
    }

    schedules <- lapply(site_timestamps, function(timestamps) {
      n_times <- length(timestamps)
      assessment_n <- max(1L, floor(n_times * 0.1))
      initial_n <- max(2L, floor(n_times * 0.5))
      latest_start <- n_times - assessment_n + 1L
      list(
        timestamps = timestamps,
        assessment_n = assessment_n,
        initial_n = initial_n,
        latest_start = latest_start,
        available_starts = latest_start - initial_n
      )
    })
    n_folds <- min(
      as.integer(v),
      as.integer(min(
        vapply(schedules, `[[`, numeric(1), "available_starts")
      ))
    )
    schedules <- lapply(schedules, function(schedule) {
      schedule$starts <- as.integer(round(seq(
        from = schedule$initial_n + 1L,
        to = schedule$latest_start,
        length.out = n_folds
      )))
      schedule
    })

    splits <- lapply(seq_len(n_folds), function(fold) {
      analysis_rows <- integer()
      assessment_rows <- integer()

      for (site in names(schedules)) {
        schedule <- schedules[[site]]
        start <- schedule$starts[[fold]]
        assessment_times <- schedule$timestamps[
          seq.int(start, start + schedule$assessment_n - 1L)
        ]
        rows <- site_rows[[site]]
        analysis_rows <- c(
          analysis_rows,
          rows[data[[date_col]][rows] < schedule$timestamps[[start]]]
        )
        assessment_rows <- c(
          assessment_rows,
          rows[data[[date_col]][rows] %in% assessment_times]
        )
      }

      rsample::make_splits(
        list(
          analysis = sort(analysis_rows),
          assessment = sort(assessment_rows)
        ),
        data
      )
    })
    rsample::manual_rset(splits, paste0("Time", seq_along(splits)))
  }

  result_dir <- here("Results", "rf")
  dir.create(result_dir, recursive = TRUE, showWarnings = FALSE)

  cli::cli_alert_info("Starting full model fit for {.val {label}}")
  temporal_split <- make_time_split(df, time_prop)
  train_data <- add_case_weights(temporal_split$train)
  test_data <- temporal_split$test

  cli::cli_alert_info(
    "Temporal holdout: earliest {.val {time_prop * 100}}% within each site for training; each site's later observations for testing."
  )

  base_recipe <- recipe(
    stats::reformulate(setdiff(names(train_data), response), response),
    data = train_data
  ) |>
    step_rm(all_date(), any_of(c(site_col, "name"))) |>
    step_string2factor(all_nominal_predictors())

  cli::cli_alert_info("Prepping recipe...")
  prep_recipe <- prep(base_recipe)
  predictor_names <- summary(prep_recipe) |>
    filter(role == "predictor") |>
    pull(variable)
  n_predictors <- length(predictor_names)
  if (n_predictors < 1L) {
    cli::cli_abort(
      "No predictors remain after removing date and grouping columns."
    )
  }

  # RFE is currently disabled; retain the object and output for compatibility.
  rfe_fit <- list()
  selected_vars <- predictor_names
  cli::cli_alert_success(
    "Using {.val {length(selected_vars)}} features (RFE is disabled)."
  )
  writeLines(
    selected_vars,
    file.path(result_dir, paste0(label, "_selected_predictors.txt"))
  )
  cli::cli_alert_info(
    "Selected variables: {.val {paste(selected_vars, collapse = ', ')}}"
  )

  final_recipe <- recipe(
    stats::reformulate(c(selected_vars, "case_wt"), response),
    data = train_data
  ) |>
    step_string2factor(all_nominal_predictors())

  tune_spec <- rand_forest(
    mtry = tune(),
    trees = tune(),
    min_n = tune()
  ) |>
    set_mode("classification") |>
    set_engine(
      "ranger",
      importance = "permutation",
      seed = seed,
      num.threads = 1
    )

  make_tune_workflow <- function(recipe) {
    workflow() |>
      add_recipe(recipe) |>
      add_model(tune_spec) |>
      workflows::add_case_weights(case_wt)
  }

  #metrics <- metric_set(f_meas, roc_auc, pr_auc, mn_log_loss, kap)

  macro_f1 <- yardstick::metric_tweak(
    "macro_f1",
    yardstick::f_meas,
    estimator = "macro"
  )

  metrics <- metric_set(
    macro_f1,
    yardstick::bal_accuracy,
    yardstick::roc_auc,
    yardstick::pr_auc,
    yardstick::mn_log_loss,
    yardstick::kap
  )

  tune_model <- function(recipe, resamples, context) {
    cli::cli_alert_info("Tuning hyperparameters: {.val {context}}")
    set.seed(seed)
    make_tune_workflow(recipe) |>
      tune_bayes(
        resamples = resamples,
        metrics = metrics,
        initial = tune_initial,
        iter = tune_iter,
        param_info = parameters(
          trees(range = c(500L, 2000L)),
          mtry(range = c(min(2L, n_predictors), n_predictors)),
          min_n(range = c(1L, 20L))
        ),
        control = control_bayes(
          no_improve = 15L,
          verbose = TRUE,
          save_pred = TRUE
        )
      )
  }

  build_final_workflow <- function(recipe, best_params) {
    final_model <- rand_forest(
      trees = best_params$trees,
      mtry = best_params$mtry,
      min_n = best_params$min_n
    ) |>
      set_mode("classification") |>
      set_engine(
        "ranger",
        importance = "permutation",
        seed = seed,
        num.threads = 1
      )

    workflow() |>
      add_recipe(recipe) |>
      add_model(final_model) |>
      workflows::add_case_weights(case_wt)
  }

  tune_res <- tune_model(
    recipe = final_recipe,
    resamples = make_time_folds(train_data, time_folds),
    context = "temporal training set"
  )
  cli::cli_alert_success("Temporal tuning completed.")
  best_params <- tune_res |> select_best(metric = "macro_f1")

  final_wf <- build_final_workflow(final_recipe, best_params)
  final_fit <- final_wf |> fit(data = train_data)
  cli::cli_alert_success("Final temporal model fit completed.")

  train_pred <- predict(final_fit, train_data)
  test_pred <- predict(final_fit, test_data)
  conf_test <- confusionMatrix(test_pred$.pred_class, test_data[[response]])

  write_csv(
    as_tibble(conf_test$table),
    file.path(result_dir, paste0(label, "_temporal_confusion_matrix.csv"))
  )
  write_csv(
    enframe(conf_test$overall, name = "metric", value = "estimate"),
    file.path(result_dir, paste0(label, "_temporal_overall_performance.csv"))
  )
  write_csv(
    as.data.frame(conf_test$byClass) |> rownames_to_column("class"),
    file.path(result_dir, paste0(label, "_temporal_class_performance.csv"))
  )

  temporal_predictions <- bind_rows(
    train_data |>
      select(-case_wt) |>
      mutate(.pred_class = train_pred$.pred_class, .set = "train_early"),
    test_data |>
      mutate(.pred_class = test_pred$.pred_class, .set = "test_late")
  )
  write_csv(
    temporal_predictions,
    file.path(result_dir, paste0(label, "_temporal_predictions.csv"))
  )

  tibble(
    predictor = names(extract_fit_parsnip(final_fit)$fit$variable.importance),
    importance = extract_fit_parsnip(final_fit)$fit$variable.importance,
    prediction_error = extract_fit_parsnip(final_fit)$fit$prediction.error
  ) |>
    arrange(desc(importance)) |>
    write_csv(file.path(result_dir, paste0(label, "_importance.csv")))

  validation_manifest <- temporal_split$cutoffs |>
    transmute(
      validation = "temporal_holdout_by_site",
      training_rule = paste0(
        site_col,
        " == ",
        site,
        " and ",
        date_col,
        " <= ",
        cutoff
      ),
      assessment_rule = paste0(
        site_col,
        " == ",
        site,
        " and ",
        date_col,
        " > ",
        cutoff
      ),
      training_rows,
      assessment_rows,
      training_timestamps,
      assessment_timestamps
    )
  site_validation <- NULL
  if (isTRUE(validate_sites)) {
    sites <- sort(unique(df[[site_col]]))
    if (length(sites) < 2L) {
      cli::cli_abort("LOSO-CV requires at least two sites.")
    }
    cli::cli_alert_info(
      paste(
        "Starting nested leave-one-site-out validation across",
        "{.val {length(sites)}} sites."
      )
    )

    site_fits <- vector("list", length(sites))
    site_predictions <- vector("list", length(sites))
    site_best_params <- vector("list", length(sites))

    for (i in seq_along(sites)) {
      held_out_site <- sites[[i]]
      cli::cli_alert_info(
        paste(
          "LOSO fold {.val {i}}/{.val {length(sites)}}:",
          "holding out {.val {held_out_site}}."
        )
      )
      outer_train <- df[df[[site_col]] != held_out_site, , drop = FALSE] |>
        add_case_weights()
      outer_test <- df[df[[site_col]] == held_out_site, , drop = FALSE]

      # The assessment site is absent from this fold's tuning and model fit.
      outer_recipe <- recipe(
        stats::reformulate(c(selected_vars, "case_wt"), response),
        data = outer_train
      ) |>
        step_string2factor(all_nominal_predictors())
      outer_tune <- tune_model(
        recipe = outer_recipe,
        resamples = make_time_folds(outer_train, time_folds),
        context = paste("LOSO fold excluding", held_out_site)
      )
      outer_best <- outer_tune |> select_best(metric = "macro_f1")
      outer_wf <- build_final_workflow(outer_recipe, outer_best)
      outer_fit <- fit(outer_wf, data = outer_train)
      outer_pred <- predict(outer_fit, outer_test)

      site_fits[[i]] <- outer_fit
      site_predictions[[i]] <- outer_test |>
        mutate(
          .pred_class = outer_pred$.pred_class,
          .held_out_site = as.character(held_out_site)
        )
      site_best_params[[i]] <- outer_best |>
        mutate(.held_out_site = as.character(held_out_site), .before = 1)
    }

    names(site_fits) <- as.character(sites)
    loso_predictions <- bind_rows(site_predictions)
    loso_best_params <- bind_rows(site_best_params)
    loso_confusion <- confusionMatrix(
      loso_predictions$.pred_class,
      loso_predictions[[response]]
    )
    loso_site_metrics <- loso_predictions |>
      group_by(.held_out_site) |>
      yardstick::metrics(
        truth = !!rlang::sym(response),
        estimate = .pred_class
      ) |>
      ungroup()

    write_csv(
      loso_predictions,
      file.path(result_dir, paste0(label, "_site_loso_predictions.csv"))
    )
    write_csv(
      loso_best_params,
      file.path(result_dir, paste0(label, "_site_loso_best_params.csv"))
    )
    write_csv(
      loso_site_metrics,
      file.path(result_dir, paste0(label, "_site_loso_metrics.csv"))
    )
    write_csv(
      as_tibble(loso_confusion$table),
      file.path(result_dir, paste0(label, "_site_loso_confusion_matrix.csv"))
    )
    write_csv(
      enframe(loso_confusion$overall, name = "metric", value = "estimate"),
      file.path(result_dir, paste0(label, "_site_loso_overall_performance.csv"))
    )
    write_csv(
      as.data.frame(loso_confusion$byClass) |> rownames_to_column("class"),
      file.path(result_dir, paste0(label, "_site_loso_class_performance.csv"))
    )

    site_validation <- list(
      fits = site_fits,
      predictions = loso_predictions,
      best_params = loso_best_params,
      metrics = loso_site_metrics,
      confusion_matrix = loso_confusion
    )
    validation_manifest <- bind_rows(
      validation_manifest,
      tibble(
        validation = "site_loso",
        training_rule = paste0(site_col, " != ", as.character(sites)),
        assessment_rule = paste0(site_col, " == ", as.character(sites)),
        training_rows = vapply(
          sites,
          function(site) sum(df[[site_col]] != site),
          integer(1)
        ),
        assessment_rows = vapply(
          sites,
          function(site) sum(df[[site_col]] == site),
          integer(1)
        ),
        training_timestamps = NA_integer_,
        assessment_timestamps = NA_integer_
      )
    )
    cli::cli_alert_success("Nested leave-one-site-out validation completed.")
  }

  write_csv(
    validation_manifest,
    file.path(result_dir, paste0(label, "_validation_manifest.csv"))
  )

  plan(sequential)
  list(
    rfe_fit = rfe_fit,
    final_fit = final_fit,
    final_wf = final_wf,
    selected_vars = selected_vars,
    response = response,
    date_col = date_col,
    site_col = site_col,
    model_spec = list(
      response = response,
      label = label,
      date_col = date_col,
      site_col = site_col,
      time_prop = time_prop,
      class_weights = class_weights,
      time_folds = time_folds,
      tune_initial = tune_initial,
      tune_iter = tune_iter,
      validate_sites = validate_sites,
      seed = seed
    ),
    best_params = best_params,
    tune_results = tune_res,
    train_data = train_data,
    test_data = test_data,
    temporal_cutoffs = temporal_split$cutoffs,
    confusion_matrix = conf_test,
    validation_manifest = validation_manifest,
    site_validation = site_validation
  )
}

#' Prepare the information required for a whole-pipeline SHAP bootstrap
#'
#' The fitted forest alone is insufficient for this bootstrap: every replicate
#' must resample the original data, rerun the temporal split/tuning, and refit
#' the model. Save the returned object alongside the fitted model objects.
make_shap_bootstrap_input <- function(fit_result, data) {
  if (is.null(fit_result$model_spec)) {
    stop(
      "`fit_result` does not contain `model_spec`; refit with the current model function.",
      call. = FALSE
    )
  }
  required_columns <- c(
    fit_result$model_spec$response,
    fit_result$model_spec$date_col,
    fit_result$model_spec$site_col
  )
  missing_columns <- setdiff(required_columns, names(data))
  if (length(missing_columns) > 0L) {
    stop(
      "`data` is missing required column(s): ",
      paste(missing_columns, collapse = ", "),
      call. = FALSE
    )
  }

  list(data = data, model_spec = fit_result$model_spec)
}
