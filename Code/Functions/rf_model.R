# Variable Labels ---------------------------------------------------------
var_names <- c(
  "water.temp" = "Water Temp",
  "water.temp_19d" = "19 Day Water Temp",
  "water.temp_26d" = "26 Day Water Temp",
  "Q_0d" = "Q",
  "Q_1d" = "Q",
  "Q_7d" = "7 Day Q",
  "Q_19d" = "19 Day Q",
  "Q_60d" = "60 Day Q",
  "PPT_1d" = "PPT",
  "PPT_7d" = "7 Day PPT",
  "PPT_14d" = "14 Day PPT",
  "PPT_43d" = "43 Day PPT",
  "PPT_60d" = "60 Day PPT",
  "PPT_56d" = "56 Day PPT",
  "PPT_59d" = "59 Day PPT",
  "Depth_60d" = "60 Day Depth",
  "ET_1d" = "ET",
  "ET_7d" = "7 Day ET",
  "ET_13d" = "13 Day ET",
  "ET_14d" = "14 Day ET",
  "ET_52d" = "52 Day ET",
  "ET_53d" = "53 Day ET",
  "Depth_1d" = "Depth",
  "Depth_7d" = "7 Day Depth",
  "Depth_13d" = "13 Day Depth",
  "Depth_41d" = "41 Day Depth",
  "Depth_45d" = "45 Day Depth",
  "Depth_60d" = "60 Day Depth",
  "headGradient" = "Head Gradient",
  "headGradient_1d" = "Head Gradient",
  "headGradient_7d" = "7 Day Head Gradient",
  "headGradient_14d" = "14 Day Head Gradient",
  "headGradient_30d" = "30 Day Head Gradient",
  "headGradient_42d" = "42 Day Head Gradient",
  "headGradient_45d" = "45 Day Head Gradient",
  "headGradient_60d" = "60 Day Head Gradient",
  "excProb" = "Exceedance Prob.",
  "avgPar" = "Light",
  "avgPar_7d" = "7 Day Light",
  "avgPar_14d" = "14 Day Light",
  "avgPar_43d" = "43 Day Light",
  "avgPar_44d" = "44 Day Light",
  "a_cent" = "Alpha Centrality",
  "active_network_length_m" = "ASDN",
  "active_network_length_m_0d" = "ASDN",
  "percent_network_wet" = "% Wet",
  "water.temp_7d" = "7 Day Water Temp",
  "water.temp_60d" = "60 Day Water Temp"
)
# Fit Full Model ----------------------------------------------------------
fit_full_model <- function(df, response = "Cluster", label = "all_clusters") {
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

  set.seed(42)

  registerDoFuture()
  plan(multisession, workers = parallel::detectCores() - 1)
  on.exit(
    {
      plan(sequential)
    },
    add = TRUE
  )

  cli::cli_alert_info("Starting full model fit for {.val {label}}")

  data_split <- initial_split(df, prop = 0.8)
  train_data <- training(data_split)
  test_data <- testing(data_split)
  # Calculate class weights
  cluster_counts <- table(train_data$Cluster)
  # cluster_weights <- max(cluster_counts) / cluster_counts # Calculate inverse-frequency weights (the "balanced" approach)
  # cluster_weights <- nrow(train_data) /
  #    (length(cluster_counts) * cluster_counts) # or use n / (k * n_k)
  # cluster_weights <- sqrt(max(cluster_counts) / cluster_counts) # Square root of inverse frequency — a softer correction that doesn't fully equalize classes. Useful when the minority class is noisy and you don't want to amplify that noise as aggressively.
  # cluster_weights <- log(1 + max(cluster_counts) / cluster_counts ) # Log-based weighting — even gentler, compresses extreme imbalances.
  # cluster_weights <- c("1" = 1, "2" = 1, "3" = 1) # No weights
  # cluster_weights <- c("1" = .2, "2" = .4, "3" = .4) # Best balanced compromise
  cluster_weights <- c("1" = .2, "2" = .3, "3" = .5) # BEST SO FAR

  # Add case weights to train data
  train_data <- train_data |>
    mutate(
      case_wt = case_when(
        Cluster == "1" ~ cluster_weights["1"],
        Cluster == "2" ~ cluster_weights["2"],
        Cluster == "3" ~ cluster_weights["3"]
      ),
      case_wt = importance_weights(case_wt)
    )

  # Recipe
  base_recipe <- recipe(
    as.formula(paste(response, "~ .")),
    data = train_data
  ) |>
    step_rm(all_date(), any_of(c("Site", "name"))) |>
    update_role(all_of(response), new_role = "outcome") |>
    step_string2factor(all_nominal())

  cli::cli_alert_info("Prepping recipe...")
  prep_recipe <- prep(base_recipe)
  predictor_names <- juice(prep_recipe) |>
    select(-all_of(response)) |>
    names()
  n_predictors <- length(predictor_names)

  # Recursive Feature Elimination (RFE)
  cli::cli_alert_info("Starting RFE...")

  #
  # ctrl <- rfeControl(
  #   functions = rfFuncs,
  #   method = "repeatedcv",
  #   repeats = 5,
  #   verbose = FALSE,
  #   returnResamp = "final",
  #   allowParallel = TRUE
  # )
  #
  # rfe_fit <- suppressMessages(
  #   suppressWarnings(
  #     withDoRNG(rfe(
  #       base_recipe,
  #       train_data,
  #       sizes = c(1:n_predictors),
  #       rfeControl = ctrl,
  #       metric = "Kappa"
  #     ))
  #   )
  # )
  rfe_fit <- list()
  selected_vars <- predictor_names[predictor_names != "case_wt"]
  cli::cli_alert_success("RFE selected {.val {length(selected_vars)}} features")
  writeLines(
    selected_vars,
    here("Results/rf", paste0(label, "_selected_predictors.txt"))
  )
  cli::cli_alert_info(
    "Selected variables: {.val {paste(selected_vars, collapse = ', ')}}"
  )
  final_recipe <- recipe(
    as.formula(paste(
      response,
      "~",
      paste(c(selected_vars, "case_wt"), collapse = " + ")
    )),
    data = train_data
  )

  tune_spec <- rand_forest(
    mtry = tune(),
    trees = tune(),
    min_n = tune()
  ) |>
    set_mode("classification") |>
    set_engine("ranger", importance = "permutation", seed = 42, num.threads = 1)

  tune_wf <- workflow() |>
    add_recipe(final_recipe) |>
    add_model(tune_spec) |>
    add_case_weights(case_wt)
  tune_folds <- vfold_cv(train_data, v = 5, strata = response)
  # rf_tune_grid <- grid_regular(
  #   trees(range = c(100, 1000)),
  #   mtry(range = c(2, length(selected_vars))),
  #   min_n(range = c(2, 40)),
  #   levels = 4
  # )
  metrics <- metric_set(roc_auc, pr_auc, mn_log_loss, kap, f_meas)

  cli::cli_alert_info("Starting hyperparameter tuning...")
  # tune_res <- tune_wf |>
  #   tune_grid(resamples = tune_folds, grid = rf_tune_grid, metrics = metrics)

  tune_res <- tune_wf |>
    tune_bayes(
      resamples = tune_folds,
      metrics = metrics,
      initial = 15,
      iter = 50,
      param_info = parameters(
        trees(range = c(500, 2000)),
        mtry(range = c(2, length(selected_vars))),
        min_n(range = c(1, 20))
      ),
      control = control_bayes(
        no_improve = 10L,
        verbose = TRUE,
        save_pred = TRUE
      )
    )

  cli::cli_alert_success("Tuning completed.")
  # write_csv(
  #   tune_res |>
  #     collect_metrics() |>
  #     filter(.metric == "kap") |>
  #     select(mean, min_n, mtry, trees),
  #   here("Results/rf", paste0(label, "_tune_results.csv"))
  # )

  best_params <- tune_res |> select_best(metric = "f_meas")
  # write_csv(
  #   tibble(best_params),
  #   here("Results/rf", paste0(label, "_best_params.csv"))
  # )

  final_model <- rand_forest(
    trees = best_params$trees,
    mtry = best_params$mtry,
    min_n = best_params$min_n
  ) |>
    set_mode("classification") |>
    set_engine("ranger", importance = "permutation", seed = 42, num.threads = 1)

  final_wf <- workflow() |>
    add_recipe(final_recipe) |>
    add_model(final_model) |>
    add_case_weights(case_wt)
  final_fit <- final_wf |> fit(data = train_data)

  cli::cli_alert_success("Final model fit completed.")

  train_pred <- predict(final_fit, train_data)
  test_pred <- predict(final_fit, test_data)
  # Confusion Matrix
  conf_test <- confusionMatrix(test_pred$.pred_class, test_data[[response]])
  write_csv(
    as_tibble(conf_test$table),
    here("Results/rf", paste0(label, "_confusion_matrix.csv"))
  )
  write_csv(
    as_tibble(conf_test$overall),
    here("Results/rf", paste0(label, "_overall_preformance.csv"))
  )
  write_csv(
    as_tibble(conf_test$byClass),
    here("Results/rf", paste0(label, "_class_preformance.csv"))
  )

  bind_rows(
    train_data |>
      select(-case_wt) |>
      mutate(.pred_class = train_pred$.pred_class, .set = "train"),
    test_data |> mutate(.pred_class = test_pred$.pred_class, .set = "test")
  ) |>
    write_csv(here("Results/rf", paste0(label, "_predictions.csv")))

  tibble(
    predictor = names(extract_fit_parsnip(final_fit)$fit$variable.importance),
    importance = extract_fit_parsnip(final_fit)$fit$variable.importance,
    oobMSE = extract_fit_parsnip(final_fit)$fit$prediction.error
  ) |>
    arrange(desc(importance)) |>
    write_csv(here("Results/rf", paste0(label, "_importance.csv")))

  plan(sequential)

  return(list(
    rfe_fit = rfe_fit,
    final_fit = final_fit,
    final_wf = final_wf,
    selected_vars = selected_vars,
    response = response,
    best_params = best_params,
    tune_results = tune_res,
    train_data = train_data,
    test_data = test_data,
    confusion_matrix = conf_test
  ))
}

# fit_lagged_model <- function(df, response = "Cluster", label = "all_clusters") {
#   suppressPackageStartupMessages({
#     library(tidyverse)
#     library(tidymodels)
#     library(themis)
#     library(colino)
#     library(ranger)
#     library(caret)
#     library(future)
#     library(doFuture)
#     library(here)
#     library(cli)
#   })#
#   set.seed(42)

#   registerDoFuture()
#   plan(multisession, workers = parallel::detectCores() - 1)
#   on.exit(
#     {
#       plan(sequential)
#     },
#     add = TRUE
#   )
##   cli::cli_alert_info("Starting full model fit for {.val {label}}")
#
#data_split <- initial_split(df, prop = 0.8)
#   train_data <- training(data_split)
#   test_data <- testing(data_split)
#
#  # Recipe
#   base_recipe <- recipe(
#     as.formula(paste(response, "~ .")),
#     data = train_data
#   ) |>
#     step_rm(all_date(), any_of(c("Site", "name"))) |>
#     update_role(all_of(response), new_role = "outcome") |>
#     step_string2factor(all_nominal()) |>
#     step_naomit(all_predictors(), all_outcomes(), skip = FALSE) |>
#     #step_corr(all_numeric_predictors(), threshold = 0.7) |>
#     #step_normalize(all_numeric_predictors()) |>
#     step_smote(all_outcomes(), over_ratio = 1)
#
#  cli::cli_alert_info("Prepping recipe...")
#   prep_recipe <- prep(base_recipe)
#   predictor_names <- juice(prep_recipe) |> select(-all_of(response)) |> names()
#   n_predictors <- length(predictor_names)
#
#  # Recursive Feature Elimination (RFE)
#   cli::cli_alert_info("Starting RFE...")
#
#   trl <- rfeControl(
#     functions = rfFuncs,
#     method = "repeatedcv",
#     repeats = 5,
#     verbose = FALSE,
#     returnResamp = "final",
#     allowParallel = TRUE
#   )
#
#   re_fit <- suppressMessages(
#     suppressWarnings(
#       withDoRNG(rfe(
#         base_recipe,
#         train_data,
#         sizes = c(1:n_predictors),
#         rfeControl = ctrl,
#         metric = "Kappa"
#       ))
#     )
#   )
#
#   seected_vars <- rfe_fit$optVariables
#   cli::cli_alert_success(
#     "RFE selected {.val {length(selected_vars)}} features from {.val {n_predictors}} predictors"
#   )
#   writeLines(
#     selected_vars,
#     here("Results/rf", paste0(label, "_selected_predictors.txt"))
#   )
#   cli::cli_alert_info(
#     "Selected variables: {.val {paste(selected_vars, collapse = ', ')}}"
#   )
#
#   finl_recipe <- recipe(
#     as.formula(paste(response, "~", paste(selected_vars, collapse = " + "))),
#     data = train_data
#   )
#
#   tunespec <- rand_forest(
#     mtry = tune(),
#     trees = tune(),
#     min_n = tune()
#   ) |>
#     set_mode("classification") |>
#     set_engine("ranger", importance = "permutation", seed = 42, num.threads = 1)
#
#   tune_f <- workflow() |> add_recipe(final_recipe) |> add_model(tune_spec)
#   tune_folds <- vfold_cv(train_data, v = 5, strata = response)
#   rf_tune_grid <- grid_regular(
#     trees(range = c(100, 1000)),
#     mtry(range = c(2, length(selected_vars))),
#     min_n(range = c(2, 40)),
#     levels = 4
#   )
#   metrics <- metric_set(roc_auc, pr_auc, mn_log_loss, kap, f_meas)
#
#   cli::ci_alert_info("Starting hyperparameter tuning...")
#   tune_res <- tune_wf |>
#     tune_grid(resamples = tune_folds, grid = rf_tune_grid, metrics = metrics)
#
#   cli::cl_alert_success("Tuning completed.")
#   write_csv(
#     tune_res |>
#       collect_metrics() |>
#       filter(.metric == "kap") |>
#       select(mean, min_n, mtry, trees),
#     here("Results/rf", paste0(label, "_tune_results.csv"))
#   )
#
#   best_parms <- tune_res |> select_best(metric = "kap")
#   write_csv(
#     tibble(best_params),
#     here("Results/rf", paste0(label, "_best_params.csv"))
#   )
#
#   final_modl <- rand_forest(
#     trees = best_params$trees,
#     mtry = best_params$mtry,
#     min_n = best_params$min_n
#   ) |>
#     set_mode("classification") |>
#     set_engine("ranger", importance = "permutation", seed = 42, num.threads = 1)
#
#   final_wf < workflow() |> add_recipe(final_recipe) |> add_model(final_model)
#   final_fit <- final_wf |> fit(data = train_data)
#
#   cli::cli_alrt_success("Final model fit completed.")
#
#   train_pred < predict(final_fit, train_data)
#   test_pred <- predict(final_fit, test_data)
#   # Confusion Matrix
#   conf_test <- confusionMatrix(test_pred$.pred_class, test_data[[response]])
#   write_csv(
#     as_tibble(conf_test$table),
#     here("Results/rf", paste0(label, "_confusion_matrix.csv"))
#   )
#   write_csv(
#     as_tibble(conf_test$overall),
#     here("Results/rf", paste0(label, "_overall_preformance.csv"))
#   )
#   write_csv(
#     as_tibble(conf_test$byClass),
#     here("Results/rf", paste0(label, "_class_preformance.csv"))
#   )
#
#   bind_rows(
#    train_data |> mutate(.pred_class = train_pred$.pred_class, .set = "train"),
#     test_data |> mutate(.pred_class = test_pred$.pred_class, .set = "test")
#   ) |>
#     write_csv(here("Results/rf", paste0(label, "_predictions.csv")))
#
#   tibble(
#     redictor = names(extract_fit_parsnip(final_fit)$fit$variable.importance),
#     importance = extract_fit_parsnip(final_fit)$fit$variable.importance,
#     oobMSE = extract_fit_parsnip(final_fit)$fit$prediction.error
#   ) |>
#     arrange(desc(importance)) |>
#     write_csv(here("Results/rf", paste0(label, "_importance.csv")))
#
#   plan(sequential
#
#   return(list(
#    rfe_fit = rfe_fit,
#     final_fit = final_fit,
#     final_wf = final_wf,
#     selected_vars = selected_vars,
#     response = response,
#     best_params = best_params,
#     train_data = train_data,
#     confusion_matrix = conf_test
#   ))
# }

# Function to extract LE data for plotting
get_ale_data <- function(ale_object) {
  # Get the raw ALE effect data
  ale_effects <- get(ale_object, what = "ale")

  # Process each cluster
  all_data <- map_dfr(names(ale_effects), function(cluster_name) {
    cluster_data <- ale_effects[[cluster_name]]

    # Process each variable for this cluster
    map_dfr(names(cluster_data), function(var_name) {
      var_data <- cluster_data[[var_name]]

      # Create a data frame with the ALE values
      data.frame(
        Cluster = cluster_name,
        variable = var_name,
        x_values = var_data[[paste0(var_name, ".ceil")]], # x-axis values
        ale_values = var_data$.y, # ALE effect values
        n_obs = var_data$.n, # number of observations
        ale_lower = if (".y_lo" %in% names(var_data)) var_data$.y_lo else NA,
        ale_upper = if (".y_hi" %in% names(var_data)) var_data$.y_hi else NA
      )
    })
  }) |>
    as_tibble() |>
    mutate(Cluster = factor(Cluster))

  return(all_data)
}


var_names <- c(
  "water.temp" = "Water Temp",
  "water.temp_0d" = "Water Temp",
  "water.temp_1d" = "1 Day Water Temp",
  "water.temp_2d" = "2 Day Water Temp",
  "water.temp_3d" = "3 Day Water Temp",
  "water.temp_4d" = "4 Day Water Temp",
  "water.temp_5d" = "5 Day Water Temp",
  "water.temp_6d" = "6 Day Water Temp",
  "water.temp_7d" = "7 Day Water Temp",
  "water.temp_8d" = "8 Day Water Temp",
  "water.temp_9d" = "9 Day Water Temp",
  "water.temp_10d" = "10 Day Water Temp",
  "water.temp_11d" = "11 Day Water Temp",
  "water.temp_12d" = "12 Day Water Temp",
  "water.temp_13d" = "13 Day Water Temp",
  "water.temp_14d" = "14 Day Water Temp",
  "water.temp_15d" = "15 Day Water Temp",
  "water.temp_16d" = "16 Day Water Temp",
  "water.temp_17d" = "17 Day Water Temp",
  "water.temp_18d" = "18 Day Water Temp",
  "water.temp_19d" = "19 Day Water Temp",
  "water.temp_20d" = "20 Day Water Temp",
  "water.temp_21d" = "21 Day Water Temp",
  "water.temp_22d" = "22 Day Water Temp",
  "water.temp_23d" = "23 Day Water Temp",
  "water.temp_24d" = "24 Day Water Temp",
  "water.temp_25d" = "25 Day Water Temp",
  "water.temp_26d" = "26 Day Water Temp",
  "water.temp_27d" = "27 Day Water Temp",
  "water.temp_28d" = "28 Day Water Temp",
  "water.temp_29d" = "29 Day Water Temp",
  "water.temp_30d" = "30 Day Water Temp",
  "water.temp_31d" = "31 Day Water Temp",
  "water.temp_32d" = "32 Day Water Temp",
  "water.temp_33d" = "33 Day Water Temp",
  "water.temp_34d" = "34 Day Water Temp",
  "water.temp_35d" = "35 Day Water Temp",
  "water.temp_36d" = "36 Day Water Temp",
  "water.temp_37d" = "37 Day Water Temp",
  "water.temp_38d" = "38 Day Water Temp",
  "water.temp_39d" = "39 Day Water Temp",
  "water.temp_40d" = "40 Day Water Temp",
  "water.temp_41d" = "41 Day Water Temp",
  "water.temp_42d" = "42 Day Water Temp",
  "water.temp_43d" = "43 Day Water Temp",
  "water.temp_44d" = "44 Day Water Temp",
  "water.temp_45d" = "45 Day Water Temp",
  "water.temp_46d" = "46 Day Water Temp",
  "water.temp_47d" = "47 Day Water Temp",
  "water.temp_48d" = "48 Day Water Temp",
  "water.temp_49d" = "49 Day Water Temp",
  "water.temp_50d" = "50 Day Water Temp",
  "water.temp_51d" = "51 Day Water Temp",
  "water.temp_52d" = "52 Day Water Temp",
  "water.temp_53d" = "53 Day Water Temp",
  "water.temp_54d" = "54 Day Water Temp",
  "water.temp_55d" = "55 Day Water Temp",
  "water.temp_56d" = "56 Day Water Temp",
  "water.temp_57d" = "57 Day Water Temp",
  "water.temp_58d" = "58 Day Water Temp",
  "water.temp_59d" = "59 Day Water Temp",
  "water.temp_60d" = "60 Day Water Temp",
  "Q" = "Discharge",
  "Q_0d" = "Discharge",
  "Q_1d" = "1 Day Discharge",
  "Q_2d" = "2 Day Discharge",
  "Q_3d" = "3 Day Discharge",
  "Q_4d" = "4 Day Discharge",
  "Q_5d" = "5 Day Discharge",
  "Q_6d" = "6 Day Discharge",
  "Q_7d" = "7 Day Discharge",
  "Q_8d" = "8 Day Discharge",
  "Q_9d" = "9 Day Discharge",
  "Q_10d" = "10 Day Discharge",
  "Q_11d" = "11 Day Discharge",
  "Q_12d" = "12 Day Discharge",
  "Q_13d" = "13 Day Discharge",
  "Q_14d" = "14 Day Discharge",
  "Q_15d" = "15 Day Discharge",
  "Q_16d" = "16 Day Discharge",
  "Q_17d" = "17 Day Discharge",
  "Q_18d" = "18 Day Discharge",
  "Q_19d" = "19 Day Discharge",
  "Q_20d" = "20 Day Discharge",
  "Q_21d" = "21 Day Discharge",
  "Q_22d" = "22 Day Discharge",
  "Q_23d" = "23 Day Discharge",
  "Q_24d" = "24 Day Discharge",
  "Q_25d" = "25 Day Discharge",
  "Q_26d" = "26 Day Discharge",
  "Q_27d" = "27 Day Discharge",
  "Q_28d" = "28 Day Discharge",
  "Q_29d" = "29 Day Discharge",
  "Q_30d" = "30 Day Discharge",
  "Q_31d" = "31 Day Discharge",
  "Q_32d" = "32 Day Discharge",
  "Q_33d" = "33 Day Discharge",
  "Q_34d" = "34 Day Discharge",
  "Q_35d" = "35 Day Discharge",
  "Q_36d" = "36 Day Discharge",
  "Q_37d" = "37 Day Discharge",
  "Q_38d" = "38 Day Discharge",
  "Q_39d" = "39 Day Discharge",
  "Q_40d" = "40 Day Discharge",
  "Q_41d" = "41 Day Discharge",
  "Q_42d" = "42 Day Discharge",
  "Q_43d" = "43 Day Discharge",
  "Q_44d" = "44 Day Discharge",
  "Q_45d" = "45 Day Discharge",
  "Q_46d" = "46 Day Discharge",
  "Q_47d" = "47 Day Discharge",
  "Q_48d" = "48 Day Discharge",
  "Q_49d" = "49 Day Discharge",
  "Q_50d" = "50 Day Discharge",
  "Q_51d" = "51 Day Discharge",
  "Q_52d" = "52 Day Discharge",
  "Q_53d" = "53 Day Discharge",
  "Q_54d" = "54 Day Discharge",
  "Q_55d" = "55 Day Discharge",
  "Q_56d" = "56 Day Discharge",
  "Q_57d" = "57 Day Discharge",
  "Q_58d" = "58 Day Discharge",
  "Q_59d" = "59 Day Discharge",
  "Q_60d" = "60 Day Discharge",
  "PPT" = "PPT",
  "PPT_0d" = "PPT",
  "PPT_1d" = "1 Day PPT",
  "PPT_2d" = "2 Day PPT",
  "PPT_3d" = "3 Day PPT",
  "PPT_4d" = "4 Day PPT",
  "PPT_5d" = "5 Day PPT",
  "PPT_6d" = "6 Day PPT",
  "PPT_7d" = "7 Day PPT",
  "PPT_8d" = "8 Day PPT",
  "PPT_9d" = "9 Day PPT",
  "PPT_10d" = "10 Day PPT",
  "PPT_11d" = "11 Day PPT",
  "PPT_12d" = "12 Day PPT",
  "PPT_13d" = "13 Day PPT",
  "PPT_14d" = "14 Day PPT",
  "PPT_15d" = "15 Day PPT",
  "PPT_16d" = "16 Day PPT",
  "PPT_17d" = "17 Day PPT",
  "PPT_18d" = "18 Day PPT",
  "PPT_19d" = "19 Day PPT",
  "PPT_20d" = "20 Day PPT",
  "PPT_21d" = "21 Day PPT",
  "PPT_22d" = "22 Day PPT",
  "PPT_23d" = "23 Day PPT",
  "PPT_24d" = "24 Day PPT",
  "PPT_25d" = "25 Day PPT",
  "PPT_26d" = "26 Day PPT",
  "PPT_27d" = "27 Day PPT",
  "PPT_28d" = "28 Day PPT",
  "PPT_29d" = "29 Day PPT",
  "PPT_30d" = "30 Day PPT",
  "PPT_31d" = "31 Day PPT",
  "PPT_32d" = "32 Day PPT",
  "PPT_33d" = "33 Day PPT",
  "PPT_34d" = "34 Day PPT",
  "PPT_35d" = "35 Day PPT",
  "PPT_36d" = "36 Day PPT",
  "PPT_37d" = "37 Day PPT",
  "PPT_38d" = "38 Day PPT",
  "PPT_39d" = "39 Day PPT",
  "PPT_40d" = "40 Day PPT",
  "PPT_41d" = "41 Day PPT",
  "PPT_42d" = "42 Day PPT",
  "PPT_43d" = "43 Day PPT",
  "PPT_44d" = "44 Day PPT",
  "PPT_45d" = "45 Day PPT",
  "PPT_46d" = "46 Day PPT",
  "PPT_47d" = "47 Day PPT",
  "PPT_48d" = "48 Day PPT",
  "PPT_49d" = "49 Day PPT",
  "PPT_50d" = "50 Day PPT",
  "PPT_51d" = "51 Day PPT",
  "PPT_52d" = "52 Day PPT",
  "PPT_53d" = "53 Day PPT",
  "PPT_54d" = "54 Day PPT",
  "PPT_55d" = "55 Day PPT",
  "PPT_56d" = "56 Day PPT",
  "PPT_57d" = "57 Day PPT",
  "PPT_58d" = "58 Day PPT",
  "PPT_59d" = "59 Day PPT",
  "PPT_60d" = "60 Day PPT",
  "ET" = "ET",
  "ET_0d" = "ET",
  "ET_1d" = "1 Day ET",
  "ET_2d" = "2 Day ET",
  "ET_3d" = "3 Day ET",
  "ET_4d" = "4 Day ET",
  "ET_5d" = "5 Day ET",
  "ET_6d" = "6 Day ET",
  "ET_7d" = "7 Day ET",
  "ET_8d" = "8 Day ET",
  "ET_9d" = "9 Day ET",
  "ET_10d" = "10 Day ET",
  "ET_11d" = "11 Day ET",
  "ET_12d" = "12 Day ET",
  "ET_13d" = "13 Day ET",
  "ET_14d" = "14 Day ET",
  "ET_15d" = "15 Day ET",
  "ET_16d" = "16 Day ET",
  "ET_17d" = "17 Day ET",
  "ET_18d" = "18 Day ET",
  "ET_19d" = "19 Day ET",
  "ET_20d" = "20 Day ET",
  "ET_21d" = "21 Day ET",
  "ET_22d" = "22 Day ET",
  "ET_23d" = "23 Day ET",
  "ET_24d" = "24 Day ET",
  "ET_25d" = "25 Day ET",
  "ET_26d" = "26 Day ET",
  "ET_27d" = "27 Day ET",
  "ET_28d" = "28 Day ET",
  "ET_29d" = "29 Day ET",
  "ET_30d" = "30 Day ET",
  "ET_31d" = "31 Day ET",
  "ET_32d" = "32 Day ET",
  "ET_33d" = "33 Day ET",
  "ET_34d" = "34 Day ET",
  "ET_35d" = "35 Day ET",
  "ET_36d" = "36 Day ET",
  "ET_37d" = "37 Day ET",
  "ET_38d" = "38 Day ET",
  "ET_39d" = "39 Day ET",
  "ET_40d" = "40 Day ET",
  "ET_41d" = "41 Day ET",
  "ET_42d" = "42 Day ET",
  "ET_43d" = "43 Day ET",
  "ET_44d" = "44 Day ET",
  "ET_45d" = "45 Day ET",
  "ET_46d" = "46 Day ET",
  "ET_47d" = "47 Day ET",
  "ET_48d" = "48 Day ET",
  "ET_49d" = "49 Day ET",
  "ET_50d" = "50 Day ET",
  "ET_51d" = "51 Day ET",
  "ET_52d" = "52 Day ET",
  "ET_53d" = "53 Day ET",
  "ET_54d" = "54 Day ET",
  "ET_55d" = "55 Day ET",
  "ET_56d" = "56 Day ET",
  "ET_57d" = "57 Day ET",
  "ET_58d" = "58 Day ET",
  "ET_59d" = "59 Day ET",
  "ET_60d" = "60 Day ET",
  "Depth" = "Depth",
  "Depth_0d" = "Depth",
  "Depth_1d" = "1 Day Depth",
  "Depth_2d" = "2 Day Depth",
  "Depth_3d" = "3 Day Depth",
  "Depth_4d" = "4 Day Depth",
  "Depth_5d" = "5 Day Depth",
  "Depth_6d" = "6 Day Depth",
  "Depth_7d" = "7 Day Depth",
  "Depth_8d" = "8 Day Depth",
  "Depth_9d" = "9 Day Depth",
  "Depth_10d" = "10 Day Depth",
  "Depth_11d" = "11 Day Depth",
  "Depth_12d" = "12 Day Depth",
  "Depth_13d" = "13 Day Depth",
  "Depth_14d" = "14 Day Depth",
  "Depth_15d" = "15 Day Depth",
  "Depth_16d" = "16 Day Depth",
  "Depth_17d" = "17 Day Depth",
  "Depth_18d" = "18 Day Depth",
  "Depth_19d" = "19 Day Depth",
  "Depth_20d" = "20 Day Depth",
  "Depth_21d" = "21 Day Depth",
  "Depth_22d" = "22 Day Depth",
  "Depth_23d" = "23 Day Depth",
  "Depth_24d" = "24 Day Depth",
  "Depth_25d" = "25 Day Depth",
  "Depth_26d" = "26 Day Depth",
  "Depth_27d" = "27 Day Depth",
  "Depth_28d" = "28 Day Depth",
  "Depth_29d" = "29 Day Depth",
  "Depth_30d" = "30 Day Depth",
  "Depth_31d" = "31 Day Depth",
  "Depth_32d" = "32 Day Depth",
  "Depth_33d" = "33 Day Depth",
  "Depth_34d" = "34 Day Depth",
  "Depth_35d" = "35 Day Depth",
  "Depth_36d" = "36 Day Depth",
  "Depth_37d" = "37 Day Depth",
  "Depth_38d" = "38 Day Depth",
  "Depth_39d" = "39 Day Depth",
  "Depth_40d" = "40 Day Depth",
  "Depth_41d" = "41 Day Depth",
  "Depth_42d" = "42 Day Depth",
  "Depth_43d" = "43 Day Depth",
  "Depth_44d" = "44 Day Depth",
  "Depth_45d" = "45 Day Depth",
  "Depth_46d" = "46 Day Depth",
  "Depth_47d" = "47 Day Depth",
  "Depth_48d" = "48 Day Depth",
  "Depth_49d" = "49 Day Depth",
  "Depth_50d" = "50 Day Depth",
  "Depth_51d" = "51 Day Depth",
  "Depth_52d" = "52 Day Depth",
  "Depth_53d" = "53 Day Depth",
  "Depth_54d" = "54 Day Depth",
  "Depth_55d" = "55 Day Depth",
  "Depth_56d" = "56 Day Depth",
  "Depth_57d" = "57 Day Depth",
  "Depth_58d" = "58 Day Depth",
  "Depth_59d" = "59 Day Depth",
  "Depth_60d" = "60 Day Depth",
  "headGradient" = "Head Gradient",
  "headGradient_0d" = "Head Gradient",
  "headGradient_1d" = "1 Day Head Gradient",
  "headGradient_2d" = "2 Day Head Gradient",
  "headGradient_3d" = "3 Day Head Gradient",
  "headGradient_4d" = "4 Day Head Gradient",
  "headGradient_5d" = "5 Day Head Gradient",
  "headGradient_6d" = "6 Day Head Gradient",
  "headGradient_7d" = "7 Day Head Gradient",
  "headGradient_8d" = "8 Day Head Gradient",
  "headGradient_9d" = "9 Day Head Gradient",
  "headGradient_10d" = "10 Day Head Gradient",
  "headGradient_11d" = "11 Day Head Gradient",
  "headGradient_12d" = "12 Day Head Gradient",
  "headGradient_13d" = "13 Day Head Gradient",
  "headGradient_14d" = "14 Day Head Gradient",
  "headGradient_15d" = "15 Day Head Gradient",
  "headGradient_16d" = "16 Day Head Gradient",
  "headGradient_17d" = "17 Day Head Gradient",
  "headGradient_18d" = "18 Day Head Gradient",
  "headGradient_19d" = "19 Day Head Gradient",
  "headGradient_20d" = "20 Day Head Gradient",
  "headGradient_21d" = "21 Day Head Gradient",
  "headGradient_22d" = "22 Day Head Gradient",
  "headGradient_23d" = "23 Day Head Gradient",
  "headGradient_24d" = "24 Day Head Gradient",
  "headGradient_25d" = "25 Day Head Gradient",
  "headGradient_26d" = "26 Day Head Gradient",
  "headGradient_27d" = "27 Day Head Gradient",
  "headGradient_28d" = "28 Day Head Gradient",
  "headGradient_29d" = "29 Day Head Gradient",
  "headGradient_30d" = "30 Day Head Gradient",
  "headGradient_31d" = "31 Day Head Gradient",
  "headGradient_32d" = "32 Day Head Gradient",
  "headGradient_33d" = "33 Day Head Gradient",
  "headGradient_34d" = "34 Day Head Gradient",
  "headGradient_35d" = "35 Day Head Gradient",
  "headGradient_36d" = "36 Day Head Gradient",
  "headGradient_37d" = "37 Day Head Gradient",
  "headGradient_38d" = "38 Day Head Gradient",
  "headGradient_39d" = "39 Day Head Gradient",
  "headGradient_40d" = "40 Day Head Gradient",
  "headGradient_41d" = "41 Day Head Gradient",
  "headGradient_42d" = "42 Day Head Gradient",
  "headGradient_43d" = "43 Day Head Gradient",
  "headGradient_44d" = "44 Day Head Gradient",
  "headGradient_45d" = "45 Day Head Gradient",
  "headGradient_46d" = "46 Day Head Gradient",
  "headGradient_47d" = "47 Day Head Gradient",
  "headGradient_48d" = "48 Day Head Gradient",
  "headGradient_49d" = "49 Day Head Gradient",
  "headGradient_50d" = "50 Day Head Gradient",
  "headGradient_51d" = "51 Day Head Gradient",
  "headGradient_52d" = "52 Day Head Gradient",
  "headGradient_53d" = "53 Day Head Gradient",
  "headGradient_54d" = "54 Day Head Gradient",
  "headGradient_55d" = "55 Day Head Gradient",
  "headGradient_56d" = "56 Day Head Gradient",
  "headGradient_57d" = "57 Day Head Gradient",
  "headGradient_58d" = "58 Day Head Gradient",
  "headGradient_59d" = "59 Day Head Gradient",
  "headGradient_60d" = "60 Day Head Gradient",
  "avgPar" = "Light",
  "avgPar_0d" = "Light",
  "avgPar_1d" = "1 Day Light",
  "avgPar_2d" = "2 Day Light",
  "avgPar_3d" = "3 Day Light",
  "avgPar_4d" = "4 Day Light",
  "avgPar_5d" = "5 Day Light",
  "avgPar_6d" = "6 Day Light",
  "avgPar_7d" = "7 Day Light",
  "avgPar_8d" = "8 Day Light",
  "avgPar_9d" = "9 Day Light",
  "avgPar_10d" = "10 Day Light",
  "avgPar_11d" = "11 Day Light",
  "avgPar_12d" = "12 Day Light",
  "avgPar_13d" = "13 Day Light",
  "avgPar_14d" = "14 Day Light",
  "avgPar_15d" = "15 Day Light",
  "avgPar_16d" = "16 Day Light",
  "avgPar_17d" = "17 Day Light",
  "avgPar_18d" = "18 Day Light",
  "avgPar_19d" = "19 Day Light",
  "avgPar_20d" = "20 Day Light",
  "avgPar_21d" = "21 Day Light",
  "avgPar_22d" = "22 Day Light",
  "avgPar_23d" = "23 Day Light",
  "avgPar_24d" = "24 Day Light",
  "avgPar_25d" = "25 Day Light",
  "avgPar_26d" = "26 Day Light",
  "avgPar_27d" = "27 Day Light",
  "avgPar_28d" = "28 Day Light",
  "avgPar_29d" = "29 Day Light",
  "avgPar_30d" = "30 Day Light",
  "avgPar_31d" = "31 Day Light",
  "avgPar_32d" = "32 Day Light",
  "avgPar_33d" = "33 Day Light",
  "avgPar_34d" = "34 Day Light",
  "avgPar_35d" = "35 Day Light",
  "avgPar_36d" = "36 Day Light",
  "avgPar_37d" = "37 Day Light",
  "avgPar_38d" = "38 Day Light",
  "avgPar_39d" = "39 Day Light",
  "avgPar_40d" = "40 Day Light",
  "avgPar_41d" = "41 Day Light",
  "avgPar_42d" = "42 Day Light",
  "avgPar_43d" = "43 Day Light",
  "avgPar_44d" = "44 Day Light",
  "avgPar_45d" = "45 Day Light",
  "avgPar_46d" = "46 Day Light",
  "avgPar_47d" = "47 Day Light",
  "avgPar_48d" = "48 Day Light",
  "avgPar_49d" = "49 Day Light",
  "avgPar_50d" = "50 Day Light",
  "avgPar_51d" = "51 Day Light",
  "avgPar_52d" = "52 Day Light",
  "avgPar_53d" = "53 Day Light",
  "avgPar_54d" = "54 Day Light",
  "avgPar_55d" = "55 Day Light",
  "avgPar_56d" = "56 Day Light",
  "avgPar_57d" = "57 Day Light",
  "avgPar_58d" = "58 Day Light",
  "avgPar_59d" = "59 Day Light",
  "avgPar_60d" = "60 Day Light",
  "active_network_length_m" = "ASDN",
  "active_network_length_m_0d" = "ASDN",
  "active_network_length_m_1d" = "1 Day ASDN",
  "active_network_length_m_2d" = "2 Day ASDN",
  "active_network_length_m_3d" = "3 Day ASDN",
  "active_network_length_m_4d" = "4 Day ASDN",
  "active_network_length_m_5d" = "5 Day ASDN",
  "active_network_length_m_6d" = "6 Day ASDN",
  "active_network_length_m_7d" = "7 Day ASDN",
  "active_network_length_m_8d" = "8 Day ASDN",
  "active_network_length_m_9d" = "9 Day ASDN",
  "active_network_length_m_10d" = "10 Day ASDN",
  "active_network_length_m_11d" = "11 Day ASDN",
  "active_network_length_m_12d" = "12 Day ASDN",
  "active_network_length_m_13d" = "13 Day ASDN",
  "active_network_length_m_14d" = "14 Day ASDN",
  "active_network_length_m_15d" = "15 Day ASDN",
  "active_network_length_m_16d" = "16 Day ASDN",
  "active_network_length_m_17d" = "17 Day ASDN",
  "active_network_length_m_18d" = "18 Day ASDN",
  "active_network_length_m_19d" = "19 Day ASDN",
  "active_network_length_m_20d" = "20 Day ASDN",
  "active_network_length_m_21d" = "21 Day ASDN",
  "active_network_length_m_22d" = "22 Day ASDN",
  "active_network_length_m_23d" = "23 Day ASDN",
  "active_network_length_m_24d" = "24 Day ASDN",
  "active_network_length_m_25d" = "25 Day ASDN",
  "active_network_length_m_26d" = "26 Day ASDN",
  "active_network_length_m_27d" = "27 Day ASDN",
  "active_network_length_m_28d" = "28 Day ASDN",
  "active_network_length_m_29d" = "29 Day ASDN",
  "active_network_length_m_30d" = "30 Day ASDN",
  "active_network_length_m_31d" = "31 Day ASDN",
  "active_network_length_m_32d" = "32 Day ASDN",
  "active_network_length_m_33d" = "33 Day ASDN",
  "active_network_length_m_34d" = "34 Day ASDN",
  "active_network_length_m_35d" = "35 Day ASDN",
  "active_network_length_m_36d" = "36 Day ASDN",
  "active_network_length_m_37d" = "37 Day ASDN",
  "active_network_length_m_38d" = "38 Day ASDN",
  "active_network_length_m_39d" = "39 Day ASDN",
  "active_network_length_m_40d" = "40 Day ASDN",
  "active_network_length_m_41d" = "41 Day ASDN",
  "active_network_length_m_42d" = "42 Day ASDN",
  "active_network_length_m_43d" = "43 Day ASDN",
  "active_network_length_m_44d" = "44 Day ASDN",
  "active_network_length_m_45d" = "45 Day ASDN",
  "active_network_length_m_46d" = "46 Day ASDN",
  "active_network_length_m_47d" = "47 Day ASDN",
  "active_network_length_m_48d" = "48 Day ASDN",
  "active_network_length_m_49d" = "49 Day ASDN",
  "active_network_length_m_50d" = "50 Day ASDN",
  "active_network_length_m_51d" = "51 Day ASDN",
  "active_network_length_m_52d" = "52 Day ASDN",
  "active_network_length_m_53d" = "53 Day ASDN",
  "active_network_length_m_54d" = "54 Day ASDN",
  "active_network_length_m_55d" = "55 Day ASDN",
  "active_network_length_m_56d" = "56 Day ASDN",
  "active_network_length_m_57d" = "57 Day ASDN",
  "active_network_length_m_58d" = "58 Day ASDN",
  "active_network_length_m_59d" = "59 Day ASDN",
  "active_network_length_m_60d" = "60 Day ASDN"
)
