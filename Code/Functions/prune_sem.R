
plot_sem_graph <- function(sem_fit, round_digits = 2, file = NULL, width = 3600, height = 2338) {
  library(DiagrammeR)
  coefs_df <- coefficients(sem_fit)
  coefs_df <- coefs_df[, !(is.na(colnames(coefs_df)) | colnames(coefs_df) == "")]
  
  # Process coefficients
  coefs_df <- coefs_df |>
    dplyr::filter(!is.na(Predictor), !is.na(Response)) |>
    dplyr::mutate(
      Estimate = as.numeric(Estimate),
      P.Value = as.numeric(P.Value),
      #Std.Estimate = as.numeric(Std.Estimate),
      label = paste0(round(Estimate, round_digits),
                     ifelse(P.Value < 0.001, "***",
                            ifelse(P.Value < 0.01, "**",
                                   ifelse(P.Value < 0.05, "*", "")))),
      color = ifelse(Estimate >= 0, "green3", "firebrick"),
      penwidth = 2.5 * abs(Estimate),
      style = ifelse(P.Value < 0.05, "solid", "dotted")
    )
  
  # Identify all seasonal components
  sin_cos_rows <- which(coefs_df$Predictor %in% c("DOY_sin", "DOY_cos"))
  sin_rows <- which(coefs_df$Predictor == "DOY_sin")
  cos_rows <- which(coefs_df$Predictor == "DOY_cos")
  
  # Find all unique responses that have either sin or cos predictors
  sin_responses <- coefs_df$Response[sin_rows]
  cos_responses <- coefs_df$Response[cos_rows]
  responses_with_seasonal <- unique(c(sin_responses, cos_responses))
  
  # Create a new dataframe for the combined seasonal effects
  season_combined <- data.frame()
  
  for (resp in responses_with_seasonal) {
    sin_exists <- any(coefs_df$Predictor == "DOY_sin" & coefs_df$Response == resp)
    cos_exists <- any(coefs_df$Predictor == "DOY_cos" & coefs_df$Response == resp)
    
    # If both sin and cos exist for this response
    if (sin_exists && cos_exists) {
      sin_row <- coefs_df[which(coefs_df$Predictor == "DOY_sin" & coefs_df$Response == resp), ]
      cos_row <- coefs_df[which(coefs_df$Predictor == "DOY_cos" & coefs_df$Response == resp), ]
      
      # Calculate magnitude of combined seasonal effect (Pythagorean theorem)
      combined_magnitude <- sqrt(sin_row$Std.Estimate^2 + cos_row$Std.Estimate^2)
      
      # Determine combined p-value (conservative approach - use the smaller p-value)
      combined_p <- min(sin_row$P.Value, cos_row$P.Value)
      
      # Determine dominant direction (use the term with larger absolute effect)
      dominant_direction <- ifelse(abs(sin_row$Std.Estimate) > abs(cos_row$Std.Estimate), 
                                   sign(sin_row$Std.Estimate), 
                                   sign(cos_row$Std.Estimate))
      
      # Create template row (could be either sin or cos)
      template_row <- sin_row
    } else if (sin_exists) {
      # If only sin exists
      template_row <- coefs_df[which(coefs_df$Predictor == "DOY_sin" & coefs_df$Response == resp), ]
      combined_magnitude <- abs(template_row$Std.Estimate)
      combined_p <- template_row$P.Value
      dominant_direction <- sign(template_row$Std.Estimate)
    } else {
      # If only cos exists
      template_row <- coefs_df[which(coefs_df$Predictor == "DOY_cos" & coefs_df$Response == resp), ]
      combined_magnitude <- abs(template_row$Std.Estimate)
      combined_p <- template_row$P.Value
      dominant_direction <- sign(template_row$Std.Estimate)
    }
    
    combined_estimate <- dominant_direction * combined_magnitude
    
    # Create new row
    new_row <- template_row
    new_row$Predictor <- "Season"
    new_row$Std.Estimate <- combined_estimate
    new_row$Estimate <- combined_estimate # Simplified for display
    new_row$P.Value <- combined_p
    new_row$label <- paste0(round(combined_estimate, round_digits),
                            ifelse(combined_p < 0.001, "***",
                                   ifelse(combined_p < 0.01, "**",
                                          ifelse(combined_p < 0.05, "*", ""))))
    new_row$color <- ifelse(combined_estimate >= 0, "green3", "firebrick")
    new_row$penwidth <- 10 * abs(combined_estimate)
    new_row$style <- ifelse(combined_p < 0.05, "solid", "dotted")
    
    season_combined <- rbind(season_combined, new_row)
  }
  
  # Remove ALL original DOY_sin and DOY_cos rows
  coefs_df <- coefs_df[!(coefs_df$Predictor %in% c("DOY_sin", "DOY_cos")), ]
  
  # Add the combined seasonal effects
  coefs_df <- rbind(coefs_df, season_combined)
  
  # Create digraph
  dot_lines <- c(
    "digraph SEM {", 
    "graph [layout = dot, rankdir = TB, splines = true];",
    "node [shape = box, style = filled, fillcolor = lightgray, fontsize = 32, width = 1.5, height = 0.8];",
    "edge [fontsize = 28];"
  )
  
  for (i in seq_len(nrow(coefs_df))) {
    dot_lines <- c(dot_lines,
                   paste0('"', coefs_df$Predictor[i], '" -> "', coefs_df$Response[i],
                          '" [label = "', coefs_df$label[i],
                          '", color = "', coefs_df$color[i],
                          '", penwidth = ', (round(coefs_df$penwidth[i], 2)),
                          ', style = ', coefs_df$style[i], '];'
                   )
    )
  }
  
  dot_lines <- c(dot_lines, "}")
  dot_string <- paste(dot_lines, collapse = "\n")
  
  graph <- DiagrammeR::grViz(dot_string)
  
  # Save to PNG if filename is provided
  if (!is.null(file)) {
    svg <- DiagrammeRsvg::export_svg(graph)
    svg_char <- charToRaw(svg)
    rsvg::rsvg_png(svg_char, file = file, width = width, height = height)
  }
  
  return(graph)
}

plot_lavaan <- function(sem_fit, round_digits = 2, file = NULL, width = 3600, height = 2338) {
  # Get parameter estimates
  coefs_df <- lavaan::parameterEstimates(sem_fit, standardized = TRUE)
  
  # Keep only regression paths (i.e., regressions)
  coefs_df <- coefs_df |>
    subset(op == "~" & !is.na(lhs) & !is.na(rhs)) |>
    transform(
      Predictor = rhs,
      Response = lhs,
      Estimate = est,
      Std.Estimate = std.all,
      P.Value = pvalue
    )
  
  # Add aesthetics
  coefs_df <- coefs_df |>
    transform(
      label = paste0(round(Estimate, round_digits),
                     ifelse(P.Value < 0.001, "***",
                            ifelse(P.Value < 0.01, "**",
                                   ifelse(P.Value < 0.05, "*", "")))),
      color = ifelse(Estimate >= 0, "green3", "firebrick"),
      penwidth = 10 * abs(Std.Estimate),
      style = ifelse(P.Value < 0.05, "solid", "dotted")
    )
  
  # Seasonal component aggregation
  sin_cos_rows <- which(coefs_df$Predictor %in% c("DOY_sin", "DOY_cos"))
  sin_rows <- which(coefs_df$Predictor == "DOY_sin")
  cos_rows <- which(coefs_df$Predictor == "DOY_cos")
  
  sin_responses <- coefs_df$Response[sin_rows]
  cos_responses <- coefs_df$Response[cos_rows]
  responses_with_seasonal <- unique(c(sin_responses, cos_responses))
  
  season_combined <- data.frame()
  
  for (resp in responses_with_seasonal) {
    sin_exists <- any(coefs_df$Predictor == "DOY_sin" & coefs_df$Response == resp)
    cos_exists <- any(coefs_df$Predictor == "DOY_cos" & coefs_df$Response == resp)
    
    if (sin_exists && cos_exists) {
      sin_row <- coefs_df[coefs_df$Predictor == "DOY_sin" & coefs_df$Response == resp, ]
      cos_row <- coefs_df[coefs_df$Predictor == "DOY_cos" & coefs_df$Response == resp, ]
      
      combined_magnitude <- sqrt(sin_row$Std.Estimate^2 + cos_row$Std.Estimate^2)
      combined_p <- min(sin_row$P.Value, cos_row$P.Value)
      dominant_direction <- ifelse(abs(sin_row$Std.Estimate) > abs(cos_row$Std.Estimate),
                                   sign(sin_row$Std.Estimate),
                                   sign(cos_row$Std.Estimate))
      template_row <- sin_row
    } else if (sin_exists) {
      template_row <- coefs_df[coefs_df$Predictor == "DOY_sin" & coefs_df$Response == resp, ]
      combined_magnitude <- abs(template_row$Std.Estimate)
      combined_p <- template_row$P.Value
      dominant_direction <- sign(template_row$Std.Estimate)
    } else {
      template_row <- coefs_df[coefs_df$Predictor == "DOY_cos" & coefs_df$Response == resp, ]
      combined_magnitude <- abs(template_row$Std.Estimate)
      combined_p <- template_row$P.Value
      dominant_direction <- sign(template_row$Std.Estimate)
    }
    
    combined_estimate <- dominant_direction * combined_magnitude
    
    new_row <- template_row
    new_row$Predictor <- "DOY (sin and cos)"
    new_row$Std.Estimate <- combined_estimate
    new_row$Estimate <- combined_estimate
    new_row$P.Value <- combined_p
    new_row$label <- paste0(round(combined_estimate, round_digits),
                            ifelse(combined_p < 0.001, "***",
                                   ifelse(combined_p < 0.01, "**",
                                          ifelse(combined_p < 0.05, "*", ""))))
    new_row$color <- ifelse(combined_estimate >= 0, "green3", "firebrick")
    new_row$penwidth <- 10 * abs(combined_estimate)
    new_row$style <- ifelse(combined_p < 0.05, "solid", "dotted")
    
    season_combined <- rbind(season_combined, new_row)
  }
  
  # Remove individual DOY_sin and DOY_cos paths
  coefs_df <- coefs_df[!(coefs_df$Predictor %in% c("DOY_sin", "DOY_cos")), ]
  
  # Add seasonal summary paths
  coefs_df <- rbind(coefs_df, season_combined)
  
  # Build graphviz dot string
  dot_lines <- c(
    "digraph SEM {", 
    "graph [layout = dot, rankdir = TB, splines = true];",
    "node [shape = box, style = filled, fillcolor = lightgray, fontsize = 32, width = 1.5, height = 0.8];",
    "edge [fontsize = 28];"
  )
  
  for (i in seq_len(nrow(coefs_df))) {
    dot_lines <- c(dot_lines,
                   paste0('"', coefs_df$Predictor[i], '" -> "', coefs_df$Response[i],
                          '" [label = "', coefs_df$label[i],
                          '", color = "', coefs_df$color[i],
                          '", penwidth = ', round(coefs_df$penwidth[i], 2),
                          ', style = ', coefs_df$style[i], '];')
    )
  }
  
  dot_lines <- c(dot_lines, "}")
  dot_string <- paste(dot_lines, collapse = "\n")
  graph <- DiagrammeR::grViz(dot_string)
  
  if (!is.null(file)) {
    svg <- DiagrammeRsvg::export_svg(graph)
    svg_char <- charToRaw(svg)
    rsvg::rsvg_png(svg_char, file = file, width = width, height = height)
  }
  
  return(graph)
}


prune_sem <- function(sem_model, data, alpha = 0.05, max_iter = Inf, parallel = TRUE, cores = NULL) {
  # Load required packages
  library(piecewiseSEM)
  library(lme4)
  library(cli)
  
  # Define helper function to calculate AIC
  get_model_fit <- function(model) {
    aic_result <- piecewiseSEM::AIC_psem(model)
    return(aic_result$AIC)  # Lower is better
  }
  
  # Define helper function to remove paths
  remove_path <- function(model, response_var, predictor_var, data) {
    current_model <- model[[response_var]]
    
    # Store the original call's data name (works for both lm and lmerMod)
    original_data_name <- if (inherits(current_model, "lmerMod")) {
      deparse(current_model@call$data)
    } else {
      deparse(current_model$call$data)
    }
    
    # Make sure we're using the current data (works for both lm and lmerMod)
    if (inherits(current_model, "lmerMod")) {
      current_model@call$data <- data
    } else {
      current_model$call$data <- data
    }
    
    current_formula <- current_model |>
      formula() |>
      deparse() |>
      paste(collapse = "") |>
      gsub("\\s+", " ", x = _) |>
      trimws()
    
    # Handle both lm and lmer models
    if (inherits(current_model, "lmerMod")) {
      # Extract random effects for lmer models
      random_matches <- gregexpr("\\+? ?\\([^\\)]+\\|[^\\)]+\\)", current_formula)[[1]]
      random_part <- if (random_matches[1] != -1) {
        regmatches(current_formula, gregexpr("\\+? ?\\([^\\)]+\\|[^\\)]+\\)", current_formula))[[1]] |>
          paste(collapse = " ")
      } else ""
    } else {
      random_part <- ""
    }
    
    # Modify fixed effects
    fixed_part <- gsub("\\+? ?\\([^\\)]+\\|[^\\)]+\\)", "", current_formula) |>
      gsub("\\s+", " ", x = _) |>
      trimws()
    
    fixed_terms <- fixed_part |>
      sub("^.*~", "", x = _) |>
      strsplit(split = "\\+") |>
      unlist() |>
      trimws()
    
    # Defensive check: remove predictor_var and drop empty
    fixed_terms <- fixed_terms[fixed_terms != predictor_var & fixed_terms != ""]
    
    # If everything is removed, use intercept-only model
    new_fixed <- if (length(fixed_terms) > 0) paste(fixed_terms, collapse = " + ") else "1"
    
    # Create new formula
    new_formula_char <- tryCatch({
      formula_str <- paste(
        sub("~.*", "", current_formula),
        "~",
        new_fixed
      )
      
      # Add random effects back if this is a mixed model
      if (inherits(current_model, "lmerMod") && nchar(random_part) > 0) {
        formula_str <- paste(formula_str, random_part)
      }
      
      formula_str |>
        gsub("\\s+", " ", x = _) |>
        gsub("~ \\+", "~", x = _) |>
        trimws()
    }, error = function(e) {
      warning("Formula creation failed: ", conditionMessage(e))
      return(NULL)
    })
    
    if (is.null(new_formula_char)) {
      return(list(
        model = NULL,
        fit = Inf,
        response = response_var,
        predictor = predictor_var,
        success = FALSE,
        error = "Failed to construct valid formula"
      ))
    }
    
    # Update model
    new_model <- model
    tryCatch({
      if (inherits(current_model, "lmerMod")) {
        new_model[[response_var]] <- suppressWarnings(update(current_model, formula. = as.formula(new_formula_char), 
                                                             data = data))
      } else {
        new_model[[response_var]] <- suppressWarnings(update(current_model, formula. = as.formula(new_formula_char), 
                                                             data = data))
      }
      
      new_model <- suppressWarnings(update(new_model, data = data))
      # Validate model before continuing
      tryCatch({
        new_fit <- get_model_fit(new_model)
      }, error = function(e) {
        stop("Model update succeeded but validation failed: ", conditionMessage(e))
      })
      
      new_fit <- get_model_fit(new_model)
      
      return(list(
        model = new_model,
        fit = new_fit,
        response = response_var,
        predictor = predictor_var,
        success = TRUE,
        error = NULL
      ))
    }, error = function(e) {
      return(list(
        model = NULL,
        fit = Inf,
        response = response_var,
        predictor = predictor_var,
        success = FALSE,
        error = conditionMessage(e)
      ))
    })
  }
  
  # Set up parallel processing
  if (parallel) {
    library(parallel)
    if (is.null(cores)) {
      cores <- detectCores() - 1
      cores <- max(1, cores)  # Ensure at least 1 core
    }
    cl <- makeCluster(cores)
    on.exit(stopCluster(cl), add = TRUE)  # Ensure cluster stops
    
    # Export required objects and functions
    clusterExport(cl, 
                  varlist = c("remove_path", "get_model_fit", "data"),
                  envir = environment())
    
    # Load required packages on workers
    clusterEvalQ(cl, {
      library(piecewiseSEM)
      library(lme4)
    })
    cli::cli_alert_info("Using parallel processing with {cores} cores")
  }
  
  # Initialize pruning process
  current_model <- sem_model
  current_fit <- get_model_fit(current_model)
  changed <- TRUE
  iter <- 0
  warnings_collected <- character()
  paths_removed <- 0
  
  # Main pruning loop
  while (changed && iter < max_iter) {
    iter <- iter + 1
    changed <- FALSE
    sem_summary <- NULL
    local_warnings <- character()
    
    # Get model summary with warning handling
    sem_summary <- withCallingHandlers(
      summary(current_model),
      warning = function(w) {
        local_warnings <<- c(local_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    warnings_collected <- c(warnings_collected, local_warnings)
    
    # Get coefficient table
    coef_table <- sem_summary$coefficients
    
    # Find weak paths (p > alpha)
    # First convert Estimate to numeric safely
    coef_table$Estimate <- as.numeric(as.character(coef_table$Estimate))
    
    # Now create the weak_paths selection
    weak_paths <- suppressWarnings({coef_table |>
        select(1:8) |> 
        subset(
          (as.numeric(P.Value) > alpha |          # Ensure numeric comparison
            abs(as.numeric(Estimate)) < 0.1) &      # Explicit numeric conversion
            !grepl("(Intercept)", Predictor, fixed = TRUE) &
            !grepl("1 \\| Site", Predictor, fixed = TRUE) &
            !grepl("^Season =", Predictor) &       # More specific season exclusion
            Response != "Season"
        ) |>
        dplyr::arrange(desc(as.numeric(P.Value)), abs(Estimate))
    })
    
    if (nrow(weak_paths) > 0) {
      # Prepare path removal tasks
      path_tasks <- lapply(1:nrow(weak_paths), function(i) {
        list(
          response = weak_paths$Response[i],
          predictor = weak_paths$Predictor[i],
          p_value = weak_paths$P.Value[i]
        )
      })
      
      # Evaluate paths in parallel or serial
      if (parallel && length(path_tasks) > 1) {
        results <- parLapply(cl, path_tasks, function(task) {
          remove_path(current_model, task$response, task$predictor, data)
        })
      } else {
        results <- lapply(path_tasks, function(task) {
          remove_path(current_model, task$response, task$predictor, data)
        })
      }
      
      # Find best improvement
      best_idx <- NULL
      best_fit <- current_fit
      
      for (i in seq_along(results)) {
        result <- results[[i]]
        if (result$success && result$fit < best_fit) {
          best_idx <- i
          best_fit <- result$fit
        }
      }
      
      # Apply best improvement if found
      if (!is.null(best_idx)) {
        best_result <- results[[best_idx]]
        task <- path_tasks[[best_idx]]
        
        cli::cli_alert_success("Iteration {iter}: Removed {.val {best_result$predictor}} → {.val {best_result$response}} (p = {sprintf('%.3f', task$p_value)}, AIC improved: {sprintf('%.2f', current_fit)} → {sprintf('%.2f', best_result$fit)})")
        
        current_model <- best_result$model
        current_fit <- best_result$fit
        changed <- TRUE
        paths_removed <- paths_removed + 1
      } else {
        # Report skipped paths
        for (i in seq_along(results)) {
          result <- results[[i]]
          task <- path_tasks[[i]]
          
          if (result$success) {
            cli::cli_alert_info("Skipped removing {.val {result$predictor}} → {.val {result$response}} (p = {sprintf('%.3f', task$p_value)}, AIC would worsen: {sprintf('%.2f', current_fit)} → {sprintf('%.2f', result$fit)})")
          } else {
            cli::cli_alert_warning("Error when trying to remove {.val {result$predictor}} → {.val {result$response}}: {result$error}")
          }
        }
      }
    }
  }
  
  # Final messages
  if (paths_removed == 0) {
    cli::cli_alert_info("No paths were removed. Either all paths are significant at alpha = {.val {alpha}} or removing any path would worsen model fit.")
  } else {
    cli::cli_alert_success("Pruning complete. Removed {.val {paths_removed}} paths. Final AIC: {.val {sprintf('%.2f', current_fit)}}")
  }
  
  if (iter >= max_iter) {
    cli::cli_alert_warning("Reached maximum iterations ({.val {max_iter}}).")
  }
  
  # Show unique warnings if any
  unique_warnings <- unique(warnings_collected)
  if (length(unique_warnings) > 0) {
    cli::cli_h2("Warnings collected during pruning")
    cli::cli_ul()
    purrr::walk(unique_warnings, ~cli::cli_li(.x))
    cli::cli_end()
  }
  
  return(current_model)
}

prune_sem <- function(sem_model, data, alpha = 0.05, max_iter = Inf, parallel = TRUE, cores = NULL) {
  library(piecewiseSEM)
  library(lme4)
  library(cli)
  library(dplyr)
  library(combinat)
  
  get_model_fit <- function(model) {
    aic_result <- piecewiseSEM::AIC_psem(model)
    return(aic_result$AIC)
  }
  
  remove_path <- function(model, response_var, predictor_var, data) {
    current_model <- model[[response_var]]
    if (inherits(current_model, "lmerMod")) {
      current_model@call$data <- data
    } else {
      current_model$call$data <- data
    }
    
    current_formula <- formula(current_model) |> deparse() |> paste(collapse = "") |> gsub("\\s+", " ", x = _) |> trimws()
    
    if (inherits(current_model, "lmerMod")) {
      random_matches <- gregexpr("\\+? ?\\([^\\)]+\\|[^\\)]+\\)", current_formula)[[1]]
      random_part <- if (random_matches[1] != -1) {
        regmatches(current_formula, gregexpr("\\+? ?\\([^\\)]+\\|[^\\)]+\\)", current_formula))[[1]] |> paste(collapse = " ")
      } else ""
    } else {
      random_part <- ""
    }
    
    fixed_part <- gsub("\\+? ?\\([^\\)]+\\|[^\\)]+\\)", "", current_formula) |> gsub("\\s+", " ", x = _) |> trimws()
    
    fixed_terms <- fixed_part |>
      sub("^.*~", "", x = _) |>
      strsplit(split = "\\+") |>
      unlist() |>
      trimws()
    
    fixed_terms <- fixed_terms[fixed_terms != predictor_var & fixed_terms != ""]
    new_fixed <- if (length(fixed_terms) > 0) paste(fixed_terms, collapse = " + ") else "1"
    
    new_formula_char <- tryCatch({
      formula_str <- paste(sub("~.*", "", current_formula), "~", new_fixed)
      if (inherits(current_model, "lmerMod") && nchar(random_part) > 0) {
        formula_str <- paste(formula_str, random_part)
      }
      formula_str |> gsub("\\s+", " ", x = _) |> gsub("~ \\+", "~", x = _) |> trimws()
    }, error = function(e) return(NULL))
    
    if (is.null(new_formula_char)) {
      return(list(model = NULL, fit = Inf, response = response_var, predictor = predictor_var, success = FALSE))
    }
    
    new_model <- model
    tryCatch({
      new_model[[response_var]] <- update(current_model, formula. = as.formula(new_formula_char), data = data)
      new_model <- update(new_model, data = data)
      new_fit <- get_model_fit(new_model)
      return(list(model = new_model, fit = new_fit, response = response_var, predictor = predictor_var, success = TRUE))
    }, error = function(e) {
      return(list(model = NULL, fit = Inf, response = response_var, predictor = predictor_var, success = FALSE))
    })
  }
  
  if (parallel) {
    library(parallel)
    if (is.null(cores)) cores <- max(1, detectCores() - 1)
    cl <- makeCluster(cores)
    on.exit(stopCluster(cl), add = TRUE)
    clusterExport(cl, varlist = c("remove_path", "get_model_fit", "data"), envir = environment())
    clusterEvalQ(cl, { library(piecewiseSEM); library(lme4) })
    cli::cli_alert_info("Using parallel processing with {cores} cores")
  }
  
  current_model <- sem_model
  current_fit <- get_model_fit(current_model)
  iter <- 0
  paths_removed <- 0
  
  while (iter < max_iter) {
    iter <- iter + 1
    sem_summary <- summary(current_model)
    coef_table <- sem_summary$coefficients
    coef_table$Estimate <- as.numeric(as.character(coef_table$Estimate))
    
    weak_paths <- suppressWarnings({
      coef_table |> select(1:8) |> 
        subset((as.numeric(P.Value) > alpha | abs(as.numeric(Estimate)) < 0.1) &
                 !grepl("(Intercept)", Predictor, fixed = TRUE) &
                 !grepl("1 \\| Site", Predictor, fixed = TRUE)) |>
        dplyr::arrange(desc(as.numeric(P.Value)), abs(Estimate))
    })
    
    if (nrow(weak_paths) == 0) break
    
    combos <- unlist(lapply(1:min(nrow(weak_paths), 2), function(k) {
      combn(1:nrow(weak_paths), k, simplify = FALSE)
    }), recursive = FALSE)
    
    evaluate_combo <- function(idxs) {
      trial_model <- current_model
      for (i in idxs) {
        path <- weak_paths[i, ]
        result <- remove_path(trial_model, path$Response, path$Predictor, data)
        if (!result$success) return(NULL)
        trial_model <- result$model
      }
      list(model = trial_model, fit = get_model_fit(trial_model), removed = weak_paths[idxs, ])
    }
    
    results <- if (parallel) {
      parLapply(cl, combos, evaluate_combo)
    } else {
      lapply(combos, evaluate_combo)
    }
    
    results <- Filter(Negate(is.null), results)
    if (length(results) == 0) break
    
    best_result <- results[[which.min(sapply(results, \(x) x$fit))]]
    if (best_result$fit < current_fit) {
      removed_descriptions <- apply(best_result$removed, 1, function(row) paste0(row["Predictor"], " → ", row["Response"]))
      cli::cli_alert_success("Iteration {iter}: Removed {paste(removed_descriptions, collapse = ', ')} (AIC improved: {sprintf('%.2f', current_fit)} → {sprintf('%.2f', best_result$fit)})")
      current_model <- best_result$model
      current_fit <- best_result$fit
      paths_removed <- paths_removed + nrow(best_result$removed)
    } else {
      cli::cli_alert_info("No combination of paths improved AIC this round.")
      break
    }
  }
  
  if (paths_removed == 0) {
    cli::cli_alert_info("No paths were removed. Either all paths are significant at alpha = {.val {alpha}} or removing any path would worsen model fit.")
  } else {
    cli::cli_alert_success("Pruning complete. Removed {.val {paths_removed}} paths. Final AIC: {.val {sprintf('%.2f', current_fit)}}")
  }
  
  return(current_model)
}

prune_sem <- function(sem_model, data, alpha = 0.05, max_iter = Inf, parallel = TRUE, cores = NULL) {
  # Load required packages
  library(piecewiseSEM)
  library(lme4)
  library(nlme)  # Added for lme models
  library(cli)
  
  # Define helper function to calculate AIC
  get_model_fit <- function(model) {
    aic_result <- piecewiseSEM::AIC_psem(model)
    return(aic_result$AIC)  # Lower is better
  }
  
  # Helper function to check model class
  get_model_class <- function(model) {
    if (inherits(model, "lmerMod")) return("lmerMod")
    if (inherits(model, "glmmTMB")) return("glmmTMB")
    if (inherits(model, "lme")) return("lme")
    if (inherits(model, "lm")) return("lm")
    return("unknown")
  }
  
  # Define helper function to remove paths
  remove_path <- function(model, response_var, predictor_var, data) {
    current_model <- model[[response_var]]
    
    # Get model class
    model_class <- get_model_class(current_model)
    
    if (model_class == "unknown") {
      return(list(
        model = NULL,
        fit = Inf,
        response = response_var,
        predictor = predictor_var,
        success = FALSE,
        error = paste("Unsupported model class:", class(current_model)[1])
      ))
    }
    
    # Get the current formula as a string
    current_formula <- formula(current_model) |>
      deparse() |>
      paste(collapse = "") |>
      gsub("\\s+", " ", x = _) |>
      trimws()
    
    # Handle random effects differently based on model type
    random_part <- ""
    if (model_class == "lmerMod") {
      # Extract random effects for lmer models
      random_matches <- gregexpr("\\+? ?\\([^\\)]+\\|[^\\)]+\\)", current_formula)[[1]]
      if (random_matches[1] != -1) {
        random_part <- regmatches(current_formula, 
                                  gregexpr("\\+? ?\\([^\\)]+\\|[^\\)]+\\)", current_formula))[[1]] |>
          paste(collapse = " ")
      }
    }
    
    # Modify fixed effects
    fixed_part <- gsub("\\+? ?\\([^\\)]+\\|[^\\)]+\\)", "", current_formula) |>
      gsub("\\s+", " ", x = _) |>
      trimws()
    
    # Extract the response and fixed effects
    response_str <- sub("^(.*) ~.*$", "\\1", fixed_part) |> trimws()
    
    fixed_terms <- fixed_part |>
      sub("^.*~", "", x = _) |>
      strsplit(split = "\\+") |>
      unlist() |>
      trimws()
    
    # Remove the target predictor and empty terms
    fixed_terms <- fixed_terms[fixed_terms != predictor_var & fixed_terms != ""]
    
    # If everything is removed, use intercept-only model
    new_fixed <- if (length(fixed_terms) > 0) paste(fixed_terms, collapse = " + ") else "1"
    
    # Create new formula string
    new_formula_str <- paste(response_str, "~", new_fixed)
    
    # Add random effects back for lmer models
    if (model_class == "lmerMod" && nchar(random_part) > 0) {
      new_formula_str <- paste(new_formula_str, random_part)
    }
    
    # Clean up the formula string
    new_formula_str <- new_formula_str |>
      gsub("\\s+", " ", x = _) |>
      gsub("~ \\+", "~", x = _) |>
      trimws()
    
    # Convert to formula object
    new_formula <- as.formula(new_formula_str)
    
    # Create updated model based on model type
    new_model <- model
    tryCatch({
      if (model_class == "lm" || model_class == "lmerMod") {
        # For lm and lmerMod, update works with formula.
        new_model[[response_var]] <- update(current_model, formula. = new_formula, data = data)
      } else if (model_class == "lme") {
        # For lme models, we need to recreate the model
        # Extract all important parts from the original model call
        orig_call <- current_model$call
        
        # Create a new call to lme with the new formula
        new_call <- quote(nlme::lme())
        new_call$fixed <- new_formula
        
        # Copy over all other arguments from the original call except 'fixed'
        for (arg_name in names(orig_call)[-1]) {  # Skip the first element (function name)
          if (arg_name != "fixed") {
            new_call[[arg_name]] <- orig_call[[arg_name]]
          }
        }
        
        # Set data explicitly
        new_call$data <- quote(data)
        
        # Evaluate the new call
        new_model[[response_var]] <- eval(new_call)
      }
      
      # Update the full SEM model with the new component
      new_model <- update(new_model, data = data)
      
      # Calculate the new model fit
      new_fit <- get_model_fit(new_model)
      
      return(list(
        model = new_model,
        fit = new_fit,
        response = response_var,
        predictor = predictor_var,
        success = TRUE,
        error = NULL
      ))
    }, error = function(e) {
      return(list(
        model = NULL,
        fit = Inf,
        response = response_var,
        predictor = predictor_var,
        success = FALSE,
        error = conditionMessage(e)
      ))
    })
  }
  
  # Set up parallel processing
  if (parallel) {
    library(parallel)
    if (is.null(cores)) {
      cores <- detectCores() - 1
      cores <- max(1, cores)  # Ensure at least 1 core
    }
    cl <- makeCluster(cores)
    on.exit(stopCluster(cl), add = TRUE)  # Ensure cluster stops
    
    # Export required objects and functions
    clusterExport(cl, 
                  varlist = c("remove_path", "get_model_fit", "get_model_class", "data"),
                  envir = environment())
    
    # Load required packages on workers
    clusterEvalQ(cl, {
      library(piecewiseSEM)
      library(lme4)
      library(nlme)
    })
    cli::cli_alert_info("Using parallel processing with {cores} cores")
  }
  
  # Initialize pruning process
  current_model <- sem_model
  current_fit <- get_model_fit(current_model)
  changed <- TRUE
  iter <- 0
  warnings_collected <- character()
  paths_removed <- 0
  
  # Main pruning loop
  while (changed && iter < max_iter) {
    iter <- iter + 1
    changed <- FALSE
    sem_summary <- NULL
    local_warnings <- character()
    
    # Get model summary with warning handling
    sem_summary <- withCallingHandlers(
      summary(current_model),
      warning = function(w) {
        local_warnings <<- c(local_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    warnings_collected <- c(warnings_collected, local_warnings)
    
    # Get coefficient table
    coef_table <- sem_summary$coefficients
    
    # Find weak paths (p > alpha)
    # First convert Estimate to numeric safely
    coef_table$Estimate <- as.numeric(as.character(coef_table$Estimate))
    
    # Now create the weak_paths selection
    weak_paths <- suppressWarnings({coef_table |>
        select(1:8) |> 
        subset(
          as.numeric(P.Value) > alpha &          # Ensure numeric comparison
            abs(as.numeric(Estimate)) < 0.1 &      # Explicit numeric conversion
            !grepl("(Intercept)", Predictor, fixed = TRUE) &
            !grepl("1 \\| Site", Predictor, fixed = TRUE) &
            !grepl("^Season =", Predictor) &       # More specific season exclusion
            Response != "Season"
        ) |>
        dplyr::arrange(desc(as.numeric(P.Value)), abs(Estimate))
    })
    
    if (nrow(weak_paths) > 0) {
      # Prepare path removal tasks
      path_tasks <- lapply(1:nrow(weak_paths), function(i) {
        list(
          response = weak_paths$Response[i],
          predictor = weak_paths$Predictor[i],
          p_value = weak_paths$P.Value[i]
        )
      })
      
      # Evaluate paths in parallel or serial
      if (parallel && length(path_tasks) > 1) {
        results <- parLapply(cl, path_tasks, function(task) {
          remove_path(current_model, task$response, task$predictor, data)
        })
      } else {
        results <- lapply(path_tasks, function(task) {
          remove_path(current_model, task$response, task$predictor, data)
        })
      }
      
      # Find best improvement
      best_idx <- NULL
      best_fit <- current_fit
      
      for (i in seq_along(results)) {
        result <- results[[i]]
        if (result$success && result$fit < best_fit) {
          best_idx <- i
          best_fit <- result$fit
        }
      }
      
      # Apply best improvement if found
      if (!is.null(best_idx)) {
        best_result <- results[[best_idx]]
        task <- path_tasks[[best_idx]]
        
        cli::cli_alert_success("Iteration {iter}: Removed {.val {best_result$predictor}} → {.val {best_result$response}} (p = {sprintf('%.3f', task$p_value)}, AIC improved: {sprintf('%.2f', current_fit)} → {sprintf('%.2f', best_result$fit)})")
        
        current_model <- best_result$model
        current_fit <- best_result$fit
        changed <- TRUE
        paths_removed <- paths_removed + 1
      } else {
        # Report skipped paths
        for (i in seq_along(results)) {
          result <- results[[i]]
          task <- path_tasks[[i]]
          
          if (result$success) {
            cli::cli_alert_info("Skipped removing {.val {result$predictor}} → {.val {result$response}} (p = {sprintf('%.3f', task$p_value)}, AIC would worsen: {sprintf('%.2f', current_fit)} → {sprintf('%.2f', result$fit)})")
          } else {
            cli::cli_alert_warning("Error when trying to remove {.val {result$predictor}} → {.val {result$response}}: {result$error}")
          }
        }
      }
    }
  }
  
  # Final messages
  if (paths_removed == 0) {
    cli::cli_alert_info("No paths were removed. Either all paths are significant at alpha = {.val {alpha}} or removing any path would worsen model fit.")
  } else {
    cli::cli_alert_success("Pruning complete. Removed {.val {paths_removed}} paths. Final AIC: {.val {sprintf('%.2f', current_fit)}}")
  }
  
  if (iter >= max_iter) {
    cli::cli_alert_warning("Reached maximum iterations ({.val {max_iter}}).")
  }
  
  # Show unique warnings if any
  unique_warnings <- unique(warnings_collected)
  if (length(unique_warnings) > 0) {
    cli::cli_h2("Warnings collected during pruning")
    cli::cli_ul()
    purrr::walk(unique_warnings, ~cli::cli_li(.x))
    cli::cli_end()
  }
  
  return(current_model)
}


iterative_trim_sem <- function(model, data, p_threshold = 0.05, min_std_coef = 0.1, max_iterations = 20) {
  library(lavaan)
  
  # Initial fit
  fit <- sem(model, data = data)
  current_model <- model
  iteration <- 0
  removed_paths <- c()
  
  cat("Starting model trimming process...\n")
  
  while(iteration < max_iterations) {
    iteration <- iteration + 1
    cat(paste0("Iteration ", iteration, ":\n"))
    
    # Get parameter estimates
    params <- parameterEstimates(fit)
    
    # Filter to only regression paths (excludes variances, covariances, etc.)
    reg_paths <- params[params$op == "~", ]
    
    # Find weakest path based on p-value and standardized coefficient
    std_est <- standardizedSolution(fit)
    std_est <- std_est[std_est$op == "~", ]
    
    # Merge standardized values into reg_paths
    reg_paths$std.est <- NA
    for(i in 1:nrow(reg_paths)) {
      idx <- which(std_est$lhs == reg_paths$lhs[i] & std_est$rhs == reg_paths$rhs[i])
      if(length(idx) > 0) {
        reg_paths$std.est[i] <- std_est$est.std[idx[1]]
      }
    }
    
    # Find non-significant paths
    nonsig_paths <- reg_paths[reg_paths$pvalue > p_threshold | abs(reg_paths$std.est) < min_std_coef, ]
    
    # If no more non-significant paths, break
    if(nrow(nonsig_paths) == 0) {
      cat("No more non-significant paths. Model trimming complete.\n")
      break
    }
    
    # Create path strings for comparison
    nonsig_paths$path_string <- paste0(nonsig_paths$lhs, " ~ ", nonsig_paths$rhs)
    
    # Check if paths have already been removed
    nonsig_paths <- nonsig_paths[!nonsig_paths$path_string %in% removed_paths, ]
    
    # If no more non-removed paths to consider, break
    if(nrow(nonsig_paths) == 0) {
      cat("No more non-significant paths to remove. Model trimming complete.\n")
      break
    }
    
    # Sort by p-value (highest first) and then by absolute standardized coefficient (lowest first)
    nonsig_paths <- nonsig_paths[order(-nonsig_paths$pvalue, abs(nonsig_paths$std.est)), ]
    
    # Remove the weakest path
    path_to_remove <- nonsig_paths$path_string[1]
    cat(paste0("Removing path: ", path_to_remove, 
               " (p-value = ", round(nonsig_paths$pvalue[1], 4),
               ", std.coef = ", round(nonsig_paths$std.est[1], 4), ")\n"))
    
    # Add to list of removed paths
    removed_paths <- c(removed_paths, path_to_remove)
    
    # Parse the model string to remove the path
    model_lines <- strsplit(current_model, "\n")[[1]]
    new_model_lines <- c()
    
    # Extract path components
    path_parts <- strsplit(path_to_remove, " ~ ")[[1]]
    lhs <- trimws(path_parts[1])
    rhs <- trimws(path_parts[2])
    
    for(line in model_lines) {
      # Skip empty lines or comment lines
      if(trimws(line) == "" || grepl("^\\s*#", line)) {
        new_model_lines <- c(new_model_lines, line)
        next
      }
      
      if(grepl(paste0("^\\s*", lhs, "\\s*~"), line)) {
        # This line contains the LHS variable we need to modify
        
        # Extract all RHS variables
        parts <- strsplit(line, "~")[[1]]
        rhs_part <- parts[2]
        
        # Split RHS into individual predictors
        predictors <- unlist(strsplit(rhs_part, "\\+"))
        predictors <- trimws(predictors)
        
        # Create a pattern to match the exact predictor
        predictor_pattern <- paste0("^", rhs, "$")
        # Remove the specific predictor we want to eliminate
        predictors <- predictors[!grepl(predictor_pattern, predictors)]
        
        # If we have no predictors left, skip this modification to avoid errors
        if(length(predictors) == 0) {
          new_model_lines <- c(new_model_lines, line)
          next
        }
        
        # Create a new line with the remaining predictors
        new_line <- paste0(lhs, " ~ ", paste(predictors, collapse = " + "))
        new_model_lines <- c(new_model_lines, new_line)
      } else {
        new_model_lines <- c(new_model_lines, line)
      }
    }
    
    # Update the current model
    current_model <- paste(new_model_lines, collapse = "\n")
    
    # Try to refit the model
    tryCatch({
      fit <- sem(current_model, data = data)
      
      # Print fit indices
      cat(paste0("New model fit: Chi-square = ", round(fitMeasures(fit)["chisq"], 2),
                 ", df = ", round(fitMeasures(fit)["df"], 2),
                 ", p = ", round(fitMeasures(fit)["pvalue"], 4),
                 ", CFI = ", round(fitMeasures(fit)["cfi"], 3),
                 ", RMSEA = ", round(fitMeasures(fit)["rmsea"], 3),
                 "\n\n"))
    }, error = function(e) {
      cat("Error in model fitting:", e$message, "\n")
      cat("Stopping model trimming process.\n")
      break
    })
  }
  
  if(iteration >= max_iterations) {
    cat("Maximum iterations reached. Process stopped.\n")
  }
  
  cat("Paths removed:\n")
  print(removed_paths)
  
  return(list(
    final_model = current_model,
    final_fit = fit,
    removed_paths = removed_paths,
    iterations = iteration
  ))
}

plot_psem_chord <- function(psem_model, var_domains, domain_colors,
                            alpha = 0.2, effect_colors = c(positive = "#008080", negative = "#CD5C5C")) {
  library(piecewiseSEM)
  library(circlize)
  
  # Step 1: Extract standardized coefficients
  std_solution <- coefs(psem_model, standardize = "scale") |>
    subset(P.Value < 0.05 & Response != Predictor)
  
  # Step 2: Create edge list
  edges <- with(std_solution, data.frame(
    from = Predictor,
    to = Response,
    weight = Estimate
  ))
  
  # Step 3: Assign colors based on effect direction
  edges$col <- ifelse(edges$weight > 0, effect_colors["positive"], effect_colors["negative"])
  
  # Step 4: Map node colors based on domains
  vars <- unique(c(edges$from, edges$to))
  grid_colors <- setNames(domain_colors[var_domains[vars]], vars)
  
  # Step 5: Plot chord diagram
  circos.clear()
  chordDiagram(
    edges,
    grid.col = grid_colors,
    col = edges$col,
    transparency = alpha,
    directional = 1,
    diffHeight = 0.05,
    preAllocateTracks = 1
  )
  
  # Step 6: Add R² values
  r2_df <- rsquared(psem_model)
  r2_values <- setNames(r2_df$R.squared, r2_df$Response)
  suppressMessages(
  for (var in names(r2_values)) {
    if (var %in% vars) {
      circos.track(track.index = 1, panel.fun = function(x, y) {
        if (CELL_META$sector.index == var) {
          r2_text <- sprintf("R² = %.2f", r2_values[[var]])
          circos.text(
            CELL_META$xcenter, 
            CELL_META$cell.ylim[2] + mm_y(2), 
            r2_text,
            cex = 0.8,
            facing = "bending.inside",
            niceFacing = TRUE
          )
        }
      }, bg.border = NA)
    }
  })
}

plot_psem_chord <- function(psem_model, var_domains, domain_colors,
                            alpha = 0.2,
                            effect_colors = c(positive = "#008080", negative = "#CD5C5C"),
                            file = NULL, width = 25.4, height = 25.4, res = 600) {
  library(piecewiseSEM)
  library(circlize)
  
  if (!is.null(file)) {
    png(filename = file, width = width, height = height, units = "cm", res = res)
    on.exit(dev.off())
  }
  
  # Step 1: Extract standardized coefficients (only significant)
  std_solution <- coefs(psem_model, standardize = "scale") |>
    subset(P.Value < 0.05 & Response != Predictor)
  
  # Step 2: Create edge list
  edges <- with(std_solution, data.frame(
    from = Predictor,
    to = Response,
    weight = Estimate
  ))
  
  # Step 3: Assign colors based on effect direction
  edges$col <- ifelse(edges$weight > 0, effect_colors["positive"], effect_colors["negative"])
  
  # Step 4: Map node colors based on domains
  vars <- unique(c(edges$from, edges$to))
  grid_colors <- setNames(domain_colors[var_domains[vars]], vars)
  
  # Step 5: Plot chord diagram
  circos.clear()
  chordDiagram(
    edges,
    grid.col = grid_colors,
    col = edges$col,
    transparency = alpha,
    directional = 1,
    diffHeight = 0.05,
    preAllocateTracks = 1
  )
  
  # Step 6: Add R² values (use Marginal R² if available)
  r2_df <- rsquared(psem_model)
  r2_values <- if ("Marginal" %in% names(r2_df)) {
    setNames(r2_df$Marginal, r2_df$Response)
  } else {
    setNames(r2_df$R.squared, r2_df$Response)
  }
  
  suppressMessages(
    for (var in names(r2_values)) {
      if (var %in% vars) {
        circos.track(track.index = 1, panel.fun = function(x, y) {
          if (CELL_META$sector.index == var) {
            r2_text <- sprintf("R² = %.2f", r2_values[[var]])
            circos.text(
              CELL_META$xcenter,
              CELL_META$cell.ylim[2] + mm_y(2),
              r2_text,
              cex = 2,
              facing = "bending.inside",
              niceFacing = TRUE
            )
          }
        }, bg.border = NA)
      }
    }
  )
}

plot_psem_chord <- function(psem_model, var_domains, domain_colors,
                            alpha = 0.2,
                            effect_colors = c(positive = "#008080", negative = "#CD5C5C"),
                            file = NULL, width = 25.4, height = 25.4, res = 600,
                            label_cex = 1.2) {  # Added label_cex parameter
  
  library(piecewiseSEM)
  library(circlize)
  
  if (!is.null(file)) {
    png(filename = file, width = width, height = height, units = "cm", res = res)
    on.exit(dev.off())
  }
  
  # Step 1: Extract coefficients (only significant)
  std_solution <- coefs(psem_model, standardize = "none") |>
    subset(P.Value < 0.05 & Response != Predictor)
  
  # Step 2: Create edge list
  edges <- with(std_solution, data.frame(
    from = Predictor,
    to = Response,
    weight = Estimate
  ))
  
  # Step 3: Assign colors based on effect direction
  edges$col <- ifelse(edges$weight > 0, effect_colors["positive"], effect_colors["negative"])
  
  # Step 4: Map node colors based on domains
  vars <- unique(c(edges$from, edges$to))
  grid_colors <- setNames(domain_colors[var_domains[vars]], vars)
  
  # Step 5: Plot chord diagram with proper label size
  circos.clear()
  chordDiagram(
    edges,
    grid.col = grid_colors,
    col = edges$col,
    transparency = alpha,
    directional = 1,
    diffHeight = 0.05,
    annotationTrack = c("grid", "axis"),  # Show both grid and axis
    preAllocateTracks = list(track.height = 0.1)  # Only specify once
  )
  
  # Add custom labels with adjusted size
  circos.track(track.index = 1, panel.fun = function(x, y) {
    circos.text(CELL_META$xcenter, CELL_META$ycenter, CELL_META$sector.index, 
                facing = "bending.inside", niceFacing = TRUE,
                adj = c(0, 0.5), cex = label_cex)
  }, bg.border = NA)
  
  # Step 6: Add R² values (use Marginal R² if available)
  r2_df <- rsquared(psem_model)
  r2_values <- if ("Marginal" %in% names(r2_df)) {
    setNames(r2_df$Marginal, r2_df$Response)
  } else {
    setNames(r2_df$R.squared, r2_df$Response)
  }
  
  suppressMessages(
    for (var in names(r2_values)) {
      if (var %in% vars) {
        circos.track(track.index = 1, panel.fun = function(x, y) {
          if (CELL_META$sector.index == var) {
            r2_text <- sprintf("R² = %.2f", r2_values[[var]])
            circos.text(
              CELL_META$xcenter,
              CELL_META$cell.ylim[2] + mm_y(2),
              r2_text,
              cex = 1,
              facing = "bending.inside",
              niceFacing = TRUE
            )
          }
        }, bg.border = NA)
      }
    }
  )
}




# # Edited prune_sem to support glmmTMB models
# prune_sem <- function(sem_model, data, alpha = 0.05, max_iter = Inf, parallel = TRUE, cores = NULL) {
#   library(piecewiseSEM)
#   library(glmmTMB)
#   library(lme4)
#   library(cli)
#   library(dplyr)
#   library(combinat)
#   
#   get_model_fit <- function(model) {
#     aic_result <- piecewiseSEM::AIC_psem(model)
#     return(aic_result$AIC)
#   }
#   
#   remove_path <- function(model, response_var, predictor_var, data) {
#     current_model <- model[[response_var]]
#     current_formula <- formula(current_model)
#     
#     response_str <- deparse(current_formula[[2]])
#     rhs <- attr(terms(current_formula), "term.labels")
#     rhs <- setdiff(rhs, predictor_var)
#     rhs_fixed <- paste(rhs[!grepl("\\|", rhs)], collapse = " + ")
#     rhs_random <- paste(rhs[grepl("\\|", rhs)], collapse = " + ")
#     
#     rhs_full <- if (rhs_fixed != "") rhs_fixed else "1"
#     if (rhs_random != "") rhs_full <- paste(rhs_full, "+", rhs_random)
#     
#     new_formula <- as.formula(paste(response_str, "~", rhs_full))
#     
#     new_model <- model
#     tryCatch({
#       new_model[[response_var]] <- update(current_model, formula. = new_formula, data = data)
#       new_model <- update(new_model, data = data)
#       new_fit <- get_model_fit(new_model)
#       return(list(model = new_model, fit = new_fit, response = response_var, predictor = predictor_var, success = TRUE))
#     }, error = function(e) {
#       return(list(model = NULL, fit = Inf, response = response_var, predictor = predictor_var, success = FALSE))
#     })
#   }
#   
#   if (parallel) {
#     library(parallel)
#     if (is.null(cores)) cores <- max(1, detectCores() - 1)
#     cl <- makeCluster(cores)
#     on.exit(stopCluster(cl), add = TRUE)
#     clusterExport(cl, varlist = c("remove_path", "get_model_fit", "data"), envir = environment())
#     clusterEvalQ(cl, { library(piecewiseSEM); library(glmmTMB); library(lme4) })
#     cli::cli_alert_info("Using parallel processing with {cores} cores")
#   }
#   
#   current_model <- sem_model
#   current_fit <- get_model_fit(current_model)
#   iter <- 0
#   paths_removed <- 0
#   
#   while (iter < max_iter) {
#     iter <- iter + 1
#     sem_summary <- suppressWarnings(summary(current_model, standardize = "none"))
#     coef_table <- sem_summary$coefficients
#     coef_table$Estimate <- as.numeric(as.character(coef_table$Estimate))
#     
#     weak_paths <- suppressWarnings({
#       coef_table |> select(1:7) |> 
#         subset((as.numeric(P.Value) > alpha | abs(as.numeric(Estimate)) < 0.1) &
#                  !grepl("(Intercept)", Predictor, fixed = TRUE) &
#                  !grepl("1 \\| Site", Predictor, fixed = TRUE)) |>
#         dplyr::arrange(desc(as.numeric(P.Value)), abs(Estimate))
#     })
#     
#     if (nrow(weak_paths) == 0) break
#     
#     combos <- unlist(lapply(1:min(nrow(weak_paths), 2), function(k) {
#       combn(1:nrow(weak_paths), k, simplify = FALSE)
#     }), recursive = FALSE)
#     
#     evaluate_combo <- function(idxs) {
#       trial_model <- current_model
#       for (i in idxs) {
#         path <- weak_paths[i, ]
#         result <- remove_path(trial_model, path$Response, path$Predictor, data)
#         if (!result$success) return(NULL)
#         trial_model <- result$model
#       }
#       list(model = trial_model, fit = get_model_fit(trial_model), removed = weak_paths[idxs, ])
#     }
#     
#     results <- if (parallel) {
#       parLapply(cl, combos, evaluate_combo)
#     } else {
#       lapply(combos, evaluate_combo)
#     }
#     
#     results <- Filter(Negate(is.null), results)
#     if (length(results) == 0) break
#     
#     best_result <- results[[which.min(sapply(results, \(x) x$fit))]]
#     if (best_result$fit < current_fit) {
#       removed_descriptions <- apply(best_result$removed, 1, function(row) paste0(row["Predictor"], " → ", row["Response"]))
#       cli::cli_alert_success("Iteration {iter}: Removed {paste(removed_descriptions, collapse = ', ')} (AIC improved: {sprintf('%.2f', current_fit)} → {sprintf('%.2f', best_result$fit)})")
#       current_model <- best_result$model
#       current_fit <- best_result$fit
#       paths_removed <- paths_removed + nrow(best_result$removed)
#     } else {
#       cli::cli_alert_info("No combination of paths improved AIC this round.")
#       break
#     }
#   }
#   
#   if (paths_removed == 0) {
#     cli::cli_alert_info("No paths were removed. Either all paths are significant at alpha = {.val {alpha}} or removing any path would worsen model fit.")
#   } else {
#     cli::cli_alert_success("Pruning complete. Removed {.val {paths_removed}} paths. Final AIC: {.val {sprintf('%.2f', current_fit)}}")
#   }
#   
#   return(current_model)
# }
# 


library(piecewiseSEM)
library(glmmTMB)

# prune_sem <- function(sem_model, 
#                       p_threshold = 0.05, 
#                       est_threshold = 0.1,
#                       verbose = TRUE,
#                       strict = TRUE) {
#   
#   # Get initial model fit
#   current_fit <- summary(sem_model, standardize = "none", .progressBar = F)
#   current_aic <- current_fit$AIC$AIC
#   current_c <- current_fit$Cstat$Fisher.C
#   current_p <- current_fit$Cstat$P.Value
#   
#   if(verbose) {
#     cat("Initial model:\n")
#     cat("AIC:", current_aic, "\n")
#     cat("Fisher's C:", current_c, "\n")
#     cat("P-value:", current_p, "\n\n")
#   }
#   
#   improved <- TRUE
#   iteration <- 1
#   
#   while(improved) {
#     if(verbose) cat("=== Iteration", iteration, "===\n")
#     
#     # Get coefficients and exclude correlated errors
#     coefs <- current_fit$coefficients
#     coefs <- coefs[!grepl("~~", coefs$Predictor), ]
#     
#     # Identify candidate paths to remove (either high p OR low estimate)
#     weak_paths <- coefs[
#       (coefs$P.Value > p_threshold) | 
#         (abs(coefs$Estimate) < est_threshold),
#     ]
#     
#     # Sort by weakest first (combination of p-value and effect size)
#     weak_paths$weakness_score <- 
#       scale(-log10(weak_paths$P.Value)) + 
#       scale(-abs(weak_paths$Estimate))
#     weak_paths <- weak_paths[order(weak_paths$weakness_score), ]
#     
#     if(nrow(weak_paths) == 0) {
#       if(verbose) cat("No more weak paths meeting thresholds\n")
#       break
#     }
#     
#     # Try removing paths one at a time
#     tested_removals <- 0
#     improved_this_round <- FALSE
#     
#     for(i in 1:nrow(weak_paths)) {
#       weakest <- weak_paths[i, ]
#       
#       if(verbose) {
#         cat(sprintf("\nTesting removal: %s ~ %s (P=%.4f, Est=%.4f)",
#                     weakest$Response, weakest$Predictor,
#                     weakest$P.Value, weakest$Estimate))
#       }
#       
#       # Create new model without this path
#       new_model <- sem_model
#       
#       # Find and update the relevant component
#       for(j in seq_along(new_model)) {
#         if(inherits(new_model[[j]], "glmmTMB")) {
#           resp <- all.vars(formula(new_model[[j]]))[1]
#           preds <- all.vars(formula(new_model[[j]]))[-1]
#           
#           if(resp == weakest$Response && weakest$Predictor %in% preds) {
#             new_formula <- suppressWarnings(update(
#               formula(new_model[[j]]), 
#               paste(". ~ . -", weakest$Predictor)
#             ))
#             new_model[[j]] <- suppressMessages(update(
#               new_model[[j]], 
#               formula = new_formula
#               )
#             )
#             break
#           }
#         }
#       }
#       
#       # Evaluate new model
#       new_fit <- try(suppressWarnings(summary(new_model, standardize = "none", .progressBar = F)), silent = TRUE)
#       if(inherits(new_fit, "try-error")) next
#       
#       new_aic <- new_fit$AIC$AIC
#       new_c <- new_fit$Cstat$Fisher.C
#       new_p <- new_fit$Cstat$P.Value
#       
#       if(verbose) {
#         cat(sprintf(
#           "\n  New fit: AIC=%.1f (Δ%.1f), C=%.1f (P=%.3f)",
#           new_aic, new_aic - current_aic,
#           new_c, new_p
#         ))
#       }
#       
#       # Check if removal improved model
#       aic_improved <- new_aic < current_aic
#       fit_not_worse <- if(strict) new_p > 0.05 else TRUE
#       
#       if(aic_improved && fit_not_worse) {
#         if(verbose) cat(" --> KEEPING removal\n")
#         sem_model <- new_model
#         current_fit <- new_fit
#         current_aic <- new_aic
#         current_c <- new_c
#         current_p <- new_p
#         improved_this_round <- TRUE
#         break # Move to next iteration after any improvement
#       } else {
#         if(verbose) cat(" --> Rejecting removal\n")
#       }
#       
#       tested_removals <- tested_removals + 1
#       if(tested_removals >= 5 && improved_this_round) break # Limit tests per iteration
#     }
#     
#     improved <- improved_this_round
#     iteration <- iteration + 1
#     
#     if(verbose && !improved) cat("\nNo improvements this round\n")
#   }
#   
#   # Final model summary
#   final_fit <- summary(sem_model, standardize = "none", .progressBar = F)
#   if(verbose) {
#     cat("\n\n=== Final Model ===\n")
#     cat("AIC:", final_fit$AIC$AIC, "\n")
#     cat("Fisher's C:", final_fit$Cstat$Fisher.C, "\n")
#     cat("P-value:", final_fit$Cstat$P.Value, "\n")
#     cat("Paths removed:", sum(!coefs$Predictor %in% 
#                                 final_fit$coefficients$Predictor), "\n")
#   }
#   
#   return(sem_model)
# }

remove_weak_paths <- function(sem_model, p_threshold = 0.05, verbose = TRUE) {
  # Initial model summary
  current_model <- sem_model
  current_summary <- summary(current_model)
  
  # Track model fit statistics
  old_fit <- list(
    aic = current_summary$AIC[1, 1],
    chisq = current_summary$ChiSq$Chisq,
    chisq_p = current_summary$ChiSq$P.Value,
    fisher_c = current_summary$Cstat$Fisher.C,
    fisher_p = current_summary$Cstat$P.Value
  )
  
  if(verbose) {
    cat("Starting model:\n")
    cat("  AIC:", old_fit$aic, "\n")
    cat("  Chi-squared P-value:", old_fit$chisq_p, "\n")
    cat("  Fisher's C P-value:", old_fit$fisher_p, "\n\n")
  }
  
  # Continue removing paths until no more paths can be removed
  improvement <- TRUE
  iteration <- 1
  
  while(improvement) {
    # Get the coefficients table
    coefs <- current_summary$coefficients
    
    # Filter out correlation coefficients (those starting with "~~")
    coefs <- coefs[!grepl("^~~", coefs$Response), ]
    
    # Identify candidate paths to remove (p-value > threshold)
    candidates <- coefs[coefs$P.Value > p_threshold, ]
    
    # If no candidates, we're done
    if(nrow(candidates) == 0) {
      if(verbose) cat("No more paths with P-value >", p_threshold, "found.\n")
      break
    }
    
    # Process candidates one by one until we find one that can be removed
    path_removed <- FALSE
    candidate_index <- 1
    
    while(!path_removed && candidate_index <= nrow(candidates)) {
      # Get the next candidate path
      worst_path <- candidates[candidate_index, ]
      
      if(verbose) {
        cat("Iteration", iteration, "- Considering removal of path:\n")
        cat("  ", worst_path$Response, "~", worst_path$Predictor, 
            "(Estimate:", worst_path$Estimate, ", P-value:", worst_path$P.Value, ")\n")
      }
      
      # Create a new formula for the affected response variable
      # Get the current formula for this response
      current_formula <- formula(current_model[[as.character(worst_path$Response)]])
      
      # Convert to character for string manipulation
      formula_str <- Reduce(paste, deparse(current_formula))
      
      # Extract the right-hand side
      rhs <- sub(".*~\\s*", "", formula_str)
      
      # Split the right-hand side into terms
      terms <- unlist(strsplit(rhs, "\\s*\\+\\s*"))
      
      # Remove the term we want to drop
      terms <- terms[!trimws(terms) %in% c(as.character(worst_path$Predictor))]
      
      # If no terms are left, we can't remove this predictor
      if(length(terms) == 0) {
        if(verbose) cat("  Cannot remove this path - it's the only predictor for this response.\n")
        candidate_index <- candidate_index + 1
        next
      }
      
      # Rebuild the formula
      new_rhs <- paste(terms, collapse = " + ")
      new_formula_str <- paste(worst_path$Response, "~", new_rhs)
      new_formula <- as.formula(new_formula_str)
      
      # Create a copy of the model and update the formula
      new_model <- current_model
      
      # Use try-catch to handle potential errors when updating the model
      skip_to_next <- FALSE
      
      tryCatch({
        new_model[[as.character(worst_path$Response)]] <- update(current_model[[as.character(worst_path$Response)]], formula = new_formula)
        # Update the psem object
        new_model <- update(new_model)
        # Get the new summary
        new_summary <- summary(new_model)
        
        # Compare the fit - with safe handling of missing values
        new_fit <- list(
          aic = ifelse(is.null(new_summary$AIC[1, 1]), NA, new_summary$AIC[1, 1]),
          chisq = ifelse(is.null(new_summary$ChiSq$Chisq), NA, new_summary$ChiSq$Chisq),
          chisq_p = ifelse(is.null(new_summary$ChiSq$P.Value), NA, new_summary$ChiSq$P.Value),
          fisher_c = ifelse(is.null(new_summary$Cstat$Fisher.C), NA, new_summary$Cstat$Fisher.C),
          fisher_p = ifelse(is.null(new_summary$Cstat$P.Value), NA, new_summary$Cstat$P.Value)
        )
        
        # Check for NA values in fit measures
        if(any(is.na(unlist(new_fit)))) {
          if(verbose) {
            cat("  Model fit calculation failed - skipping this path.\n")
          }
          skip_to_next <- TRUE
        } else {
          # Standard fit criteria
          aic_ok <- !is.na(new_fit$aic) && !is.na(old_fit$aic) && (new_fit$aic <= old_fit$aic + 2)
          chisq_ok <- !is.na(new_fit$chisq_p) && (new_fit$chisq_p > 0.05)
          fisher_ok <- !is.na(new_fit$fisher_p) && (new_fit$fisher_p > 0.05)
          
          # Check for dramatic changes
          dramatic_change <- FALSE
          if(!is.na(old_fit$chisq_p) && !is.na(new_fit$chisq_p)) {
            dramatic_change <- (old_fit$chisq_p > 0.5 && new_fit$chisq_p < 0.05) ||
              (new_fit$chisq_p == 0)
          }
          
          fit_ok <- aic_ok && chisq_ok && fisher_ok && !dramatic_change
          
          if(fit_ok) {
            if(verbose) {
              cat("  Removed path. New model fit:\n")
              cat("    AIC:", new_fit$aic, "(Change:", round(new_fit$aic - old_fit$aic, 3), ")\n")
              cat("    Chi-squared P-value:", new_fit$chisq_p, "\n")
              cat("    Fisher's C P-value:", new_fit$fisher_p, "\n\n")
            }
            
            # Update the current model and fit
            current_model <- new_model
            current_summary <- new_summary
            old_fit <- new_fit
            iteration <- iteration + 1
            path_removed <- TRUE
          } else {
            if(verbose) {
              cat("  Removing this path worsens model fit. Trying next candidate path.\n")
              cat("    AIC:", new_fit$aic, "(Change:", round(new_fit$aic - old_fit$aic, 3), ")\n")
              cat("    Chi-squared P-value:", new_fit$chisq_p, "\n")
              cat("    Fisher's C P-value:", new_fit$fisher_p, "\n\n")
            }
            skip_to_next <- TRUE
          }
        }
      }, error = function(e) {
        if(verbose) {
          cat("  Error updating the model after removing this path:\n")
          cat("  ", conditionMessage(e), "\n")
          cat("  Skipping this path.\n\n")
        }
        skip_to_next <- TRUE
      })
      
      if(skip_to_next) {
        candidate_index <- candidate_index + 1
      }
    }
    
    # If we've gone through all candidates and didn't remove any paths, we're done
    if(!path_removed) {
      if(verbose) {
        cat("No more paths can be removed without worsening model fit. Stopping iterations.\n\n")
      }
      improvement <- FALSE
    }
  }
  
  if(verbose) {
    cat("Final model after", iteration - 1, "iterations:\n")
    cat("  AIC:", old_fit$aic, "\n")
    cat("  Chi-squared P-value:", old_fit$chisq_p, "\n")
    cat("  Fisher's C P-value:", old_fit$fisher_p, "\n\n")
    
    # Print the structural equations of the final model
    cat("Structural Equations of final model:\n")
    print(current_model)
  }
  
  return(current_model)
}
remove_weak_paths <- function(sem_model, p_threshold = 0.05, verbose = TRUE) {
  current_model <- sem_model
  current_summary <- suppressWarnings(
    summary(current_model, standardize = "none", .progressBar = FALSE)
  )
  
  old_fit <- list(
    aic = current_summary$AIC[1, 1],
    chisq_p = current_summary$ChiSq$P.Value,
    fisher_p = current_summary$Cstat$P.Value
  )
  
  if (verbose) {
    cat("Starting model:\n")
    cat("  AIC:", old_fit$aic, "\n")
    cat("  Chi-squared P-value:", old_fit$chisq_p, "\n")
    cat("  Fisher's C P-value:", old_fit$fisher_p, "\n\n")
  }
  
  failed_paths <- c()
  improvement <- TRUE
  iteration <- 1
  
  while (improvement) {
    coefs <- current_summary$coefficients
    coefs <- coefs[!grepl("^~~", coefs$Response), ]
    candidates <- coefs[coefs$P.Value > p_threshold, ]
    
    if (nrow(candidates) == 0) {
      if (verbose) cat("No more paths with P-value >", p_threshold, "found.\n")
      break
    }
    
    candidates <- candidates[order(-candidates$P.Value, abs(candidates$Estimate)), ]
    path_ids <- paste(candidates$Response, "~", candidates$Predictor)
    valid_candidates <- candidates[!path_ids %in% failed_paths, ]
    
    if (nrow(valid_candidates) == 0) {
      if (verbose) cat("No more valid paths to try removing. Stopping iterations.\n")
      break
    }
    
    worst_path <- valid_candidates[1, ]
    current_path_id <- paste(worst_path$Response, "~", worst_path$Predictor)
    
    if (verbose) {
      cat("Iteration", iteration, "- Considering removal of path:\n")
      cat("  ", worst_path$Response, "~", worst_path$Predictor, 
          "(Estimate:", worst_path$Estimate, ", P-value:", worst_path$P.Value, ")\n")
    }
    
    current_formula <- formula(current_model[[as.character(worst_path$Response)]])
    formula_str <- Reduce(paste, deparse(current_formula))
    rhs <- sub(".*~\\s*", "", formula_str)
    terms <- unlist(strsplit(rhs, "\\s*\\+\\s*"))
    terms <- terms[!trimws(terms) %in% c(as.character(worst_path$Predictor))]
    
    if (length(terms) == 0) {
      if (verbose) cat("  Cannot remove this path - it's the only predictor for this response.\n")
      failed_paths <- c(failed_paths, current_path_id)
      next
    }
    
    new_rhs <- paste(terms, collapse = " + ")
    new_formula <- as.formula(paste(worst_path$Response, "~", new_rhs))
    
    new_model <- current_model
    path_succeeded <- FALSE
    
    tryCatch({
      new_model[[as.character(worst_path$Response)]] <- update(
        current_model[[as.character(worst_path$Response)]], formula = new_formula
      )
      new_model <- update(new_model)
      new_summary <- suppressWarnings(
        summary(new_model, standardize = "none", .progressBar = FALSE)
      )
      
      new_fit <- list(
        aic = ifelse(is.null(new_summary$AIC[1, 1]), NA, new_summary$AIC[1, 1]),
        chisq_p = ifelse(is.null(new_summary$ChiSq$P.Value), NA, new_summary$ChiSq$P.Value),
        fisher_p = ifelse(is.null(new_summary$Cstat$P.Value), NA, new_summary$Cstat$P.Value)
      )
      
      if (any(is.na(unlist(new_fit)))) {
        if (verbose) cat("  Model fit calculation failed - skipping this path.\n")
        failed_paths <- c(failed_paths, current_path_id)
      } else {
        aic_decreased <- (new_fit$aic < old_fit$aic)
        chisq_ok <- new_fit$chisq_p > 0.05
        fisher_ok <- new_fit$fisher_p > 0.05
        
        fit_ok <- aic_decreased && chisq_ok && fisher_ok
        
        if (fit_ok) {
          if (verbose) {
            cat("  Removed path. New model fit:\n")
            cat("    AIC:", new_fit$aic, "(Change:", round(new_fit$aic - old_fit$aic, 3), ")\n")
            cat("    Chi-squared P-value:", new_fit$chisq_p, "\n")
            cat("    Fisher's C P-value:", new_fit$fisher_p, "\n\n")
          }
          current_model <- new_model
          current_summary <- new_summary
          old_fit <- new_fit
          iteration <- iteration + 1
          path_succeeded <- TRUE
        } else {
          if (verbose) {
            cat("  Removing this path does not improve AIC enough or worsens fit. Skipping.\n")
            cat("    AIC:", new_fit$aic, "(Change:", round(new_fit$aic - old_fit$aic, 3), ")\n")
            cat("    Chi-squared P-value:", new_fit$chisq_p, "\n")
            cat("    Fisher's C P-value:", new_fit$fisher_p, "\n\n")
          }
          failed_paths <- c(failed_paths, current_path_id)
        }
      }
    }, error = function(e) {
      if (verbose) {
        cat("  Error updating the model after removing this path:\n")
        cat("  ", conditionMessage(e), "\n")
        cat("  Skipping this path.\n\n")
      }
      failed_paths <- c(failed_paths, current_path_id)
    })
    
    if (!path_succeeded) {
      remaining_candidates <- path_ids[!path_ids %in% failed_paths]
      if (length(remaining_candidates) == 0) {
        if (verbose) {
          cat("No more paths can be removed without worsening model fit. Stopping iterations.\n\n")
        }
        improvement <- FALSE
      }
    }
  }
  
  if (verbose) {
    cat("Final model after", iteration - 1, "iterations:\n")
    cat("  AIC:", old_fit$aic, "\n")
    cat("  Chi-squared P-value:", old_fit$chisq_p, "\n")
    cat("  Fisher's C P-value:", old_fit$fisher_p, "\n\n")
    cat("Structural Equations of final model:\n")
    print(current_model)
  }
  
  return(current_model)
}

plot_sem_graph <- function(sem_fit, round_digits = 2, file = NULL, width = 3600, height = 2338) {
  library(DiagrammeR)
  coefs_df <- coefficients(sem_fit)
  coefs_df <- coefs_df[, !(is.na(colnames(coefs_df)) | colnames(coefs_df) == "")]
  
  # Process coefficients
  coefs_df <- coefs_df |>
    dplyr::filter(!is.na(Predictor), !is.na(Response)) |>
    dplyr::mutate(
      Estimate = as.numeric(Estimate),
      P.Value = as.numeric(P.Value),
      label = paste0(round(Estimate, round_digits),
                     ifelse(P.Value < 0.001, "***",
                            ifelse(P.Value < 0.01, "**",
                                   ifelse(P.Value < 0.05, "*", "")))),
      color = ifelse(Estimate >= 0, "green3", "firebrick"),
      penwidth = 1 * abs(Estimate),
      style = ifelse(P.Value < 0.05, "solid", "dotted")
    )
  
  # Extract correlations (bidirectional relationships)
  corr_rows <- grep("~~", coefs_df$Response)
  corr_df <- data.frame()
  
  if (length(corr_rows) > 0) {
    for (row in corr_rows) {
      # Extract the variable names from the correlation notation
      var1 <- gsub("~~", "", coefs_df$Response[row])
      var2 <- gsub("~~", "", coefs_df$Predictor[row])
      
      # Create a new row for correlation
      new_row <- data.frame(
        Response = var1,
        Predictor = var2,
        Estimate = coefs_df$Estimate[row],
        P.Value = coefs_df$P.Value[row],
        label = paste0(round(coefs_df$Estimate[row], round_digits),
                       ifelse(coefs_df$P.Value[row] < 0.001, "***",
                              ifelse(coefs_df$P.Value[row] < 0.01, "**",
                                     ifelse(coefs_df$P.Value[row] < 0.05, "*", "")))),
        color = ifelse(coefs_df$Estimate[row] >= 0, "green", "firebrick"),
        penwidth = 2.5 * abs(coefs_df$Estimate[row]),
        style = ifelse(coefs_df$P.Value[row] < 0.05, "solid", "dotted"),
        is_correlation = TRUE,
        stringsAsFactors = FALSE
      )
      
      corr_df <- rbind(corr_df, new_row)
    }
  }
  
  # Remove correlation rows from original coefficients
  if (length(corr_rows) > 0) {
    coefs_df <- coefs_df[-corr_rows, ]
  }
  
  # Add is_correlation column to original df
  coefs_df$is_correlation <- FALSE
  
  # Combine regular coefficients and correlations
  if (nrow(corr_df) > 0) {
    # Make sure all columns match
    common_cols <- intersect(names(coefs_df), names(corr_df))
    all_coefs <- rbind(coefs_df[common_cols], corr_df[common_cols])
  } else {
    all_coefs <- coefs_df
  }
  
  # Create digraph
  dot_lines <- c(
    "digraph SEM {", 
    "graph [layout = dot, rankdir = TB, splines = true];",
    "node [shape = box, style = filled, fillcolor = lightgray, fontsize = 32, width = 1.5, height = 0.8];",
    "edge [fontsize = 28];"
  )
  
  for (i in seq_len(nrow(all_coefs))) {
    if (all_coefs$is_correlation[i]) {
      # For correlations, use bidirectional edges with dir=both
      dot_lines <- c(dot_lines,
                     paste0('"', all_coefs$Response[i], '" -> "', all_coefs$Predictor[i],
                            '" [label = "', all_coefs$label[i],
                            '", color = "', all_coefs$color[i],
                            '", penwidth = ', (round(all_coefs$penwidth[i], 2)),
                            ', style = ', all_coefs$style[i],
                            ', dir = both, arrowhead = normal, arrowtail = normal];'
                     )
      )
    } else {
      # For regular directed relationships
      dot_lines <- c(dot_lines,
                     paste0('"', all_coefs$Predictor[i], '" -> "', all_coefs$Response[i],
                            '" [label = "', all_coefs$label[i],
                            '", color = "', all_coefs$color[i],
                            '", penwidth = ', (round(all_coefs$penwidth[i], 2)),
                            ', style = ', all_coefs$style[i], '];'
                     )
      )
    }
  }
  
  dot_lines <- c(dot_lines, "}")
  dot_string <- paste(dot_lines, collapse = "\n")
  
  graph <- DiagrammeR::grViz(dot_string)
  
  # Save to PNG if filename is provided
  if (!is.null(file)) {
    svg <- DiagrammeRsvg::export_svg(graph)
    svg_char <- charToRaw(svg)
    rsvg::rsvg_png(svg_char, file = file, width = width, height = height)
  }
  
  return(graph)
}




plot_psem_chord <- function(psem_model, var_domains, domain_colors,
                            var_labels = NULL,
                            alpha = 0.2,
                            effect_colors = c(positive = "#008080", negative = "#CD5C5C"),
                            file = NULL, width = 25.4, height = 25.4, res = 600,
                            label_cex = 1.2) {
  
  library(piecewiseSEM)
  library(circlize)
  
  if (!is.null(file)) {
    png(filename = file, width = width, height = height, units = "cm", res = res, bg = "transparent")
    on.exit(dev.off())
  }
  
  std_solution <- coefs(psem_model, standardize = "none")
  
  # Identify correlated errors
  is_correlated_error <- grepl("~~", std_solution$Response) | grepl("~~", std_solution$Predictor)
  
  # Directed edges
  directed_edges <- std_solution[!is_correlated_error & std_solution$P.Value < 0.05 & std_solution$Response != std_solution$Predictor, ]
  directed_edges <- with(directed_edges, data.frame(
    from = Predictor,
    to = Response,
    weight = Estimate,
    col = ifelse(Estimate > 0, effect_colors["positive"], effect_colors["negative"]),
    directional = 1
  ))
  
  # Correlated errors
  get_var <- function(x) gsub("~~", "", x)
  correlated_errors <- std_solution[is_correlated_error & std_solution$P.Value < 0.05, ]
  correlated_errors <- with(correlated_errors, data.frame(
    from = get_var(Predictor),
    to = get_var(Response),
    weight = Estimate,
    col = "#999999",
    directional = 0
  ))
  
  # Combine edges
  all_edges <- rbind(directed_edges, correlated_errors)
  vars <- unique(c(all_edges$from, all_edges$to))
  
  # Compute R² values
  r2_df <- rsquared(psem_model)
  r2_values <- if ("Marginal" %in% names(r2_df)) {
    setNames(r2_df$Marginal, r2_df$Response)
  } else {
    setNames(r2_df$R.squared, r2_df$Response)
  }
  
  # Apply labels if provided
  if (!is.null(var_labels)) {
    all_edges$from <- var_labels[all_edges$from]
    all_edges$to <- var_labels[all_edges$to]
    names(r2_values) <- var_labels[names(r2_values)]
    grid_colors <- setNames(domain_colors[var_domains[names(var_labels)]], var_labels)
  } else {
    grid_colors <- setNames(domain_colors[var_domains[vars]], vars)
  }
  
  # Plot
  circos.clear()
  chordDiagram(
    all_edges[, c("from", "to", "weight")],
    grid.col = grid_colors,
    col = all_edges$col,
    transparency = alpha,
    directional = all_edges$directional,
    diffHeight = 0,
    annotationTrack = c("grid", "axis"),
    preAllocateTracks = list(track.height = 0.1)
  )
  
  circos.track(track.index = 1, panel.fun = function(x, y) {
    circos.text(CELL_META$xcenter, CELL_META$ylim[1] + mm_y(3), CELL_META$sector.index, 
                facing = "bending.inside", 
                niceFacing = TRUE,
                adj = c(0.5, 0.5),
                cex = label_cex)
  }, bg.border = NA)
  
  # Add R²
  suppressMessages(
    for (var in names(r2_values)) {
      circos.track(track.index = 1, panel.fun = function(x, y) {
        if (CELL_META$sector.index == var) {
          r2_text <- sprintf("R² = %.2f", r2_values[[var]])
          circos.text(
            CELL_META$xcenter,
            CELL_META$cell.ylim[2] + mm_y(2),
            r2_text,
            cex = 1,
            facing = "bending.inside",
            niceFacing = TRUE,
            adj = c(0.5, 0.5)
          )
        }
      }, bg.border = NA)
    }
  )
}
