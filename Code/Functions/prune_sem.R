
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
      Std.Estimate = as.numeric(Std.Estimate),
      label = paste0(round(Estimate, round_digits),
                     ifelse(P.Value < 0.001, "***",
                            ifelse(P.Value < 0.01, "**",
                                   ifelse(P.Value < 0.05, "*", "")))),
      color = ifelse(Estimate >= 0, "green3", "firebrick"),
      penwidth = 10 * abs(Std.Estimate),
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
        new_model[[response_var]] <- update(current_model, formula. = as.formula(new_formula_char), data = data)
      } else {
        new_model[[response_var]] <- update(current_model, formula. = as.formula(new_formula_char), data = data)
      }
      
      new_model <- update(new_model, data = data)
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
          as.numeric(P.Value) > alpha &          # Ensure numeric comparison
            abs(as.numeric(Estimate)) < 0.1 &      # Explicit numeric conversion
            !grepl("(Intercept)", Predictor, fixed = TRUE) &
            !grepl("1 \\| waterYear", Predictor, fixed = TRUE) &
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
    weight = Std.Estimate
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
plot_lavaan_chord <- function(lav_fit, var_domains, domain_colors,
                              alpha = 0.2, effect_colors = c(positive = "#008080", negative = "#CD5C5C")) {
  library(lavaan)
  library(circlize)
  
  # Step 1: Extract standardized regression coefficients
  std_solution <- standardizedSolution(lav_fit) |>
    subset(op == "~" & pvalue < 0.05)
  
  # Step 2: Extract R-squared values
  r2_values <- inspect(lav_fit, "r2")
  
  # Step 3: Create edge list
  edges <- with(std_solution, data.frame(
    from = rhs,
    to = lhs,
    weight = est.std
  ))
  
  # Step 4: Assign colors based on effect direction
  edges$col <- ifelse(edges$weight > 0, effect_colors["positive"], effect_colors["negative"])
  
  # Step 5: Map node colors based on domains
  vars <- unique(c(edges$from, edges$to))
  grid_colors <- setNames(domain_colors[var_domains[vars]], vars)
  
  # Step 6: Plot chord diagram
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
  
  # Step 7: Add R² values to plotted variables
  suppressMessages(
  for (var in names(r2_values)) {
    if (var %in% vars) {
      circos.track(track.index = 1, panel.fun = function(x, y) {
        if (CELL_META$sector.index == var) {
          r2_text <- sprintf("R² = %.2f", r2_values[var])
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
  }
  )
}
