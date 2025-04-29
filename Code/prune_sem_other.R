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

remove_weak_paths <- function(model, data, threshold = 0.05, 
                                        est_threshold = 0.1, 
                                        estimator = "MLR", missing = "FIML",
                                        try_modindices = TRUE,
                                        mi_threshold = 3.84) {  # Chi-square critical value at alpha=0.05
  # Initial fit
  fit <- sem(model, data = data, estimator = estimator, missing = missing, parallel = TRUE)
  
  # Get initial fit measures
  current_aic <- fitMeasures(fit, "aic")
  current_bic <- fitMeasures(fit, "bic")
  cat("Initial model: AIC =", round(current_aic, 2), 
      "BIC =", round(current_bic, 2), "\n")
  
  # Continue until no more weak paths found
  more_to_remove <- TRUE
  removed_paths <- data.frame()
  added_paths <- data.frame()
  
  # PHASE 1: Remove weak paths
  cat("\n----- PHASE 1: REMOVING WEAK PATHS -----\n\n")
  
  while(more_to_remove) {
    # Get parameter estimates
    params <- parameterEstimates(fit)
    
    # Filter for regression parameters with p > threshold AND low estimates
    weak_paths <- params[params$op == "~" & 
                           params$pvalue > threshold & 
                           abs(params$est) < est_threshold, ]
    
    if(nrow(weak_paths) == 0) {
      more_to_remove <- FALSE
      cat("No more weak paths to remove.\n")
    } else {
      # Sort by p-value (highest first)
      weak_paths <- weak_paths[order(-weak_paths$pvalue), ]
      
      # Try each path in order until we find one that improves fit
      removed_any <- FALSE
      
      for(i in 1:nrow(weak_paths)) {
        path_to_remove <- weak_paths[i, ]
        
        # Get the parameter table
        pt <- parTable(fit)
        
        # Find the row to remove
        row_idx <- which(pt$lhs == path_to_remove$lhs & 
                           pt$op == "~" & 
                           pt$rhs == path_to_remove$rhs)
        
        if(length(row_idx) > 0) {
          # Make a copy of the parameter table
          new_pt <- pt[-row_idx, ]
          
          # Try to fit the model with the updated parameter table
          tryCatch({
            new_fit <- sem(new_pt, data = data, estimator = estimator, missing = missing, parallel = TRUE)
            
            # Check if the fit improved
            new_aic <- fitMeasures(new_fit, "aic")
            new_bic <- fitMeasures(new_fit, "bic")
            
            if(new_bic < current_bic) {  # Using BIC as criteria for improvement
              cat("Removing:", path_to_remove$lhs, "~", path_to_remove$rhs, 
                  "(p =", round(path_to_remove$pvalue, 3), 
                  ", est =", round(path_to_remove$est, 3), ")\n")
              cat("Fit improved: AIC from", round(current_aic, 2), "to", round(new_aic, 2),
                  ", BIC from", round(current_bic, 2), "to", round(new_bic, 2), "\n")
              
              # Update the current fit
              fit <- new_fit
              current_aic <- new_aic
              current_bic <- new_bic
              
              # Record the removed path
              removed_paths <- rbind(removed_paths, 
                                     data.frame(lhs = path_to_remove$lhs,
                                                op = path_to_remove$op,
                                                rhs = path_to_remove$rhs,
                                                est = path_to_remove$est,
                                                pvalue = path_to_remove$pvalue,
                                                aic_change = current_aic - new_aic,
                                                bic_change = current_bic - new_bic,
                                                stringsAsFactors = FALSE))
              
              removed_any <- TRUE
              break  # Exit the for loop if we found a path to remove
            } else {
              cat("Tried removing:", path_to_remove$lhs, "~", path_to_remove$rhs, 
                  "(p =", round(path_to_remove$pvalue, 3), 
                  ", est =", round(path_to_remove$est, 3), ")\n")
              cat("Fit did not improve: AIC from", round(current_aic, 2), "to", round(new_aic, 2),
                  ", BIC from", round(current_bic, 2), "to", round(new_bic, 2), "\n")
              cat("Path kept in model, trying next candidate...\n")
            }
          }, error = function(e) {
            cat("Error fitting model after removing", path_to_remove$lhs, "~", 
                path_to_remove$rhs, ":", e$message, "\nTrying next path.\n")
          })
        } else {
          cat("Could not find parameter", path_to_remove$lhs, "~", path_to_remove$rhs, 
              "in the table. Trying next path.\n")
        }
      }
      
      # If we tried all paths and none improved fit, exit the loop
      if(!removed_any) {
        cat("Tried all candidate paths but none improved model fit. Stopping removal phase.\n")
        more_to_remove <- FALSE
      }
    }
  }
  
  # PHASE 2: Add suggested paths from score tests (alternative to modification indices)
  if(try_modindices) {
    cat("\n----- PHASE 2: ADDING PATHS FROM SCORE TESTS -----\n\n")
    
    # Use score tests instead of modification indices
    tryCatch({
      # Get all observed variable names
      obs_names <- lavNames(fit, "ov")
      
      # Create a list of potential paths to test
      potential_paths <- list()
      for(lhs in obs_names) {
        for(rhs in obs_names) {
          # Skip if same variable or if the path would create a cycle
          if(lhs != rhs) {
            # Check if this path already exists in the model
            pt <- parTable(fit)
            existing_idx <- which(pt$lhs == lhs & pt$op == "~" & pt$rhs == rhs)
            
            if(length(existing_idx) == 0) {
              potential_paths[[length(potential_paths) + 1]] <- list(lhs = lhs, rhs = rhs)
            }
          }
        }
      }
      
      # Test paths in batches to avoid too many iterations
      added_any <- FALSE
      batch_size <- min(10, length(potential_paths))
      
      if(length(potential_paths) > 0) {
        cat("Testing", length(potential_paths), "potential paths in batches of", batch_size, "\n")
        
        for(batch_start in seq(1, length(potential_paths), by = batch_size)) {
          batch_end <- min(batch_start + batch_size - 1, length(potential_paths))
          batch <- potential_paths[batch_start:batch_end]
          
          for(path in batch) {
            # Create test syntax for this path
            test_syntax <- paste0(path$lhs, " ~ ", path$rhs)
            
            # Attempt score test
            tryCatch({
              score_test <- lavTestScore(fit, add = test_syntax, standardized = F)
              
              if(is.list(score_test) && !is.null(score_test$uni)) {
                test_result <- score_test$uni
                mi_value <- test_result$X2[1]  # Use score test chi-square
                
                if(mi_value > mi_threshold) {
                  # Try adding this path
                  pt <- parTable(fit)
                  
                  # Create a new row for the parameter table
                  new_row <- data.frame(
                    id = max(pt$id) + 1,
                    lhs = path$lhs,
                    op = "~",
                    rhs = path$rhs,
                    user = 1,
                    group = 1,
                    free = 1,
                    label = "",
                    plabel = paste0(".p", max(as.numeric(gsub("^.p([0-9]+)\\.$", "\\1", pt$plabel))) + 1, "."),
                    start = 0,  # Default starting value
                    stringsAsFactors = FALSE
                  )
                  
                  # Add columns that exist in pt but might be missing in new_row
                  for(col in names(pt)) {
                    if(!(col %in% names(new_row))) {
                      new_row[[col]] <- NA
                    }
                  }
                  
                  # Add the new path to the parameter table
                  new_pt <- rbind(pt, new_row[names(pt)])
                  
                  # Try to fit the model with the updated parameter table
                  tryCatch({
                    new_fit <- sem(new_pt, data = data, estimator = estimator, missing = missing, parallel = TRUE)
                    
                    # Check if the fit improved
                    new_aic <- fitMeasures(new_fit, "aic")
                    new_bic <- fitMeasures(new_fit, "bic")
                    
                    if(new_bic < current_bic) {  # Using BIC as criteria for improvement
                      cat("Adding:", path$lhs, "~", path$rhs, 
                          "(Score test =", round(mi_value, 2), ")\n")
                      cat("Fit improved: AIC from", round(current_aic, 2), "to", round(new_aic, 2),
                          ", BIC from", round(current_bic, 2), "to", round(new_bic, 2), "\n")
                      
                      # Update the current fit
                      fit <- new_fit
                      current_aic <- new_aic
                      current_bic <- new_bic
                      
                      # Get the actual parameter estimate
                      new_est <- parameterEstimates(fit)
                      path_est <- new_est[new_est$lhs == path$lhs & 
                                            new_est$op == "~" & 
                                            new_est$rhs == path$rhs, "est"]
                      
                      # Record the added path
                      added_paths <- rbind(added_paths, 
                                           data.frame(lhs = path$lhs,
                                                      op = "~",
                                                      rhs = path$rhs,
                                                      score_test = mi_value,
                                                      est = ifelse(length(path_est) > 0, path_est[1], NA),
                                                      aic_change = current_aic - new_aic,
                                                      bic_change = current_bic - new_bic,
                                                      stringsAsFactors = FALSE))
                      
                      added_any <- TRUE
                      break  # Exit the inner loop and move to next batch
                    } else {
                      cat("Tried adding:", path$lhs, "~", path$rhs, 
                          "(Score test =", round(mi_value, 2), ")\n")
                      cat("Fit did not improve: AIC from", round(current_aic, 2), "to", round(new_aic, 2),
                          ", BIC from", round(current_bic, 2), "to", round(new_bic, 2), "\n")
                    }
                  }, error = function(e) {
                    cat("Error fitting model after adding", path$lhs, "~", 
                        path$rhs, ":", e$message, "\n")
                  })
                }
              }
            }, error = function(e) {
              cat("Error computing score test for", path$lhs, "~", path$rhs, ":", e$message, "\n")
            })
          }
          
          if(added_any) {
            # If we found a path to add in this batch, reset and continue from the beginning
            added_any <- FALSE
            break
          }
        }
      } else {
        cat("No potential paths to test.\n")
      }
    }, error = function(e) {
      cat("Error in path addition phase:", e$message, "\nSkipping addition phase.\n")
    })
  }
  
  # Compare final model with original
  final_aic <- fitMeasures(fit, "aic")
  final_bic <- fitMeasures(fit, "bic")
  original_fit <- sem(model, data = data, estimator = estimator, missing = missing, parallel = TRUE)
  original_aic <- fitMeasures(original_fit, "aic")
  original_bic <- fitMeasures(original_fit, "bic")
  
  cat("\n----- FINAL MODEL SUMMARY -----\n\n")
  cat("Original model: AIC =", round(original_aic, 2), 
      "BIC =", round(original_bic, 2), "\n")
  cat("Final model:    AIC =", round(final_aic, 2), 
      "BIC =", round(final_bic, 2), "\n")
  cat("Improvement:    AIC =", round(original_aic - final_aic, 2),
      "BIC =", round(original_bic - final_bic, 2), "\n\n")
  
  cat("Paths removed:", nrow(removed_paths), "\n")
  cat("Paths added:", nrow(added_paths), "\n")
  
  # Return the final model and information about removed and added paths
  return(list(fit = fit, 
              removed_paths = removed_paths,
              added_paths = added_paths,
              original_fit = original_fit))
}
