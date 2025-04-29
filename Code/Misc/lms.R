# Load necessary libraries
library(tidyverse)
library(here)
library(rsample)
library(progress)
library(ggthemes)
library(MASS)  # For stepwise regression

# Load the data
dailyDat <- read_csv(here("Data/dailyData.csv"))

# Step 1: Define response and predictor variables
response_vars <- c("minDO.pctsat", "maxDO.pctsat", "avgDO.pctsat", "DOpctsat.amp", "probHypSat")
predictor_vars <- c(
  "PercWet_1d", "PercWet_7d", "PercWet_14d", "PercWet_30d", "PercWet_60d",
  "dPercWet_1d", "dPercWet_7d", "dPercWet_14d", "dPercWet_30d", "dPercWet_60d",
  "PPT_1d", "PPT_7d", "PPT_14d", "PPT_30d", "PPT_60d", 
  "Q_1d", "Q_7d", "Q_14d", "Q_30d", "Q_60d", "exeProb",
  "Depth_1d", "Depth_7d", "Depth_14d", "Depth_30d", "Depth_60d"
)

# Define grouped predictors
grouped_predictors <- list(
  perc_wet = c("PercWet_1d", "PercWet_7d", "PercWet_14d", "PercWet_30d", "PercWet_60d"),
  dPercWet = c("dPercWet_1d", "dPercWet_7d", "dPercWet_14d", "dPercWet_30d", "dPercWet_60d"),
  PPT = c("PPT_1d", "PPT_7d", "PPT_14d", "PPT_30d", "PPT_60d"),
  Q = c("Q_1d", "Q_7d", "Q_14d", "Q_30d", "Q_60d"),
  exeProb = c("exeProb"),
  Depth = c("Depth_1d", "Depth_7d", "Depth_14d", "Depth_30d", "Depth_60d")
)

# Step 3: Generate combinations
generate_filtered_combos <- function() {
  all_combos <- map(3:6, ~combn(predictor_vars, .x, simplify = FALSE)) %>%
    purrr::flatten()
  
  filtered_combos <- purrr::keep(all_combos, function(combo) {
    group_counts <- sapply(grouped_predictors, function(group) sum(combo %in% group))
    valid_grouping <- all(group_counts <= 1)
    q_present <- any(combo %in% grouped_predictors$Q)
    depth_present <- any(combo %in% grouped_predictors$Depth)
    no_q_and_depth <- !(q_present && depth_present)
    valid_grouping && no_q_and_depth
  })
  
  return(filtered_combos)
}

predictor_combos <- generate_filtered_combos()

# Step 4: Stepwise regression function
stepwise_model <- function(response, predictors, train_data) {
  if (length(predictors) == 0) {
    return(list(
      full_predictors = NA,
      stepwise_predictors = NA,
      model = NULL,
      aic = NA
    ))
  }
  
  lmForm <- as.formula(paste(response, "~", paste(predictors, collapse = " + ")))
  full_model <- tryCatch(
    lm(lmForm, data = train_data),
    error = function(e) {
      message("Error in full model creation: ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(full_model)) {
    return(list(
      full_predictors = predictors,
      stepwise_predictors = NA,
      model = NULL,
      aic = NA
    ))
  }
  
  step_model <- tryCatch(
    stepAIC(full_model, direction = "both", trace = FALSE),
    error = function(e) {
      message("Error in stepwise regression: ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(step_model)) {
    return(list(
      full_predictors = predictors,
      stepwise_predictors = NA,
      model = NULL,
      aic = NA
    ))
  }
  
  list(
    full_predictors = predictors,
    stepwise_predictors = names(coef(step_model))[-1],  # Exclude intercept
    model = step_model,
    aic = AIC(step_model)
  )
}

# Step 5: General model function for multiple model types
run_model <- function(response, predictors, train_data, test_data, model_type = "linear") {
  if (length(predictors) == 0) {
    return(list(
      model = NULL, 
      rmse = NA, 
      r_squared = NA, 
      aic = NA, 
      predictors = NA, 
      response = response, 
      model_type = model_type
    ))
  }
  
  # Handle log transformations
  train_data_mod <- train_data
  test_data_mod <- test_data
  suppressWarnings(
  if (model_type == "log-log") {
    train_data_mod <- train_data %>%
      mutate(across(c(response, all_of(predictors)), ~ log(. + 1))) %>% 
      drop_na()
    test_data_mod <- test_data %>%
      mutate(across(c(response, all_of(predictors)), ~ log(. + 1))) %>% 
      drop_na()
  } else if (model_type == "log-linear") {
    train_data_mod <- train_data %>%
      mutate(across(c(response), ~ log(. + 1))) %>% 
      drop_na()
    test_data_mod <- test_data %>%
      mutate(across(c(response), ~ log(. + 1))) %>% 
      drop_na()
  } else if (model_type == "linear-log") {
    train_data_mod <- train_data %>%
      mutate(across(c(predictors), ~ log(. + 1))) %>% 
      drop_na()
    test_data_mod <- test_data %>%
      mutate(across(c(predictors), ~ log(. + 1))) %>% 
      drop_na()
  }
  )
  # Create formula
  lmForm <- as.formula(paste(response, "~", "(", paste(predictors, collapse = " + "), ")^2"))
  
  model <- tryCatch(
    lm(lmForm, data = train_data_mod),
    error = function(e) {
      message("Error in lm model creation: ", e$message)
      return(NULL)
    }
  )
  
  if (is.null(model)) {
    return(list(
      model = NULL, 
      rmse = NA, 
      r_squared = NA, 
      aic = NA, 
      predictors = predictors, 
      response = response, 
      model_type = model_type
    ))
  }
  
  predictions <- predict(model, newdata = test_data_mod)
  
  # Back-transform predictions for log-transformed response
  if (model_type == "log-linear" || model_type == "log-log") {
    predictions <- exp(predictions)
  }
  
  rmse <- sqrt(mean((predictions - test_data[[response]])^2))
  r_squared <- summary(model)$r.squared
  aic <- AIC(model)
  
  list(
    model = model, 
    rmse = rmse, 
    r_squared = r_squared, 
    aic = aic, 
    predictors = predictors, 
    response = response, 
    model_type = model_type
  )
}

# Step 6: Prepare data
scaling_params <- dailyDat %>%
  dplyr::select(where(is.numeric)) %>%
  summarise(across(everything(), list(mean = \(x) mean(x, na.rm = TRUE), sd = \(x) sd(x, na.rm = TRUE)))
  ) %>%
  pivot_longer(cols = everything(), names_to = c("variable", ".value"), names_sep = "_")

metDat_model <- dailyDat %>%
  ungroup() %>% 
  dplyr::select(all_of(c(response_vars, predictor_vars))) %>% 
  drop_na()  %>%
  mutate(across(where(is.numeric), ~ scale(.)[, 1]))

set.seed(123)
data_split <- initial_split(metDat_model, prop = 0.7)
train_data <- training(data_split)
test_data <- testing(data_split)

# Step 7: Run models for all types
pb <- progress_bar$new(
  format = "Running models [:bar] :percent Elapsed: :elapsed ETA: :eta",
  total = length(response_vars) * length(predictor_combos) * 4,  # 4 model types
  clear = FALSE,
  width = 80
)

model_types <- c("linear")

results <- lapply(response_vars, function(response) {
  lapply(predictor_combos, function(predictors) {
    lapply(model_types, function(model_type) {
      pb$tick()
      step_result <- stepwise_model(response, predictors, train_data)
      
      if (is.null(step_result$model)) {
        return(NULL)
      }
      
      final_model <- run_model(response, step_result$stepwise_predictors, train_data, test_data, model_type)
      final_model$full_predictors <- step_result$full_predictors
      final_model$stepwise_predictors <- step_result$stepwise_predictors
      final_model
    })
  })
})

# Step 8: Extract results
extract_results <- function(results_list) {
  do.call(rbind, lapply(results_list, function(res) {
    do.call(rbind, lapply(res, function(model_res_by_type) {
      do.call(rbind, lapply(model_res_by_type, function(model_result) {
        if (is.null(model_result)) return(NULL)
        data.frame(
          response = model_result$response,
          model_type = model_result$model_type,
          full_predictors = paste(model_result$full_predictors, collapse = ", "),
          stepwise_predictors = paste(model_result$stepwise_predictors, collapse = ", "),
          rmse = model_result$rmse,
          r_squared = model_result$r_squared,
          aic = model_result$aic
        )
      }))
    }))
  }))
}

results <- extract_results(results)
results <- results %>% 
  drop_na(rmse) %>% 
  write_csv(here("Results/lmResults.csv"))

# Step 10: Rank models and analyze
bestLM <- results %>%
  group_by(response) %>%
  arrange(response, rmse, r_squared, model_type) %>%
  slice_head(n = 1)

# Step 11: Function to create observed vs predicted plots
plot_observed_vs_predicted <- function(best_model_info, train_data, test_data) {
  library(ggplot2)
  
  plots <- list()
  
  for (i in 1:nrow(best_model_info)) {
    # Extract details for the current best model
    response <- best_model_info$response[i]
    predictors <- unlist(strsplit(best_model_info$stepwise_predictors[i], ", "))
    model_type <- best_model_info$model_type[i]
    
    # Refit the model on the training data
    lmForm <- as.formula(paste(response, "~", "(", paste(predictors, collapse = " + "), ")^2"))
    model <- lm(lmForm, data = train_data)
    
    # Generate predictions on the test data
    predictions <- predict(model, newdata = test_data)
    
    # Back-transform predictions if necessary
    if (model_type %in% c("log-log", "log-linear")) {
      predictions <- exp(predictions) - 1
    }
    
    # Create a data frame for plotting
    plot_data <- data.frame(
      Observed = test_data[[response]],
      Predicted = predictions
    )
    
    # Calculate R-squared
    r_squared <- summary(lm(Predicted ~ Observed, data = plot_data))$r.squared
    model_formula <- paste0(response, " ~ ", paste(predictors, collapse = " + "))
    
    # Create the plot
    p <- ggplot(plot_data, aes(x = Observed, y = Predicted)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(
        title = paste("Observed vs Predicted for", response),
        subtitle = paste0("Formula: (",model_type, ") " , model_formula, "\nR² = ", round(r_squared, 3)),
        x = "Observed Values",
        y = "Predicted Values"
      ) +
      theme_few()
    
    plots[[i]] <- p
  }
  
  return(plots)
}


# Step 12: Apply the plotting function
best_model_info <- bestLM %>%
  group_by(response) %>%
  slice_head(n = 1)  # Ensure only the top model for each response variable is used

plots <- plot_observed_vs_predicted(best_model_info, train_data, test_data)

# Step 13: Display or save the plots
for (i in seq_along(plots)) {
  print(plots[[i]])
  # Optionally save plots
  ggsave(filename = paste0("plot_", i, ".png"), plot = plots[[i]], path = "Figs/lmFigs", device = "png", width = 21, height = 15, units = "cm", dpi = "retina")
}
