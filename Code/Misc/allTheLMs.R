#!/usr/bin/env Rscript

library(tidyverse)
options(readr.show_col_types = F)
library(here)
library(rsample)
library(furrr)
library(future.apply)
options(parallelly.fork.enable = T)
library(progressr)
library(purrr)

dailyDat <- read_csv("~/Desktop/dailyDat.csv")

# Dynamically allocate cores, leaving 1 core free
num_cores <- parallel::detectCores() - 1
plan(multicore, workers = num_cores) # Enable progress reporting
handlers(global = TRUE)
# Use a handler that displays ETA and elapsed time
handlers(handler_txtprogressbar(format = ":percent [:bar] ETA: :eta Elapsed: :elapsed"))


# Define the function
my_fcn <- function(response_vars, predictor_combos, train_data, test_data, model_types) {
  p <- progressor(along = response_vars)  # Set up progress bar for the outer loop
  
  # Parallelized outer loop with future_lapply
  future_lapply(response_vars, function(response) {
    p(sprintf("Starting response: %s", response))
    
    # Inner loops for predictor combinations and model types
    lapply(predictor_combos, function(predictors) {

      
      lapply(model_types, function(model_type) {

        # Run the model
        result <- run_model(response, predictors, train_data, test_data, model_type)
        
        cat(sprintf("Finished model type: %s for response: %s\n", model_type, response))
        return(result)
      })
    })
  })
}

# Define model_types
model_types <- c("linear", "log-log", "log-linear", "linear-log")

# Define run_model function (already defined earlier)
run_model <- function(response, predictors, train_data, test_data, model_type = "linear") {
  # Ensure WaterYear is a factor in both datasets
  train_data$WaterYear <- as.factor(train_data$WaterYear)
  test_data$WaterYear <- as.factor(test_data$WaterYear)
  
  # Apply transformations based on model type
  if (model_type == "log-log") {
    train_data <- train_data %>%
      mutate(across(all_of(c(response, predictors)) & -WaterYear, ~ log(. + 1)))
    test_data <- test_data %>%
      mutate(across(all_of(c(response, predictors)) & -WaterYear, ~ log(. + 1)))
  } else if (model_type == "log-linear") {
    train_data <- train_data %>%
      mutate(across(all_of(c(predictors)) & -WaterYear, ~ log(. + 1)))
    test_data <- test_data %>%
      mutate(across(all_of(c(predictors)) & -WaterYear, ~ log(. + 1)))
  } else if (model_type == "linear-log") {
    train_data <- train_data %>%
      mutate(across(all_of(response), ~ log(. + 1)))
    test_data <- test_data %>%
      mutate(across(all_of(response), ~ log(. + 1)))
  }
  
  # Define the formula
  lmForm <- as.formula(paste(response, "~", "(", paste(predictors, collapse = " + "), ")^2"))
  
  # Fit the model using lm()
  model <- lm(lmForm, data = train_data)
  
  # Make predictions
  predictions <- predict(model, newdata = test_data)
  
  # Back-transform predictions if necessary
  if (model_type %in% c("log-log", "log-linear")) {
    predictions <- exp(predictions) - 1
  }
  
  # Calculate RMSE and R-squared
  rmse <- sqrt(mean((predictions - test_data[[response]])^2))
  r_squared <- summary(model)$r.squared
  
  list(
    model = model, rmse = rmse, r_squared = r_squared,
    predictors = predictors, response = response, model_type = model_type
  )
}

# Define response variables and predictor variables
response_vars <- c("minDO.pctsat", "maxDO.pctsat", "avgDO.pctsat", "DOpctsat.amp", "probHypSat", "PC1", "PC2", "cluster")
predictor_vars <- c(
  "PercWet_1d", "PercWet_7d", "PercWet_14d", "PercWet_30d", "PercWet_60d",
  "PPT_1d", "PPT_7d", "PPT_14d", "PPT_30d", "PPT_60d", 
  "Q_1d", "Q_7d", "Q_14d", "Q_30d", "Q_60d", "exeProb",
  "Depth_1d", "Depth_7d", "Depth_14d", "Depth_30d", "Depth_60d", "WaterYear"
)

# Group predictors into valid combinations
grouped_predictors <- list(
  perc_wet = c("PercWet_1d", "PercWet_7d", "PercWet_14d", "PercWet_30d", "PercWet_60d"),
  PPT = c("PPT_1d", "PPT_7d", "PPT_14d", "PPT_30d", "PPT_60d"),
  Q = c("Q_1d", "Q_7d", "Q_14d", "Q_30d", "Q_60d"),
  exeProb = c("exeProb"),
  Depth = c("Depth_1d", "Depth_7d", "Depth_14d", "Depth_30d", "Depth_60d")
)

generate_filtered_combos <- function() {
  all_combos <- map(2:4, ~ combn(predictor_vars, .x, simplify = FALSE)) %>%
    purrr::flatten()
  
  filtered_combos <- purrr::keep(all_combos, function(combo) {
    group_counts <- sapply(grouped_predictors, function(group) sum(combo %in% group))
    all(group_counts <= 1)  # Ensure no more than 1 variable is selected from each group
  })
  
  return(filtered_combos)
}

predictor_combos <- generate_filtered_combos()

# Prepare data
metDat_model <- dailyDat %>%
  ungroup() %>% 
  select(all_of(c(response_vars, predictor_vars))) %>% 
  drop_na() %>%
  mutate(WaterYear = as.factor(WaterYear)) %>% 
  mutate(across(where(is.numeric), ~ scale(.)[, 1]))

set.seed(123)
data_split <- initial_split(metDat_model, prop = 0.7)
train_data <- training(data_split)
test_data <- testing(data_split)

# Execute the workflow with progress tracking
with_progress({
  results <- my_fcn(response_vars, predictor_combos, train_data, test_data, model_types)
})


# Step 8: Extract results into dataframes (unchanged)
extract_results <- function(results_list) {
  do.call(rbind, lapply(results_list, function(res) {
    do.call(rbind, lapply(res, function(model_res) {
      do.call(rbind, lapply(model_res, function(model_result) {
        data.frame(
          response = model_result$response,
          predictors = paste(model_result$predictors, collapse = ", "),
          model_type = model_result$model_type,
          rmse = model_result$rmse,
          r_squared = model_result$r_squared
        )
      }))
    }))
  }))
}

results <- extract_results(results)
write_csv(results, "~/Desktop/results.csv")
# Shut down the parallel backend when done
plan(sequential)




