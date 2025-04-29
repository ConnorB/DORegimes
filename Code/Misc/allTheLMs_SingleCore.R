library(tidyverse)
library(rsample)
library(progressr)
library(furrr)
library(ggthemes)
library(here)

options(readr.show_col_types = FALSE)

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

# Step 2: Generate combinations efficiently
generate_filtered_combos <- function() {
  all_combos <- map(2:6, ~combn(predictor_vars, .x, simplify = FALSE)) %>%
    flatten()
  
  filtered_combos <- keep(all_combos, function(combo) {
    group_counts <- sapply(grouped_predictors, function(group) sum(combo %in% group))
    valid_grouping <- all(group_counts <= 1)
    no_q_and_depth <- !any(combo %in% grouped_predictors$Q & combo %in% grouped_predictors$Depth)
    valid_grouping && no_q_and_depth
  })
  
  return(filtered_combos)
}

# Step 3: Generate valid predictor combinations
predictor_combos <- generate_filtered_combos()

# Step 5: Create functions to run models
run_model <- function(response, predictors, train_data, test_data, model_type = "linear") {
  suppressWarnings(
  # Apply transformations based on model type
  if (model_type == "log-log") {
    train_data <- train_data %>%
      mutate(across(all_of(c(response, predictors)), ~ log(. + 1)))
    test_data <- test_data %>%
      mutate(across(all_of(c(response, predictors)), ~ log(. + 1)))
  } else if (model_type == "log-linear") {
    train_data <- train_data %>%
      mutate(across(all_of(c(predictors)), ~ log(. + 1)))
    test_data <- test_data %>%
      mutate(across(all_of(c(predictors)), ~ log(. + 1)))
  } else if (model_type == "linear-log") {
    train_data <- train_data %>%
      mutate(across(all_of(response), ~ log(. + 1)))
    test_data <- test_data %>%
      mutate(across(all_of(response), ~ log(. + 1)))
  })
  
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
# Prepare data
metDat_model <- dailyDat %>%
  ungroup() %>% 
  select(all_of(c(response_vars, predictor_vars))) %>% 
  drop_na() %>%
  mutate(across(where(is.numeric), ~ scale(.)[, 1]))

set.seed(123)
data_split <- initial_split(metDat_model, prop = 0.7)
train_data <- training(data_split)
test_data <- testing(data_split)

# Step 4: Set up parallel processing and progress tracking
plan(multisession)  # Use multiple sessions for parallel processing
handlers(global = TRUE)  # Enable global handlers for progress

model_types <- c("linear")
total_combos <- length(response_vars) * length(predictor_combos) * length(model_types)

# Track progress
with_progress({
  p <- progressr::progressor(steps = total_combos)
  
  results <- future_map(response_vars, function(response) {
    map(predictor_combos, function(predictors) {
      map(model_types, function(model_type) {
        p()
        run_model(response, predictors, train_data, test_data, model_type)
      })
    })
  })
})
plan(sequential)
# Step 8: Extract results into dataframes
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
results <- results %>% 
  drop_na(rmse) %>% 
  write_csv(here("Results/lmResults.csv"))

results <- read_csv(here("Results/lmResults.csv"))
# Step 10: Rank models and analyze
bestLM <- results %>%
  group_by(response, model_type) %>%
  arrange(response, model_type, rmse, r_squared) %>%
  slice_head(n = 1)

# Step 11: Function to create observed vs predicted plots

plot_observed_vs_predicted <- function(best_model_info, train_data, test_data) {
  library(ggplot2)
  
  plots <- list()
  
  for (i in 1:nrow(best_model_info)) {
    # Extract details for the current best model
    response <- best_model_info$response[i]
    predictors <- unlist(strsplit(best_model_info$predictors[i], ", "))
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
  ggsave(filename = paste0("plot_", i, ".png"), plot = plots[[i]], path = here("/Figs/lmFigs"), device = "png", width = 21, height = 15, units = "cm", dpi = "retina")
}

