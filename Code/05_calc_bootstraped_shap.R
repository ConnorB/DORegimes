library(future)
library(future.apply)
library(progressr)
library(here)
handlers(handler_progress(
  format = "Bootstrap :current/:total [:bar] ETA: :eta"
))

# Load model and data
load(file = here("Results/rf/model_objects_lag_rounded.RData"))

# Configuration
n_bootstrap <- 1000
n_samples <- nrow(test_data)
background_n <- 50
n_workers <- availableCores() - 1
output_path <- here("Results/rf/rf_bootstrapped_shap.rds")

# Estimated runtime: ~2 days
# Single iteration ≈ 26 min → (26 * 1000) / 9 workers ≈ 48 hrs

set.seed(42)
bootstrap_seeds <- sample.int(1e7, n_bootstrap)

plan(
  cluster,
  workers = c(rep("localhost", n_workers), rep("connor@Connors-Mini.local", 8))
)
with_progress({
  p <- progressr::progressor(along = seq_len(n_bootstrap))

  shap_list <- future_lapply(
    seq_len(n_bootstrap), # Iterate i = 1, 2, ..., 1000
    function(i) {
      # Draw a bootstrap resample
      set.seed(bootstrap_seeds[i]) # Set iteration-specific seed
      boot_idx <- sample(seq_len(n_samples), size = n_samples, replace = TRUE) # Sample row indices with replacement
      boot_data <- test_data[boot_idx, ] # Create the resampled dataset

      # Compute Kernel SHAP values
      system.time(
        ks <- kernelshap::kernelshap(
          object = fitted_model, # The trained model
          X = boot_data, # Observations to explain
          feature_names = selected_vars, # Which features to compute SHAP values for
          #bg_n = 50, # Sample 50 background obs from X for the SHAP baseline
          #exact = FALSE, # Use approximate (sampling-based) Shapley, not exact
          #tol = 0.05, # Convergence tolerance, stop iterating when SHAP estimates stabilize within this threshold
          #max_iter = 20, # Cap the number of coalition-sampling iterations
          parallel = TRUE, # Don't nest parallelism inside each worker
          parallel_args = list(packages = "ranger"),
          verbose = T, #FALSE,
          num.threads = 1 # Force single-threaded prediction with ranger
        )
      )
      # Signal one unit of progress to the progress bar
      p()
      # Return only the SHAP value matrix (observations × features)
      ks$S
    },
    future.seed = TRUE,
    future.packages = c("kernelshap", "ranger") # Load these packages on each worker
  )
})
# Cleanup and save
plan(sequential)
saveRDS(shap_list, output_path)
beepr::beep(2)
