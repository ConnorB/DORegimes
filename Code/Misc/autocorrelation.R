run_and_compare_models <- function(response, formula_fixed, data, date_var = "Date", group_var = "Site",
                                   arma_orders = list(c(1, 0), c(1, 1), c(2, 1)),
                                   correlation_structures = c("none", "corAR1", "corARMA", "corExp", "corGaus")) {
  models <- list()
  group_formula <- as.formula(paste0("~1|", group_var))
  cor_form <- as.formula(paste0("~ as.numeric(", date_var, ") | ", group_var))
  
  safe_fit <- function(expr) tryCatch(expr, error = function(e) e)
  
  for (corr in correlation_structures) {
    if (corr == "none") {
      models[["base"]] <- safe_fit(
        lme(fixed = as.formula(paste(response, formula_fixed)),
            random = group_formula,
            data = data, na.action = na.omit)
      )
      
    } else if (corr == "corAR1") {
      models[["AR1"]] <- safe_fit(
        lme(fixed = as.formula(paste(response, formula_fixed)),
            random = group_formula,
            correlation = corAR1(form = cor_form),
            data = data, na.action = na.omit)
      )
      
    } else if (corr == "corARMA") {
      for (order in arma_orders) {
        name <- paste0("ARMA_", paste(order, collapse = "_"))
        models[[name]] <- safe_fit(
          lme(fixed = as.formula(paste(response, formula_fixed)),
              random = group_formula,
              correlation = corARMA(p = order[1], q = order[2], form = cor_form),
              data = data, na.action = na.omit)
        )
      }
      
    } else if (corr == "corExp") {
      models[["Exp"]] <- safe_fit(
        lme(fixed = as.formula(paste(response, formula_fixed)),
            random = group_formula,
            correlation = corExp(form = cor_form),
            data = data, na.action = na.omit)
      )
      
    } else if (corr == "corGaus") {
      models[["Gaus"]] <- safe_fit(
        lme(fixed = as.formula(paste(response, formula_fixed)),
            random = group_formula,
            correlation = corGaus(form = cor_form),
            data = data, na.action = na.omit)
      )
    }
  }
  
  return(models)
}


library(nlme)
library(MASS)
library(parallel)
# STEP 1: Make cluster
n_cores <- detectCores() - 1
cl <- makeCluster(n_cores)

# STEP 2: Export everything needed to workers
clusterExport(cl, varlist = c("semDat", "run_and_compare_models"))

# STEP 3: Also need to load packages on each core
clusterEvalQ(cl, {
  library(nlme)
  library(MASS)
})

# STEP 4: Model specification list
model_specs <- list(
  avgPar           = list(formula = "~ PPT_tot + DOY"),
  water.temp       = list(formula = "~ avgPar + PPT_tot + DOY"),
  Depth            = list(formula = "~ water.temp + avgPar + PPT_tot + DOY + excProb + headGradient + percentConnected + AlphaCentrality + upstream_length + PercWet"),
  excProb          = list(formula = "~ water.temp + avgPar + PPT_tot + DOY + percentConnected + AlphaCentrality + upstream_length + PercWet"),
  headGradient     = list(formula = "~ avgPar + water.temp + PPT_tot + DOY + percentConnected + AlphaCentrality + upstream_length + PercWet"),
  percentConnected = list(formula = "~ water.temp + avgPar + PPT_tot + DOY"),
  AlphaCentrality  = list(formula = "~ water.temp + avgPar + PPT_tot + DOY"),
  upstream_length  = list(formula = "~ water.temp + avgPar + PPT_tot + DOY"),
  PercWet          = list(formula = "~ water.temp + avgPar + PPT_tot + DOY"),
  DOSat.exc        = list(formula = "~ water.temp + PPT_tot + Depth + flowState + excProb + percentConnected + AlphaCentrality + upstream_length + PercWet + headGradient + avgPar + DOY"),
  DOpctsat.amp     = list(formula = "~ DOSat.exc + water.temp + PPT_tot + Depth + flowState + excProb + percentConnected + AlphaCentrality + upstream_length + PercWet + headGradient + avgPar + DOY")
)

# STEP 5: Prepare task list (response + its model spec)
model_tasks <- lapply(names(model_specs), function(name) {
  list(response = name, spec = model_specs[[name]])
})

# STEP 6: Export model specs list to workers (not just semDat and function)
clusterExport(cl, varlist = c("semDat", "run_and_compare_models"))

# STEP 7: Run parallel models
model_results <- parLapply(cl, model_tasks, function(task) {
  response <- task$response
  spec <- task$spec
  
  result <- tryCatch({
    run_and_compare_models(response = response,
                           formula_fixed = spec$formula,
                           data = semDat)
  }, error = function(e) e)
  
  list(response = response, result = result)
})

# STEP 8: Stop cluster
stopCluster(cl)

# STEP 9: Rebuild results list
model_results <- setNames(lapply(model_results, function(x) x$result), 
                          sapply(model_results, function(x) x$response))


library(lmtest)
evaluate_models <- function(model_list) {
  # Handle the case where the model list is actually an error
  if (inherits(model_list, "error") || inherits(model_list, "simpleError")) {
    return(data.frame(
      model_name = "ERROR",
      AIC = NA,
      dw_p_value = NA,
      dw_ok = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  
  evaluate_one <- function(model, name) {
    aic_val <- tryCatch(AIC(model), error = function(e) NA)
    
    dw_p <- tryCatch({
      resids <- residuals(model, type = "normalized")
      dw <- lmtest::dwtest(resids ~ 1)
      dw$p.value
    }, error = function(e) NA)
    
    dw_val <- tryCatch({
      resids <- residuals(model, type = "normalized")
      dw <- lmtest::dwtest(resids ~ 1)
      dw$statistic
    }, error = function(e) NA)
    
    data.frame(
      model_name = name,
      AIC = aic_val,
      dw_p_value = round(dw_p, 4),
      dw_stat = round(dw_val, 4),
      stringsAsFactors = FALSE
    )
  }
  
  results <- do.call(rbind, Map(evaluate_one, model_list, names(model_list)))
  return(results)
}


model_eval <- dplyr::bind_rows(
  lapply(names(model_evaluation_summary), function(resp) {
    df <- model_evaluation_summary[[resp]]
    if (!is.null(df) && is.data.frame(df)) {
      df$response <- resp
      return(df)
    } else {
      return(NULL)
    }
  })
) |>
  rownames_to_column("model_index") |> 
  dplyr::select(response, model_name, AIC, dw_p_value, dw_stat)

best_models <- model_eval |>
  filter(dw_p_value>= 0.05) |> 
  group_by(response) |>
  slice_min(order_by = AIC, n = 1, with_ties = FALSE) |>
  ungroup()


library(brms)

# Base linear model
mod_PPT_1d <- lm(PPT_1d ~ DOY, data = semDat)

mod_avgPar <- brm(
  avgPar ~ PPT_1d + DOY + (1 | Site) + gp(cont_time, by = Site),
  data = semDat,
  cores = 4,
  iter = 500,
  chains = 2,
  save_model = here("Results/brms/stan/avgPar.txt"),
  file = here("Results/brms/models/avgPar.rds")
)

mod_water.temp <- brm(
  water.temp ~ avgPar + PPT_1d + DOY + gp(cont_time, by = Site),
  data = semDat,
  cores = 4,
  iter = 500,
  save_model = here("Results/brms/stan/water_temp.txt"),
  file = here("Results/brms/models/water_temp.rds")
)

mod_Depth_1d <- brm(
  Depth_1d ~ water.temp + avgPar + PPT_1d + DOY +
    excProb + headGradient_1d + percentConnected + AlphaCentrality +
    upstream_length + PercWet_1d + (1 | Site) + gp(cont_time, by = Site),
  data = semDat,
  cores = 4,
  iter = 500,
  save_model = here("Results/brms/stan/Depth_1d.txt"),
  file = here("Results/brms/models/Depth_1d.rds")
)

mod_excProb <- brm(
  excProb ~ water.temp + avgPar + PPT_1d + DOY +
    percentConnected + AlphaCentrality + upstream_length + PercWet_1d + 
    (1 | Site) + gp(cont_time, by = Site),
  data = semDat,
  family = gaussian(),
  control = list(adapt_delta = 0.95, max_treedepth = 15),
  cores = 4,
  iter = 500,
  save_model = here("Results/brms/stan/excProb.txt"),
  file = here("Results/brms/models/excProb.rds")
)

mod_headGradient_1d <- brm(
  headGradient_1d ~ avgPar + water.temp + PPT_1d + DOY +
    percentConnected + AlphaCentrality + upstream_length + PercWet_1d +
    (1 | Site) + gp(cont_time, by = Site),
  data = semDat,
  cores = 4,
  iter = 500,
  save_model = here("Results/brms/stan/headGradient_1d.txt"),
  file = here("Results/brms/models/headGradient_1d.rds")
)

mod_percentConnected <- brm(
  percentConnected ~ water.temp + avgPar + PPT_1d + DOY + gp(cont_time, by = Site),
  data = semDat,
  cores = 4,
  iter = 500,
  save_model = here("Results/brms/stan/percentConnected.txt"),
  file = here("Results/brms/models/percentConnected.rds")
)

mod_AlphaCentrality <- brm(
  AlphaCentrality ~ water.temp + avgPar + percentConnected + PPT_1d +
    DOY + gp(cont_time, by = Site),
  data = semDat,
  cores = 4,
  iter = 500,
  save_model = here("Results/brms/stan/AlphaCentrality.txt"),
  file = here("Results/brms/models/AlphaCentrality.rds")
)

mod_upstream_length <- brm(
  upstream_length ~ water.temp + avgPar + AlphaCentrality + percentConnected +
    PPT_1d + DOY + (1 | Site) + gp(cont_time, by = Site),
  data = semDat,
  cores = 4,
  iter = 500,
  save_model = here("Results/brms/stan/upstream_length.txt"),
  file = here("Results/brms/models/upstream_length.rds")
)

mod_PercWet_1d <- brm(
  PercWet_1d ~ water.temp + avgPar + percentConnected + upstream_length +
    AlphaCentrality + PPT_1d + DOY + gp(cont_time, by = Site),
  data = semDat,
  cores = 4,
  iter = 500,
  save_model = here("Results/brms/stan/PercWet_1d.txt"),
  file = here("Results/brms/models/PercWet_1d.rds")
)

mod_DOSat.exc <- brm(
  DOSat.exc ~ water.temp + PPT_1d + Depth_1d + excProb + percentConnected +
    AlphaCentrality + upstream_length + PercWet_1d + headGradient_1d +
    avgPar + DOY + gp(cont_time, by = Site),
  data = semDat,
  cores = 4,
  iter = 500,
  save_model = here("Results/brms/stan/DOSat.exc.txt"),
  file = here("Results/brms/models/DOSat.exc.rds")
)

mod_DOpctsat.amp <- brm(
  DOpctsat.amp ~ DOSat.exc + water.temp + PPT_1d + Depth_1d +
    excProb + percentConnected + AlphaCentrality + upstream_length +
    PercWet_1d + headGradient_1d + avgPar + DOY +
    (1 | Site) + gp(cont_time, by = Site),
  data = semDat,
  cores = 4,
  iter = 500,
  save_model = here("Results/brms/stan/DOpctsat.amp.txt"),
  file = here("Results/brms/models/DOpctsat.amp.rds")
)




library(glmmTMB)

#semDat <- semDat |> 
#  mutate(cont_time = numFactor(cont_time))

# Base linear model
mod_PPT_tot <- lm(PPT_tot ~ DOY, data = semDat)
mod_avgPar <- glmmTMB(
  avgPar ~ PPT_tot + DOY + (1 | Site) + ar(cont_time + 0 | Site),
  data = semDat,
  sparseX = c(cond = TRUE),
  control = glmmTMBControl(
    optimizer = optim,
    optArgs = list(method = "BFGS", maxit = 1000)
  )
)
mod_water.temp <- glmmTMB(
  water.temp ~ avgPar + PPT_tot + DOY + ar(cont_time + 0 | Site),
  data = semDat,
  sparseX = c(cond = TRUE),
  control = glmmTMBControl(
    optimizer = optim,
    optArgs = list(method = "BFGS", maxit = 1000)
  )
)
mod_Depth <- glmmTMB(
  Depth ~ water.temp + avgPar + PPT_tot + DOY +
    excProb + headGradient + percentConnected + AlphaCentrality +
    upstream_length + PercWet + (1 | Site) + ar(cont_time + 0 | Site),
  data = semDat,
  sparseX = c(cond = TRUE),
  control = glmmTMBControl(
    optimizer = optim,
    optArgs = list(method = "BFGS", maxit = 1000)
  )
)
mod_excProb <- glmmTMB(
  excProb ~ water.temp + avgPar + PPT_tot + DOY +
    percentConnected + AlphaCentrality + upstream_length + PercWet + 
    (1 | Site) + ar(cont_time + 0 | Site),
  data = semDat,
  sparseX = c(cond = TRUE),
  control = glmmTMBControl(
    optimizer = optim,
    optArgs = list(method = "BFGS", maxit = 1000)
  )
)
mod_headGradient <- glmmTMB(
  headGradient ~ avgPar + water.temp + PPT_tot + DOY +
    percentConnected + AlphaCentrality + upstream_length + PercWet +
    (1 | Site) + ar(cont_time + 0 | Site),
  data = semDat,
  sparseX = c(cond = TRUE),
  control = glmmTMBControl(
    optimizer = optim,
    optArgs = list(method = "BFGS", maxit = 1000)
  )
)
mod_percentConnected <- glmmTMB(
  percentConnected ~ water.temp + avgPar + PPT_tot + DOY + ar(cont_time + 0 | Site),
  data = semDat,
  sparseX = c(cond = TRUE),
  control = glmmTMBControl(
    optimizer = optim,
    optArgs = list(method = "BFGS", maxit = 1000)
  )
)
mod_AlphaCentrality <- glmmTMB(
  AlphaCentrality ~ water.temp + avgPar + percentConnected + PPT_tot +
    DOY + ar(cont_time + 0 | Site),
  data = semDat,
  sparseX = c(cond = TRUE),
  control = glmmTMBControl(
    optimizer = optim,
    optArgs = list(method = "BFGS", maxit = 1000)
  )
)
mod_upstream_length <- glmmTMB(
  upstream_length ~ water.temp + avgPar + AlphaCentrality + percentConnected +
    PPT_tot + DOY + (1 | Site) + ar(cont_time + 0 | Site),
  data = semDat,
  sparseX = c(cond = TRUE),
  control = glmmTMBControl(
    optimizer = optim,
    optArgs = list(method = "BFGS", maxit = 1000)
  )
)
mod_PercWet <- glmmTMB(
  PercWet ~ water.temp + avgPar + percentConnected + upstream_length +
    AlphaCentrality + PPT_tot + DOY + ar(cont_time + 0 | Site),
  data = semDat,
  sparseX = c(cond = TRUE),
  control = glmmTMBControl(
    optimizer = optim,
    optArgs = list(method = "BFGS", maxit = 1000)
  )
)
mod_DOSat.exc <- glmmTMB(
  DOSat.exc ~ water.temp + PPT_tot + Depth + excProb + percentConnected +
    AlphaCentrality + upstream_length + PercWet + headGradient +
    avgPar + DOY + ar(cont_time + 0 | Site),
  data = semDat,
  sparseX = c(cond = TRUE),
  control = glmmTMBControl(
    optimizer = optim,
    optArgs = list(method = "BFGS", maxit = 1000)
  )
)
mod_DOpctsat.amp <- glmmTMB(
  DOpctsat.amp ~ DOSat.exc + water.temp + PPT_tot + Depth +
    excProb + percentConnected + AlphaCentrality + upstream_length +
    PercWet + headGradient + avgPar + DOY +
    (1 | Site) + ar(cont_time | Site),
  data = semDat,
  sparseX = c(cond = TRUE),
  control = glmmTMBControl(
    optimizer = optim,
    optArgs = list(method = "BFGS", maxit = 1000)
  )
)