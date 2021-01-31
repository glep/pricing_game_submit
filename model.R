# In this module, we ask you to define your pricing model, in R.

# TODO: load your packages here.
suppressPackageStartupMessages({
  library(recipes)
  library(xgboost)
  library(mgcv)
  library(janitor)
  library(tidyverse)
})
# Don't forget to list all packages you use to the `install.R` file.
source("utils.R")

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")
conflicted::conflict_prefer("collapse", "dplyr")
conflicted::conflict_prefer("slice", "dplyr")
conflicted::conflict_prefer("step", "recipes")


# (optional) data pre-processing function.
preprocess_X_data <- function (x_raw){
  # Data preprocessing function: given X_raw, clean the data for training or prediction.
  
  # Parameters
  # X_raw : Dataframe, with the columns described in the data dictionary.
  #   Each row is a different contract. This data has not been processed.
  
  # Returns
  # A cleaned / preprocessed version of the dataset
  
  
  
  # The result trained_model is something that you will save in the next section
  return(x_raw) # change this to return the cleaned data
}

# xgb combine code --------------------------------------------------------

define_xgb_recipe_freq <- function(df) {
  rec <- 
    recipe(
      data = df[1, ],
      formula = claim ~ .
    ) %>% 
    update_role(expo, new_role = "weight") %>% 
    update_role(unique_id, new_role = "id") %>% 
    update_role(id_policy, new_role = "policy_number") %>% 
    update_role(c(year), new_role = "control") %>% 
    update_role(claim_amount, new_role = "tw_outcome") %>% 
    update_role(fold, new_role = "cv_fold") %>% 
    step_other(vh_make_model, threshold = 2e3) %>% 
    step_mutate(density = population / town_surface_area, role = "predictor") %>% 
    step_dummy_xgb(df = df) 
  rm(df)
  rec
}

define_xgb_recipe_sev <- function(df) {
  rec <- 
    recipe(
      data = df[1, ],
      formula = claim_amount ~ .
    ) %>% 
    update_role(expo, new_role = "weight") %>% 
    update_role(unique_id, new_role = "id") %>% 
    update_role(id_policy, new_role = "policy_number") %>% 
    update_role(c(year), new_role = "control") %>% 
    update_role(claim, new_role = "freq_outcome") %>% 
    update_role(fold, new_role = "cv_fold") %>% 
    step_other(vh_make_model, threshold = 2e3) %>% 
    step_mutate(density = population / town_surface_area, role = "predictor") %>% 
    step_dummy_xgb(df = df) 
  rm(df)
  rec
}

fit_freqsev_xgb <- function(recipe, training_data, params, model_type) {
  set.seed(params$seed)
  
  prep_rec <- prep(x = recipe, training = training_data, retain = FALSE)
  baked_data <- bake(prep_rec, new_data = training_data)
  
  # Add offset variable by control features
  baked_data <- baked_data %>% 
    add_control_average(
      df              = .,
      model_type      = model_type,
      control_vars    = rec_has_role(prep_rec, "control"),
      target_var      = rec_has_role(prep_rec, "outcome"),
      expo_var        = rec_has_role(prep_rec, "weight"),
      name_offset_var = "offset"
    ) 
  
  df_control <- baked_data %>% 
    select(all_of(rec_has_role(prep_rec, "control")), offset) %>% 
    distinct()
  
  # Put data in xgb format
  dmat <- 
    xgb.DMatrix(
      data        = baked_data %>% select(all_of(rec_has_role(prep_rec, "predictor"))) %>% as.matrix(),
      label       = baked_data %>% pull(all_of(rec_has_role(prep_rec, "outcome"))),
      base_margin = baked_data %>% pull(offset) %>% log()
    )
  
  params$seed <- NULL
  
  # Run cross-validation models
  xgb_cv <-
    xgboost::xgb.cv(
      data                  = dmat,
      params                = params,
      nrounds               = 2e4,
      nthread               = parallel::detectCores() - 1,
      nfold                 = 5,
      early_stopping_rounds = 20,
      verbose               = FALSE
    )
  
  message(Sys.time(), "\tbest iteration: \t", xgb_cv$best_iteration)
  
  xgb_model <- 
    xgb.train(
      params  = params,
      data    = dmat,
      nrounds = xgb_cv$best_iteration,
      nthread = parallel::detectCores() - 1
    )
  
  rm(baked_data, dmat, xgb_cv, training_data)
  
  list(
    recipe     = prep_rec,
    model      = xgb_model,
    df_control = df_control
  )
  
}

fit_combine <- function(df) {
  #### Combine part
  
  freq_rec <- define_xgb_recipe_freq(df)
  sev_rec  <- define_xgb_recipe_sev(df)
  
  freq_params <- 
    list(max.depth              = 4,
         min_child_weight       = 1e3,
         eta                    = .03,
         objective              = "count:poisson",
         gamma                  = 0,
         subsample              = 0.8,
         colsample_bytree       = 0.8,
         base_score             = 1,
         eval_metric            = "poisson-nloglik",
         seed                   = 2508)
  sev_params <- 
    list(max.depth              = 4,
         min_child_weight       = 1e3,
         eta                    = .03,
         objective              = "reg:gamma",
         gamma                  = 0,
         subsample              = 0.8,
         colsample_bytree       = 0.8,
         base_score             = 1,
         eval_metric            = "gamma-nloglik",
         seed                   = 2508)
  
    list(
      freq_model = 
        fit_freqsev_xgb(
          recipe        = freq_rec,
          training_data = df,
          params        = freq_params,
          model_type    = "freq"
        ),
      sev_model  =
        fit_freqsev_xgb(
          recipe = sev_rec,
          training_data = df %>% filter(claim > 0),
          params = sev_params,
          model_type = "sev"
        )
    )
}

predict_expected_claim_xgb_combine <- function(model, x_raw){
  ## freq part
  baked_data <- bake(model$freq_model$recipe, new_data = x_raw)
  baked_data <- 
    left_join(
      baked_data,
      model$freq_model$df_control,
      by = rec_has_role(model$freq_model$recipe, "control")
    )
  if (!all.equal(baked_data$unique_id, x_raw$unique_id))
    stop("problem in predict_expected_claim_xgb_base")
  
  df_pred_freq <- baked_data %>% 
    select(unique_id, offset) %>% 
    mutate(
      pred_freq =
        predict(
          object = model$freq_model$model, 
          newdata = baked_data %>%
            select(all_of(rec_has_role(model$freq_model$recipe, "predictor"))) %>% 
            as.matrix()
        ),
      pred_freq = pred_freq * offset
    ) %>% 
    select(-offset)
  
  baked_data_sev <- bake(model$sev_model$recipe, new_data = x_raw)
  baked_data_sev <- 
    left_join(
      baked_data_sev,
      model$sev_model$df_control,
      by = rec_has_role(model$sev_model$recipe, "control")
    )
  if (!all.equal(baked_data$unique_id, x_raw$unique_id))
    stop("problem in predict_expected_claim_xgb_base")
  
  
  df_pred_sev <- baked_data_sev %>% 
    select(unique_id, offset) %>% 
    mutate(
      pred_sev =
        predict(
          object = model$sev_model$model, 
          newdata = baked_data_sev %>%
            select(all_of(rec_has_role(model$sev_model$recipe, "predictor"))) %>% 
            as.matrix()
        ),
      pred_sev = pred_sev * offset
    ) %>% 
    select(-offset)
  
  full_join(
    df_pred_freq,
    df_pred_sev,
    by = "unique_id"
  ) %>% 
    mutate(pred = pred_freq * pred_sev) 
}
  
# GAM noveh part ----------------------------------------------------------

define_mixed_gam_recipe <- function(df) {
  rec <- 
    recipe(
      data = df[1, ],
      formula = claim_amount ~ .
    ) %>% 
    update_role(expo, new_role = "weight") %>% 
    update_role(unique_id, new_role = "id") %>% 
    update_role(id_policy, new_role = "policy_number") %>% 
    update_role(c(year), new_role = "control") %>% 
    update_role(claim, new_role = "freq_outcome") %>% 
    update_role(fold, new_role = "cv_fold") %>% 
    step_mutate(density = population / town_surface_area, role = "predictor") %>% 
    step_range(c(drv_age1, drv_age2), min = 18, max = 75) %>% 
    step_mutate(drv_age2 = if_else(drv_drv2 == "Yes", true = drv_age2, false = -10)) %>% 
    step_meanimpute(intersect(all_numeric(), all_predictors())) %>%
    step_novel(all_nominal()) %>%
    step_string2factor(all_nominal()) %>% 
    step_modeimpute(intersect(all_nominal(), all_predictors())) %>%
    step_knnimpute(c(vh_speed, vh_value, vh_weight))
  rm(df)
  rec
}

fit_model_tweedie_noveh <- function (df) {
  
  rec <- define_mixed_gam_recipe(df) %>% prep(training = df, retain = FALSE)
  baked_data <- bake(rec, new_data = df)
  
  res_gam <- 
    bam(
      formula = claim_amount ~
        s(pol_no_claims_discount, bs = "tp") +
        pol_coverage +
        s(pol_duration, bs = "tp") +
        s(pol_sit_duration, bs = "tp") +
        pol_pay_freq +
        pol_payd +
        pol_usage +
        s(drv_age1, by = drv_sex1, bs = "tp") +
        s(drv_age_lic1, bs = "tp") +
        s(drv_age2, by = drv_drv2, bs = "tp") +
        s(vh_age, bs = "tp") +
        vh_fuel +
        vh_type +
        s(vh_speed, bs = "tp") +
        s(vh_value, bs = "tp") +
        s(vh_weight, bs = "tp") +
        s(density, bs = "tp") +
        s(population, bs = "tp"),
      family = Tweedie(p = 1.46, link = "log"),
      data = baked_data,
      nthreads = 5,
      discrete = TRUE
    )
  
  rm(df, baked_data)
  list(
    recipe = rec,
    model = res_gam
  )
  
}

predict_expected_claim_tweedie_gam <- function(model, x_raw){
  baked_data <- 
    bake(
      model$recipe,
      new_data = x_raw
    )
  
  x_raw %>% 
    select(unique_id) %>% 
    mutate(
      pred = predict(model$model, newdata = baked_data, type = "response") %>%
        as.numeric()
    ) 
}

# Fit functions -----------------------------------------------------------



fit_model <- function (x_raw, y_raw){
  # Model training function: given training data (X_raw, y_raw), train this pricing model.
  
  # Parameters
  # X_raw : Dataframe, with the columns described in the data dictionary.
  # 	Each row is a different contract. This data has not been processed.
  # y_raw : a array, with the value of the claims, in the same order as contracts in X_raw.
  # 	A one dimensional array, with values either 0 (most entries) or >0.
  
  # Returns
  # self: (optional), this instance of the fitted model.
  
  
  # This function trains your models and returns the trained model.
  capping_threshold <- 1e4
  df <- bind_cols(y_raw, x_raw) %>% as_tibble() %>% 
    mutate(expo = 1) %>% 
    mutate(claim = as.numeric(claim_amount > 0)) %>% 
    mutate(claim_amount = pmin(claim_amount, capping_threshold)) %>% 
    mutate(unique_id = row_number(), fold = 1)
  
  
  # Combine part
  combine_model <- fit_combine(df)
  gam_model <- fit_model_tweedie_noveh(df)
  
  pred_combine <- predict_expected_claim_xgb_combine(combine_model, df)
  pred_gam <- predict_expected_claim_tweedie_gam(gam_model, df)
  
  mult_constants <- 
    bind_cols(
      y_raw,
      pred_combine %>% select(pred_combine = pred),
      pred_gam %>% select(pred_gam = pred)
    ) %>% 
    as_tibble() %>% 
    summarise(across(c(pred_combine, pred_gam), ~ sum(claim_amount) / sum(.x)))
  
  list(
    xgb_model      = combine_model,
    gam_model      = gam_model,
    mult_constants = mult_constants
  )
  
}


predict_expected_claim <- function(model, x_raw){
  # Model prediction function: predicts the average claim based on the pricing model.
  
  # This functions estimates the expected claim made by a contract (typically, as the product
  # of the probability of having a claim multiplied by the average cost of a claim if it occurs),
  # for each contract in the dataset X_raw.
  
  # This is the function used in the RMSE leaderboard, and hence the output should be as close
  # as possible to the expected cost of a contract.
  
  # Parameters
  # ----------
  # X_raw : Dataframe, with the columns described in the data dictionary.
  # 	Each row is a different contract. This data has not been processed.
  
  # Returns
  # -------
  # avg_claims: a one-dimensional array of the same length as X_raw, with one
  #     average claim per contract (in same order). These average claims must be POSITIVE (>0).
  x_raw <- x_raw %>% mutate(unique_id = row_number())
  
  xgb_predict <- predict_expected_claim_xgb_combine(model$xgb_model, x_raw) 
  gam_predict <- predict_expected_claim_tweedie_gam(model$gam_model, x_raw) 
  
  full_join(
    xgb_predict %>% transmute(unique_id, pred_xgb = pred),
    gam_predict %>% transmute(unique_id, pred_gam = pred) 
  ) %>% 
    mutate(
      pred_xgb = pred_xgb * model$mult_constants$pred_combine,
      pred_gam = pred_gam * model$mult_constants$pred_gam
    ) %>% 
    mutate(pred = .5 * (pred_xgb + pred_gam)) %>% 
    arrange(unique_id) %>% 
    pull(pred)
}


predict_premium <- function(model, x_raw){
  # Model prediction function: predicts premiums based on the pricing model.
  
  # This function outputs the prices that will be offered to the contracts in X_raw.
  # premium will typically depend on the average claim predicted in 
  # predict_expected_claim, and will add some pricing strategy on top.
  
  # This is the function used in the average profit leaderboard. Prices output here will
  # be used in competition with other models, so feel free to use a pricing strategy.
  
  # Parameters
  # ----------
  # X_raw : Dataframe, with the columns described in the data dictionary.
  # 	Each row is a different contract. This data has not been processed.
  
  # Returns
  # -------
  # prices: a one-dimensional array of the same length as X_raw, with one
  #     price per contract (in same order). These prices must be POSITIVE (>0).
  
  pmax(25, predict_expected_claim(model, x_raw) * 1.2)
}


save_model <- function(model){
  # Saves this trained model to a file.
  
  # This is used to save the model after training, so that it can be used for prediction later.
  
  # Do not touch this unless necessary (if you need specific features). If you do, do not
  #  forget to update the load_model method to be compatible.
  
  # Save in `trained_model.RData`.
  
  write_rds(model, file = "trained_model.RDS")
}


load_model <- function(){ 
  # Load a saved trained model from the file `trained_model.RData`.
  
  #    This is called by the server to evaluate your submission on hidden data.
  #    Only modify this *if* you modified save_model.
  
  read_rds("trained_model.RDS")
}