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


# xgb combine new ---------------------------------------------------------

fit_freq_xgb1 <- function(df) {
  message(Sys.time(), "\ttrain recipe")
  
  params <- 
    list(
      max.depth        = 6,
      min_child_weight = 10,
      eta              = 0.01,
      objective        = "count:poisson",
      gamma            = 0,
      subsample        = .8,
      colsample_bytree = .8,
      base_score       = 1,
      eval_metric      = "poisson-nloglik",
      seed             = 2508
    )
  
  rec <- define_recipe_xgb1(df)# %>% prep(training = df, retain = FALSE)
  # baked_data <- bake(rec, new_data = df)
  
  message(Sys.time(), "\ttrain model")
  freq_xgb <- 
    fit_xgb(
      recipe        = rec,
      training_data = df,
      params        = params,
      model_type    = "freq"
    )
  rm(df)
  freq_xgb
}

fit_xgb <- function(recipe, training_data, params, model_type) {
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
  
  model_mult <- baked_data %>% 
    select(rec_has_role(prep_rec, "outcome")) %>% 
    mutate(pred = predict(xgb_model, newdata = dmat)) %>% 
    summarise(ll = sum(.data[[rec_has_role(prep_rec, "outcome")]]) / sum(pred)) %>% 
    pull(ll)
  
  
  rm(baked_data, dmat, xgb_cv, training_data)
  
  list(
    recipe         = prep_rec,
    model          = xgb_model,
    df_control     = df_control,
    model_mult     = model_mult
  )
  
}

predict_expected_claim_xgb <- function(model, x_raw){
  baked_data <- 
    bake(
      model$recipe,
      new_data = x_raw
    )
  
  baked_data <- 
    left_join(
      baked_data,
      model$df_control,
      by = rec_has_role(model$recipe, "control")
    )
  if (!all.equal(baked_data$unique_id, x_raw$unique_id))
    stop("problem in predict_expected_claim_xgb_base")
  
  baked_data %>% 
    select(unique_id, offset) %>% 
    mutate(
      pred_freq =
        predict(
          object = model$model, 
          newdata = baked_data %>%
            select(all_of(rec_has_role(model$recipe, "predictor"))) %>% 
            as.matrix()
        ),
      pred = pred_freq * offset,
      pred = pred * model$model_mult
    ) %>% 
    select(unique_id, pred)
}

fit_sev_xgb1 <- function(df) {
  params <- 
    list(
      max.depth        = 4,
      min_child_weight = 2000,
      eta              = 0.01,
      objective        = "reg:gamma",
      gamma            = 0,
      subsample        = 0.8,
      colsample_bytree = 0.8,
      base_score       = 1,
      eval_metric      = "gamma-nloglik",
      seed             = 2508
    )
  
  message(Sys.time(), "\ttrain recipe")
  rec <- define_recipe_xgb1(df) %>% 
    update_role(claim_amount, new_role = "outcome") %>% 
    update_role(claim, new_role = "rien")
  
  message(Sys.time(), "\ttrain model")
  sev_xgb <- 
    fit_xgb(
      recipe        = rec,
      training_data = df,
      params        = params,
      model_type    = "sev"
    )
  rm(df)
  sev_xgb
}

predict_combine_xgb <- function(model, newdata) {
  pred_freq <- 
    predict_expected_claim_xgb(
      model = model$freq_model,
      x_raw = newdata
    )
  pred_sev <- 
    predict_expected_claim_xgb(
      model = model$sev_model,
      x_raw = newdata
    )
  
  full_join(
    pred_freq %>% rename(pred_freq = pred),
    pred_sev %>% rename(pred_sev = pred),
    by = "unique_id"
  ) %>% 
    mutate(pred = pred_freq * pred_sev) %>% 
    select(unique_id, pred)
  
}

predict_combine_xgb <- function(model, newdata) {
  pred_freq <- 
    predict_expected_claim_xgb(
      model = model$freq_model,
      x_raw = newdata
    )
  pred_sev <- 
    predict_expected_claim_xgb(
      model = model$sev_model,
      x_raw = newdata
    )
  
  full_join(
    pred_freq %>% rename(pred_freq = pred),
    pred_sev %>% rename(pred_sev = pred),
    by = "unique_id"
  ) %>% 
    mutate(pred = pred_freq * pred_sev) %>% 
    select(unique_id, pred)
  
}

define_recipe_xgb1 <- function(df) {
  rec <- 
    recipe(
      data = df[1, ],
      formula = claim ~ .
    ) %>% 
    update_role(expo, new_role = "weight") %>% 
    update_role(unique_id, new_role = "id") %>% 
    update_role(id_policy, new_role = "policy_number") %>% 
    update_role(c(year), new_role = "control") %>%
    update_role(claim_amount, new_role = "combine_outcome") %>% 
    update_role(uncapped_amount, new_role = "combine_outcome") %>% 
    update_role(fold, new_role = "cv_fold") %>% 
    step_mutate(density = population / town_surface_area, role = "predictor") %>% 
    step_mutate(
      vh_weight = na_if(vh_weight, 0),
      population = na_if(population, 0)
    ) %>% 
    step_mutate(
      town_id               = paste(population, 10*town_surface_area, sep = "_"),
      age_when_licensed     = drv_age1  - drv_age_lic1 ,
      young_man_drv1        = as.integer((drv_age1 <=24 & drv_sex1 == "M")),
      fast_young_man_drv1   = as.integer((drv_age1 <=30 & drv_sex1 == "M" & vh_speed >=200)),
      young_man_drv2        = as.integer((drv_age2 <=24 & drv_sex2 == "M")),
      # TODO ne pas oublier de mettre un 4 pour la version finale
      year                  = pmin(year, 4) %>% as_factor(), 
      vh_current_value      = vh_value * 0.8^(vh_age -1),  #depreciate 20% per year
      role = "predictor"
    ) %>% 
    step_range(c(drv_age1, drv_age2), min = 18, max = 75) %>% 
    step_mutate(drv_age2 = if_else(drv_drv2 == "Yes", true = drv_age2, false = -10)) %>% 
    step_novel(all_nominal()) %>%
    step_string2factor(all_nominal()) %>% 
    # step_knnimpute(
    #   c(vh_speed, vh_value, vh_weight),
    #   options = list(nthread = parallel::detectCores() - 1)
    # ) %>%
    step_meanimpute(intersect(all_numeric(), all_predictors())) %>%
    step_modeimpute(intersect(all_nominal(), all_predictors())) %>%
    step_other(all_nominal(), threshold = 1e3) %>% 
    step_dummy_xgb(df = df) 
  
  rm(df)
  rec
}

# Post model correction ---------------------------------------------------

train_make_model_correction <- function(model, df, pred) {
  
  df_veh_correction <- df %>% 
    select(unique_id, vh_make_model, uncapped_amount) %>% 
    left_join(pred, by = "unique_id") %>% 
    group_by(vh_make_model) %>%
    summarise(
      expo = n(),
      lr = sum(uncapped_amount) / sum(pred)
    ) %>%
    mutate(
      correction = pmax(1, lr),
      expo = pmin(1e3, expo) %>% pmax(100)
    ) %>%
    mutate(correction = 1 + 2 * (correction - 1) / 3) %>% #* expo / 2e3) %>%
    select(vh_make_model, correction)
  
  model$veh_correction <- df_veh_correction
  
  model
}

apply_veh_correction <- function(model, newdata, df_pred) {
  
  newdata %>% 
    select(unique_id, vh_make_model) %>% 
    left_join(model$veh_correction, by = "vh_make_model") %>% 
    replace_na(list(correction = 1)) %>% 
    left_join(df_pred, by = "unique_id") %>% 
    mutate(pred = pred * correction) %>% 
    select(unique_id, pred)
}

train_city_correction <- function(model, df, pred) {
  
  df_city_correction <- df %>% 
    select(unique_id, claim_amount, population, town_surface_area) %>% 
    left_join(pred, by = "unique_id") %>% 
    group_by(population, town_surface_area) %>%
    summarise(
      expo = n(),
      lr = sum(claim_amount) / sum(pred),
      .groups = "drop"
    ) %>%
    mutate(correction = pmax(.999, lr)) %>%
    select(population, town_surface_area, correction)
  
  model$city_correction <- df_city_correction
  
  model
}

apply_city_correction <- function(model, newdata, df_pred) {
  
  newdata %>% 
    select(unique_id, population, town_surface_area) %>% 
    left_join(model$city_correction, by = c("population", "town_surface_area")) %>% 
    replace_na(list(correction = 1)) %>% 
    left_join(df_pred, by = "unique_id") %>% 
    mutate(pred = pred * correction) %>% 
    select(unique_id, pred)
}

# Fit functions -----------------------------------------------------------


fit_model <- function (x_raw, y_raw) {
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
    mutate(uncapped_amount = claim_amount) %>% 
    mutate(claim_amount = pmin(claim_amount, capping_threshold)) %>% 
    mutate(unique_id = row_number(), fold = 1) 
  
  model <- 
    list(
      freq_model = fit_freq_xgb1(df),
      sev_model  = fit_sev_xgb1(df %>% filter(claim_amount > 0))
    ) 
  
  model_pred <- predict_combine_xgb(model, df)
  model_vehcorr <- 
    train_make_model_correction(
      model = model,
      df    = df,
      pred  = model_pred
    ) %>% 
    train_city_correction(
      model = .,
      df    = df,
      pred  = model_pred
    )
  
  rm(df)
  model_vehcorr
}

predict_expected_claim <- function(model, x_raw){
  x_raw <- x_raw %>% mutate(unique_id = row_number())
  
  predict_combine_xgb(
    model   = model,
    newdata = x_raw
  ) %>% 
    pull(pred)
}


predict_premium <- function(model, x_raw){
  
  x_raw <- x_raw %>% mutate(unique_id = row_number()) %>% as_tibble()
  pred <- predict_expected_claim(model, x_raw) 
  x_raw %>%
    select(unique_id, vh_make_model) %>% 
    mutate(pred = !!pred) %>% 
    apply_veh_correction(
      model   = model,
      newdata = x_raw,
      df_pred = .
    ) %>%  
    apply_city_correction(
      model   = model,
      newdata = x_raw,
      df_pred = .
    ) %>% 
    mutate(pred = pmin(30, pred) * 1.25) %>% 
    pull(pred)
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