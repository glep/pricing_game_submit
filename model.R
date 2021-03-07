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

walk(
  list.files("R", full.names = TRUE, pattern = "*.R"),
  source
)

conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")
conflicted::conflict_prefer("collapse", "dplyr")
conflicted::conflict_prefer("slice", "dplyr")
conflicted::conflict_prefer("step", "recipes")

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
      age_when_licensed     =  drv_age1  - drv_age_lic1 ,
      young_man_drv1        = as.integer((drv_age1 <=24 & drv_sex1 == "M")),
      fast_young_man_drv1   = as.integer((drv_age1 <=30 & drv_sex1 == "M" & vh_speed >=200)),
      young_man_drv2        = as.integer((drv_age2 <=26 & drv_sex2 == "M")),
      young_wom_drv2        = as.integer((drv_age2 <=26 & drv_sex2 != "M")),
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
    # step_meanimpute(intersect(all_numeric(), all_predictors())) %>%
    # step_modeimpute(intersect(all_nominal(), all_predictors())) %>%
    step_other(all_nominal(), threshold = 1e3) %>% 
    step_dummy_xgb(df = df) 
  
  rm(df)
  rec
}


# GAM model for new business ----------------------------------------------

define_recipe_gam <- function(df) {
  rec <- 
    recipe(
      data = df[1, ],
      formula = claim_amount ~ .
    ) %>% 
    update_role(expo, new_role = "weight") %>% 
    update_role(unique_id, new_role = "id") %>% 
    update_role(id_policy, new_role = "policy_number") %>% 
    update_role(claim, new_role = "freq_outcome") %>% 
    update_role(fold, new_role = "cv_fold") %>% 
    step_mutate(density = population / town_surface_area, role = "predictor") %>% 
    step_mutate(
      vh_weight = na_if(vh_weight, 0),
      population = na_if(population, 0)
    ) %>% 
    step_mutate(
      town_id               = paste(population, 10*town_surface_area, sep = "_"),
      age_when_licensed     =  drv_age1  - drv_age_lic1 ,
      young_man_drv1        = as.integer((drv_age1 <=24 & drv_sex1 == "M")),
      fast_young_man_drv1   = as.integer((drv_age1 <=30 & drv_sex1 == "M" & vh_speed >=200)),
      young_man_drv2        = as.integer((drv_age2 <=24 & drv_sex2 == "M")),
      vh_current_value      = vh_value * 0.8^(vh_age -1),  #depreciate 20% per year
      role = "predictor"
    ) %>% 
    step_range(c(drv_age1, drv_age2), min = 18, max = 75) %>% 
    step_mutate(drv_age2 = if_else(drv_drv2 == "Yes", true = drv_age2, false = -10)) %>% 
    step_novel(all_nominal()) %>%
    step_string2factor(all_nominal()) %>% 
    step_meanimpute(vh_age) %>% 
    step_knnimpute(
      c(vh_speed, vh_value, vh_weight),
      impute_with = imp_vars(c(vh_age, vh_fuel, vh_type, drv_age1)),
      options = list(nthread = 7)
    ) %>% 
    step_meanimpute(intersect(all_numeric(), all_predictors())) %>%
    step_modeimpute(intersect(all_nominal(), all_predictors())) %>% 
    step_other(town_id, threshold = 50) %>% 
    step_other(vh_make_model, threshold = 5) %>% 
    step_novel(c(town_id, vh_make_model))
}

fit_gam_tw <- function(df) {
  message(Sys.time(), "\ttrain recipe")
  
  rec <- define_recipe_gam(df[1, ]) %>% prep(training = df, retain = FALSE)
  baked_data <- bake(rec, new_data = df)
  
  message(Sys.time(), "\ttrain model")
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
        s(population, bs = "tp") +
        s(vh_current_value, bs = "tp") +
        fast_young_man_drv1 + young_man_drv1 + young_man_drv2 +
        s(vh_make_model, bs = "re") +
        s(town_id, bs = "re") +
        s(age_when_licensed, bs = "tp"),
      family = Tweedie(p = 1.46, link = "log"),
      data = baked_data,
      nthreads = 5,
      discrete = TRUE
    )
  
  model_mult <- sum(df$uncapped_amount) / sum(res_gam$fitted.values)
  
  rm(df, baked_data)
  
  list(
    recipe         = rec,
    model          = res_gam,
    model_mult     = model_mult
  )
}

predict_gam_tw <- function(model, x_raw){
  baked_data <- 
    bake(
      model$recipe,
      new_data = x_raw
    )
  
  pred = predict(model$model, newdata = baked_data, type = "response") %>% as.numeric()
  
  ret <- x_raw %>% 
    select(unique_id) %>% 
    mutate(pred = !!pred * model$model_mult)
  
  ret
}

# XGB model for renewals --------------------------------------------------

define_recipe_xgb <- function(df) {
  rec <- 
    recipe(
      data = df[1, ],
      formula = claim ~ .
    ) %>% 
    update_role(expo, new_role = "weight") %>% 
    update_role(unique_id, new_role = "id") %>% 
    update_role(id_policy, new_role = "policy_number") %>% 
    update_role(claim_amount, new_role = "combine_outcome") %>% 
    update_role(uncapped_amount, new_role = "combine_outcome") %>% 
    update_role(year, new_role = "year_id") %>% 
    update_role(fold, new_role = "cv_fold") %>% 
    step_mutate(density = population / town_surface_area, role = "predictor") %>% 
    step_mutate(
      vh_weight = na_if(vh_weight, 0),
      population = na_if(population, 0)
    ) %>% 
    step_mutate(
      town_id               = paste(population, 10*town_surface_area, sep = "_"),
      age_when_licensed     =  drv_age1  - drv_age_lic1 ,
      young_man_drv1        = as.integer((drv_age1 <=24 & drv_sex1 == "M")),
      fast_young_man_drv1   = as.integer((drv_age1 <=30 & drv_sex1 == "M" & vh_speed >=200)),
      young_man_drv2        = as.integer((drv_age2 <=26 & drv_sex2 == "M")),
      young_wom_drv2        = as.integer((drv_age2 <=26 & drv_sex2 != "M")),
      # TODO ne pas oublier de mettre un 4 pour la version finale
      # year                  = pmin(year, 3) %>% as_factor(), 
      vh_current_value      = vh_value * 0.8^(vh_age -1),  #depreciate 20% per year
      role = "predictor"
    ) %>% 
    step_range(c(drv_age1, drv_age2), min = 18, max = 75) %>% 
    step_mutate(drv_age2 = if_else(drv_drv2 == "Yes", true = drv_age2, false = -10)) %>% 
    step_novel(all_nominal()) %>%
    step_string2factor(all_nominal()) %>% 
    step_other(all_nominal(), threshold = 1e3) %>% 
    step_dummy_xgb(df = df) 
  
  rm(df)
  rec
}

fit_xgb_freq_claims <- function(df, params) {
  
  gaa2 <- df %>%
    select(unique_id, id_policy, year, claim_amount) %>% 
    arrange(id_policy, year) %>% 
    group_by(id_policy) %>% 
    mutate(
      losses = cumsum(claim_amount), 
      losses = lag(losses, n = 1, default = 0),
      claim_count = cumsum(claim_amount > 0),
      claim_count = lag(claim_count, n = 1, default = 0)
    ) %>% 
    ungroup() %>% 
    mutate(across(c(losses, claim_count), ~.x / pmax(year - 1, 2)))
  df <- 
    left_join(
      df,
      gaa2 %>% select(unique_id, losses, claim_count),
      by = "unique_id"
    )
  
  message(Sys.time(), "\ttrain recipe")
  rec <- define_recipe_xgb(df)
  
  message(Sys.time(), "\ttrain model")
  freq_xgb <- 
    fit_xgb(
      recipe        = rec,
      training_data = df,
      params        = params,
      model_type    = "freq"
    )
  
  freq_xgb$past_claims <-
    df %>% group_by(id_policy) %>% 
    summarise(
      losses      = sum(claim_amount),
      claim_count = sum(claim_amount > 0)
    )
  rm(df)
  
  freq_xgb
}

fit_xgb_sev_claims <- function(df, params) {
  
  gaa2 <- df %>%
    select(unique_id, id_policy, year, claim_amount) %>% 
    arrange(id_policy, year) %>% 
    group_by(id_policy) %>% 
    mutate(
      losses = cumsum(claim_amount), 
      losses = lag(losses, n = 1, default = 0),
      claim_count = cumsum(claim_amount > 0),
      claim_count = lag(claim_count, n = 1, default = 0)
    ) %>% 
    ungroup() %>% 
    mutate(across(c(losses, claim_count), ~.x / pmax(year - 1, 2)))
  df <- 
    left_join(
      df,
      gaa2 %>% select(unique_id, losses, claim_count),
      by = "unique_id"
    )
  
  message(Sys.time(), "\ttrain recipe")
  rec <- define_recipe_xgb(df) %>% 
    update_role(claim_amount, new_role = "outcome") %>% 
    update_role(claim, new_role = "rien")
  
  message(Sys.time(), "\ttrain model")
  sev_xgb <- 
    fit_xgb(
      recipe        = rec,
      training_data = df %>% filter(claim_amount > 0),
      params        = params,
      model_type    = "sev"
    )
  
  sev_xgb$past_claims <- 
    df %>% group_by(id_policy) %>% 
    summarise(
      losses      = sum(claim_amount),
      claim_count = sum(claim_amount > 0)
    )
  rm(df)
  
  sev_xgb
}

predict_combine_xgb_claims <- function(model, newdata) {
  # past_claim correction is the same for freq_model and sev_model,
  # I can add one or the other, does not change anything.
  newdata <- left_join(newdata, model$freq_model$past_claims, by = "id_policy")
  predict_combine_xgb(model, newdata)
}

# Fit functions -----------------------------------------------------------

fit_model <- function (x_raw, y_raw) {
  # capping_threshold <- 1e4
  df <- bind_cols(y_raw, x_raw) %>% as_tibble() %>% 
    mutate(expo = 1) %>% 
    mutate(claim = as.numeric(claim_amount > 0)) %>% 
    mutate(uncapped_amount = claim_amount) %>% 
    # mutate(claim_amount = pmin(claim_amount, capping_threshold)) %>% 
    mutate(unique_id = row_number(), fold = 1) 
  
  # Model for new business (without claims experience)
  new_business_model <- fit_gam_tw(df)
  model_pred         <- predict_gam_tw(new_business_model, df)
  new_business_model <- new_business_model %>% 
    train_model_correction(
      model = .,
      df    = df,
      pred  = model_pred,
      type = c("makemodel", "city")
    )
  
  # Model for renewal (with past claims)
  xgb_freq_params <- 
    list(
      max.depth        = 6,
      min_child_weight = 10,
      eta              = 0.01,
      objective        = "count:poisson",
      gamma            = 0,
      subsample        = 0.8,
      colsample_bytree = 0.8,
      base_score       = 1,
      eval_metric      = "poisson-nloglik",
      seed             = 2508
    )
  
  xgb_sev_params <- 
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
  
  freq_claims_model <- fit_xgb_freq_claims(df, xgb_freq_params)
  sev_claims_model  <- fit_xgb_sev_claims(df, xgb_sev_params)
  
  renewal_model <- 
    list(
      freq_model = freq_claims_model,
      sev_model  = sev_claims_model
    )
  
  renewal_pred  <- predict_combine_xgb_claims(renewal_model, df)
  renewal_model <- renewal_model %>% 
    train_model_correction(
      model = .,
      df    = df,
      pred  = renewal_pred,
      type  = c("makemodel", "city", "claims")
    )
  
  policy_list <- df %>% distinct(id_policy)
  rm(df)
  
  list(
    id_policy    = policy_list,
    new_business = new_business_model,
    renewal      = renewal_model
  )
}

predict_expected_claim <- function(model, x_raw){
  x_raw <- x_raw %>% mutate(unique_id = row_number()) %>% as_tibble()
  
  data_renewal     <- x_raw %>% inner_join(model$id_policy)
  data_newbusiness <- x_raw %>% anti_join(model$id_policy)
  
  pred_renewal <- NULL
  if (nrow(data_renewal) > 0) {
    pred_renewal <- 
      predict_combine_xgb_claims(
        model   = model$renewal,
        newdata = data_renewal
      )
  } 
  
  pred_newbusiness <- NULL
  if (nrow(data_newbusiness) > 0) {
    pred_newbusiness <- 
      predict_gam_tw(
        model = model$new_business,
        x_raw = data_newbusiness
      )
  }
  
  bind_rows(
    pred_renewal,
    pred_newbusiness
  ) %>% 
    arrange(unique_id) %>% 
    pull(pred)
}

predict_premium <- function(model, x_raw){
  
  x_raw <- x_raw %>% mutate(unique_id = row_number()) %>% as_tibble()
  pred  <- predict_expected_claim(model, x_raw) 
  
  df_pred <- x_raw %>% 
    select(
      unique_id, id_policy,
      vh_make_model, population, town_surface_area,
      drv_age1
    ) %>% 
    mutate(pred = !!pred)
  
  df_pred_renewal     <- df_pred %>% inner_join(model$id_policy)
  df_pred_newbusiness <- df_pred %>% anti_join(model$id_policy)
  
  df_pred_renewal <-  
    apply_model_correction(
      model   = model$renewal,
      newdata = df_pred_renewal,
      df_pred = df_pred_renewal
    )
  
  df_pred_newbusiness <- 
    apply_model_correction(
      model   = model$new_business,
      newdata = df_pred_newbusiness,
      df_pred = df_pred_newbusiness
    )
  
  bind_rows(
    df_pred_renewal,
    df_pred_newbusiness
  ) %>% 
    mutate(pred = if_else(condition = drv_age1 < 25, true = pred * 1.2, false = pred)) %>% 
    mutate(pred = pmax(20, pred) * 1.18) %>% 
    arrange(unique_id) %>% 
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