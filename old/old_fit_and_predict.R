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
