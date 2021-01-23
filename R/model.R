# In this module, we ask you to define your pricing model, in R.

# TODO: load your packages here.
# Don't forget to list all packages you use to the `install.R` file.


# (optional) data pre-processing function.
preprocess_X_data <- function (x_raw){
  # Data preprocessing function: given X_raw, clean the data for training or prediction.

  # Parameters
  # ----------
  # X_raw : Dataframe, with the columns described in the data dictionary.
  #   Each row is a different contract. This data has not been processed.

  # Returns
  # -------
  # A cleaned / preprocessed version of the dataset

  # YOUR CODE HERE ------------------------------------------------------
  
  
  # ---------------------------------------------------------------------
  # The result trained_model is something that you will save in the next section
  return(x_raw) # change this to return the cleaned data
}


# Feel free to add other functions/constants to use in your model.

define_recipe <- function(df) {
  recipe(
    data = df[1, ],
    formula = claim_amount ~ .
  ) %>% 
    # update_role(expo, new_role = "weight") %>% 
    # update_role(unique_id, new_role = "id") %>% 
    update_role(id_policy, new_role = "policy_number") %>% 
    # update_role(c(year), new_role = "control") %>% 
    # update_role(claim, new_role = "freq_outcome") %>% 
    # update_role(fold, new_role = "cv_fold") %>% 
    step_mutate(density = population / town_surface_area, role = "predictor") %>% 
    step_range(c(drv_age1, drv_age2), min = 18, max = 75) %>% 
    step_mutate(drv_age2 = if_else(drv_drv2 == "Yes", true = drv_age2, false = -10)) %>% 
    step_meanimpute(intersect(all_numeric(), all_predictors())) %>%
    step_novel(all_nominal()) %>%
    step_string2factor(all_nominal()) %>% 
    step_modeimpute(intersect(all_nominal(), all_predictors())) %>%
    step_knnimpute(c(vh_speed, vh_value, vh_weight)) %>% 
    step_other(vh_make_model, threshold = 5)
}


fit_model <- function (x_raw, y_raw){
	# Model training function: given training data (X_raw, y_raw), train this pricing model.

	# Parameters
	# ----------
	# X_raw : Dataframe, with the columns described in the data dictionary.
	# 	Each row is a different contract. This data has not been processed.
	# y_raw : a array, with the value of the claims, in the same order as contracts in X_raw.
	# 	A one dimensional array, with values either 0 (most entries) or >0.

	# Returns
	# -------
	# self: (optional), this instance of the fitted model.

	
  # This function trains your models and returns the trained model.

  # First, create all new features, if necessary
  
  df <- bind_cols(y_raw, x_raw) %>% as_tibble() 
  
  rec <- define_recipe(df) %>% prep(training = df, retain = FALSE)
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
        s(population, bs = "tp") +
        s(vh_make_model, bs = "re"),
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
	
  y_predict = predict(model, newdata = x_raw)  # tweak this to work with your model

  return(y_predict)  
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
	
  return(predict_expected_claim(model, x_raw) * 2)  # Default: multiply prices by 2
}


save_model <- function(model){
  # Saves this trained model to a file.

  # This is used to save the model after training, so that it can be used for prediction later.

  # Do not touch this unless necessary (if you need specific features). If you do, do not
  #  forget to update the load_model method to be compatible.
	
  # Save in `trained_model.RData`.

  save(model, file='trained_model.RData')
}


load_model <- function(){ 
 # Load a saved trained model from the file `trained_model.RData`.

 #    This is called by the server to evaluate your submission on hidden data.
 #    Only modify this *if* you modified save_model.

  load('trained_model.RData')
  return(model)
}