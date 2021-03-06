train_model_correction <- function(model, df, pred, type) {
  if ("makemodel" %in% type) {
    model <- 
      train_make_model_correction(
        model = model,
        df    = df,
        pred  = pred
      )
  }
  
  if ("city" %in% type) {
    model <-
      train_city_correction(
        model = model,
        df    = df,
        pred  = pred
      )
  }
  
  if ("claims" %in% type) {
    model <- 
      train_claims_correction(
        model = model,
        df    = df,
        pred  = pred
      )
  }
  
  return(model)
}

apply_model_correction <- function(model, newdata, df_pred) {
  if ("veh_correction" %in% names(model)) {
    message("Post model make_model correction")
    df_pred <- apply_veh_correction(model, newdata, df_pred)
  }
  
  if ("city_correction" %in% names(model)) {
    df_pred <- apply_city_correction(model, newdata, df_pred)
    message("Post model city correction")
  }
  
  if ("claims_correction" %in% names(model)) {
    df_pred <- apply_claims_correction(model, newdata, df_pred)
    message("Post model Claims correction")
  }
  
  df_pred
}

# make_model correction ---------------------------------------------------

train_make_model_correction <- function(model, df, pred) {
  df_veh_correction <- df %>% 
    select(unique_id, vh_make_model, uncapped_amount) %>% 
    left_join(pred, by = "unique_id") %>% 
    group_by(vh_make_model) %>%
    summarise(lr = sum(uncapped_amount) / sum(pred)) %>%
    mutate(correction = pmax(1, lr)) %>%
    select(vh_make_model, correction)
  
  model$veh_correction <- df_veh_correction
  
  model
}

apply_veh_correction <- function(model, newdata, df_pred) {
  newdata %>% 
    select(unique_id, vh_make_model) %>% 
    left_join(model$veh_correction, by = "vh_make_model") %>% 
    # When a vehicule is unknown, surcharge by 50%, just in case.
    replace_na(list(correction = 1.5)) %>% 
    left_join(df_pred, by = "unique_id") %>% 
    mutate(pred = pred * correction) %>% 
    select(unique_id, pred)
}

# City correction ---------------------------------------------------------

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
    mutate(correction = pmax(1, lr)) %>%
    select(population, town_surface_area, correction)
  
  model$city_correction <- df_city_correction
  
  model
}

apply_city_correction <- function(model, newdata, df_pred) {
  
  newdata %>% 
    select(unique_id, population, town_surface_area) %>% 
    left_join(model$city_correction, by = c("population", "town_surface_area")) %>% 
    # When city is unknown, no surcharge
    replace_na(list(correction = 1)) %>% 
    left_join(df_pred, by = "unique_id") %>% 
    mutate(pred = pred * correction) %>% 
    select(unique_id, pred)
}

# Claims correction -------------------------------------------------------

train_claims_correction <- function(model, df, pred) {
  nb_year <- df %>% distinct(year) %>% nrow()
  
  gaa <- df %>%
    select(unique_id, id_policy, year, claim_amount) %>% 
    arrange(id_policy, year) %>% 
    group_by(id_policy) %>% 
    mutate(
      claim_count = cumsum(claim_amount > 0),
      claim_count = lag(claim_count, n = 1, default = 0)
    ) %>% 
    ungroup() %>% 
    left_join(pred, by = "unique_id") %>%
    mutate(hist_freq = claim_count / year) 
  
  df_claims_correction <- df %>%
    group_by(id_policy) %>%
    summarise(hist_freq = sum(claim_amount > 0) / nb_year) %>% 
    left_join(
      gaa %>% 
        group_by(hist_freq) %>% 
        summarise(lr = sum(claim_amount) / sum(pred)) %>% 
        add_row(hist_freq = 1, lr = 10),
      by = "hist_freq"
    ) %>% 
    mutate(correction = pmax(1, lr)) %>% 
    select(id_policy, correction)
  
  model$claims_correction <- df_claims_correction
  
  model
}

apply_claims_correction <- function(model, newdata, df_pred) {
  
  newdata %>% 
    select(unique_id, id_policy) %>% 
    left_join(model$claims_correction, by = "id_policy") %>% 
    replace_na(list(correction = 1)) %>% 
    left_join(df_pred, by = "unique_id") %>% 
    mutate(pred = pred * correction) %>% 
    select(unique_id, pred)
}