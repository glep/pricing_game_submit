#' Generates dummies for nominal variables
#'
#' @details
#'    With tree-based models, one must be careful with one-hot encoding.
#'    The features with more than two levels must be one-hot encoded (one dummy by level)
#'    but for the features with only two levels, it is not necessary to build two dummies.
#'    This function automatizes the steps
#'
#' @param rec recipe to be modified
#' @param df data to train the recipe (needs the data to compute the number of levels by feature)
#' @param nominal_vars Nominal variables for which dummies need to be created
#'    (by default, all nominal predictors)
#'
#' @return The modified recipe
#' @export
step_dummy_xgb <-
  function(
    rec,
    df,
    nominal_vars
  ) {
    
    prep_rec <- prep(rec, training = df, retain = TRUE)
    nominal_vars <- intersect(rec_has_type(prep_rec, "nominal"), rec_has_role(prep_rec, "predictor"))
    
    df_nb_mod <- juice(prep_rec) %>%
      summarise(across(all_of(nominal_vars), n_distinct, na.rm = TRUE)) %>%
      pivot_longer(cols = everything())
    
    feature_onehot <- df_nb_mod %>% filter(value > 2) %>% pull(name)
    feature_n_onehot <- df_nb_mod %>% filter(value == 2) %>% pull(name)
    
    rm(df, prep_rec)
    
    rec %>%
      step_dummy(all_of(feature_onehot), one_hot = TRUE, naming = customized_recipe_dummy_names) %>%
      step_dummy(all_of(feature_n_onehot), one_hot = FALSE, naming = customized_recipe_dummy_names)
  }

rec_has_type <- function(rec, .type) {
  rec$term_info %>% filter(type == .type) %>% pull(variable)
} 

rec_has_role <- function(rec, .role) {
  rec$term_info %>% filter(role == .role) %>% pull(variable)
}

customized_recipe_dummy_names <- function (var, lvl, ordinal = FALSE, sep = "__") {
  paste(var, janitor::make_clean_names(lvl, use_make_names = TRUE), sep = sep)
}

#' @description Add average by control features.
#'
#' @param df data.frame
#' @param control_vars character vector, control variables
#' @param target_var character, target variable, loss_cost of claims
#' @param expo_var character, name of exposure variable
#' @param name_offset_var character, name of variable to create
#' @param model_type One of c("tweedie", "freq", "sev"), type of model
#'
#' @return data.frame with additional column name_offset_var
#' @export
add_control_average <- 
  function(df, model_type, control_vars, target_var, expo_var, name_offset_var = "offset") {
    if (model_type == "tweedie") {
      df %>%
        group_by(across(all_of(control_vars))) %>%
        mutate(
          {{ name_offset_var }} :=
            sum(.data[[target_var]] * .data[[expo_var]]) /
            sum(.data[[expo_var]])
        ) %>%
        ungroup()
    } else if (model_type == "freq") {
      df %>%
        group_by(across(all_of(control_vars))) %>%
        mutate(
          {{ name_offset_var }} := .data[[expo_var]] *
            sum(.data[[target_var]]) / sum(.data[[expo_var]])
        ) %>%
        ungroup()
    } else if (model_type == "sev") {
      df %>%
        group_by(across(all_of(control_vars))) %>%
        mutate(
          {{ name_offset_var }} := mean(.data[[target_var]])
        ) %>%
        ungroup()
    }
  }

