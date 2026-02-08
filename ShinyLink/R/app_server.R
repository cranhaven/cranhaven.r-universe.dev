#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  app_state <- reactiveValues(

    # Data frame during data cleaning
    dfA_uploaded = NULL,
    dfB_uploaded = NULL,
    # True/FALSE
    dfA_cleaned_duplicate = NULL,
    dfB_cleaned_duplicate = NULL,

    dfA_cleaned_assignment = NULL,
    dfB_cleaned_assignment = NULL,

    dfA_cleaned_gender = NULL,
    dfB_cleaned_gender = NULL,

    dfA_cleaned_date = NULL,
    dfB_cleaned_date = NULL,

    dfA_cleaned_imputation = NULL,
    dfB_cleaned_imputation = NULL,

    # Final cleaned data frame
    state_dfA = NULL,
    state_dfB = NULL,

    # Simple match settings
    matching_variables = c("firstname", "middlename", "lastname", "race", "sex"),
    string_matching = NULL,
    numeric_matching = NULL,
    partial_matching = c("firstname", "lastname"),

    # Simple match results
    matched_results = NULL,

    # Advanced parameters
    cut_a = NULL, # Lower bound for full string-distance match
    cut_p = NULL, # Lower bound for partial string-distance match
    # w_lambda = NULL, # Weighting of the MLE and prior estimate for the lambda parameter
    # w_pi = NULL, # Weighting of the MLE and prior estimate for the pi parameter

    # estimate_only = NULL,
    dedupe_matches = NULL,
    # linprog_dedupe = NULL,

    n_cores = NULL, # The number of cores to parallelize
    tol_em = NULL, # Convergence tolerance for the EM algorithm
    # threshold_match = NULL, # Lower bound for the posterior probability of a match that will be accepted

    # Advanced match results
    advanced_results = NULL

  )

  mod_uploading_server("uploading", app_state, session)
  mod_cleaning_duplicate_server("cleaning_duplicate", app_state, session)
  mod_cleaning_assignment_server("cleaning_assignment", app_state, session)
  mod_cleaning_gender_server("cleaning_gender", app_state, session)
  mod_cleaning_date_server("cleaning_date", app_state, session)
  mod_cleaning_imputation_server("cleaning_imputation", app_state, session)
  mod_simple_settings_server("simple_settings", app_state, session)
  mod_simple_results_server("simple_results", app_state, session)

  mod_manual_inspection_server("manual_inspection", app_state, session)

  mod_simple_details_server("simple_details", app_state, session)
  mod_advanced_parameters_server("advanced_parameters", app_state, session)
  # mod_advanced_results_server("advanced_results", app_state, session)
  mod_advanced_details_server("advanced_details", app_state, session)

}



