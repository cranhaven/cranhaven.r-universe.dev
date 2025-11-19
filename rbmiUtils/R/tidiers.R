#' Tidy and Annotate a Pooled Object for Publication
#'
#' This function processes a pooled analysis object of class `pool` into a tidy tibble format.
#' It adds contextual information, such as whether a parameter is a treatment comparison or a least squares mean,
#' dynamically identifies visit names from the `parameter` column, and provides additional columns for parameter type,
#' least squares mean type, and visit.
#'
#' @param pool_obj A pooled analysis object of class `pool`.
#'
#' @return A tibble containing the processed pooled analysis results. The tibble includes columns for the parameter,
#' description, estimates, standard errors, confidence intervals, p-values, visit, parameter type, and least squares mean type.
#'
#' @details The function rounds numeric columns to three decimal places for presentation. It dynamically processes
#' the `parameter` column by separating it into components (e.g., type of estimate, reference vs. alternative arm, and visit),
#' and provides informative descriptions in the output.
#'
#' @examples
#' # Example usage:
#' library(dplyr)
#' library(rbmi)
#'
#' data("ADMI")
#' N_IMPUTATIONS <- 100
#' BURN_IN <- 200
#' BURN_BETWEEN <- 5
#'
#' # Convert key columns to factors
#' ADMI$TRT <- factor(ADMI$TRT, levels = c("Placebo", "Drug A"))
#' ADMI$USUBJID <- factor(ADMI$USUBJID)
#' ADMI$AVISIT <- factor(ADMI$AVISIT)
#'
#' # Define key variables for ANCOVA analysis
#'  vars <- set_vars(
#'   subjid = "USUBJID",
#'   visit = "AVISIT",
#'   group = "TRT",
#'   outcome = "CHG",
#'   covariates = c("BASE", "STRATA", "REGION")  # Covariates for adjustment
#'  )
#'
#' # Specify the imputation method (Bayesian) - need for pool step
#' method <- rbmi::method_bayes(
#'   n_samples = N_IMPUTATIONS,
#'   control = rbmi::control_bayes(
#'     warmup = BURN_IN,
#'     thin = BURN_BETWEEN
#'     )
#'   )
#'
#' # Perform ANCOVA Analysis on Each Imputed Dataset
#' ana_obj_ancova <- analyse_mi_data(
#'   data = ADMI,
#'   vars = vars,
#'   method = method,
#'   fun = ancova,  # Apply ANCOVA
#'   delta = NULL   # No sensitivity analysis adjustment
#' )
#'
#' pool_obj_ancova <- pool(ana_obj_ancova)
#' tidy_df <- tidy_pool_obj(pool_obj_ancova)
#'
#' # Print tidy data frames
#' print(tidy_df)
#'
#' @export
tidy_pool_obj <- function(pool_obj) {
  # Check object is of class 'pool'
  if (!inherits(pool_obj, "pool")) {
    stop("Input object must be of class 'pool'")
  }

  # Convert pool_obj to tibble
  df <- dplyr::as_tibble(pool_obj)

  # Process the 'parameter' column by separating it
  df <- df |>
    tidyr::separate(
      parameter,
      into = c("parameter_type", "lsm_type", "visit"),
      sep = "_",
      fill = "right",
      extra = "merge",
      remove = FALSE
    )

  # Adjust 'lsm_type' and 'visit' based on 'parameter_type'
  df <- df |>
    dplyr::mutate(
      visit = dplyr::case_when(
        parameter_type == 'lsm' ~ visit,
        parameter_type == 'trt' & !is.na(lsm_type) ~ lsm_type, # Include visit, which will be in lsm_type for trt comparisons
        TRUE ~ NA_character_
      ),
      lsm_type = dplyr::if_else(
        parameter_type == 'lsm',
        lsm_type,
        NA_character_
      ),
      lsm_type = dplyr::if_else(
        parameter_type == 'trt',
        NA_character_,
        lsm_type
      )
    )

  # Create informative descriptions for parameters
  df <- df |>
    dplyr::mutate(
      description = dplyr::case_when(
        parameter_type == 'trt' & !is.na(lsm_type) ~
          paste('Treatment Comparison at', visit),
        parameter_type == 'trt' ~ 'Treatment Comparison',
        parameter_type == 'lsm' & !is.na(visit) ~
          paste(
            'Least Squares Mean for',
            ifelse(lsm_type == 'ref', 'Reference', 'Alternative'),
            'at',
            visit
          ),
        parameter_type == 'lsm' ~
          paste(
            'Least Squares Mean for',
            ifelse(lsm_type == 'ref', 'Reference', 'Alternative')
          ),
        TRUE ~ parameter
      )
    )

  # Select and arrange the columns for the publication-ready table
  df <- df |>
    dplyr::select(
      parameter,
      description,
      visit,
      parameter_type,
      lsm_type,
      est,
      se,
      lci,
      uci,
      pval
    )

  return(df)
}
