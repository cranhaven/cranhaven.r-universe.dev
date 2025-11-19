#' Apply Analysis Function to Multiple Imputed Datasets
#'
#' This function applies an analysis function (e.g., ANCOVA) to imputed datasets and stores the results for later pooling. It is designed to work with multiple imputed datasets and apply a given analysis function to each imputation iteration.
#'
#' @param data A data frame containing the imputed datasets. The data frame should include a variable (e.g., `IMPID`) that identifies distinct imputation iterations.
#' @param vars A list specifying key variables used in the analysis (e.g., `subjid`, `visit`, `group`, `outcome`). Required.
#' @param method A character string or object specifying the method used for analysis (e.g., Bayesian imputation). Defaults to `NULL`.
#' @param fun A function that will be applied to each imputed dataset. Defaults to `rbmi::ancova`. Must be a valid analysis function.
#' @param delta A `data.frame` used for delta adjustments, or `NULL` if no delta adjustments are needed. Defaults to `NULL`.
#' @param ... Additional arguments passed to the analysis function `fun`.
#'
#' @details
#' The function loops through distinct imputation datasets (identified by `IMPID`), applies the provided analysis function `fun`, and stores the results for later pooling. If a `delta` dataset is provided, it will be merged with the imputed data to apply the specified delta adjustment before analysis.
#'
#' @return An object of class `analysis` containing the results from applying the analysis function to each imputed dataset.
#'
#' @examples
#' # Example usage with an ANCOVA function
#' library(dplyr)
#' library(rbmi)
#' library(rbmiUtils)
#' set.seed(123)
#' data("ADMI")
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
#'  method <- rbmi::method_bayes(
#'  n_samples = 20,
#'  control = rbmi::control_bayes(
#'    warmup = 20,
#'    thin = 1
#'    )
#'  )
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
#' @export
analyse_mi_data <- function(
  data = NULL,
  vars = NULL,
  method = NULL,
  fun = rbmi::ancova,
  delta = NULL,
  ...
) {
  # Check for missing inputs
  if (is.null(data)) stop("`data` cannot be NULL.")

  # check IMPID is in data
  if (!"IMPID" %in% names(data))
    stop(
      "`data` must contain a variable `IMPID` to identify distinct imputation iterations."
    )

  if (is.null(vars)) stop("`vars` cannot be NULL. Specify key variables.")

  ## asset function
  assertthat::assert_that(
    is.function(fun),
    msg = "`fun` must be a function"
  )

  ## check on delta
  assertthat::assert_that(
    is.null(delta) | is.data.frame(delta),
    msg = "`delta` must be NULL or a data.frame"
  )

  ## check delta has correct variables and then apply
  if (!is.null(delta)) {
    expected_vars <- c(
      vars$subjid,
      vars$visit,
      "delta"
    )
    assertthat::assert_that(
      all(expected_vars %in% names(delta)),
      msg = sprintf(
        "The following variables must exist within `delta`: `%s`",
        paste0(expected_vars, collapse = "`, `")
      )
    )

    ## apply delta to data set adding to outcome
    data <- data |>
      dplyr::left_join(delta, by = c(vars$subjid, vars$visit, vars$group)) |>
      dplyr::mutate(
        !!rlang::sym(vars$outcome) := .data[[vars$outcome]] + .data$delta
      )
  }

  # Loop through distinct imputation data sets based on IMPID in a single data frame
  results <- data |>
    dplyr::group_split(IMPID) |>
    lapply(
      function(dat_subset, ...) {
        # Perform analysis on the subset of data corresponding to each imputation
        fun(dat_subset, vars, ...)
      },
      ...
    )

  fun_name <- deparse(substitute(fun))

  if (length(fun_name) > 1) {
    fun_name <- "<Anonymous Function>"
  } else if (is.null(fun_name)) {
    fun_name <- "<NULL>"
  }

  ret <- as_analysis2(
    results = results,
    fun_name = fun_name,
    delta = delta,
    fun = fun,
    method = method
  )

  return(ret)
}


#' Construct an rbmi `analysis` object
#'
#' @description
#' This is a helper function to create an analysis object that stores the
#' results from multiple imputation analyses. It validates the results and
#' ensures proper class assignment.
#'
#' This is a modification of the rbmi::as_analysis function.
#'
#' @param results A list containing the analysis results for each imputation.
#' @param method The method object used for the imputation.
#' @param delta Optional. A delta dataset used for adjustment.
#' @param fun The analysis function that was used.
#' @param fun_name The name of the analysis function (used for printing).
#'
#' @return An object of class `analysis` with the results and associated metadata.
as_analysis2 <- function(
  results,
  method,
  delta = NULL,
  fun = NULL,
  fun_name = NULL
) {
  next_class <- switch(
    class(method)[[2]],
    bayes = "rubin",
    approxbayes = "rubin",
    condmean = ifelse(
      method$type == "jackknife",
      "jackknife",
      "bootstrap"
    ),
    bmlmi = "bmlmi"
  )

  assertthat::assert_that(
    is.list(results),
    length(next_class) == 1,
    is.character(next_class),
    next_class %in% c("jackknife", "bootstrap", "rubin", "bmlmi")
  )

  x <- list(
    results = rbmi::as_class(results, c(next_class, "list")),
    delta = delta,
    fun = fun,
    fun_name = fun_name,
    method = method
  )
  class(x) <- c("analysis", "list")

  return(x)
}
