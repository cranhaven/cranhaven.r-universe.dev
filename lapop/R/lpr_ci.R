#' Compute Design-Based Proportion and Confidence Interval
#'
#' Computes a weighted proportion (mean of a binary outcome) and its
#' confidence interval using complex survey design features. When
#' stratification and PSU variables are supplied, the function uses
#' Taylor linearized variance estimation via the \pkg{survey} package.
#'
#' If both `strata` and `psu` are provided, a full complex survey design
#' is declared. If they are `NULL`, the function computes a weighted
#' estimate assuming simple random sampling (SRS) with weights.
#'
#' Lonely PSUs are handled using `survey.lonely.psu = "adjust"`.
#'
#' @param data A data frame containing the outcome and survey design variables.
#' @param outcome Character string. Name of a binary variable coded 0/1.
#' @param weight Character string. Name of the sampling weight variable.
#'   Default is `"weight1500"`.
#' @param strata Character string. Name of the stratification variable.
#'   Default is `NULL`. If provided together with `psu`, a complex survey
#'   design is used.
#' @param psu Character string. Name of the primary sampling unit (cluster)
#'   variable. Default is `NULL`.
#' @param conf.level Numeric. Confidence level for the interval.
#'   Default is `0.95`.
#' @param na.rm Logical. If `TRUE`, rows with missing values in the
#'   required variables are removed prior to estimation.
#'
#' @return A data frame with:
#' \describe{
#'   \item{prop}{Estimated proportion (mean of binary outcome).}
#'   \item{lb}{Lower bound of the confidence interval.}
#'   \item{ub}{Upper bound of the confidence interval.}
#'   \item{se}{Standard error of the estimate.}
#' }
#'
#' @details
#' Variance estimation is performed using Taylor linearization as implemented
#' in \code{\link[survey]{svymean}}. When both `strata` and `psu` are supplied,
#' clustering and stratification are incorporated in the variance estimator.
#'
#' If `strata` and `psu` are not provided, the function assumes a weighted
#' simple random sample and estimates variance accordingly.
#'
#' @examples
#' \donttest{
#' # Design-based estimate
#' data(cm23)
#' lpr_ci(data = cm23,
#'          outcome = "b13",
#'          weight = "weight1500",
#'          strata = "strata",
#'          psu = "upm")
#'
#' # Weighted SRS estimate
#' data(bra23)
#' lpr_ci(data = bra23,
#'          outcome = "b13",
#'          weight = "wt")
#' }
#'
#' @import survey
#' @importFrom stats confint
#' @export

lpr_ci <- function(data,
                     outcome,      # binary variable (0/1)
                     weight = "weight1500",         # weight variable (e.g., "weight1500")
                     strata = NULL,  # strata variable (optional)
                     psu = NULL,     # PSU/cluster variable (optional)
                     conf.level = 0.95,
                     na.rm = TRUE) {

  # Remove missing values if requested
  vars_needed <- c(outcome, weight, strata, psu)
  vars_needed <- vars_needed[!is.null(vars_needed)]

  if (na.rm) {
    data <- data[complete.cases(data[, vars_needed]), ]
  }

  # Define survey design
  if (!is.null(strata) && !is.null(psu)) {

    options(survey.lonely.psu = "adjust") # FOR SINGLE PSU COUNTRIES

    design <- survey::svydesign(
      ids     = as.formula(paste0("~", psu)),
      strata  = as.formula(paste0("~", strata)),
      weights = as.formula(paste0("~", weight)),
      data    = data,
      nest    = TRUE
    )

  } else {

    # Weighted but no complex design
    design <- survey::svydesign(
      ids     = ~1,
      weights = as.formula(paste0("~", weight)),
      data    = data
    )
  }

  # Estimate proportion
  formula_outcome <- as.formula(paste0("~", outcome))

  est <- survey::svymean(formula_outcome, design)

  ci  <- stats::confint(est, level = conf.level)

  result <- data.frame(
    prop = coef(est)[1],
    lb   = ci[1],
    ub   = ci[2],
    se   = SE(est)[1]
  )

  return(result)
}
