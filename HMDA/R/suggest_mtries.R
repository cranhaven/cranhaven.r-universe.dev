#' @title Suggest Alternative mtries Values
#' @description Provides a set of candidate values for the
#'   \code{mtries} parameter used in Random Forest models.
#'   The suggestions are computed based on the number of
#'   predictors (\code{p}) and the modeling family. For
#'   classification, the common default is \code{sqrt(p)},
#'   while for regression it is typically \code{p/3}. For
#'   family, alternative candidates are offered to aid model
#'   tuning.
#'
#' @param p      Integer. The number of features (predictors)
#'               in the dataset. This value is used to compute
#'               candidate mtries.
#' @param family Character. Must be either
#'               "classification" or "regression". This
#'               determines the set of candidate values.
#'
#' @details
#'   For classification, the default is often
#'   \code{sqrt(p)}; alternative suggestions include
#'   \code{log2(p)} and \code{p^(1/3)}. For regression,
#'   the typical default is \code{p/3}, but candidates such as
#'   \code{p/2} or \code{p/5} may also be useful. The best
#'   choice depends on the data structure and predictor
#'   correlations.
#'
#' @return An integer vector of candidate values for
#'         \code{mtries}.
#'
#' @examples
#' \dontrun{
#'   # For a classification task with 100 predictors:
#'   suggest_mtries(p = 100, family = "classification")
#'
#'   # For a regression task with 100 predictors:
#'   suggest_mtries(p = 100, family = "regression")
#' }
#'
#' @export
#' @author E. F. Haghish
suggest_mtries <- function(p, family = c("classification", "regression")) {

  # Match argument for safety
  family <- match.arg(family)

  # Some “safe” minimum to avoid 0 or negative
  # In case p is very small
  safe_floor <- function(x) {
    v <- floor(x)
    #max(v, 1)
  }

  # Prepare suggestion sets
  if (family == "classification") {
    # Common default: sqrt(p)
    # Additional suggestions: log2(p), p^(1/3), p/2, etc.
    candidates <- c(
      sqrt(p),
      log2(p),
      p^(1/3),
      (sqrt(p) + log2(p) + p^(1/3))/2,
      (sqrt(p) + log2(p))/2,
      sqrt(p) + (p^(1/3))/2
      #p/2  # Might be large, but sometimes used
    )

  } else {
    # family == "regression"
    # Common default: p/3
    # Additional suggestions: p/2, p/5, sqrt(p), etc.
    candidates <- c(
      p/3,
      p/4,
      p/2,
      p/5,
      (p/3 + p/2)/2 #conservative list
    )
  }

  # Convert to integer and ensure non-zero
  candidates_int <- unique(safe_floor(candidates))

  # Return
  return(candidates_int)
}

