#' Standardize covariate vector for balance constraint
#'
#' This function is used by \code{\link{generate_constraints}()} to standardize
#' covariate vectors to become balance constraints.
#' The function divides the covariate values by the treated or average group
#' standard deviation (across strata).
#'
#' @param z a factor with the \code{i}th entry equal to the treatment of unit \code{i}.
#' @param x a covariate vector with \code{i}th entry equal to the
#'   covariate value of unit \code{i}. This should have the same order of units and
#'   length as \code{z}.
#' @param denom_variance character stating what variance to use in the standardization:
#'   either the default "treated", meaning the standardization will use the
#'   treated variance (across all strata), where the treated group is declared in
#'   the \code{treated} argument, or "pooled", meaning
#'   the standardization will use the average of the variances of each treatment group.
#' @param treated which treatment value should be considered the treated units. This
#' must be one of the values of \code{z}.
#' @param autogen_missing whether to automatically generate missingness constraint
#'   and how heavily to prioritize it. Should be a numeric
#'   or \code{NULL} value. \code{NULL} indicates that
#'   a constraint to balance the rate of missingness (denoted by \code{NA}
#'   in \code{x}) should not be automatically generated. Note that this is not
#'   recommended unless the user has already accounted for missing values.
#'   If not \code{NULL}, \code{autogen_missing} should be a numeric stating how heavily
#'   to prioritize generated missingness constraints over covariate constraint.
#'   The default is 50.
#' @return A list with two components:
#' \describe{
#' \item{covariate}{a balance constraint for the standardized covariate values
#'   of all unites.}
#' \item{missingness}{a corresponding balance constraint for the rate of missingness if
#' \code{autogen_missing} not \code{NULL}, otherwise \code{NULL}.}
#' }
#' @importFrom stats sd
#' @export

stand <- function(z, x, denom_variance = "treated", treated = 1, autogen_missing = 50) {

  # Verify inputs ----
  if (!denom_variance %in% c("treated", "pooled")) {
    stop("* `denom_variance` must be one of `treated` or `pooled`.",
         call. = FALSE)
  }
  if (!is.vector(x)) {
    stop("`x` must be a vector.",
         call. = FALSE)
  }
  if (!is.vector(z) & !is.factor(z)) {
    stop("`z` must be a factor",
         call. = FALSE)
  }
  if (length(z) != length(x)) {
    stop("`z` and `x` must have the same length.",
         call. = FALSE)
  }
  z <- factor(z)
  if (!treated %in% levels(z)) {
    stop("`treated` must be one of the levels of `z`.",
         call. = FALSE)
  }

  # Define the value by which to scale ----
  variances <- sapply(levels(z), function(group) var(x[z == group], na.rm = TRUE))
  variances[is.na(variances)] <- 0
  if (denom_variance == "treated") {
    scl <- sqrt(variances[levels(z) == treated])
    # If there is no variance in treated group, use pooled value
    # (which is half of the control variance since treated variance = 0)
    if (is.na(scl) || scl == 0) {
      warning("There is a covariate with no variance in the treated group. Standardization will thus use the average of the group variances for this covariate.")
      denom_variance  <- "pooled"
    }
  }
  if (denom_variance == "pooled") {
    scl <- sqrt(mean(variances))
  }

  # Perform standardization ----
  x_stand <- x / scl

  # Deal with missing data (coded as NA) ----
  # Add missingness constraint
  miss_stand <- NULL
  if (!is.null(autogen_missing) && sum(is.na(x)) > 0) {
    miss <- is.na(x)
    if (denom_variance == "treated") {
      scl_miss <- sd(miss[z == treated])
      if (is.na(scl_miss) || scl_miss == 0) {
        scl_miss <- sqrt(mean(sapply(levels(z), function(group) (sd(miss[z == group], na.rm = TRUE))^2)))
      }
    } else if (denom_variance == "pooled") {
      scl_miss <- sqrt(mean(sapply(levels(z), function(group) (sd(miss[z == group], na.rm = TRUE))^2)))
    }
    miss_stand <- miss / scl_miss
  }

  return(list("covariate" = x_stand, "missingness" = miss_stand))
}
