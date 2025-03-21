#' @title Combining AICs
#'
#' @family wqs pool
#' @keywords pool
#' @seealso pool.mi
#'
#' @description
#' Combines individual AIC estimates of separate  models to get a sense of overall model fit.
#'
#' @details
#' The WQS model fits using different completely observed datasets are combined in Stage 3 of multiple imputation. Similar to combining WQS parameter estimates, the mean of individual AIC estimates is taken as the central tendency estimate of WQS model fit. The standard deviation between individual AIC estimates indicates the difference in WQS model fits due to below the detection limit values.
#'
#' A vector of AICs may be generated from \code{\link[miWQS]{do.many.wqs}}().
#'
#' @section Warning:
#' If AIC is a vector with one element, the AIC is returned as a character rounded to the nearest whole number with a warning printed that AIC cannot be combined.

#' @param AIC  A vector of AICs to combine with length equal to the number of models completed (i.e. K).
#'
#' @return The overall fit of a model across all imputation models: the mean AIC +/- the standard error. Saved as a 1x1 character vector.
#'
#' @examples
#' # AICs from do.many.wqs() example are as follows.
#' bayes.AIC <- c(1295.380, 1295.669)
#' combine.AIC(bayes.AIC)
#'
#' # One AIC
#' combine.AIC(1295.380)
#' @export combine.AIC

combine.AIC <- function(AIC) {
  stopifnot(is.numeric(AIC))

  if (length(AIC) == 1) {
    warning("There is only one model fit, and nothing to combine.", call. = TRUE)
    AIC.pool <- as.character(format(AIC, nsmall = 1))
  } else {
    # Calculate mean and standard deviation as a vector.
    stat <- cbind(mean = mean(AIC), sd = sd(AIC))
    # Return average model fit
    AIC.pool <- suppressWarnings(as.character(formatMeanSd(stat, nsmall = 1)))
    AIC.pool <- strsplit(AIC.pool, ",")[[1]] # Remove the comma: Add Jul 22, 2019
  }

  return(AIC.pool)
}
