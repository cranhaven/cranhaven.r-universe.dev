#' Probability Confidence Interval
#'
#' Calculates the confidence interval around a vector of probabilities
#' using the quantiles based on the specified significance level.
#'
#' @name probCI
#' @docType methods
#' @rdname probCI-methods
#' @export
#' @aliases probCI-method
#' @usage probCI(probabilities, alpha=0.05)
#' @param probabilities A numeric vector of probabilities for which the confidence interval is desired.
#' @param alpha The significance level used for constructing the confidence interval; default is 0.05.
#' @return A list containing the lower and upper bounds of the confidence intervals for each probability.
#' @examples
#' probabilities <- c(0.1, 0.2, 0.3, 0.4, 0.5)
#' probCI(probabilities)
#' probCI(probabilities, alpha = 0.1)
#' @importFrom stats quantile
setGeneric("probCI", function(probabilities, alpha = 0.05) {
  standardGeneric("probCI")
})

#' Method to calculate confidence intervals for a vector of probabilities
#'
#' This method calculates the lower and upper bounds of the confidence interval for each element in the
#' input vector of probabilities using the given alpha level.
#'
#' @param probabilities A numeric vector of probabilities.
#' @param alpha A numeric value specifying the significance level for the confidence intervals; defaults to 0.05.
#' @return A numeric vector containing the lower and upper quantile bounds for each probability in the input vector.
#' @importFrom stats quantile
#' @export
setMethod("probCI", "numeric",
          function(probabilities, alpha = 0.05) {
            if(alpha>=1 | alpha<=0){
              stop("alpha should be in the range [0-1]!")
            }
            # Calculate the confidence interval around the probabilities
            lower <- stats::quantile(probabilities, alpha / 2)
            upper <- stats::quantile(probabilities, 1 - alpha / 2)
            # Return a matrix with both lower and upper bounds
            return(cbind(lower, upper))
          })
