#' Compute Confidence Interval using Delta Method
#'
#' Internal method to compute the probability using delta method which approximates
#' the variance of a function of random variables (in this case, the ratio) based on the variance of the original estimates.
#'
#' @name deltaCI
#' @export
#' @param cond list with the result of the perturbed probability for `mj` and `co` and correspondent CI.
#' @return The Confidence Interval for the conditional probability.
#' @examples
#'   cond <- list(
#'   c(P = 0.3597117, `2.5%` = 0.3074215, `97.5%` = 0.4075315),
#'   c(P = 0.5682882, `2.5%` = 0.4560553, `97.5%` = 0.6823438))
#'   deltaCI(cond)
#' @importFrom data.table copy
#' @importFrom copula pMvdc
setGeneric("deltaCI",
           def = function(cond) {
             standardGeneric("deltaCI")
           })

#' @rdname deltaCI
#' @description
#'
#' `deltaCI` general method.
#' Internal method to compute the probability using delta method which approximates
#' the variance of a function of random variables (in this case, the ratio) based on the variance of the original estimates.
#' @param cond list with the result of the perturbed probability for `mj` and `co` and correspondent CI.
#' @return Numeric vector representing the computed probability and confidence intervals using the perturbed copula and delta method.
#' @importFrom stats setNames

setMethod("deltaCI",
          definition=function(cond) {
            p <- sapply(cond, `[[`, 'P')
            lower <- sapply(cond, `[[`, '2.5%')
            upper <- sapply(cond, `[[`, '97.5%')
            # Compute SE
            se <- (upper - lower) / (2 * 1.96)
            r_hat <- p[1] / p[2]
            #  Delta Method
            var_r <- (1/p[2])^2 * se[1]^2 + (-p[1]/p[2]^2)^2 * se[2]^2
            #get CI
            z <- 1.96
            ci <- c(r_hat - z * sqrt(var_r), r_hat + z * sqrt(var_r))
            setNames(c(r_hat, ci), c("P", "2.5%", "97.5%"))
          })



