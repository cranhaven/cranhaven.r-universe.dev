#' Computes the Bayesian Information Criterion
#'
#' Computes the Bayesian Information Criterion of a model (Schwarz, 1978).
#'
#' @param mod A fitted model of class \code{lm} or \code{merMod}.
#'
#' @importFrom stats logLik nobs
#'
#' @examples
#' data(mtcars)
#' mod1 <- lm(mpg ~ cyl, mtcars)
#' bic(mod1)
#'
#' @author Ladislas Nalborczyk <\email{ladislas.nalborczyk@@gmail.com}>
#'
#' @seealso \code{\link{aic}}, \code{\link{ictab}}
#'
#' @references Schwarz, G. (1978). Estimating the dimension of a model.
#' Annals of Statistics, 6, 461-464.
#'
#' @export

bic <- function(mod) {

    n <- nobs(mod)
    ll <- logLik(mod)[1]
    k <- attr(logLik(mod), "df")

    bic <- -2 * ll + k * log(n)

    return(bic)

}
