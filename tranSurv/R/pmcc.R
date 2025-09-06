#' Product-Moment Correlation Coefficient
#'
#' \code{pmcc} computes the conditional product-moment correlation coefficient proposed by Chen et al. (1996).
#' The conditional product-moment correlation coefficient uses only the uncensored events.
#'
#' @param trun left truncation time satisfying \code{trun} <= \code{obs}.
#' @param obs observed failure time, must be the same length as \code{trun}, might be right-censored.
#' @param a a numeric transformation parameter. The default value is 0, which applies no transformation.
#' This parameter must be greater than \code{-1}.
#' See \code{?tranSurvfit} for the transformation model structure.
#' @param trans a character string specifying the transformation structure. The following are permitted:
#' \describe{
#'   \item{linear}{linear transformation structure,}
#'   \item{log}{log-linear transformation structure,}
#'   \item{exp}{exponential transformation structure.}
#' }
#'
#' @return A numeric value representing the product-moment correlation coefficient.
#'
#' #' @seealso \code{\link{trSurvfit}}
#' @export
#' @example inst/examples/ex_pmcc.R
pmcc <- function(trun, obs, a = 0, trans = "linear") {
    out <- NULL
    out$Call <- match.call()
    n <- length(trun)
    if (inherits(trans, "character")) {
        if (trans == "linear") FUN <- function(X, T, a) (T + a * X) / (1 + a)
        if (trans == "log") FUN <- function(X, T, a) exp((log(replace(T, 0, 1)) + a * log(X))/(1 + a))
        if (trans == "log2") FUN <- function(X, T, a) exp((1 + a) * log(replace(T, 0, 1)) - a * log(X))
        if (trans == "exp") FUN <- function(X, T, a) log((exp(T) + a * exp(X)) / (1 + a))
    } else {
        FUN <- match.fun(trans)
    }
    trun <- mapply(FUN, X = obs, T = trun, a = a)
    res <- vector("double", 2)
    pmc <- .C("pmccC", as.double(trun), as.double(obs), as.integer(n),
              tmp = as.double(res), PACKAGE = "tranSurv")$tmp
    out$PE <- pmc[1]
    out$SE <- pmc[2]
    out$STAT <- pmc[1] / sqrt(pmc[2])
    out$p.value <- 2 - 2 * pnorm(abs(pmc[1]) / sqrt(pmc[2]))
    out$trans <- trans
    out$a <- a
    class(out) <- "pmcc"
    return(out)
}
