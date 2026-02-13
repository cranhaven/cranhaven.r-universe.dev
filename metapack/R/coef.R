#' get the posterior mean of fixed-effect coefficients
#' @param object a class of `bsynthesize`
#' @param ... other arguments
#' @return Coefficients extracted from the model object `object`
#' @md
#' @export
#' @method coef bsynthesis
coef.bsynthesis <- function(object, ...) {
    stopifnot(inherits(object, "bsynthesis"))

    # undo scaling
    if (object$scale_x) {
        if (object$multivariate) {
            J <- ncol(object$Outcome)
            xcols <- ncol(object$XCovariate)
            tlength <- nrow(object$mcmc.draws$theta)
            trlength <- tlength - xcols * J
            tscale <- c(rep(unname(attr(object$XCovariate, "scaled:scale")), J), rep(1, trlength))
        } else {
            xcols <- ncol(object$XCovariate)
            tlength <- nrow(object$mcmc.draws$theta)
            trlength <- tlength - xcols
            tscale <- c(unname(attr(object$XCovariate, "scaled:scale")), rep(1, trlength))    
        }
    } else {
        tlength <- nrow(object$mcmc.draws$theta)
        tscale <- rep(1, tlength)
    }

    theta.post <- vapply(1:object$mcmc$nkeep, function(ikeep) {
            object$mcmc.draws$theta[,ikeep] / tscale
        }, FUN.VALUE = numeric(tlength))
    rowMeans(theta.post)
}