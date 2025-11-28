#' Extract Coefficients
#'
#' This is an internal method to extract coefficients from the list of the fitted distributions
#' for each variable resulting from \code{\link{fit_dist_pbox}}. This method handles potential issues
#' with parameter extraction from the complex objects created by GAM-like models.
#'
#' @name coefAll2
#' @docType methods
#' @rdname coefAll2-methods
#' @export
#' @include pbox.R
#' @aliases coefAll2-method
#' @usage coefAll2(obj, deviance = FALSE)
#' @param obj An object typically resulting from \code{fit_dist_pbox}.
#' @param deviance Logical value indicating whether to compute deviance for the fitted model.
#' @return A list of coefficients, possibly including 'mu', 'sigma', 'nu', and 'tau', depending on
#' the model specification in \code{obj}. If \code{deviance} is TRUE, it also includes the deviance of the model.
#' @examples
#' data(SEAex)
#' pbx <- set_pbox(SEAex)
#' coefAll2(pbx@fit[[1]]$allDistrs$Thailand)
setGeneric("coefAll2", function(obj, deviance = FALSE) {
  standardGeneric("coefAll2")
})

#' Method for extracting coefficients from GAM-like models
#'
#' @param obj A model object, typically from a GAM-like fitting procedure.
#' @param deviance A Boolean flag that when TRUE calculates the deviance of the model.
#' @return A list containing model coefficients and optionally deviance.
#' @export

setMethod("coefAll2",
          definition=function (obj, deviance = FALSE){
            #fix to issue with sigma in function coefAll of gamlss
            out <- list()
            if ("mu" %in% obj$parameters)
              out$mu <- obj$mu
            if ("sigma" %in% obj$parameters)
              out$sigma <- obj$sigma
            if ("nu" %in% obj$parameters)
              out$nu <- obj$nu
            if ("tau" %in% obj$parameters)
              out$tau <- obj$tau
            if (deviance)
              out$deviance <- deviance(obj)
            return(out)
          })

