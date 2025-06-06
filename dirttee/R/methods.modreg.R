#' @title
#' Methods for fitted modreg model
#'
#' @description 
#' Methods for \code{modreg} objects returned by the mode regression function.
#'
#' @aliases coef.modreg coefficients.modreg print.modreg summary.modreg print.summary.modreg
#' @name methods
#' @rdname methods
#'
#' @importFrom stats coefficients
#' 
#' @param x,object A modreg object
#' @param ... further arguments passed to or from other methods
#' 
#' @returns \code{coef} returns a named numerical vector with coefficients
#' 
#' @export


coefficients.modreg <-
  function(object,...){
    object$reg$coefficients
  }

#' @rdname methods  
#' @export
  
coef.modreg <-
  function(object,...){
    object$reg$coefficients
  }

#' @rdname methods  
#' @export  
  
print.modreg <-
function(x, ...){
  cat("\nFormula:\n", deparse(x$called$formula), "\n", sep = "")
  cat("\nCoefficients:\n")
  print(summary(x$reg)$p.coeff, ...)
  
  cat("\nBandwidth type:", x$called$bw)
  if(!is.null(x$lambda))cat('\nLambda: ', x$lambda)
}

#' @rdname methods
#' @export

summary.modreg <-
  function(object, ...){
    
    out <- list(
      formula = deparse(object$called$formula),
      coefficients = summary(object$reg)$p.coeff,
      bw = object$bw,
      aic = object$aic,
      edf = object$edf,
      pseudologlik = object$pseudologlik,
      converged = object$converged,
      bw_method = object$called$bw,
      hp_status = object$hp_opt$status,
      hp_iterations = object$hp_opt$iterations
    )
  
    out[["iterations"]] <- object$iterations
    
    if(object$called$bw == "Plugin"){
      out$iterations_plugin <- object$iterations_plugin
      if(!is.null(object$cova)) out$SE <- sqrt(diag(object$cova))
    }
   
    class(out) <- "summary.modreg"
    return(out)

  }