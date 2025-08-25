#' Posterior predictive check
#'
#' @param stanTKdata List of Data require for computing
#' @param \dots Arguments passed to `rstan::sampling` (e.g. iter, chains).
#' 
#' @return An object of class `fitTK` containing two object: \code{stanTKdata}
#' the data set used for inference and \code{stanfit}  returned by `rstan::sampling`
#' 
#' @rdname fitTK
#' 
#' @export
#' 
fitTK <- function(stanTKdata, ...){
   UseMethod("fitTK")
}


#' Bayesian inference of TK model with Stan
#' 
#' @rdname fitTK
#'
#' @export
#' 
fitTK.stanTKdataCST <- function(stanTKdata, ...) {
   # remove additional variables
   dataFit <- stanTKdata
   dataFit$origin_data <- NULL
   stanfit <- rstan::sampling(stanmodels$TK, data = dataFit, ...)
   out <- list(stanTKdata = stanTKdata, stanfit = stanfit)
   class(out) <- append("fitTK", class(out))
   return(out)
}


#' Bayesian inference of TK model with variable exposure profile (BETA version)
#'
#' @rdname fitTK
#' 
#' @export
#'
 fitTK.stanTKdataVAR <- function(stanTKdata, ...) {
   stanfit <- rstan::sampling(stanmodels$odeTK, data = stanTKdata, ...)
   out <- list(stanTKdata = stanTKdata, stanfit = stanfit)
   class(out) <- append("fitTK", class(out))
   return(out)
 }