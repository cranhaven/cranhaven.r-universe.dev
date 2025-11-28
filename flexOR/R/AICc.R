#' @title AICc: Calculation for GAM Models
#'
#' @description
#' Calculates AICc (Akaike Information Criterion corrected for small sample sizes)
#' for Generalized Additive Models (GAM).
#'
#' @param object An object of class "Gam" or "gam" representing a fitted GAM model.
#'
#' @details
#' This function calculates the AICc value (Akaike Information Criterion corrected
#' for small sample sizes) for a given GAM model. AICc is a measure of model fit
#' that penalizes the number of parameters in the model to avoid overfitting.
#'
#' @return
#' A numeric value representing the AICc for the GAM model.
#'
#' @examples
#' # Load dataset
#' data(PimaIndiansDiabetes2, package="mlbench");
#' 
#' # Fit GAM model
#' fit <- mgcv::gam(
#'   diabetes ~ s(age) + s(mass) + s(pedigree) + pressure + glucose,
#'   data=PimaIndiansDiabetes2,
#'   family=binomial
#' );
#'
#' # Calculate AICc
#' AICc(fit);
#' 
#' @seealso
#' \code{\link{gam}}, \code{\link{logLik}}, \code{\link{AIC}}
#' 
#' @keywords models nonlinear regression smooth
#' @importFrom stats logLik
#' @export
AICc <- function(object) {
  if ( !( inherits(x=object, what="Gam") | inherits(x=object, what="gam") ) )
    {stop("'object' must inherit from either class 'Gam' or 'gam'");}
  ll <- logLik(object);
  d <- attributes(ll)$df;
  if ( inherits(x=object, what="Gam") ) {n <- attributes(ll)$nobs;}
  if ( inherits(x=object, what="gam") ) {n <- object$df.null + 1;}
  aic <- -2*logLik(ll)[1]+2*attributes(ll)$df;
  caic <- aic + 2*d*(d+1)/(n-d-1);
  caic[d+1 >= n] <- Inf;
  attributes(caic)[c('df','nobs','class')] <- NULL;
  return(caic);
} # AICc
