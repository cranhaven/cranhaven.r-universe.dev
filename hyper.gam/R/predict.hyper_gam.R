

#' @title Prediction of [hyper_gam] Model
#' 
#' @description
#' Prediction of [hyper_gam] model.
#' 
#' @param object an [hyper_gam] model
#' 
#' @param newdata test \link[spatstat.geom]{hyperframe}, with at least 
#' the response \eqn{y^{\text{new}}} and
#' the \link[base]{double}-hypercolumn \eqn{X^{\text{new}}}
#' tabulated on the same grid as the training hypercolumn \eqn{X}.
#' If missing, the training data `object$data` will be used.
#' 
#' @param sign_adjusted \link[base]{logical} scalar, default `TRUE`
#' 
#' @param sgn (internal use) \link[base]{numeric} scalar, either `-1` or `1`, 
#' the \link[base]{sign} of [cor_xy()] return, to be used in the sign adjustment 
#' 
#' @param ... additional parameters, currently not in use.
#' 
#' @details 
#' 
#' The `S3` method [predict.hyper_gam()] computes 
#' the sign-adjusted \link[mgcv]{gam} model prediction.
#' The sign-adjustment ensures
#' that the return
#' is positively associated with the **training** hypercolumn \eqn{X}
#' at the selected tabulating grid.
#' 
#' @returns 
#' The `S3` method [predict.hyper_gam()] returns a 
#' \link[base]{double} \link[base]{vector}.
#' 
#' @keywords internal
#' @importFrom mgcv predict.gam
#' @export predict.hyper_gam
#' @export
predict.hyper_gam <- function(
    object, 
    newdata = object$data,
    sign_adjusted = TRUE,
    sgn = if (sign_adjusted) object |> cor_xy(probs = .5) |> sign() else 1,
    ...
) {
  
  xname <- attr(object, which = 'xname', exact = TRUE)
  newdata <- augdata(data = newdata, xname = xname)
  
  # do we really need to check the `$x` and `$L` of `newdata` and `olddata` being the same???
  # from tzh's previous code, we do need to check '$L' are the same
  newL <- newdata$L
  oldL <- object$data$L
  if (length(newl <- unique.default(newL)) != 1L) stop()
  oldl <- unique.default(oldL)
  if (!all.equal.numeric(newl, oldl)) stop()
  # what about `$x` ?

  fv <- predict.gam(object = object, newdata = newdata) |> # mgcv::predict.gam returns 'array'
    as.double() # ?base::as.double much faster than ?base::c
  
  return(fv * sgn)
    
}






