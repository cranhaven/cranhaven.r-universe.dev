#' Predict from a cosinor model
#'
#' Given a time variable and optional covariates, generate predicted values from
#' a cosinor fit. Default prediction is the mean value, optionally can predict
#' at a given month
#'
#' @param object An object of class \code{cglmm}.
#' @param newdata Optional new data.
#' @param ... other arguments passed to \code{glmmTMB:::predict.glmmTMB}.
#'
#' @return Returns predicted values from the cosinor model.
#'
#' @srrstats {RE4.9}
#' @srrstats {G1.4}
#'
#' @examples
#'
#' fit <- cglmm(vit_d ~ X + amp_acro(time,
#'   group = "X",
#'   n_components = 1,
#'   period = 12
#' ), data = vitamind)
#' predict(fit)
#'
#' @export
#'


predict.cglmm <- function(object, newdata, ...) {
  if (missing(newdata)) {
    return(stats::predict(object$fit, ...))
  }

  # pass new dataset that's being used for prediction in this function
  nd <- update_formula_and_data(
    data = newdata,
    # get the formula that was originally to cglmm()
    formula = eval(object$cglmm.calls$cglmm$formula)
  )$newdata
  # only keep the newdata that's returned from update_formula_and_data()

  return(stats::predict(object$fit, newdata = nd, ...))
}
