#' Extracting the Model Frame from OPSR Model Fits
#'
#' @param formula an object of class `"opsr"`.
#' @param ... a mix of further arguments such as `data`, `na.action` or `subset`,
#'   passed to the default method.
#'
#' @return A [`data.frame`] containing the variables used in `formula$formula`.
#' @method model.frame opsr
#' @seealso [`stats::model.frame`]
#'
#' @export
model.frame.opsr <- function(formula, ...) {
  ## almost identical to model.frame.lm
  object <- formula
  dots <- list(...)
  nargs <- dots[match(c("data", "na.action", "subset"), names(dots), 0)]
  if (length(nargs)  || is.null(object$model)) {
    mf <- object$call
    m <- match(c("formula", "data", "subset", "weights", "na.action"), names(mf), 0)
    mf <- mf[c(1, m)]
    mf[[1]] <- as.name("model.frame")
    mf[names(nargs)] <- nargs  # overwrite
    mf$formula <- object$formula
    mf <- eval(mf, environment(object$formula))  # if fun is a function or a formula then environment(fun) returns the environment associated with that function or formula.
    mf
  } else {
    object$model
  }
}
