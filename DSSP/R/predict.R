#' Predictions from a model with new data.
#'
#' @param object a fitted dsspMod object.
#' @param newdata a data frame for which to evaluate predictions.
#' @param ... optional and ignored arguments.
#'
#' @return returns matrix with posterior densities for each row in the input data.
#' @export
#'
#' @examples
#' data("meuse.all", package = "gstat")
#' sp::coordinates(meuse.all) <- ~ x + y
#' meuse.fit <- DSSP(
#'   formula = log(zinc) ~ 1, data = meuse.all[1:155, ], N = 100, function(x) -2 * log(1 + x),
#'   pars = c(0.001, 0.001)
#' )
#' preds <- predict(meuse.fit, meuse.all[156:164, ])
predict.dsspMod <- function(object, newdata, ...) {
  if (missing(newdata)) {
    return(object$y_fitted)
  }
  if (!any(class(newdata) %in% c("SpatialPointsDataFrame", "SpatialPoints"))) {
    sp::coordinates(newdata) <- object$coords
  }
  w.pred <- sp::coordinates(newdata)
  if (any(!grepl("scaled", names(attributes(w.pred))))) {
    w.pred <- scale(w.pred, center = object$coord_scaling$center, scale = object$coord_scaling$scale)
  }

  X <- object$X
  y <- object$Y
  eta <- object$eta
  delta <- object$delta
  nu <- object$nu

  mt <- stats::terms(object$formula, data = newdata)
  mf <- stats::lm(object$formula, data = newdata, method = "model.frame")
  new_x <- stats::model.matrix(mt, mf)

  N <- length(eta)
  n <- length(y)
  X <- rbind(X, w.pred)
  m <- nrow(X) - n
  Y <- c(y, rep(0, m))

  ##  Make augmented M matrix
  M.list <- make.M(X, covariates = rbind(object$covariates, new_x))

  ##  Extract Vectors
  v <- M.list$M.eigen$vectors

  ##  Extract Values
  ev <- M.list$M.eigen$values

  y.pred <- .sample_y_pred_cpp(list(N = N, eta = eta, ev = ev, v = v, Y = Y, delta = delta, n = n, m = m, nu = nu))

  y.pred * object$y_scaling$scale + object$y_scaling$center
}
