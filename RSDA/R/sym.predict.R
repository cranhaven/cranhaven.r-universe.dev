#' Predict method to CM and CRM regression model
#' @name sym.predict
#' @aliases sym.predict
#' @author Oldemar Rodriguez Rojas
#' @description To execute predict method the Center Method (CR) and Center and Range Method (CRM) to
#' Linear regression.
#' @param model The output of lm method.
#' @param new.sym.data Should be a symbolic data table read with the function read.sym.table(...).
#' @param ... additional arguments affecting the predictions produced.
#' @return sym.predict produces a vector of predictions or a matrix of predictions and bounds
#' with column names fit, lwr, and upr if interval is set. For type = 'terms' this is a
#' matrix with a column per term and may have an attribute 'constant'
#' @references
#' LIMA-NETO, E.A., DE CARVALHO, F.A.T., (2008). Centre and range method
#' to fitting a linear regression model on symbolic interval data. Computational
#' Statistics and Data Analysis 52, 1500-1515.
#'
#' LIMA-NETO, E.A., DE CARVALHO, F.A.T., (2010). Constrained linear regression models
#' for symbolic interval-valued variables. Computational Statistics and
#' Data Analysis 54, 333-347.
#'
#' @seealso sym.glm
#' @examples
#' data(int_prost_train)
#' data(int_prost_test)
#' model <- sym.lm(lpsa ~ ., sym.data = int_prost_train, method = "cm")
#' pred.cm <- sym.predict(model, int_prost_test)
#' pred.cm
#' @keywords Symbolic lm
#' @export
#'
sym.predict <- function(model, ...) {
  UseMethod("sym.predict")
}

#' @rdname sym.predict
#' @export
#' @importFrom  stats predict
sym.predict.symbolic_lm_cm <- function(model, new.sym.data, ...) {
  all_interval <- all(sapply(new.sym.data, function(x) any(class(x) %in% "symbolic_interval")))
  if (!all_interval) {
    stop("All variables have to be of the same type")
  }
  mins <- interval.min(new.sym.data)
  maxs <- interval.max(new.sym.data)
  pred.mins <- stats::predict(model, newdata = mins, se.fit = TRUE)
  pred.maxs <- stats::predict(model, newdata = maxs, se.fit = TRUE)
  prediction <- data.frame(
    Minimums = pred.mins$fit,
    Maximums = pred.maxs$fit
  )
  out <- list(
    MinPrediction = pred.mins,
    MaxPredictions = pred.maxs,
    Fitted = prediction
  )
  return(out)
}

#' @rdname sym.predict
#' @export
#' @importFrom  stats predict
sym.predict.symbolic_lm_crm <- function(model, new.sym.data, ...) {
  all_interval <- all(sapply(new.sym.data, function(x) any(class(x) %in% "symbolic_interval")))
  if (!all_interval) {
    stop("All variables have to be of the same type")
  }
  # Center Model
  centers <- interval.centers(new.sym.data)
  predc <- stats::predict(model$CenterModel, newdata = centers, se.fit = TRUE)
  # Range Model
  range <- interval.ranges(new.sym.data)
  predr <- stats::predict(model$RangeModel, newdata = range, se.fit = TRUE)
  res.min <- predc$fit - predr$fit
  res.max <- predc$fit + predr$fit
  Prediction <- data.frame(Minimums = res.min, Maximums = res.max)
  out <- list(
    CenterPrediction = predc,
    RangePrediction = predr,
    Fitted = Prediction
  )
  return(out)
}

#' @rdname sym.predict
#' @export
#' @importFrom  stats predict
sym.predict.symbolic_glm_cm <- function(model, new.sym.data, response, ...) {
  all_interval <- all(sapply(new.sym.data, function(x) any(class(x) %in% "symbolic_interval")))
  if (!all_interval) {
    stop("All variables have to be of the same type")
  }
  mins <- interval.min(new.sym.data)
  mins <- as.matrix(mins)
  maxs <- interval.max(new.sym.data)
  maxs <- as.matrix(maxs)
  pred.mins <- predict(model, newx = mins[, -response], s = "lambda.min")
  pred.maxs <- predict(model, newx = maxs[, -response], s = "lambda.min")
  out <- data.frame("Minimums" = pred.mins, "Maximums" = pred.maxs)
  colnames(out) <- c("Minimums", "Maximums")
  return(out)
}

#' @rdname sym.predict
#' @param response The number of the column where is the response variable in the interval data table.
#' @export
#' @importFrom  stats predict
sym.predict.symbolic_glm_crm <- function(model, new.sym.data, response, ...) {
  centers <- interval.centers(new.sym.data)
  centers <- as.matrix(centers)
  predc <- predict(model$CenterModel, newx = centers[, -response], s = "lambda.min")
  # Range Model
  range <- interval.ranges(new.sym.data)
  range <- as.matrix(range)
  predr <- predict(model$RangeModel, newx = range[, -response], s = "lambda.min")
  res.min <- predc - predr
  res.max <- predc + predr
  Prediction <- data.frame(Minimums = res.min, Maximums = res.max)
  return(Prediction)
}
