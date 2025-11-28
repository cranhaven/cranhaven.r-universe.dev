#' @keywords internal
.create_validation_set <- function(data, train.fraction) {
  if (train.fraction != 1) {
    trainIndex <- seq(1, train.fraction * nrow(data))
    valIndex <- setdiff(1:nrow(data), trainIndex)
    training.set <- data[trainIndex, ]
    validation.set <- data[valIndex, ]
  } else{
    training.set <- data
    validation.set <- NULL
  }
  return(list(training.set = training.set, validation.set = validation.set))
}

#' @keywords internal
.create_cv_folds <- function(data, cv.folds, folds.id, seed = NULL) {
  if (!is.null(folds.id)) {
    if (length(folds.id) != nrow(data))
      stop("length(folds.id) differs from the number of rows in the data set.")
    return(folds.id)
  } else{
    if (!is.null(seed))
      set.seed(seed)
    cv_index <- sample(seq(1, cv.folds),
                       size = nrow(data),
                       replace = T)
    return(cv_index)
  }
}
