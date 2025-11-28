# Auxiliary functions

makeXY <- function(formula, data) {
  mf <- model.frame(formula = formula, data = data)
  x <- model.matrix(attr(mf, "terms"), data = mf)
  y <- model.response(mf)

  list(
    y = y,
    x = x
  )
}

throw_class_error <- function(object, subclass) {
  if (!inherits(object, "emdi")) {
    error_string <- paste0(subclass, " object has to be created by the emdi
                           package for emdi methods to work.")
    stop(error_string)
  }
}


#' Quick function to estimate weighted quantiles
#'
#' @param x a numeric vector
#' @param weights a numeric vector for the weights
#' @param probs probabilities
#' @return weighted quantile
#'
#' @export


wtd.quantile <- function (x, weights = NULL, probs = NULL) {

  n <- length(x)
  order <- order(x)
  x <- x[order]

  weights <- weights[order]

  if (is.null(weights)) {
    rw <- seq_len(n) / n
  }
  else {
    rw <- cumsum(weights) / sum(weights)
  }
  q <- vapply(probs, function(p) {
    if (p == 0) {
      return(x[1])
    }
    else if (p == 1) {
      return(x[n])
    }
    select <- min(which(rw >= p))
    if (rw[select] == p) {
      mean(x[select:(select + 1)])
    }
    else {
      x[select]
    }
  }, numeric(1))
  return(unname(q))
}
