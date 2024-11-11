search2 <- function(s, xvec, include = TRUE) {
  # Finding the index of "the largest x smaller or equal to s"
  # Fn is the cdf table (x (should be ordered) and y)
  # index = which (s >= c(-Inf, xvec) & s <= c(xvec, Inf)) - 1
  index = if (include) {which (s >= c(-Inf, xvec)) - 1} else {which (s > c(-Inf, xvec)) - 1}
  max(index)
}
# search2(49.25, xvec =  48:52)
lin.interpolate <- function(s, xvec, yvec = NULL, return.y.only = FALSE, exponential.tail = TRUE) {
  x.len = length(xvec)
  low.index <- search2(s, xvec)
  lower <- xvec[low.index]
  if (low.index == x.len) {
    upper <- lower
    proportion <- 0
  } else {
    upper <- xvec[low.index + 1]
    proportion <- (s - lower) / (upper - lower)
  }
  result <- c(low.index = low.index, proportion = proportion)
  if (!is.null(yvec)) {
    lower.y <- yvec[low.index]
    upper.y <- yvec[min(x.len, low.index + 1)]
    if (is.infinite(upper.y) && exponential.tail) {
      y.interpolate <- lower.y * log (1 - s) / log (1 - lower)
    } else {
      y.interpolate <- lower.y + proportion * (upper.y - lower.y)
    }
    if (return.y.only) return(y.interpolate)
    result["y.interpolate"] <- y.interpolate
  }
  return(result)
}
lin.interpolate.vec <- Vectorize(lin.interpolate, vectorize.args = "s")
