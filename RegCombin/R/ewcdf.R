#' Compute the weighted empirical cumulative distribution
#'
#' @param x the sample
#' @param weights  the associated weights if any. Default is uniform.
#'
#' @return
#' a vector containing:
#'
#'  - the weighted empirical cumulative distribution function
#'
#'  - the cumulated weights associated to the ordered values of the random variable.

ewcdf <- function(x, weights=rep(1/length(x), length(x)))
{
  stopifnot(length(x) == length(weights))
  # remove NA's together
  nbg <- is.na(x)
  x <- x[!nbg]
  weights <- weights[!nbg]
  n <- length(x)
  if (n < 1)
    stop("'x' must have 1 or more non-missing values")
  stopifnot(all(weights >= 0))
  # sort in increasing order of x value
  ox <- order(x)
  x <- x[ox]
  w <- weights[ox]
  # find jump locations and match
  vals <- sort(unique(x))
  xmatch <- factor(match(x, vals), levels=seq(vals))
  wmatch <- tapply(w, xmatch, sum)
  wmatch[is.na(wmatch)] <- 0
  rval <- approxfun(vals, cumsum(wmatch),
                    method = "constant", yleft = 0, yright = sum(wmatch),
                    f = 0, ties = "ordered")
  class(rval) <- c("ecdf", "stepfun", class(rval))
  attr(rval, "call") <- sys.call()
  out = vector("list")
  out[[1]] <- rval
  out[[2]] <- cumsum(wmatch)
  return(out)
}
