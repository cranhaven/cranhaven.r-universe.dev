library(iterors)

# Returns a filtering iterator
ifilter <- function(it, FUN, ...) {
  it <- iteror(it)

  nextOr_ <- function(or) {
    repeat {
      x <- nextOr(it, return(or))
      if (FUN(x, ...))
        break
    }
    x
  }

  iteror_internal(nextOr_)
}

# Simple example use
it <- irnorm(1, count=10)
is.positive <- function(x) x > 0
print(as.list(ifilter(it, is.positive)))

# Example using a function with an additional argument
it <- irnorm(1, count=10)
greater.than <- function(x, y) x > y
print(as.list(ifilter(it, greater.than, 1.0)))
