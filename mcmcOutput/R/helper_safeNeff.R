
# An error-catching wrapper for coda::effectiveSize

# For a single chain, returns NA instead of error, or if n.eff = 0
safeNeff1 <- function(v) {
  tmp <- try(coda::effectiveSize(as.numeric(v)), silent=TRUE)
  if(inherits(tmp, "try-error") || tmp == 0)
    return(NA)
  return(tmp)
}

# For a data frame or matrix with a column for each parameter
safeNeff <- function(x) {
  apply(x, 2, safeNeff1)
}

