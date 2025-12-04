
# This file contains methods for classes defined in OTHER packages.

# for class 'runjags' from the runjags package

as.data.frame.runjags <- function(x, ...) {
  df <- as.data.frame(as.matrix(as.mcmc.list(x)))
  names(df) <- sub(",", "\\.", sub("\\]", "", sub("\\[", "", names(df))))
  return(invisible(df))
}




