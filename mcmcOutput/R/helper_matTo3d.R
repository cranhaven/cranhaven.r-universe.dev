
# Convert mcmcOutput to a 3d array,
#   or multi-d array to a matrix - not exported

# Retains all the attributes

matTo3d <- function(x) {
  nChains <- attr(x, "nChains")
  npars <- ncol(x)                     # number of parameters
  parnames <- colnames(x)
  ipc <- nrow(x) / nChains             # iterations per chain
  x <- unclass(x)                      # convert to plain old matrix
  dim(x) <- c(ipc, nChains, npars)     # separate the chains
  dimnames(x) <- list(NULL, 1:nChains, parnames)
  return(x)
}

# Converts array (eg, from a sims.list) to matrix
arrayToMat <- function(x, name, na.rm=TRUE) {
  if(missing(name))
    name <- deparse(substitute(x))
  x_new <- matrix(x, nrow=dim(x)[1])
  dims <- dim(x)[-1]
  t1 <- lapply(dims, function(x) 1:x)
  t2 <- do.call(expand.grid, t1)
  t3 <- apply(t2, 1, paste, collapse=",")
  colnames(x_new) <- paste0(name, "[", t3, "]")
  if(na.rm) {
    gotna <- is.na(colMeans(x_new))
    x_new <- x_new[, !gotna, drop=FALSE]
  }
  return(x_new)
}
