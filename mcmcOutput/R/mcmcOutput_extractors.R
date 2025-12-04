
# S3 methods for extractors for mcmcOutput objects

`$.mcmcOutput` <- function(x, name) {
  sl <- attr(x, "simsList")
  this <- getElement(sl, name)
  if(is.null(this)) {
    warning("Cannot find parameter '", name, "' in '", deparse(substitute(x)), "'.", call.=FALSE)
    return(NULL)
  }
  out <- x[, this]
  dims <- dim(this)
  if(is.null(dims) && length(this) > 1) # if the parameter is not scalar
    dims <- length(this)
  if(!is.null(dims))
    dim(out) <- c(dim(out)[1], dims)
  # else: parameter is scalar, just return the vector
  return(out)
}
# ........................................................................

`[.mcmcOutput` <- function(x, i, j, ..., drop=TRUE) {
  Nindices <- nargs() - 1 # ignore 'drop'
  if(Nindices > 3) {
    warning("Maximum number of indices is 3.", call.=FALSE)
    return(NULL)
  }
  out <- switch(Nindices,
    unclass(x)[, i, drop=FALSE],
    unclass(x)[i, j, drop=drop],
    {nChains <- attr(x, "nChains")
      parnames <- dimnames(x)[[2]]
      dim(x) <- c(dim(x)[1]/nChains, nChains, dim(x)[2])
      dimnames(x) <- list(NULL, 1:nChains, parnames)
      unclass(x)[i, j, ..., drop=drop]
    })
  if(Nindices == 1)
    out <- mcmcOutput(out, nChains=attr(x, "nChains"))
  return(out)
}
# ----------------------------------------------------------------
