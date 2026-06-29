reshape_python <- function(x, dim) {
  if (is.null(dim(x))) {
    # vector
    flattened <- as.vector(x)
  } else {
    rx <- reverse_dimensions(x)
    flattened <- as.vector(rx)
  }
  reshaped <- array(flattened, dim = rev(dim))
  return(reverse_dimensions(reshaped))
}

reverse_dimensions <- function(x) {
  dim <- dim(x)
  if (is.null(dim)) {
    stop("Input must be a matrix or an array.")
  }
  reversed_array <- aperm(x, rev(seq_along(dim)))
  return(reversed_array)
}
