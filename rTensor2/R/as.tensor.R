as.tensor <- function (x, drop = FALSE)
{
  stopifnot(is.array(x) || is.vector(x))
  if (is.vector(x)) {
    modes <- c(length(x))
    num_modes <- 1L
    new("Tensor", num_modes, modes, data = x)
  }
  else {
    modes <- dim(x)
    num_modes <- length(modes)
    dim1s <- which(modes == 1)
    if (drop && (length(dim1s) > 0)) {
      modes <- modes[-dim1s]
      num_modes <- num_modes - length(dim1s)
      new("Tensor", num_modes, modes, data = array(x, dim = modes))
    }
    else {
      new("Tensor", num_modes, modes, data = x)
    }
  }
}
