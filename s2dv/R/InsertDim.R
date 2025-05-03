#'Add a named dimension to an array
#'
#'Insert an extra dimension into an array at position 'posdim' with length 
#''lendim'. The array repeats along the new dimension.
#'
#'@param data An array to which the additional dimension to be added.
#'@param posdim An integer indicating the position of the new dimension.
#'@param lendim An integer indicating the length of the new dimension.
#'@param name A character string indicating the name for the new dimension. 
#'  The default value is NULL.
#'
#'@return An array as parameter 'data' but with the added named dimension.
#'
#'@examples
#'a <- array(rnorm(15), dim = c(a = 3, b = 1, c = 5, d = 1))
#'res <- InsertDim(InsertDim(a, posdim = 2, lendim = 1, name = 'e'), 4, c(f = 2))
#'dim(res)
#'
#'@import multiApply
#'@export
InsertDim <- function(data, posdim, lendim, name = NULL) {

  # Check inputs 
  ## data
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (is.vector(data) & !is.list(data)) {  #is vector
    data <- as.array(data)
  }
  if (!is.array(data)) {
    stop("Parameter 'data' must be an array.")
  }
  ## posdim
  if (!is.numeric(posdim)) {
    stop("Parameter 'posdim' must be a positive integer.")
  } else if (posdim %% 1 != 0 | posdim <= 0 | length(posdim) > 1) {
    stop("Parameter 'posdim' must be a positive integer.")
  }
  if (posdim > (length(dim(data)) + 1)) {
    stop("Parameter 'posdim' cannot excess the number of dimensions of parameter 'data' plus 1")
  }
  ## lendim
  if (!is.numeric(lendim)) {
    stop("Parameter 'lendim' must be a positive integer.")
  } else if (lendim %% 1 != 0 | lendim <= 0 | length(lendim) > 1) {
    stop("Parameter 'lendim' must be a positive integer.")
  }
  ## name
  if (is.null(name)) {
    if (is.null(names(lendim))) {
      name <- 'new'
      .warning("The name of new dimension is not given. Set the name as 'new'.")
    } else {
      name <- names(lendim)
    }
  } else {
    if (!is.character(name) | length(name) > 1) {
      stop("Parameter 'name' must be a character string.")
    }
  }

  ###############################
  # Calculate InsertDim
  names(lendim) <- name

  ## Put the new dim at the end first
  data <- array(data, dim = c(dim(data), lendim))

  ## Reorder dimension
  if (posdim == 1) {
    order <- c(length(dim(data)), 1:(length(dim(data)) - 1))
    data <- Reorder(data, order)
  } else if (posdim == length(dim(data))) {  # last dim
  
  } else { # middle dim
    order <- c(1:(posdim - 1), length(dim(data)), posdim:(length(dim(data)) - 1))
    data <- Reorder(data, order)
  }

  return(data)
}
