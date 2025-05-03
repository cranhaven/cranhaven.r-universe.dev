#'Average an array along multiple dimensions
#'
#'This function returns the mean of an array along a set of dimensions and 
#'preserves the dimension names if it has.
#'
#'@param data An array to be averaged.
#'@param dims A vector of numeric or charactor string, indicating along which 
#'  dimensions to average.
#'@param na.rm A logical value indicating whether to ignore NA values (TRUE) or 
#'  not (FALSE).
#'@param drop A logical value indicating whether to keep the averaged 
#'  dimension (FALSE) or drop it (TRUE). The default value is TRUE.
#'@return A numeric or an array with the same dimension as parameter 'data'
#'  except the 'dims' dimensions. If 'drop' is TRUE, 'dims' will be removed; if
#' 'drop' is FALSE, 'dims' will be preserved and the length will be 1. If all
#'  the dimensions are averaged out, a numeric is returned.
#'
#'@examples
#'a <- array(rnorm(24), dim = c(dat = 2, member = 3, time = 4))
#'ens_mean <- MeanDims(a, 'member')
#'dim(ens_mean)
#'ens_time_mean <- MeanDims(a, c(2, 3), drop = FALSE)
#'dim(ens_time_mean)
#'@import multiApply
#'@export
MeanDims <- function(data, dims, na.rm = FALSE, drop = TRUE) {

  # Check inputs 
  ## data
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (!is.numeric(data)) {
    stop("Parameter 'data' must be a numeric array.")
  }
  if (is.null(dim(data))) {  #is vector, turn into array
    data <- as.array(data)
  }
  ## dims
  if (is.null(dims)) {
    stop("Parameter 'dims' cannot be NULL.")
  }
  if (!is.vector(dims) | (is.vector(dims) & !is.numeric(dims) & !is.character(dims))) {
    stop("Parameter 'dims' must be a vector of numeric or character string.")
  }
  if (is.numeric(dims)) {
    if (any(dims < 1) | any(dims %% 1 != 0)) {
      stop("Parameter 'dims' must be positive integers.")
    } else if (any(dims > length(dim(data)))) {
      stop("Parameter 'dims' exceeds the dimension length of parameter 'data'.")
    } 
  }
  if (is.character(dims) && !all(dims %in% names(dim(data)))) {
    stop("Parameter 'dims' do not match the dimension names of parameter 'data'.")
  }
  ## na.rm
  if (!is.logical(na.rm) | length(na.rm) > 1) {
    stop("Parameter 'na.rm' must be one logical value.")
  }
  ## drop
  if (!is.logical(drop) | length(drop) > 1) {
    stop("Parameter 'drop' must be one logical value.")
  }

  ###############################
  # Calculate MeanDims
  dim_data <- dim(data)

  if (length(dims) == length(dim_data)) {
    if (drop) {
      data <- mean(data, na.rm = na.rm)
    } else {
      data <- array(mean(data, na.rm = na.rm), 
                    dim = rep(1, length(dim_data)))
      names(dim(data)) <- names(dim_data)
    }
  } else {
    if (is.character(dims)) {
      dims <- which(names(dim_data) %in% dims)
    }
    data <- aperm(data, c(dims, (seq_along(dim_data))[-dims]))
    data <- colMeans(data, dims = length(dims), na.rm = na.rm)

    # If data is vector
    if (is.null(dim(data))) {
      data <- array(data, dim = dim_data[-dims])
    }
    if (!drop) {
      restore_dim <- as.array(rep(1, length(dims)))
      names(restore_dim) <- names(dim_data)[dims]
      data <- array(data, dim = c(dim(data), restore_dim))
      data <- Reorder(data, names(dim_data))
    }
  }

  return(data)
}

