#'Subset a Data Array
#'
#'This function allows to subset (i.e. slice, take a chunk of) an array, in a 
#'similar way as done in the function \code{take()} in the package plyr. There
#'are two main snprovements:\cr\cr First, the input array can have dimension 
#'names, either in \code{names(dim(x))} or in the attribute 'dimensions'. If 
#'both exist, \code{names(dim(x))} is prioritized. The dimensions to subset
#'along can be specified via the parameter \code{along} either with integer
#'indices or either by their name.\cr\cr Second, there are additional ways to
#'adjust which dimensions are dropped in the resulting array: either to drop
#'all, to drop none, to drop only the ones that have been sliced or to drop
#'only the ones that have not been sliced.\cr\cr 
#'
#'@param x A named multidimensional array to be sliced. It can have dimension 
#'  names either in \code{names(dim(x))} or in the attribute 'dimensions'.
#'@param along A vector with references to the dimensions to take the subset 
#'  from: either integers or dimension names.
#'@param indices A list of indices to take from each dimension specified in 
#'  'along'. If a single dimension is specified in 'along', it can be directly
#'  provided as an integer or a vector.
#'@param drop Whether to drop all the dimensions of length 1 in the resulting 
#'  array, none, only those that are specified in 'along', or only those that 
#'  are not specified in 'along'. The possible values are: 'all' or TRUE, 'none'
#'  or FALSE, 'selected', and 'non-selected'. The default value is FALSE.
#'
#'@return An array with similar dimensions as the \code{x} input, but with 
#'  trimmed or dropped dimensions.
#'
#'@examples
#'#Example synthetic data:
#'# Dimension has name already
#'data <- 1:(2 * 3 * 372 * 1)
#'dim(data) <- c(time = 372, lon = 2, lat = 3, model = 1)
#'data_subset <- Subset(data, c('time', 'model'), 
#'                      list(1:10, TRUE), drop = 'selected')
#'dim(data_subset)
#'# Use attributes 'dimensions'
#'data <- array(1:(2 * 3 * 372 * 1), dim = c(2, 3, 372, 1))
#'attributes(data)[['dimensions']] <- c('lat', 'lon', 'time', 'model')
#'data_subset <- Subset(data, c('lon', 'lat'), list(1, 1), drop = TRUE)
#'dim(data_subset)
#'
#'@export
Subset <- function(x, along, indices, drop = FALSE) {
  # Check x
  if (!is.array(x)) {
    stop("Input array 'x' must be a numeric array.")
  }

  # Take the input array dimension names
  x_has_names_dim <- TRUE
  dim_names <- names(dim(x))
  if (is.null(dim_names)) {
    dim_names <- attr(x, 'dimensions')
    x_has_names_dim <- FALSE
  }

  if (!is.character(dim_names)) {
    if (any(sapply(along, is.character))) {
      stop("The input array 'x' doesn't have labels for the dimensions but the parameter 'along' contains dimension names.")
    }
  } else {
    if (!is.null(names(dim(x))) & !is.null(attr(x, 'dimensions'))) {
      if (any(names(dim(x)) != attr(x, 'dimensions'))) {
        warning("Found attribute 'dimensions' containing different dimension names from ",
                "dim(names(x)). Use the latter one only.")
      }
    } else if (is.null(names(dim(x))) & !is.null(attr(x, 'dimensions'))) {
      names(dim(x)) <- dim_names
    }
  }
  # Check along
  if (any(sapply(along, function(x) !is.numeric(x) && !is.character(x))) |
      length(along) == 0) {
    stop("All provided dimension indices in 'along' must be integers or character strings.")
  }
  if (any(sapply(along, is.character))) {
    req_dimnames <- along[which(sapply(along, is.character))]
    if (length(unique(req_dimnames)) < length(req_dimnames)) {
      stop("The parameter 'along' must not contain repeated dimension names.")
    }
    along[which(sapply(along, is.character))] <- match(req_dimnames, dim_names)
    if (any(is.na(along))) {
      stop("Could not match all dimension names in 'indices' with dimension names in input array 'x'.")
    }
    along <- as.numeric(along)
  }

  # Check indices
  if (!is.list(indices)) {
    if (length(along) == 1) {
      indices <- list(indices)
    } else {
      stop("Parameter 'indices' should be a list.")
    }
  }
  if (length(indices) != length(along)) {
    stop("Parameter 'along' and 'indices' should have the same length.")
  }

  # Check parameter drop
  dims_to_drop <- c()
  if (is.character(drop)) {
    if (drop == 'all') {
      drop <- TRUE
    } else if (any(drop %in% c('selected', 'non-selected', 'none'))) {
      if (drop == 'selected') {
        dims_to_drop <- along[which(sapply(indices, length) == 1)]
      } else if (drop == 'non-selected') {
        dims_to_drop <- dim(x) == 1
        dims_to_drop[along] <- FALSE
        dims_to_drop <- which(dims_to_drop)
      }
      drop <- FALSE
    } else {
      stop("Parameter 'drop' must be one of TRUE, FALSE, 'all', 'selected', 'non-selected', 'none'.")
    }
  } else if (!is.logical(drop)) {
    stop("Parameter 'drop' must be one of TRUE, FALSE, 'all', 'selected', 'non-selected', 'none'.")
  }

  # Take the subset
  nd <- length(dim(x))
  index <- as.list(rep(TRUE, nd))
  index[along] <- indices
  subset <- eval(as.call(c(as.name("["), as.name("x"), index, drop = drop)))
  # If dropped all dimensions, need to drop dimnames too
  if (is.character(dim_names) && drop == TRUE) {
    dim_names_to_remove <- unique(c(along[which(sapply(indices, length) == 1)],
                                    which(dim(x) == 1)))
    if (length(dim_names_to_remove) > 0) {
      dim_names <- dim_names[-dim_names_to_remove]
    }
    # If there is one dim left, subset won't have dimension (but it should have one). Add it back
    if (is.null(dim(subset))) {
      if (!identical(dim_names, character(0))) {
        # If there is one dim left, subset won't have dimension (but it should 
        # have one). Add it back.
        subset <- array(subset, dim = length(subset))
        names(dim(subset)) <- dim_names
      } else {  # a number left
        dim(subset) <- 1
      }
    }
  }
  # if names(dim(x)) doesn't exist, remove the dimension names
  if (!x_has_names_dim) {
    names(dim(subset)) <- NULL
  }

  # Amend the final dimensions and put dimnames and attributes
  metadata <- attributes(x)
  metadata[['dim']] <- dim(subset)
  if (length(dims_to_drop) > 0) {
    if (length(dims_to_drop) == length(metadata[['dim']])) { # a number left
      metadata[['dim']] <- 1
    } else {
      metadata[['dim']] <- metadata[['dim']][-dims_to_drop]
    }
    if (is.character(dim_names)) {
      if (!identical(dim_names[-dims_to_drop], character(0))) {
        names(metadata[['dim']]) <- dim_names[-dims_to_drop]
      }
      if ('dimensions' %in% names(attributes(x))) {
        metadata[['dimensions']] <- dim_names[-dims_to_drop]
      }
    }
  } else if (is.character(dim_names) & !identical(dim_names, character(0))) {
    if (x_has_names_dim) {
      names(metadata[['dim']]) <- dim_names
    }
    if ('dimensions' %in% names(attributes(x))) {
      metadata[['dimensions']] <- dim_names
    }
  }
  attributes(subset) <- metadata

  return(subset)
}
