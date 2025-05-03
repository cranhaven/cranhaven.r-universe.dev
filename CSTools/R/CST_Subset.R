#'Subset an object of class s2dv_cube
#'
#'This function allows to subset (i.e. slice, take a chunk of) the data inside
#'an object of class \code{s2dv_cube} and modify the dimensions, coordinates and
#'attributes accordingly, removing any variables, time steps and spatial
#'coordinates that are dropped when subsetting. It ensures that the information
#'inside the s2dv_cube remains coherent with the data it contains.\cr\cr
#'As in the function \code{Subset} from the ClimProjDiags package, the 
#'dimensions to subset along can be specified via the parameter \code{along} 
#'either with integer indices or by their name.\cr\cr
#'There are additional ways to adjust which dimensions are dropped in the 
#'resulting object: either to drop all, to drop none, to drop only the ones that
#'have been sliced or to drop only the ones that have not been sliced.\cr\cr
#'The \code{load_parameters} and \code{when} attributes of the original cube
#'are preserved. The \code{source_files} attribute is subset along the
#'\code{var_dim} and \code{dat_dim} dimensions.
#' 
#'@author Agudetse Roures Victoria, \email{victoria.agudetse@bsc.es}
#'
#'@param x An object of class \code{s2dv_cube} to be sliced.
#'@param along A vector with references to the dimensions to take the subset 
#'  from: either integers or dimension names.
#'@param indices A list of indices to take from each dimension specified in 
#'  'along'. If a single dimension is specified in 'along', it can be directly
#'  provided as an integer or a vector.
#'@param drop Whether to drop all the dimensions of length 1 in the resulting 
#'  array, none, only those that are specified in 'along', or only those that 
#'  are not specified in 'along'. The possible values are: 'all' or TRUE, 'none'
#'  or FALSE, 'selected', and 'non-selected'. The default value is FALSE.
#'@param dat_dim A character string indicating the name of dataset dimension.
#'  The default value is NULL.
#'@param var_dim A chatacter string indicating the name of the variable
#'  dimension. The default value is NULL.
#'
#'@return An object of class \code{s2dv_cube} with similar data, coordinates and 
#'  attributes as the \code{x} input, but with trimmed or dropped dimensions.
#'
#'@examples
#'#Example with sample data:
#'# Check original dimensions and coordinates
#'lonlat_temp$exp$dims
#'names(lonlat_temp$exp$coords)
#'# Subset the s2dv_cube
#'exp_subset <- CST_Subset(lonlat_temp$exp,
#'                         along = c("lat", "lon"),
#'                         indices = list(1:10, 1:10),
#'                         drop = 'non-selected')
#'# Check new dimensions and coordinates
#'exp_subset$dims
#'names(exp_subset$coords)
#' 
#'@seealso \link[ClimProjDiags]{Subset}
#'
#'@importFrom ClimProjDiags Subset
#'@export
CST_Subset <- function(x, along, indices, drop = FALSE, var_dim = NULL,
                       dat_dim = NULL) {
  # Check that x is s2dv_cube
  if (!inherits(x, 's2dv_cube')) {
    stop("Parameter 'x' must be of the class 's2dv_cube'.")
  }
  # Check var_dim
  if (!is.null(var_dim)) {
    if ((!is.character(var_dim)) || (length(var_dim) > 1)) {
      stop("Parameter 'var_dim' must be a character string.")
    }
  }
  # Check dat_dim
  if (!is.null(dat_dim)) {
    if ((!is.character(dat_dim)) || (length(dat_dim) > 1)) {
      stop("Parameter 'dat_dim' must be a character string.")
    }
  }
  # Check indices
  if (!is.list(indices)) {
    if (length(along) == 1) {
      indices <- list(indices)
    }
  }

  # Subset data
  x$data <- Subset(x$data, along = along,
                   indices = indices,
                   drop = drop)
  # Adjust dimensions
  x$dims <- dim(x$data)
  # Adjust coordinates
  for (dimension in 1:length(along)) {
    dim_name <- along[dimension]
    index <- indices[dimension]
    # Only rename coordinates that have not been dropped
    if (dim_name %in% names(x$dims)) {
      # Subset coordinate by indices
      x$coords[[dim_name]] <- .subset_with_attrs(x$coords[[dim_name]], indices = index)
    }
  }
  # Remove dropped coordinates
  for (coordinate in names(x$coords)) {
    if (!(coordinate %in% names(x$dims))) {
      x$coords[[coordinate]] <- NULL
    }
  }
  # Adjust attributes
  # Variable
  for (dimension in 1:length(along)) {
    dim_name <- along[dimension]
    index <- indices[[dimension]]
    if ((!is.null(var_dim)) && (dim_name == var_dim)) {
      x$attrs$Variable$varName <- as.vector(x$coords[[dim_name]])
    }
    if ((!is.null(dat_dim)) && (dim_name == dat_dim)) {
      x$attrs$Datasets <- as.vector(x$coords[[dim_name]])
    }
    if ((!is.null(x$attrs$source_files)) &&
	      (dim_name %in% names(dim(x$attrs$source_files)))) {
      x$attrs$source_files <- Subset(x$attrs$source_files,
                                     along = dim_name,
                                     indices = index,
                                     drop = drop)
    }
  }
  # Remove metadata from variables that were dropped
  vars_to_keep <- na.omit(match(c(names(x$dims), (x$attrs$Variable$varName)),
     		                  names(x$attrs$Variable$metadata)))
  x$attrs$Variable$metadata <- x$attrs$Variable$metadata[vars_to_keep]
  # Subset Dates
  time_along <- intersect(along, names(dim(x$attrs$Dates)))
  if (!(length(time_along) == 0)) {
    time_indices <- indices[match(time_along, along)]
    original_dates <- x$attrs$Dates
    x$attrs$Dates <- Subset(x$attrs$Dates,
                            along = time_along,
                            indices = time_indices,
                            drop = drop)
  }
  # Subset metadata
  for (variable in 1:length(names(x$attrs$Variable$metadata))) {
    if (any(along %in% names(dim(x$attrs$Variable$metadata[[variable]])))) {
      dim_along <- along[along %in% names(dim(x$attrs$Variable$metadata[[variable]]))]
      index_along <- indices[match(dim_along, along)]
      x$attrs$Variable$metadata[[variable]] <- .subset_with_attrs(x$attrs$Variable$metadata[[variable]],
                                                                  along = dim_along,
                                                                  indices = index_along,
                                                                  drop = drop)
    }
  }
  return(x)
}

# Function to subset with attributes
.subset_with_attrs <- function(x, ...) {
  args_subset <- list(...)
  if (any(is.null(dim(x)), length(dim(x)) == 1)) {
    l <- x[args_subset[['indices']][[1]]]
  } else {
    l <- Subset(x, along = args_subset[['along']], 
                indices = args_subset[['indices']],
                drop = args_subset[['drop']])
  }
  attr.names <- names(attributes(x))
  attr.names <- attr.names[attr.names != 'names']
  attr.names <- attr.names[attr.names != 'dim']
  attributes(l)[attr.names] <- attributes(x)[attr.names]
  return(l)
}
