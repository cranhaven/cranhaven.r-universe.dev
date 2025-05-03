#'Add a named dimension to an object of class s2dv_cube
#'
#'Insert an extra dimension into an array at position 'posdim' with length 
#''lendim'. The array in \code{data} repeats along the new dimension.
#'The dimensions, coordinates and attributes are modified accordingly.
#' 
#'@author Agudetse Roures Victoria, \email{victoria.agudetse@bsc.es}
#'
#'@param data An object of class \code{s2dv_cube} to which the additional
#'  dimension should be added.
#'@param posdim An integer indicating the position of the new dimension.
#'@param lendim An integer indicating the length of the new dimension. 
#'@param name A character string indicating the name for the new dimension.
#'@param values A vector containing the values of the new dimension and any
#'  relevant attributes. If NULL, a sequence of integers from 1 to lendim will
#'  be added.
#'
#'@return An object of class \code{s2dv_cube} with similar data, coordinates and 
#'attributes as the \code{data} input, but with an additional dimension.
#'
#'@examples
#'#Example with sample data:
#'# Check original dimensions and coordinates
#'lonlat_temp$exp$dims
#'names(lonlat_temp$exp$coords)
#'# Add 'variable' dimension
#'exp <- CST_InsertDim(lonlat_temp$exp,
#'                     posdim = 2,
#'                     lendim = 1,
#'                     name = "variable",
#'                     values = c("tas"))
#'# Check new dimensions and coordinates
#'exp$dims
#'exp$coords$variable
#' 
#'@seealso \link[s2dv]{InsertDim}
#'
#'@importFrom s2dv InsertDim
#'@export
CST_InsertDim <- function(data, posdim, lendim, name, values = NULL) {
  # Check inputs
  # Check 's2dv_cube'
  if (!inherits(data, 's2dv_cube')) {
    stop("Parameter 'data' must be of the class 's2dv_cube'.")
  }
  # Check name
  if (!is.character(name) || length(name) > 1) {
    stop("Parameter 'name' must be a character string")
  }
  # Check values
  if (is.null(values)) {
    warning(paste0("Parameter 'values' is not provided. Adding a sequence of ",
	                 "integers from 1 to 'lendim' as the values for the new dimension."))
    values <- 1:lendim
  } else {
    if (!(length(values) == lendim)) {
      stop(paste0("The length of the parameter 'values' must be consistent",
	                "with the parameter 'lendim'."))
    }
  }

  # Insert dim in data
  data$data <- s2dv::InsertDim(data$data, posdim = posdim, lendim = lendim,
			                         name = name)
  # Adjust dimensions
  data$dims <- dim(data$data)
  # Adjust coordinates
  data$coords[[name]] <- values
  data$coords <- data$coords[names(data$dims)]
  return(data)
}
