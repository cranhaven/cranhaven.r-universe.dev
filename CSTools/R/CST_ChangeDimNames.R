#'Change the name of one or more dimensions for an object of class s2dv_cube
#'
#'Change the names of the dimensions specified in 'original_names' to the names
#'in 'new_names'. The coordinate names and the dimensions of any attributes
#'are also modified accordingly. 
#' 
#'@author Agudetse Roures Victoria, \email{victoria.agudetse@bsc.es}
#'
#'@param data An object of class \code{s2dv_cube} whose dimension names
#'  should be changed.
#'@param original_names A single character string or a vector indicating the 
#'  dimensions to be renamed.
#'@param new_names A single character string or a vector indicating the new
#'  dimension names, in the same order as the dimensions in 'original_names'. 
#'
#'@return An object of class \code{s2dv_cube} with similar data, coordinates and 
#'attributes as the \code{data} input, but with modified dimension names.
#'
#'@examples
#'# Example with sample data:
#'# Check original dimensions and coordinates
#'lonlat_temp$exp$dims
#'names(lonlat_temp$exp$coords)
#'dim(lonlat_temp$exp$attrs$Dates)
#'# Change 'dataset' to 'dat' and 'ftime' to 'time'
#'exp <- CST_ChangeDimNames(lonlat_temp$exp,
#'                          original_names = c("dataset", "ftime"),
#'                          new_names = c("dat", "time"))
#'# Check new dimensions and coordinates
#'exp$dims
#'names(exp$coords)
#'dim(exp$attrs$Dates)
#' 
#'@export
CST_ChangeDimNames <- function(data, original_names, new_names) {
  if (!inherits(data, "s2dv_cube")) {
    stop("Parameter 'data' must be an object of class 's2dv_cube'.")
  }
  if (!is.character(original_names)) {
    stop("Parameter 'original_names' must be a character string or a ",
         "vector of character strings.")
  }
  if (!is.character(new_names)) {
    stop("Parameter 'new_names' must be a character string or a ",
         "vector of character strings.")
  }

  if (!(length(original_names) == length(new_names))) {
    stop("The number of dimension names in 'new_names' must be the same ",
         "as in 'original_names'.")
  }
  if (!all(original_names %in% names(data$dims))) {
    stop("Some of the dimensions in 'original_names' could not be found in ",
         "'data'.")
  }
  for (index in 1:length(original_names)) {
    original_name <- original_names[index]
    new_name <- new_names[index]
    # Step 1: Change dims
    names(data$dims)[which(names(data$dims) == original_name)] <- new_name
    # Step 2: Change coords
    names(data$coords)[which(names(data$coords) == original_name)] <- new_name
    # Step 3: Change attrs
    # 3.1 - Dates
    if (original_name %in% names(dim(data$attrs$Dates))) {
      names(dim(data$attrs$Dates))[which(names(dim(data$attrs$Dates)) 
                                         == original_name)] <- new_name
    }
    # 3.2 - Variable metadata
    if (original_name %in% names(data$attrs$Variable$metadata)) {
      names(data$attrs$Variable$metadata)[which(names(data$attrs$Variable$metadata) 
                                                == original_name)] <- new_name
    }
    # 3.3 - Source files
    if (original_name %in% names(dim(data$attrs$source_files))) {
      names(dim(data$attrs$source_files))[which(names(dim(data$attrs$source_files))
                                                == original_name)] <- new_name
    }
  }
  # Change data dimnames after renaming all dimensions
  dim(data$data) <- data$dims
  if (!is.null(attributes(data$data)$dimensions)) {
    attributes(data$data)$dimensions <- names(data$dims)
  }
  # Change $Dates 'dim' attribute
  attr(attributes(data$attrs$Dates)$dim, "names") <- names(dim(data$attrs$Dates))
  return(data)
}

