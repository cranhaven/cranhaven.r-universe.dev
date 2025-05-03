#'Function to  Merge Dimensions
#'
#'@author Nuria Perez-Zanon, \email{nuria.perez@bsc.es}
#'
#'@description This function merges two dimensions of the array \code{data} in a 
#''s2dv_cube' object into one. The user can select the dimensions to merge and 
#'provide the final name of the dimension. The user can select to remove NA 
#'values or keep them.
#'
#'@param data An 's2dv_cube' object
#'@param merge_dims A character vector indicating the names of the dimensions to 
#'  merge.
#'@param rename_dim a character string indicating the name of the output 
#'  dimension. If left at NULL, the first dimension name provided in parameter 
#'  \code{merge_dims} will be used.
#'@param na.rm A logical indicating if the NA values should be removed or not.
#'
#'@examples
#'data <- 1 : c(2 * 3 * 4 * 5 * 6 * 7)
#'dim(data) <- c(time = 7, lat = 2, lon = 3, monthly = 4, member = 6,
#'               dataset = 5, var = 1)
#'data[2,,,,,,] <- NA
#'data[c(3,27)] <- NA
#'data <- list(data = data)
#'class(data) <- 's2dv_cube'
#'new_data <- CST_MergeDims(data, merge_dims = c('time', 'monthly'))
#'new_data <- CST_MergeDims(data, merge_dims = c('lon', 'lat'), rename_dim = 'grid')
#'new_data <- CST_MergeDims(data, merge_dims = c('time', 'monthly'), na.rm = TRUE)
#'@export
CST_MergeDims <- function(data, merge_dims = c('ftime', 'monthly'), 
                          rename_dim = NULL, na.rm = FALSE) {
  # Check 's2dv_cube'
  if (!inherits(data, 's2dv_cube')) {
    stop("Parameter 'data' must be of the class 's2dv_cube'.")
  }
  if (is.null(rename_dim)) {
    rename_dim <- merge_dims[1]
  }
  # data
  data$data <- MergeDims(data$data, merge_dims = merge_dims,
                         rename_dim = rename_dim, na.rm = na.rm)
  # dims
  data$dims <- dim(data$data)

  # rename_dim               
  if (length(rename_dim) > 1) {
    rename_dim <- as.character(rename_dim[1])
  }
  # coords
  data$coords[merge_dims] <- NULL
  data$coords[[rename_dim]] <- 1:dim(data$data)[rename_dim]
  attr(data$coords[[rename_dim]], 'indices') <- TRUE

  # attrs
  if (all(merge_dims %in% names(dim(data$attrs$Dates)))) {
    dim(data$attrs$Dates) <- dim(data$data)[rename_dim]
  } else if (any(merge_dims %in% names(dim(data$attrs$Dates)))) {
    warning("The dimensions of 'Dates' array will be different from ",
            "the temporal dimensions in 'data'. Parameter 'merge_dims' ",
            "only includes one temporal dimension of 'Dates'.")
  }
  return(data)
}
#'Function to Split Dimension
#'
#'@author Nuria Perez-Zanon, \email{nuria.perez@bsc.es}
#'
#'@description This function merges two dimensions of an array into one. The 
#'user can select the dimensions to merge and provide the final name of the 
#'dimension. The user can select to remove NA values or keep them.
#'
#'@param data An n-dimensional array with named dimensions
#'@param merge_dims A character vector indicating the names of the dimensions to 
#'  merge.
#'@param rename_dim A character string indicating the name of the output 
#'  dimension. If left at NULL, the first dimension name provided in parameter 
#'  \code{merge_dims} will be used.
#'@param na.rm A logical indicating if the NA values should be removed or not.
#'
#'@examples
#'data <- 1 : 20
#'dim(data) <- c(time = 10, lat = 2)
#'new_data <- MergeDims(data, merge_dims = c('time', 'lat'))
#'@import abind
#'@importFrom ClimProjDiags Subset
#'@export
MergeDims <- function(data, merge_dims = c('time', 'monthly'), 
                      rename_dim = NULL, na.rm = FALSE) {
  # check data
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (is.null(dim(data))) {
    stop("Parameter 'data' must have dimensions.")
  }
  if (is.null(names(dim(data)))) {
    stop("Parameter 'data' must have dimension names.")
  }
  dims <- dim(data)
  # check merge_dims
  if (is.null(merge_dims)) {
    stop("Parameter 'merge_dims' cannot be NULL.")
  }
  if (!is.character(merge_dims)) {
    stop("Parameter 'merge_dims' must be a character vector ",
         "indicating the names of the dimensions to be merged.")
  }
  if (length(merge_dims) > 2) {
    warning("Only two dimensions can be merge, only the first two ",
            "dimension will be used. To merge further dimensions ",
            "consider to use this function multiple times.")
    merge_dims <- merge_dims[1 : 2]
  } else if (length(merge_dims) < 2) {
    stop("Parameter 'merge_dims' must be of length two.")
  }
  if (is.null(rename_dim)) {
    rename_dim <- merge_dims[1]
  }
  if (length(rename_dim) > 1) {
    warning("Parameter 'rename_dim' has length greater than 1 ",
            "and only the first element will be used.")
    rename_dim <- as.character(rename_dim[1])
  }
  if (!any(names(dims) %in% merge_dims)) {
    stop("Parameter 'merge_dims' must match with dimension ",
         "names in parameter 'data'.")
  }
  pos1 <- which(names(dims) == merge_dims[1])
  pos2 <- which(names(dims) == merge_dims[2])
  if (length(pos1) == 0 | length(pos2) == 0) {
    stop("Parameter 'merge_dims' must match with dimension ",
         "names in parameter 'data'.")
  }
  if (pos1 > pos2) {
    pos1 <- pos1 - 1
  }
  data <- lapply(1:dims[pos2], function(x) {Subset(data, along = pos2,
                 indices = x, drop = 'selected')})
  data <- abind(data, along = pos1)
  names(dim(data)) <- names(dims)[-pos2]
  if (!is.null(rename_dim)) {
    names(dim(data))[pos1] <- rename_dim
  }
  if (na.rm) {
    nas <- which(is.na(Subset(data, along = -pos1, indices = 1)))
    if (length(nas) != 0) { 
      nas <- unlist(lapply(nas, function(x) {
                    if(all(is.na(Subset(data, along = pos1,
                                        indices = x)))) {
                      return(x)}}))
      data <- Subset(data, along = pos1, indices = -nas)
    }
  }
  return(data)
}
