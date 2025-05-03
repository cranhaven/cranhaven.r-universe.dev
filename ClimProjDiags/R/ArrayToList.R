#' Split an array into list by a given array dimension
#'
#'@description This function splits an array into a list as required by 
#'PlotLayout function from package "s2dv" when parameter 'special_args' is used. 
#'The function ArrayToList allows to add names to the elements of the list in 
#'two different levels, the 'list' or the 'sublist'.
#'
#'@param data A multidimensional array.
#'@param dim A character string indicating the name of the dimension to split or 
#'  an integer indicating the position of the dimension.
#'@param level A string character 'list' or 'sublist' indicating if it should be 
#'  a list or a sublist. By default it creates a list.
#'@param names A vector of character strings to name the list (if it is a single 
#'  string, it would be reused) or a single character string to name the 
#'  elements in the sublist. 
#'
#'@return A list of arrays of the length of the dimension set in parameter 'dim'.
#'
#'@examples
#'data <- array(1:240, c(month = 12, member = 5, time = 4))
#'# Create a list:
#'datalist <- ArrayToList(data, dim = 'month', level = 'list', names = month.name)
#'class(datalist)
#'class(datalist[[1]])
#'str(datalist)
#'# Create a sublist:
#'datalist <- ArrayToList(data, dim = 'month', level = 'sublist', names = 'dots')
#'class(datalist)
#'class(datalist[[1]])
#'class(datalist[[1]][[1]])
#'str(datalist)
#'@seealso \link[s2dv]{PlotLayout} 
#'@export
ArrayToList <- function(data, dim, level = 'list', names = NULL) {
  if (is.null(dim(data))) {
    stop("Parameter 'data' must be an array or matrix.")
  }
  if (length(dim) > 1) {
    warning("The lenght of parameter 'dim' is greater than 1 and only the",
            " first element is used.")
    dim <- dim[1]
  }
  if (is.numeric(dim) & dim > length(dim(data))) {
    stop("Parameter 'dim' cannot exceed the length of 'data' dimensions.")
  }
  if (is.character(dim)) {
    dim <- which(names(dim(data)) == dim)
    if (identical(dim, integer(0))) {
      stop("Parameter 'dim' is not found in the dimension names.")
    }
  }
  if( !is.character(names)) {
    stop("Parameter 'names' is not class character.")
  }
  datalist <- asplit(data, dim)
  attributes(datalist) <- NULL
  if (level == 'list') {
    if (length(names) == 1) {
      names <- rep(names, dim(data)[dim])
    } else {
      if (length(names) != dim(data)[dim]) {
        stop("The length of parameter names could be 1 or equal to the dimension to split.")
      }
    }
    names(datalist) <- names
  } else if (level == 'sublist') {
    if (length(names) > 1) {
      warning("Parameter 'names' should be a single character string to name the sublist.",
              "Only the first element is used.")
      names <- names[1]
    }
    datalist <- lapply(datalist, function(x) {
                  res <- list(x)
                  names(res) <- names
                  return(res)})
  } else {
    stop("Parameter 'level' should be 'list' nor 'sublist'.")
  }
  return(datalist)
}
