#'Specify dimension selectors with actual values
#'
#'This is a helper function used in a Start() call to define the desired range
#'of dimensions. It specifies the actual value to be matched with the 
#'coordinate variable. See details in the documentation of the parameter 
#'\code{\dots} 'indices to take' of the function Start().
#'@param x A numeric vector or a list with two nemerics to take all the element 
#'  between the two specified values (both extremes inclusive).
#'@return Same as input, but with additional attribute 'indices', 'values', and
#'  'chunk'.
#'@examples
#' # Used in Start():
#'  data_path <- system.file('extdata', package = 'startR')
#'  path_obs <- file.path(data_path, 'obs/monthly_mean/$var$/$var$_$sdate$.nc')
#'  sdates <- c('200011', '200012')
#'  data <- Start(dat = list(list(path = path_obs)),
#'                var = 'tos',
#'                sdate = sdates,
#'                time = 'all',
#'                latitude = values(seq(-80, 80, 20)),
#'                latitude_reorder = Sort(),
#'                longitude = values(list(10, 300)),
#'                longitude_reorder = CircularSort(0, 360),
#'                return_vars = list(latitude = 'dat', 
#'                                   longitude = 'dat', 
#'                                   time = 'sdate'),
#'                retrieve = FALSE)
#'@seealso \code{\link{indices}}
#'@export
values <- function(x) {
  attr(x, 'indices') <- FALSE
  attr(x, 'values') <- TRUE
  attr(x, 'chunk') <- c(chunk = 1, n_chunks = 1)
  x
}
