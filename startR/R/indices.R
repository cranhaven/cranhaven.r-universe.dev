#'Specify dimension selectors with indices
#'
#'This is a helper function used in a Start() call to define the desired range
#'of dimensions. It selects the indices of the coordinate variable from 
#'original data. See details in the documentation of the parameter \code{\dots}
#''indices to take' of the function Start().
#'
#'@param x A numeric vector or a list with two nemerics to take all the
#'  elements between the two specified indices (both extremes inclusive).
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
#'                latitude = indices(1:2),
#'                longitude = indices(list(2, 14)),
#'                return_vars = list(latitude = 'dat', 
#'                                   longitude = 'dat', 
#'                                   time = 'sdate'),
#'                retrieve = FALSE)
#'@seealso \code{\link{values}}
#'@export
indices <- function(x) {
  attr(x, 'indices') <- TRUE
  attr(x, 'values') <- FALSE
  attr(x, 'chunk') <- c(chunk = 1, n_chunks = 1)
  x
}
