#'Sort the coordinate variable values in a Start() call
#'
#'The reorder function intended for use as parameter '<dim_name>_reorder'
#'in a call to the function Start(). This function complies with the 
#'input/output interface required by Start() defined in the documentation 
#'for the parameter \code{\dots} of that function.\cr\cr
#'The coordinate applied to Sort() consists of an increasing or decreasing 
#'sort of the values. It is useful for adjusting the latitude order.\cr\cr
#'The coordinate applied to CircularSort() consists of a circular sort of 
#'values, where any values beyond the limits specified in the parameters 
#''start' and 'end' is applied a modulus to fall in the specified 
#'range. This is useful for circular coordinates such as the Earth longitudes.
#'@name Sort
#'@aliases CircularSort
#'@param start A numeric indicating the lower bound of the circular range.
#'@param end A numeric indicating the upper bound of the circular range.
#'@param \dots Additional parameters to adjust the reorderig. See function
#'  sort() for more details. 
#'
#'@return 
#'A list of 2 containing:
#'\item{$x}{
#'  The reordered values.
#'}
#'\item{$ix}{
#'  The permutation indices of $x in the original coordinate.
#'}
#'@examples
#' # Used in Start():
#'  data_path <- system.file('extdata', package = 'startR')
#'  path_obs <- file.path(data_path, 'obs/monthly_mean/$var$/$var$_$sdate$.nc')
#'  sdates <- c('200011', '200012')
#'  data <- Start(dat = list(list(path = path_obs)),
#'                var = 'tos',
#'                sdate = sdates,
#'                time = 'all',
#'                latitude = values(list(-60, 60)),
#'                latitude_reorder = Sort(decreasing = TRUE),
#'                longitude = values(list(-120, 120)),
#'                longitude_reorder = CircularSort(-180, 180),
#'                return_vars = list(latitude = 'dat',
#'                                   longitude = 'dat',
#'                                   time = 'sdate'),
#'                retrieve = FALSE)
#'
#'@rdname Sort
#'@export
Sort <- function(...) {
  params <- list(...)
  f <- "function(x) {
    dim_bk <- dim(x)
    x <- do.call(sort, c(list(x, index.return = TRUE),
                         PARAMS))
    dim(x$x) <- dim_bk
    dim(x$ix) <- dim_bk
    x
  }"
  f <- gsub("PARAMS", deparse(params), f)
  r <- eval(parse(text = f))
  attr(r, 'circular') <- FALSE
  r
}

#'@rdname Sort
#'@export
CircularSort <- function(start, end, ...) {
  params <- list(...)
  f <- "function (x) {
    start <- START
    end <- END
    dim_bk <- dim(x)
    x <- do.call(sort, c(list((x - start) %% (end - start) + start, 
                              index.return = TRUE),
                         PARAMS))
    dim(x$x) <- dim_bk
    dim(x$ix) <- dim_bk
    x
  }"
  f <- gsub("START", deparse(start), f)
  f <- gsub("END", deparse(end), f)
  f <- gsub("PARAMS", deparse(params), f)
  r <- eval(parse(text = f))
  attr(r, 'circular') <- TRUE
  r
}
