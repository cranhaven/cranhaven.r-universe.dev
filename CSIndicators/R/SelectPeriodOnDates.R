#' Select a period on Dates
#'
#' Auxiliary function to subset dates for a specific period.
#'
#'@param dates An array of dates with named dimensions.
#'@param start An optional parameter to defined the initial date of the period 
#'  to select from the data by providing a list of two elements: the initial 
#'  date of the period and the initial month of the period.
#'@param end An optional parameter to defined the final date of the period to 
#'  select from the data by providing a list of two elements: the final day of 
#'  the period and the final month of the period.
#'@param time_dim A character string indicating the name of the dimension to 
#'  compute select the dates. By default, it is set to 'time'. More than one 
#'  dimension name matching the dimensions provided in the object 
#'  \code{data$data} can be specified.
#'@param ncores An integer indicating the number of cores to use in parallel 
#'  computation.
#'
#'@return A multidimensional array with named dimensions containing the subset of 
#'the vector dates during the period requested from \code{start} to \code{end}.
#'
#'@examples
#'Dates <- c(seq(as.Date("01-05-2000", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2000", format = "%d-%m-%Y"), by = 'day'),
#'           seq(as.Date("01-05-2001", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2001", format = "%d-%m-%Y"), by = 'day'),
#'           seq(as.Date("01-05-2002", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2002", format = "%d-%m-%Y"), by = 'day'))
#'dim(Dates) <- c(time = 214, sdate = 3)
#'Period <- SelectPeriodOnDates(Dates, start = list(21, 6), end = list(21, 9))
#'@import multiApply
#'@importFrom s2dv Reorder
#'@export
SelectPeriodOnDates <- function(dates, start, end,
                                time_dim = 'time', ncores = NULL) {
  if (is.null(dim(dates))) {
    dim(dates) <- length(dates)
    names(dim(dates)) <- time_dim
  }

  # TODO: consider NAs
  
  res <- Apply(list(dates), target_dims = time_dim,
               fun = .position,
               ini_day = start[[1]], ini_month = start[[2]],
               end_day = end[[1]], end_month = end[[2]],
               ncores = ncores)$output1
  reorder <- FALSE
  if (which(names(dim(dates)) == time_dim) != 1) {
    dimdates <- names(dim(dates))
    dates <- Reorder(dates, c(time_dim, names(dim(dates))[which(names(dim(dates)) != time_dim)]))
    reorder <- TRUE
  }
  # when 29Feb is included the length of the output changes:
  regular <- Apply(list(res), target_dims = time_dim,
                   fun = function(...) {sum(...)}, ncores = ncores)$output1
  dims <- dim(dates)
  dims[names(dims) == time_dim] <- max(regular)
  if (any(regular != max(regular))) {
    res <- Apply(list(dates, res), target_dims = time_dim,
                 fun = function(x, y) {
                     if (sum(y) != max(regular)) {
                       result <- c(x[y], NA)
                     } else { 
                       result <- x[y]
                     }
                     dim(result) <- length(result)
                     names(dim(result)) <- names(dim(x))
                     return(result)  
                     }, ncores = ncores)$output1
    res <- as.POSIXct(res, origin = '1970-01-01', tz = 'UTC')  
  } else {
    res <- dates[res]
    dim(res) <- dims
    if (reorder) {
      res <- Reorder(res, dimdates)
    }
  }
  return(res)
}
