#' Select a period on Data on 's2dv_cube' objects
#'
#' Auxiliary function to subset data for a specific period.
#'
#'@param data An 's2dv_cube' object as provided function \code{CST_Start} or 
#'  \code{CST_Load} in package CSTools.
#'@param start A parameter to defined the initial date of the period to select 
#'  from the data by providing a list of two elements: the initial date of the 
#'  period and the initial month of the period.
#'@param end A parameter to defined the final date of the period to select from 
#'  the data by providing a list of two elements: the final day of the period 
#'  and the final month of the period.
#'@param time_dim A character string indicating the name of the dimension to 
#'  compute select the dates. By default, it is set to 'time'. More than one 
#'  dimension name matching the dimensions provided in the object 
#'  \code{data$data} can be specified.
#'@param ncores An integer indicating the number of cores to use in parallel 
#'  computation.
#'
#'@return A 's2dv_cube' object containing the subset of the object 
#'\code{data$data} during the period requested from \code{start} to \code{end}.
#'
#'@examples
#'exp <- NULL
#'exp$data <- array(rnorm(5 * 3 * 214 * 2),
#'                  c(memb = 5, sdate = 3, time = 214, lon = 2)) 
#'exp$attrs$Dates <- c(seq(as.Date("01-05-2000", format = "%d-%m-%Y"), 
#'                         as.Date("30-11-2000", format = "%d-%m-%Y"), by = 'day'),
#'                     seq(as.Date("01-05-2001", format = "%d-%m-%Y"), 
#'                         as.Date("30-11-2001", format = "%d-%m-%Y"), by = 'day'),
#'                     seq(as.Date("01-05-2002", format = "%d-%m-%Y"), 
#'                         as.Date("30-11-2002", format = "%d-%m-%Y"), by = 'day'))
#'dim(exp$attrs$Dates) <- c(time = 214, sdate = 3)
#'class(exp) <- 's2dv_cube'
#'Period <- CST_SelectPeriodOnData(exp, start = list(21, 6), end = list(21, 9))
#'@import multiApply
#'@export
CST_SelectPeriodOnData <- function(data, start, end, time_dim = 'time', 
                                   ncores = NULL) {
  # Check 's2dv_cube'
  if (!inherits(data, 's2dv_cube')) {
    stop("Parameter 'data' must be of the class 's2dv_cube'.")
  }
  # Dates subset
  if (!is.null(start) && !is.null(end)) {
    if (is.null(dim(data$attrs$Dates))) {
      warning("Dimensions in 'data' element 'Dates$start' are missed and ",
              "all data would be used.")
    }
  }

  res <- SelectPeriodOnData(data$data, data$attrs$Dates,
                            start = start, end = end,
                            time_dim = time_dim, ncores = ncores)
  data$data <- res
  if (!is.null(start) && !is.null(end)) {
     data$attrs$Dates <- SelectPeriodOnDates(dates = data$attrs$Dates,
                                             start = start, end = end,
                                             time_dim = time_dim, 
                                             ncores = ncores)
  }
  return(data)
}

#' Select a period on Data on multidimensional array objects
#'
#' Auxiliary function to subset data for a specific period.
#'
#'@param data A multidimensional array with named dimensions with at least the  
#'  time dimension specified in parameter 'time_dim'. All common dimensions
#'  with 'dates' parameter need to have the same length.
#'@param dates An array of dates with named dimensions with at least the time 
#'  dimension specified in parameter 'time_dim'. All common dimensions with
#'  'data' parameter need to have the same length.
#'@param start A list with two elements to define the initial date of the period 
#'  to select from the data. The first element is the initial day of the period 
#'  and the second element is the initial month of the period.
#'@param end A list with two elements to define the final date of the period 
#'  to select from the data. The first element is the final day of the period 
#'  and the second element is the final month of the period.
#'@param time_dim A character string indicating the name of the dimension to 
#'  compute select the dates. By default, it is set to 'time'. Parameters 
#'  'data' and 'dates'
#'@param ncores An integer indicating the number of cores to use in parallel 
#'  computation.
#'
#'@return A multidimensional array with named dimensions containing the subset 
#'of the object \code{data} during the period requested from \code{start} to 
#'\code{end}.
#'
#'@examples
#'data <- array(rnorm(5 * 3 * 214 * 2),
#'              c(memb = 5, sdate = 3, time = 214, lon = 2)) 
#'Dates <- c(seq(as.Date("01-05-2000", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2000", format = "%d-%m-%Y"), by = 'day'),
#'           seq(as.Date("01-05-2001", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2001", format = "%d-%m-%Y"), by = 'day'),
#'           seq(as.Date("01-05-2002", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2002", format = "%d-%m-%Y"), by = 'day'))
#'dim(Dates) <- c(time = 214, sdate = 3)
#'Period <- SelectPeriodOnData(data, Dates, start = list(21, 6), end = list(21, 9))
#'@import multiApply
#'@importFrom ClimProjDiags Subset
#'@importFrom s2dv Reorder
#'@export
SelectPeriodOnData <- function(data, dates, start, end, 
                               time_dim = 'time', ncores = NULL) {
  if (is.null(dim(dates))) {
    dim(dates) <- length(dates)
    names(dim(dates)) <- time_dim
  }
  if (is.null(dim(data))) {
    dim(data) <- length(data)
    names(dim(data)) <- time_dim
  }

  res <- Apply(list(dates), target_dims = time_dim,
               fun = .position,
               ini_day = start[[1]], ini_month = start[[2]],
               end_day = end[[1]], end_month = end[[2]],
               ncores = ncores)$output1
  # when 29Feb is included the length of the output changes:
  regular <- Apply(list(res), target_dims = time_dim,
                   fun = function(...) {sum(...)}, ncores = ncores)$output1
  dims <- dim(data)
  dims[names(dims) == time_dim] <- max(regular)
  if (any(regular != max(regular))) {
    res <- Apply(list(data, res), target_dims = time_dim,
                 fun = function(x, y) {
                     if (sum(y) != max(regular)) {
                       result <- c(x[y], NA)
                     } else { 
                       result <- x[y]
                     }
                     dim(result) <- length(result)
                     names(dim(result)) <- names(dim(x))
                     return(result)  
                     }, output_dims = time_dim, ncores = ncores)$output1
  } else {
    res <- Apply(list(data, res), target_dims = time_dim,
                 fun = function(x, y) {
                    res <- x[y]
                    if (is.null(dim(res))) {
                      dim(res) <- 1
                      names(dim(res)) <- time_dim
                    }
                    return(res)
                 }, output_dims = time_dim, ncores = ncores)$output1 
  }
  names_res <- sort(names(dim(res)))
  names_data <- sort(names(dim(data)))
  if (!all(names_res %in% names_data)) {
    dim_remove <- names_res[-which(names_res %in% names_data)]
    indices <- as.list(rep(1, length(dim_remove)))
    res <- Subset(res, along = dim_remove, indices, drop = 'selected')
  }
  if (any(names(dims) != names(dim(res)))) {
    res <- Reorder(res, names(dims))
  }
  return(res)
}
