#'Absolute value of a relative threshold (percentile)
#'
#'Frequently, thresholds are defined by a percentile that may correspond to a 
#'different absolute value depending on the variable, gridpoint and also julian 
#'day (time). This function calculates the corresponding value of a percentile 
#'given a dataset.
#'
#'@param data An 's2dv_cube' object as provided function \code{CST_Start} or 
#'  \code{CST_Load} in package CSTools.
#'@param threshold A single scalar or vector indicating the relative 
#'  threshold(s). It must contain values between 0 and 1.
#'@param start An optional parameter to defined the initial date of the period 
#'  to selectfrom the data by providing a list of two elements: the initial date
#'  of the period and the initial month of the period. By default it is set to 
#'  NULL and the indicator is computed using all the data provided in 
#'  \code{data}.
#'@param end An optional parameter to defined the final date of the period to 
#'  select from the data by providing a list of two elements: the final day of 
#'  the period and the final month of the period. By default it is set to NULL 
#'  and the indicator is computed using all the data provided in \code{data}.
#'@param time_dim A character string indicating the name of the temporal 
#'  dimension. By default, it is set to 'time'. More than one dimension name 
#'  matching the dimensions provided in the object \code{data$data} can be 
#'  specified. This dimension is required to subset the data in a requested 
#'  period.
#'@param memb_dim A character string indicating the name of the dimension in 
#'  which the ensemble members are stored. When set it to NULL, threshold is 
#'  computed for individual members.
#'@param sdate_dim A character string indicating the name of the dimension in 
#'  which the initialization dates are stored.
#'@param na.rm A logical value indicating whether to ignore NA values (TRUE) or 
#'  not (FALSE).
#'@param ncores An integer indicating the number of cores to use in parallel 
#'  computation.
#'
#'@return An ’s2dv_cube’ object containing the corresponding values of a
#'percentile in the element \code{data}.
#'
#'@examples
#'threshold <- 0.9
#'exp <- NULL
#'exp$data <- array(rnorm(5 * 3 * 214 * 2),
#'                  c(member = 5, sdate = 3, time = 214, lon = 2)) 
#'exp$attrs$Dates <- c(seq(as.Date("01-05-2000", format = "%d-%m-%Y"), 
#'                         as.Date("30-11-2000", format = "%d-%m-%Y"), by = 'day'),
#'                     seq(as.Date("01-05-2001", format = "%d-%m-%Y"), 
#'                         as.Date("30-11-2001", format = "%d-%m-%Y"), by = 'day'),
#'                     seq(as.Date("01-05-2002", format = "%d-%m-%Y"), 
#'                         as.Date("30-11-2002", format = "%d-%m-%Y"), by = 'day'))
#'dim(exp$attrs$Dates) <- c(sdate = 3, time = 214)
#'class(exp) <- 's2dv_cube'
#'exp_probs <- CST_Threshold(exp, threshold, start = list(21, 4), end = list(21, 6))
#' 
#'@import multiApply
#'@export
CST_Threshold <- function(data, threshold, start = NULL, end = NULL,
                          time_dim = 'time', memb_dim = 'member', 
                          sdate_dim = 'sdate', na.rm = FALSE, 
                          ncores = NULL) {
  # Check 's2dv_cube'
  if (!inherits(data, 's2dv_cube')) {
    stop("Parameter 'data' must be of the class 's2dv_cube'.")
  }
  # Dates subset
  if (!is.null(start) && !is.null(end)) {
    if (is.null(dim(data$attrs$Dates))) {
      warning("Dimensions in 'data' element 'attrs$Dates' are missed and ",
              "all data would be used.")
      start <- NULL
      end <- NULL
    }
  }
  
  thres <- Threshold(data$data, threshold, dates = data$attrs$Dates, start, end,
                     time_dim = time_dim, memb_dim = memb_dim,
                     sdate_dim = sdate_dim, na.rm = na.rm, ncores = ncores)
  data$data <- thres
  data$dims <- dim(thres)
  data$coords[[memb_dim]] <- NULL
  data$coords[[sdate_dim]] <- NULL

  if (!is.null(start) && !is.null(end)) {
    data$attrs$Dates <- SelectPeriodOnDates(dates = data$attrs$Dates,
                                            start = start, end = end, 
                                            time_dim = time_dim, 
                                            ncores = ncores)
  }
  return(data)
}
#'Absolute value of a relative threshold (percentile)
#'
#'Frequently, thresholds are defined by a percentile that may correspond to a 
#'different absolute value depending on the variable, gridpoint and also julian 
#'day (time). This function calculates the corresponding value of a percentile 
#'given a dataset.
#'
#'@param data A multidimensional array with named dimensions.
#'@param threshold A single scalar or vector indicating the relative 
#'  threshold(s). It must contain values between 0 and 1.
#'@param dates A multidimensional array of dates with named dimensions matching 
#'  the temporal dimensions on parameter 'data'. By default it is NULL, to  
#'  select aperiod this parameter must be provided.
#'@param start An optional parameter to defined the initial date of the period 
#'  to select from the data by providing a list of two elements: the initial 
#'  date of the period and the initial month of the period. By default it is set
#'  to NULL and the indicator is computed using all the data provided in 
#'  \code{data}.
#'@param end An optional parameter to defined the final date of the period to 
#'  select from the data by providing a list of two elements: the final day of 
#'  the period and the final month of the period. By default it is set to NULL 
#'  and the indicator is computed using all the data provided in \code{data}.
#'@param time_dim A character string indicating the name of the temporal 
#'  dimension. By default, it is set to 'time'. More than one dimension name 
#'  matching the dimensions provided in the object \code{data$data} can be 
#'  specified. This dimension is required to subset the data in a requested 
#'  period.
#'@param memb_dim A character string indicating the name of the dimension in 
#'  which the ensemble members are stored. When set it to NULL, threshold is 
#'  computed for individual members.
#'@param sdate_dim A character string indicating the name of the dimension in 
#'  which the initialization dates are stored.
#'@param na.rm A logical value indicating whether to ignore NA values (TRUE) or 
#'  not (FALSE).
#'@param ncores An integer indicating the number of cores to use in parallel 
#'  computation.
#'
#'@return A multidimensional array with named dimensions containing the
#'corresponding values of a percentile in the element \code{data}.
#'
#'@examples
#'threshold <- 0.9
#'data <- array(rnorm(25 * 3 * 214 * 2, mean = 26), 
#'              c(member = 25, sdate = 3, time = 214, lon = 2)) 
#'thres_q <- Threshold(data, threshold)
#'data <- array(rnorm(1 * 3 * 214 * 2), c(member = 1, sdate = 3, time = 214, lon = 2))
#'res <- Threshold(data, threshold)
#' 
#'@import multiApply
#'@importFrom stats quantile
#'@export
Threshold <- function(data, threshold, dates = NULL, start = NULL, end = NULL,
                      time_dim = 'time', memb_dim = 'member', sdate_dim = 'sdate',
                      na.rm = FALSE, ncores = NULL) {
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (!is.numeric(data)) {
    stop("Parameter 'data' must be numeric.")
  }
  if (!is.array(data)) {
    dim(data) <- c(length(data), 1)
    names(dim(data)) <- c(memb_dim, sdate_dim)
  }
  if (is.null(threshold)) {
    stop("Parameter 'threshold' cannot be NULL.")
  }
   if (!is.numeric(threshold)) {
    stop("Parameter 'threshold' must be numeric.")
  }
  if (is.null(names(dim(data)))) {
    stop("Parameter 'data' must have named dimensions.")
  }

  if (!is.null(start) && !is.null(end)) {
    if (is.null(dates)) {
      warning("Parameter 'dates' is NULL and the average of the ",
              "full data provided in 'data' is computed.")
    } else {
      if (!any(c(is.list(start), is.list(end)))) {
        stop("Parameter 'start' and 'end' must be lists indicating the ",
             "day and the month of the period start and end.")
      }
      if (!is.null(dim(dates))) {
        data <- SelectPeriodOnData(data = data, dates = dates, start = start, 
                                   end = end, time_dim = time_dim, 
                                   ncores = ncores)
      } else {
        warning("Parameter 'dates' must have named dimensions if 'start' and ",
                "'end' are not NULL. All data will be used.")
      }
    }
  }   
  if (!is.null(memb_dim)) {
    dimensions <- c(sdate_dim, memb_dim)
  } else {
    dimensions <- sdate_dim
  }
  if (length(threshold) == 1) {
    thres <- Apply(data, target_dims = dimensions,
                   fun = function(x) {quantile(as.vector(x), threshold, na.rm)})$output1
  } else {
    thres <- Apply(data, target_dims = dimensions,
                   fun = function(x) {quantile(as.vector(x), threshold, na.rm)},
                   output_dims = 'probs')$output1
  }
  return(thres)
}
