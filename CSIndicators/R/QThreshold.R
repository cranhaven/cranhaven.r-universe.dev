#'Transform an absolute threshold into probabilities
#'
#'From the user's perspective, an absolute threshold can be very useful for a 
#'specific needs (e.g.: grape variety). However, this absolute threshold could 
#'be transformed to a relative threshold in order to get its frequency in a given 
#'dataset. Therefore, the function \code{QThreshold} returns the probability of 
#'an absolute threshold. This is done by computing the Cumulative Distribution 
#'Function of a sample and leaving one out. The sample used will depend on the 
#'dimensions of the data provided and the dimension names provided in sdate_dim 
#'and memb_dim parameters:
#' 
#'\itemize{
#'  \item{If a forecast (hindcast) has dimensions member and start date, and
#'        both must be used in the sample, their names should be passed in 
#'        sdate_dim and memb_dim.}
#'  \item{If a forecast (hindcast) has dimensions member and start date, and
#'        only start date must be used in the sample (the calculation is done in
#'        each separate member), memb_dim can be set to NULL.}
#'  \item{If a reference (observations) has start date dimension, the sample
#'        used is the start date dimension.}
#'  \item{If a reference (observations) doesn't have start date dimension, 
#'        the sample used must be especified in sdate_dim parameter.}
#'}
#'
#'@param data An 's2dv_cube' object as provided function \code{CST_Start} or 
#'  \code{CST_Load} in package CSTools.
#'@param threshold An 's2dv_cube' object as output of a 'CST_' function in the 
#'  same units as parameter 'data' and with the common dimensions of the element
#'  'data' of the same length. A single scalar is also possible.
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
#'  which the ensemble members are stored.
#'@param sdate_dim A character string indicating the name of the dimension in 
#'  which the initialization dates are stored. 
#'@param ncores An integer indicating the number of cores to use in parallel 
#'  computation.
#'
#'@return An 's2dv_cube' object containing the probability of an absolute 
#'threshold in the element \code{data}.
#'
#'@examples
#'threshold <- 26
#'exp <- NULL
#'exp$data <- array(abs(rnorm(112)*26), dim = c(member = 7, sdate = 8, time = 2))
#'class(exp) <- 's2dv_cube'
#'exp_probs <- CST_QThreshold(exp, threshold)
#'
#'exp$data <- array(abs(rnorm(5 * 3 * 214 * 2)*50),
#'                  c(member = 5, sdate = 3, time = 214, lon = 2)) 
#'exp$attrs$Dates <- c(seq(as.Date("01-05-2000", format = "%d-%m-%Y"), 
#'                         as.Date("30-11-2000", format = "%d-%m-%Y"), by = 'day'),
#'                     seq(as.Date("01-05-2001", format = "%d-%m-%Y"), 
#'                         as.Date("30-11-2001", format = "%d-%m-%Y"), by = 'day'),
#'                     seq(as.Date("01-05-2002", format = "%d-%m-%Y"), 
#'                         as.Date("30-11-2002", format = "%d-%m-%Y"), by = 'day'))
#'dim(exp$attrs$Dates) <- c(sdate = 3, time = 214)
#'class(exp) <- 's2dv_cube'
#'exp_probs <- CST_QThreshold(exp, threshold, start = list(21, 4), 
#'                            end = list(21, 6))
#' 
#'@import multiApply
#'@export
CST_QThreshold <- function(data, threshold, start = NULL, end = NULL,
                           time_dim = 'time', memb_dim = 'member', 
                           sdate_dim = 'sdate', ncores = NULL) {
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
  if (inherits(threshold, 's2dv_cube')) {
    threshold <- threshold$data
  }
  probs <- QThreshold(data$data, threshold, dates = data$attrs$Dates, 
                      start, end, time_dim = time_dim, memb_dim = memb_dim,
                      sdate_dim = sdate_dim, ncores = ncores)
  data$data <- probs
  data$dims <- dim(probs)
  
  if (!is.null(start) && !is.null(end)) {
    data$attrs$Dates <- SelectPeriodOnDates(dates = data$attrs$Dates,
                                            start = start, end = end, 
                                            time_dim = time_dim, 
                                            ncores = ncores)
  }
  return(data)
}
#'Transform an absolute threshold into probabilities
#'
#'From the user's perspective, an absolute threshold can be very useful for a 
#'specific needs (e.g.: grape variety). However, this absolute threshold could 
#'be transformed to a relative threshold in order to get its frequency in a given 
#'dataset. Therefore, the function \code{QThreshold} returns the probability of 
#'an absolute threshold. This is done by computing the Cumulative Distribution 
#'Function of a sample and leaving-one-ot. The sample used will depend on the 
#'dimensions of the data provided and the dimension names provided in sdate_dim 
#'and memb_dim parameters:
#'\itemize{
#'  \item{If a forecast (hindcast) has dimensions member and start date, and
#'        both must be used in the sample, their names should be passed in 
#'        sdate_dim and memb_dim.}
#'  \item{If a forecast (hindcast) has dimensions member and start date, and
#'        only start date must be used in the sample (the calculation is done in
#'        each separate member), memb_dim can be set to NULL.}
#'  \item{If a reference (observations) has start date dimension, the sample
#'        used is the start date dimension.}
#'  \item{If a reference (observations) doesn't have start date dimension, 
#'        the sample used must be especified in sdate_dim parameter.}
#'}
#'
#'@param data A multidimensional array with named dimensions.
#'@param threshold A multidimensional array with named dimensions in the same 
#'  units as parameter 'data' and with the common dimensions of the element 
#'  'data' of the same length.
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
#'  which the ensemble members are stored.
#'@param sdate_dim A character string indicating the name of the dimension in 
#'  which the initialization dates are stored. 
#'@param ncores An integer indicating the number of cores to use in parallel 
#'  computation.
#'
#'@return A multidimensional array with named dimensions containing the 
#'probability of an absolute threshold in the element \code{data}.
#'
#'@examples
#'threshold = 25
#'data <- array(rnorm(5 * 3 * 20 * 2, mean = 26), 
#'              c(member = 5, sdate = 3, time = 214, lon = 2)) 
#'
#'Dates <- c(seq(as.Date("01-05-2000", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2000", format = "%d-%m-%Y"), by = 'day'),
#'           seq(as.Date("01-05-2001", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2001", format = "%d-%m-%Y"), by = 'day'),
#'           seq(as.Date("01-05-2002", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2002", format = "%d-%m-%Y"), by = 'day'))
#'dim(Dates) <- c(sdate = 3, time = 214)
#'
#'thres_q <- QThreshold(data, threshold, dates = Dates, time_dim = 'time',
#'                      start = list(21, 4), end = list(21, 6))
#' 
#'@import multiApply
#'@importFrom ClimProjDiags Subset
#'@export
QThreshold <- function(data, threshold, dates = NULL, start = NULL, end = NULL,
                       time_dim = 'time', memb_dim = 'member', 
                       sdate_dim = 'sdate', ncores = NULL) {
  # Initial checks
  ## data
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
  if (is.null(names(dim(data)))) {
    stop("Parameter 'data' must have named dimensions.")
  }
  ## threshold
  if (is.null(threshold)) {
      stop("Parameter 'threshold' cannot be NULL.")
  }
   if (!is.numeric(threshold)) {
    stop("Parameter 'threshold' must be numeric.")
  }
  if (!is.array(threshold) && length(threshold) > 1) {
    dim(threshold) <- length(threshold)
    names(dim(threshold)) <- time_dim
  } else if (length(threshold) == 1) {
    dim(threshold) <- NULL
  }
  if (sdate_dim %in% names(dim(threshold))) {
    stop("Parameter threshold cannot have dimension 'sdate_dim'.") 
  }
  if (is.null(names(dim(threshold))) && length(threshold) > 1) {
    stop("Parameter 'threshold' must have named dimensions.")
  }
  if (is.null(memb_dim)) {
    memb_dim <- 99999
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
      if (time_dim %in% names(dim(threshold))) {
        if (dim(threshold)[time_dim] == dim(data)[time_dim]) {
          if (!is.null(dim(dates)) && sdate_dim %in% names(dim(dates))) {
            dates_thres <- Subset(dates, along = sdate_dim, indices = 1)
            threshold <- SelectPeriodOnData(data = threshold, dates = dates_thres, start, end,
                                            time_dim = time_dim, ncores = ncores)
          } else {
            threshold <- SelectPeriodOnData(threshold, dates, start, end,
                                            time_dim = time_dim, ncores = ncores)
          }
        }
      }
      if (!is.null(dim(dates))) {
        data <- SelectPeriodOnData(data = data, dates = dates, start = start, 
                                   end = end, time_dim = time_dim, ncores = ncores)
      } else {
        warning("Parameter 'dates' must have named dimensions if 'start' and ",
                "'end' are not NULL. All data will be used.")
      }
    }
  }
  
  if (length(threshold) == 1) {
    if (memb_dim %in% names(dim(data))) {
      probs <- Apply(list(data), target_dims = c(memb_dim, sdate_dim),
                     fun = .qthreshold_exp,
                     threshold, ncores = ncores)$output1
    } else {
      probs <- Apply(list(data), target_dims = sdate_dim, fun = .qthreshold_obs,
                     threshold, ncores = ncores)$output1
    }
  } else {
    target_thres <- NULL

    if (memb_dim %in% names(dim(data))) {
      if (memb_dim %in% names(dim(threshold))) {
      # comparison member by member
        probs <- Apply(list(data, threshold), 
                       target_dims = list(sdate_dim, NULL), 
                       fun = .qthreshold_obs, ncores = ncores)$output1
      } else {
        probs <- Apply(list(data, threshold),
                       target_dims = list(c(memb_dim, sdate_dim), NULL),
                       fun = .qthreshold_exp, ncores = ncores)$output1
      }
    } else {
      probs <- Apply(list(data, threshold), target_dims = list(sdate_dim, NULL),
                     fun = .qthreshold_obs, ncores = ncores)$output1
    }
  } 
  return(probs)
}
# By splitting the atomic function, a conditional repetition is avoided 
# inside the Apply loops
.qthreshold_obs <- function(data, threshold) {
  # dims data: sdate 
  dims <- dim(data)
  # no 'member' involving 
  qres <- unlist(lapply(1:dims, function(x) { 
                        ecdf(data[-x])(threshold)}))
  dim(qres) <- c(dims)
  return(qres)
}
.qthreshold_exp <- function(data, threshold) {
  qres <- unlist(
  lapply(1:(dim(data)[1]), function(x) { # dim 1: member
            lapply(1:(dim(data)[2]), function(y) { # dim 2: sdate
                   ecdf(as.vector(data[,-y]))(threshold)
                   })
        }))
  dim(qres) <- c(dim(data)[2], dim(data)[1])
  return(qres)
} 
