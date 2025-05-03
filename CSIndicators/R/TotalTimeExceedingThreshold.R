#'Total Time of a variable Exceeding (not exceeding) a Threshold
#'
#'The Total Time of a variable exceeding (or not) a Threshold. It returns the  
#'total number of days (if the data provided is daily, or the corresponding  
#'units of the data frequency) that a variable is exceeding a threshold 
#'during a period. The threshold provided must be in the same units as the 
#'variable units, i.e. to use a percentile as a scalar, the function 
#'\code{AbsToProbs} or \code{QThreshold}  may be needed (see examples). 
#'Providing maximum temperature daily data, the following agriculture 
#'indices for heat stress can be obtained by using this function:
#'\itemize{
#'  \item{'SU35', Total count of days when daily maximum temperatures exceed 
#'        35°C in the seven months from the start month given (e.g. from April 
#'        to October for start month of April).}
#'  \item{'SU36', Total count of days when daily maximum temperatures exceed 
#'        36 between June 21st and September 21st.}
#'  \item{'SU40', Total count of days when daily maximum temperatures exceed 
#'        40 between June 21st and September 21st.}
#'  \item{'Spr32', Total count of days when daily maximum temperatures exceed
#'        32 between April 21st and June 21st.}
#'}
#'
#'@param data An 's2dv_cube' object as provided function \code{CST_Start} or 
#'  \code{CST_Load} in package CSTools.
#'@param threshold If only one threshold is used, it can be an 's2dv_cube' 
#'  object or a multidimensional array with named dimensions. It must be in the 
#'  same units and with the common dimensions of the same length as parameter 
#'  'data'. It can also be a vector with the same length of 'time_dim' from 
#'  'data' or a scalar. If we want to use two thresholds: it can be a vector 
#'  of two scalars, a list of two vectors with the same length of 
#'  'time_dim' from 'data' or a list of two multidimensional arrays with the 
#'  common dimensions of the same length as parameter 'data'. If two thresholds
#'  are used, parameter 'op' must be also a vector of two elements.
#'@param op An operator '>' (by default), '<', '>=' or '<='. If  two thresholds
#'  are used it has to be a vector of a pair of two logical operators: 
#'  c('<', '>'), c('<', '>='), c('<=', '>'), c('<=', '>='), c('>', '<'), 
#'  c('>', '<='), c('>=', '<'),c('>=', '<=')).
#'@param start An optional parameter to define the initial date of the period 
#'  to select from the data by providing a list of two elements: the initial 
#'  date of the period and the initial month of the period. By default it is set
#'  to NULL and the indicator is computed using all the data provided in 
#'  \code{data}.
#'@param end An optional parameter to define the final date of the period to 
#'  select from the data by providing a list of two elements: the final day of 
#'  the period and the final month of the period. By default it is set to NULL 
#'  and the indicator is computed using all the data provided in \code{data}.
#'@param time_dim A character string indicating the name of the dimension to 
#'  compute the indicator. By default, it is set to 'time'. It can only
#'  indicate one time dimension.
#'@param na.rm A logical value indicating whether to ignore NA values (TRUE) or 
#'  not (FALSE). 
#'@param ncores An integer indicating the number of cores to use in parallel 
#'  computation.
#'
#'@return An 's2dv_cube' object containing in element \code{data} the total 
#'number of the corresponding units of the data frequency that a variable is 
#'exceeding a threshold during a period with dimensions of the input parameter 
#''data' except the dimension where the indicator has been computed. The 
#''Dates' array is updated to the dates corresponding to the beginning of the 
#'aggregated time period. A new element called 'time_bounds' will be added into 
#'the 'attrs' element in the 's2dv_cube' object. It consists of a list 
#'containing two elements, the start and end dates of the aggregated period with 
#'the same dimensions of 'Dates' element.
#' 
#'@examples
#'exp <- NULL
#'exp$data <- array(rnorm(5 * 3 * 214 * 2)*23,
#'                  c(member = 5, sdate = 3, time = 214, lon = 2)) 
#'exp$attrs$Dates <- c(seq(as.Date("01-05-2000", format = "%d-%m-%Y"), 
#'                         as.Date("30-11-2000", format = "%d-%m-%Y"), by = 'day'),
#'                     seq(as.Date("01-05-2001", format = "%d-%m-%Y"), 
#'                         as.Date("30-11-2001", format = "%d-%m-%Y"), by = 'day'),
#'                     seq(as.Date("01-05-2002", format = "%d-%m-%Y"), 
#'                         as.Date("30-11-2002", format = "%d-%m-%Y"), by = 'day'))
#'dim(exp$attrs$Dates) <- c(sdate = 3, time = 214)
#'class(exp) <- 's2dv_cube'
#'DOT <- CST_TotalTimeExceedingThreshold(exp, threshold = 23, start = list(21, 4), 
#'                                       end = list(21, 6))
#'
#'@import multiApply
#'@importFrom ClimProjDiags Subset
#'@export
CST_TotalTimeExceedingThreshold <- function(data, threshold, op = '>',
                                            start = NULL, end = NULL,
                                            time_dim = 'time',
                                            na.rm = FALSE, ncores = NULL) {
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

  if (length(op) == 1) {
    if (inherits(threshold, 's2dv_cube')) {
        threshold <- threshold$data
    }
  } else if (length(op) == 2) {
    if (inherits(threshold[[1]], 's2dv_cube')) {
      threshold[[1]] <- threshold[[1]]$data
    }
    if (inherits(threshold[[2]], 's2dv_cube')) {
      threshold[[2]] <- threshold[[2]]$data
    }
  }
  
  Dates <- data$attrs$Dates
  total <- TotalTimeExceedingThreshold(data = data$data, dates = Dates,
                                       threshold = threshold, op = op,
                                       start = start, end = end, 
                                       time_dim = time_dim, na.rm = na.rm, 
                                       ncores = ncores)
  data$data <- total
  data$dims <- dim(total)
  data$coords[[time_dim]] <- 1 : length(data$dims[[time_dim]])

  if (!is.null(Dates)) {
    if (!is.null(start) && !is.null(end)) {
      Dates <- SelectPeriodOnDates(dates = Dates, start = start, end = end,
                                   time_dim = time_dim, ncores = ncores)
    }
    if (is.null(dim(Dates))) {
      warning("Element 'Dates' has NULL dimensions. They will not be ", 
              "subset and 'time_bounds' will be missed.")
      data$attrs$Dates <- Dates
    } else {
      # Create time_bounds
      time_bounds <- NULL
      time_bounds$start <- ClimProjDiags::Subset(x = Dates, along = time_dim, 
                                                 indices = 1, drop = FALSE)
      time_bounds$end <- ClimProjDiags::Subset(x = Dates, along = time_dim, 
                                               indices = dim(Dates)[time_dim], 
                                               drop = FALSE)

      # Add Dates in attrs
      data$attrs$Dates <- time_bounds$start
      data$attrs$time_bounds <- time_bounds
    }
  }
  return(data)
}
#'Total Time of a variable Exceeding (not exceeding) a Threshold
#'
#'The Total Time of a variable exceeding (or not) a Threshold. It returns the  
#'total number of days (if the data provided is daily, or the corresponding  
#'units of the data frequency) that a variable is exceeding a threshold 
#'during a period. The threshold provided must be in the same units as the 
#'variable units, i.e. to use a percentile as a scalar, the function 
#'\code{AbsToProbs} or \code{QThreshold}  may be needed (see examples). 
#'Providing maximum temperature daily data, the following agriculture 
#'indices for heat stress can be obtained by using this function:
#'\itemize{
#'  \item{'SU35', Total count of days when daily maximum temperatures exceed 
#'        35°C in the seven months from the start month given (e.g. from April 
#'        to October for start month of April).}
#'  \item{'SU36', Total count of days when daily maximum temperatures exceed 
#'        36 between June 21st and September 21st.}
#'  \item{'SU40', Total count of days when daily maximum temperatures exceed 
#'        40 between June 21st and September 21st.}
#'  \item{'Spr32', Total count of days when daily maximum temperatures exceed
#'        32 between April 21st and June 21st.}
#'}
#'
#'@param data A multidimensional array with named dimensions.
#'@param threshold If only one threshold is used: it can be a multidimensional 
#'  array with named dimensions. It must be in the same units and with the 
#'  common dimensions of the same length as parameter 'data'. It can also be a
#'  vector with the same length of 'time_dim' from 'data' or a scalar. If we 
#'  want to use two thresholds: it can be a vector of two scalars, a list of 
#'  two vectors with the same length of 'time_dim' from 'data' or a list of 
#'  two multidimensional arrays with the common dimensions of the same length 
#'  as parameter 'data'. If two thresholds are used, parameter 'op' must be 
#'  also a vector of two elements.
#'@param op An operator '>' (by default), '<', '>=' or '<='. If  two thresholds
#'  are used it has to be a vector of a pair of two logical operators: 
#'  c('<', '>'), c('<', '>='), c('<=', '>'), c('<=', '>='), c('>', '<'), 
#'  c('>', '<='), c('>=', '<'),c('>=', '<=')).
#'@param dates A multidimensional array of dates with named dimensions matching 
#'  the temporal dimensions on parameter 'data'. By default it is NULL, to  
#'  select aperiod this parameter must be provided.
#'@param start An optional parameter to define the initial date of the period 
#'  to select from the data by providing a list of two elements: the initial 
#'  date of the period and the initial month of the period. By default it is set
#'  to NULL and the indicator is computed using all the data provided in 
#'  \code{data}.
#'@param end An optional parameter to define the final date of the period to 
#'  select from the data by providing a list of two elements: the final day of 
#'  the period and the final month of the period. By default it is set to NULL 
#'  and the indicator is computed using all the data provided in \code{data}.
#'@param time_dim A character string indicating the name of the dimension to 
#'  compute the indicator. By default, it is set to 'time'. It can only
#'  indicate one time dimension.
#'@param na.rm A logical value indicating whether to ignore NA values (TRUE) or 
#'  not (FALSE). 
#'@param ncores An integer indicating the number of cores to use in parallel 
#'  computation.
#'
#'@return A multidimensional array with named dimensions containing the total 
#'number of the corresponding units of the data frequency that a variable is 
#'exceeding a threshold during a period with dimensions of the input parameter 
#''data' except the dimension where the indicator has been computed.
#'
#'@examples
#'data <- array(rnorm(5 * 3 * 214 * 2)*23,
#'              c(member = 5, sdate = 3, time = 214, lon = 2)) 
#'Dates <- c(seq(as.Date("01-05-2000", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2000", format = "%d-%m-%Y"), by = 'day'),
#'           seq(as.Date("01-05-2001", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2001", format = "%d-%m-%Y"), by = 'day'),
#'           seq(as.Date("01-05-2002", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2002", format = "%d-%m-%Y"), by = 'day'))
#'dim(Dates) <- c(sdate = 3, time = 214)
#'DOT <- TotalTimeExceedingThreshold(data, threshold = 23, dates = Dates,
#'                                   start = list(21, 4), end = list(21, 6))
#'
#'@import multiApply
#'@importFrom stats setNames
#'@export
TotalTimeExceedingThreshold <- function(data, threshold, op = '>',
                                        dates = NULL, start = NULL, end = NULL,
                                        time_dim = 'time', na.rm = FALSE,
                                        ncores = NULL) {
  # data
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (!is.numeric(data)) {
    stop("Parameter 'data' must be numeric.")
  }
  if (!is.array(data)) {
    dim(data) <- length(data)
    names(dim(data)) <- time_dim
  }
  if (is.null(names(dim(data)))) {
    stop("Parameter 'data' must have named dimensions.")
  }
  # time_dim 
  if (!is.character(time_dim)) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!all(time_dim %in% names(dim(data)))) {
    stop("Parameter 'time_dim' is not found in 'data' dimension.")
  }
  if (length(time_dim) > 1) {
    warning("Parameter 'time_dim' has length greater than 1 and ",
            "only the first element will be used.")
    time_dim <- time_dim[1]
  }
  # op
  if (!is.character(op)) {
    stop("Parameter 'op' must be a character.")
  }
  if (length(op) == 1) {
    if (!(op %in% c('>', '<', '>=', '<=', '='))) {
      stop("Parameter 'op' must be a logical operator.")
    }
  } else if (length(op) == 2) {
    op_list <- list(c('<', '>'), c('<', '>='), c('<=', '>'), c('<=', '>='), 
                    c('>', '<'), c('>', '<='), c('>=', '<'), c('>=', '<='))
    if (!any(unlist(lapply(op_list, function(x) all(x == op))))) {
      stop("Parameter 'op' is not an accepted pair of logical operators.")
    }
  } else {
    stop("Parameter 'op' must be a logical operator with length 1 or 2.")
  }
  # threshold
  if (is.null(unlist(threshold))) {
    stop("Parameter 'threshold' cannot be NULL.")
  }
  if (!is.numeric(unlist(threshold))) {
    stop("Parameter 'threshold' must be numeric.")
  }
  if (length(op) == 2) {
    if (length(op) != length(threshold)) {
      stop(paste0("If 'op' is a pair of logical operators parameter 'threshold' ",
                  "also has to be a pair of values."))
    }
    if (!is.numeric(threshold[[1]]) | !is.numeric(threshold[[2]])) {
      stop("Parameter 'threshold' must be numeric.")
    }
    if (length(threshold[[1]]) != length(threshold[[2]])) {
      stop("The pair of thresholds must have the same length.")
    }
    if (!is.array(threshold[[1]]) && length(threshold[[1]]) > 1) { 
      if (dim(data)[time_dim] != length(threshold[[1]])) {
        stop("If parameter 'threshold' is a vector it must have the same length as data any time dimension.")
      } else {
        dim(threshold[[1]]) <- length(threshold[[1]])
        dim(threshold[[2]]) <- length(threshold[[2]])
        names(dim(threshold[[1]])) <- time_dim
        names(dim(threshold[[2]])) <- time_dim
      }
    } else if (is.array(threshold[[1]]) && length(threshold[[1]]) > 1) {
      if (is.null(names(dim(threshold[[1]])))) {
        stop("If parameter 'threshold' is an array it must have named dimensions.")
      }
      if (!is.null(dim(threshold[[2]]))) {
        if (!all(names(dim(threshold[[1]])) %in% names(dim(threshold[[2]])))) {
          stop("The pair of thresholds must have the same dimension names.")
        }
      }
      namedims <- names(dim(threshold[[1]]))
      order <- match(namedims, names(dim(threshold[[2]])))
      threshold[[2]] <- aperm(threshold[[2]], order)
      if (!all(dim(threshold[[1]]) == dim(threshold[[2]]))) {
        stop("The pair of thresholds must have the same dimensions.")
      }
      if (any(names(dim(threshold[[1]])) %in% names(dim(data)))) {
        common_dims <- dim(threshold[[1]])[names(dim(threshold[[1]])) %in% names(dim(data))]
        if (!all(common_dims == dim(data)[names(common_dims)])) {
          stop(paste0("Parameter 'data' and 'threshold' must have same length of ",
                      "all common dimensions."))
        }
      }
    } else if (length(threshold[[1]]) == 1) {
      dim(threshold[[1]]) <- NULL
      dim(threshold[[2]]) <- NULL
    }
  } else {
    if (!is.array(threshold) && length(threshold) > 1) {
      if (dim(data)[time_dim] != length(threshold)) {
        stop("If parameter 'threshold' is a vector it must have the same length as data time dimension.")
      } else {
        dim(threshold) <- length(threshold)
        names(dim(threshold)) <- time_dim
      }
    } else if (is.array(threshold) && length(threshold) > 1) {
      if (is.null(names(dim(threshold)))) {
          stop("If parameter 'threshold' is an array it must have named dimensions.")
      }
      if (any(names(dim(threshold)) %in% names(dim(data)))) {
        common_dims <- dim(threshold)[names(dim(threshold)) %in% names(dim(data))]
        if (!all(common_dims == dim(data)[names(common_dims)])) {
          stop(paste0("Parameter 'data' and 'threshold' must have same length of ",
                      "all common dimensions."))
        }
      }
    } else if (length(threshold) == 1) {
      dim(threshold) <- NULL
    }
  }
  # ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  }
  # dates
  if (!is.null(start) && !is.null(end)) {
    if (is.null(dates)) {
      warning("Parameter 'dates' is NULL and the average of the ",
              "full data provided in 'data' is computed.")
    } else {
      if (!any(c(is.list(start), is.list(end)))) {
        stop("Parameter 'start' and 'end' must be lists indicating the ",
             "day and the month of the period start and end.")
      }
      if (length(op) == 1) {
        if (time_dim %in% names(dim(threshold))) {
          if (dim(threshold)[time_dim] == dim(data)[time_dim]) {
            threshold <- SelectPeriodOnData(threshold, dates, start, end,
                                            time_dim = time_dim, ncores = ncores)
          }
        }
      } else if (length(op) == 2) {
        if (time_dim %in% names(dim(threshold[[1]]))) {
          if (dim(threshold[[1]])[time_dim] == dim(data)[time_dim]) {
            threshold[[1]] <- SelectPeriodOnData(threshold[[1]], dates, start, end,
                                                 time_dim = time_dim, ncores = ncores)
            threshold[[2]] <- SelectPeriodOnData(threshold[[2]], dates, start, end,
                                                 time_dim = time_dim, ncores = ncores)
          }
        }
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

  if (length(op) > 1) {
    thres1 <- threshold[[1]]
    thres2 <- threshold[[2]]
    if (is.null(dim(thres1))) {
      total <- Apply(list(data), target_dims = time_dim,
                    fun = .exceedthreshold, y = thres1, y2 = thres2,
                    op = op, na.rm = na.rm,
                    ncores = ncores)$output1
    } else if (any(time_dim %in% names(dim(thres1)))) {
      total <- Apply(list(data, thres1, thres2),
                     target_dims = list(time_dim, 
                                        time_dim[time_dim %in% names(dim(thres1))],
                                        time_dim[time_dim %in% names(dim(thres2))]),
                     fun = .exceedthreshold, op = op, na.rm = na.rm,
                     ncores = ncores)$output1

    } else {
      total <- Apply(list(data, thres1, thres2), 
                     target_dims = list(time_dim, thres1 = NULL, thres2 = NULL), 
                     fun = .exceedthreshold, op = op, na.rm = na.rm,
                     ncores = ncores)$output1
    }
  } else {
    if (is.null(dim(threshold))) {
      total <- Apply(list(data), target_dims = time_dim,
                     fun = .exceedthreshold,
                     y = threshold, op = op, na.rm = na.rm,
                     ncores = ncores)$output1
    } else if (any(time_dim %in% names(dim(threshold)))) {
      total <- Apply(list(data, threshold),
                     target_dims = list(time_dim, 
                                        time_dim[time_dim %in% names(dim(threshold))]),
                     fun = .exceedthreshold, op = op, na.rm = na.rm,
                     ncores = ncores)$output1   
    } else {
      total <- Apply(list(data, threshold),
                     target_dims = list(time_dim, NULL),
                     fun = .exceedthreshold, op = op, na.rm = na.rm,
                     ncores = ncores)$output1
    }
  }
  dim(total) <- c(dim(total), setNames(1, time_dim))
  return(total)
}

.exceedthreshold <- function(x, y, y2 = NULL, op = '>', na.rm) {
  y <- as.vector(y)
  y2 <- as.vector(y2)
  if (is.null(y2)) {
    if (op == '>') {
      res <- sum(x > y, na.rm = na.rm)
    } else if (op == '<') {
      res <- sum(x < y, na.rm = na.rm)
    } else if (op == '<=') {
      res <- sum(x <= y, na.rm = na.rm)
    } else {
      res <- sum(x >= y, na.rm = na.rm)
    }
  } else {
    if (all(op == c('<', '>'))) {
      res <- sum(x < y & x > y2, na.rm = na.rm)
    } else if (all(op == c('<', '>='))) {
      res <- sum(x < y & x >= y2, na.rm = na.rm)
    } else if (all(op == c('<=', '>'))) {
      res <- sum(x <= y & x > y2, na.rm = na.rm)
    } else if (all(op == c('<=', '>='))) {
      res <- sum(x <= y & x >= y2, na.rm = na.rm)
    } else if (all(op == c('>', '<'))) {
      res <- sum(x > y & x < y2, na.rm = na.rm)
    } else if (all(op == c('>', '<='))) {
      res <- sum(x > y & x <= y2, na.rm = na.rm)
    } else if (all(op ==  c('>=', '<'))) {
      res <- sum(x >= y & x < y2, na.rm = na.rm)
    } else if (all(op == c('>=', '<='))) {
      res <- sum(x >= y & x <= y2, na.rm = na.rm)
    }
  }
  return(res)
}
