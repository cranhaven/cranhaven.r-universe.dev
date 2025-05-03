#'Transform ensemble forecast into probabilities
#'
#'The Cumulative Distribution Function of a forecast is used to obtain the 
#'probabilities of each value in the ensemble. If multiple initializations 
#'(start dates) are provided, the function will create the Cumulative 
#'Distribution Function excluding the corresponding initialization. 
#'
#'@param data An 's2dv_cube' object as provided function \code{CST_Start} or 
#'  \code{CST_Load} in package CSTools.
#'@param start An optional parameter to define the initial date of the period 
#'  to select from the data by providing a list of two elements: the initial 
#'  date of the period and the initial month of the period. By default it is set
#'  to NULL and the indicator is computed using all the data provided in 
#'  \code{data}.
#'@param end An optional parameter to define the final date of the period to 
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
#'@return An 's2dv_cube' object containing the probabilites in the element \code{data}.
#'
#'@examples
#'exp <- NULL
#'exp$data <- array(rnorm(216), dim = c(dataset = 1, member = 2, sdate = 3, 
#'                  time = 9, lat = 2, lon = 2))
#'class(exp) <- 's2dv_cube'
#'exp_probs <- CST_AbsToProbs(exp)
#'exp$data <- array(rnorm(5 * 3 * 214 * 2),
#'                  c(member = 5, sdate = 3, time = 214, lon = 2)) 
#'exp$attrs$Dates <- c(seq(as.Date("01-05-2000", format = "%d-%m-%Y"), 
#'                         as.Date("30-11-2000", format = "%d-%m-%Y"), by = 'day'),
#'                     seq(as.Date("01-05-2001", format = "%d-%m-%Y"), 
#'                         as.Date("30-11-2001", format = "%d-%m-%Y"), by = 'day'),
#'                     seq(as.Date("01-05-2002", format = "%d-%m-%Y"), 
#'                         as.Date("30-11-2002", format = "%d-%m-%Y"), by = 'day'))
#'dim(exp$attrs$Dates) <- c(time = 214, sdate = 3)
#'exp_probs <- CST_AbsToProbs(data = exp, start = list(21, 4), end = list(21, 6))
#'@import multiApply
#'@importFrom stats ecdf
#'@export
CST_AbsToProbs <- function(data, start = NULL, end = NULL,
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

  probs <- AbsToProbs(data = data$data, dates = data$attrs$Dates, 
                      start = start, end = end, time_dim = time_dim, 
                      memb_dim = memb_dim, sdate_dim = sdate_dim, 
                      ncores = ncores)
  data$data <- probs
  if (!is.null(start) && !is.null(end)) {
    data$attrs$Dates <- SelectPeriodOnDates(dates = data$attrs$Dates,
                                            start = start, end = end, 
                                            time_dim = time_dim, 
                                            ncores = ncores)
  }
  return(data)
}
#'Transform ensemble forecast into probabilities
#'
#'The Cumulative Distribution Function of a forecast is used to obtain the 
#'probabilities of each value in the ensemble. If multiple initializations 
#'(start dates) are provided, the function will create the Cumulative 
#'Distribution Function excluding the corresponding initialization. 
#'
#'@param data A multidimensional array with named dimensions.
#'@param dates An optional parameter containing a vector of dates or a 
#'  multidimensional array of dates with named dimensions matching the
#'  dimensions on parameter 'data'. By default it is NULL, to select a period 
#'  this parameter must be provided. All common dimensions with 'data' need to 
#'  have the same length.
#'@param start An optional parameter to define the initial date of the period 
#'  to select from the data by providing a list of two elements: the initial 
#'  date of the period and the initial month of the period. By default it is set
#'  to NULL and the indicator is computed using all the data provided in 
#'  \code{data}.
#'@param end An optional parameter to define the final date of the period to 
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
#'probabilites in the element \code{data}.
#'
#'@examples
#'exp <- array(rnorm(216), dim = c(dataset = 1, member = 2, sdate = 3, 
#'                                 time = 9, lat = 2, lon = 2))
#'exp_probs <- AbsToProbs(exp)
#'data <- array(rnorm(5 * 3 * 61 * 1),
#'              c(member = 5, sdate = 3, time = 61, lon = 1)) 
#'Dates <- c(seq(as.Date("01-05-2000", format = "%d-%m-%Y"), 
#'               as.Date("30-06-2000", format = "%d-%m-%Y"), by = 'day'),
#'           seq(as.Date("01-05-2001", format = "%d-%m-%Y"), 
#'               as.Date("30-06-2001", format = "%d-%m-%Y"), by = 'day'),
#'           seq(as.Date("01-05-2002", format = "%d-%m-%Y"), 
#'               as.Date("30-06-2002", format = "%d-%m-%Y"), by = 'day'))
#'dim(Dates) <- c(time = 61, sdate = 3)
#'exp_probs <- AbsToProbs(data, dates = Dates, start = list(21, 4), 
#'                        end = list(21, 6))
#'
#'@import multiApply
#'@importFrom stats ecdf
#'@export
AbsToProbs <- function(data, dates = NULL, start = NULL, end = NULL,
                       time_dim = 'time', memb_dim = 'member', 
                       sdate_dim = 'sdate', ncores = NULL) {
  # data
  if (!is.numeric(data)) {
    stop("Parameter 'data' must be numeric.")
  }
  data_is_array <- TRUE
  if (!is.array(data)) {
    data_is_array <- FALSE
    dim(data) <- c(length(data), 1)
    names(dim(data)) <- c(memb_dim, sdate_dim)
    if (!is.null(start) && !is.null(end)) {
      warning("Parameter 'data' doesn't have dimension names and all ", 
              "data will be used.")
      start <- NULL
      end <- NULL
    }
  }
  # dates subset
  if (!is.null(start) && !is.null(end)) {
    if (!all(c(is.list(start), is.list(end)))) {
      stop("Parameter 'start' and 'end' must be lists indicating the ",
           "day and the month of the period start and end.")
    }
    if (is.null(dates)) {
      warning("Parameter 'dates' is not provided and all data will be used.")
    } else {
      if (is.null(dim(dates))) {
        warning("Parameter 'dates' doesn't have dimension names and all ", 
                "data will be used.")
      } else {
        data <- SelectPeriodOnData(data, dates, start, end, 
                                   time_dim = time_dim, ncores = ncores)
      }
    }
  }
  probs <- Apply(list(data), target_dims = c(memb_dim, sdate_dim), 
                 fun = .abstoprobs,
                 ncores = ncores)$output1
  if (!data_is_array) {
    dim(probs) <- NULL
  } else {
    pos <- match(names(dim(data)), names(dim(probs)))
    probs <- aperm(probs, pos)
  }

  return(probs)
}

.abstoprobs <- function(data) {
  if (dim(data)[2] > 1) { # Several sdates 
    qres <- unlist(
      lapply(1:(dim(data)[1]), function(x) { # dim 1: member
              lapply(1:(dim(data)[2]), function(y) { # dim 2: sdate
                       ecdf(as.vector(data[,-y]))(data[x, y])
                       })
          }))
    dim(qres) <- c(dim(data)[2], dim(data)[1])
  } else { # One sdate
    qres <- unlist(
    lapply(1:(dim(data)[1]), function(x) { # dim 1: member
                       ecdf(as.vector(data))(data[x, 1])
                       }))
    dim(qres) <- c(dim(data)[2], dim(data)[1])
  }
  return(qres)
}
