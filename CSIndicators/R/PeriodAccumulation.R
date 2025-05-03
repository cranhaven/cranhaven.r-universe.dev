#'Period Accumulation on 's2dv_cube' objects
#'
#'Period Accumulation computes the sum (accumulation) of a given variable in a 
#'period. Providing precipitation data, two agriculture indices can be obtained 
#'by using this function:
#'\itemize{
#'  \item{'SprR', Spring Total Precipitation: The total precipitation from 
#'        April 21th to June 21st.}
#'  \item{'HarR', Harvest Total Precipitation: The total precipitation from 
#'        August 21st to October 21st.}
#'}
#'
#'There are two possible ways of performing the accumulation. The default one 
#'is by accumulating a variable over a dimension specified with 'time_dim'. To 
#'chose a specific time period, 'start' and 'end' must be used. The other method 
#'is by using 'rollwidth' parameter. When this parameter is a positive integer, 
#'the cumulative backward sum is applied to the time dimension. If it is 
#'negative, the rolling sum is applied backwards. This function is build to  
#'be compatible with other tools in that work with 's2dv_cube' object class. The 
#'input data must be this object class. If you don't work with 's2dv_cube', see 
#'PeriodAccumulation.
#'
#'@param data An 's2dv_cube' object as provided function \code{CST_Start} or 
#'  \code{CST_Load} in package CSTools.
#'@param start An optional parameter to defined the initial date of the period 
#'  to select from the data by providing a list of two elements: the initial 
#'  date of the period and the initial m   onth of the period. By default it is 
#'  set to NULL and the indicator is computed using all the data provided in 
#'  \code{data}.
#'@param end An optional parameter to defined the final date of the period to 
#'  select from the data by providing a list of two elements: the final day of 
#'  the period and the final month of the period. By default it is set to NULL 
#'  and the indicator is computed using all the data provided in \code{data}.
#'@param time_dim A character string indicating the name of the dimension to 
#'  compute the indicator. By default, it is set to 'time'. More than one 
#'  dimension name matching the dimensions provided in the object 
#'  \code{data$data} can be specified.
#'@param rollwidth An optional parameter to indicate the number of time 
#'  steps the rolling sum is applied to. If it is positive, the rolling sum is 
#'  applied backwards 'time_dim', if it is negative, it will be forward it. When 
#'  this parameter is NULL, the sum is applied over all 'time_dim', in a 
#'  specified period. It is NULL by default.
#'@param sdate_dim (Only needed when rollwidth is used). A character string 
#'  indicating the name of the start date dimension to compute the rolling 
#'  accumulation. By default, it is set to 'sdate'.
#'@param frequency (Only needed when rollwidth is used). A character string 
#'  indicating the time frequency of the data to apply the rolling accumulation.  
#'  It can be 'daily' or 'monthly'. If it is set to 'monthly', values from
#'  continuous months will be accumulated; if it is 'daliy', values from 
#'  continuous days will be accumulated. It is set to 'monthly' by default.
#'@param na.rm A logical value indicating whether to ignore NA values (TRUE) or 
#'  not (FALSE).
#'@param ncores An integer indicating the number of cores to use in parallel 
#'  computation.
#'
#'@return An 's2dv_cube' object containing the accumulated data in the element
#'\code{data}. If parameter 'rollwidth' is not used, it will have the dimensions
#'of the input parameter 'data' except the dimension where the accumulation has
#'been computed (specified with 'time_dim'). If 'rollwidth' is used, it will be 
#'of same dimensions as input data. The 'Dates' array is updated to the 
#'dates corresponding to the beginning of the aggregated time period. A new 
#'element called 'time_bounds' will be added into the 'attrs' element in the 
#''s2dv_cube' object. It consists of a list containing two elements, the start 
#'and end dates of the aggregated period with the same dimensions of 'Dates' 
#'element. If 'rollwidth' is used, it will contain the same dimensions of 
#'parameter 'data' and the other elements of the 's2dv_cube' will not be 
#'modified.
#'
#'@examples
#'exp <- NULL
#'exp$data <- array(rnorm(216)*200, dim = c(dataset = 1, member = 2, sdate = 3, 
#'                  ftime = 9, lat = 2, lon = 2))
#'class(exp) <- 's2dv_cube'
#'TP <- CST_PeriodAccumulation(exp, time_dim = 'ftime')
#'exp$data <- array(rnorm(5 * 3 * 214 * 2), 
#'                  c(memb = 5, sdate = 3, ftime = 214, lon = 2)) 
#'Dates <- c(seq(as.Date("01-05-2000", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2000", format = "%d-%m-%Y"), by = 'day'),
#'           seq(as.Date("01-05-2001", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2001", format = "%d-%m-%Y"), by = 'day'),
#'           seq(as.Date("01-05-2002", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2002", format = "%d-%m-%Y"), by = 'day'))
#'dim(Dates) <- c(sdate = 3, ftime = 214)
#'exp$attrs$Dates <- Dates
#'SprR <- CST_PeriodAccumulation(exp, start = list(21, 4), end = list(21, 6),
#'                               time_dim = 'ftime')
#'dim(SprR$data)
#'head(SprR$attrs$Dates)
#'HarR <- CST_PeriodAccumulation(exp, start = list(21, 8), end = list(21, 10),
#'                               time_dim = 'ftime')
#'dim(HarR$data)
#'head(HarR$attrs$Dates)
#'
#'@import multiApply
#'@importFrom ClimProjDiags Subset
#'@export
CST_PeriodAccumulation <- function(data, start = NULL, end = NULL,
                                   time_dim = 'time', rollwidth = NULL, 
                                   sdate_dim = 'sdate', frequency = 'monthly',
                                   na.rm = FALSE, ncores = NULL) {
  # Check 's2dv_cube'
  if (!inherits(data, 's2dv_cube')) {
    stop("Parameter 'data' must be of the class 's2dv_cube'.")
  }
  if (!all(c('data') %in% names(data))) {
    stop("Parameter 'data' doesn't have 's2dv_cube' structure. ", 
         "Use PeriodAccumulation instead.")
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

  Dates <- data$attrs$Dates
  data$data <- PeriodAccumulation(data = data$data, dates = Dates, 
                                  start = start, end = end, 
                                  time_dim = time_dim, rollwidth = rollwidth, 
                                  sdate_dim = sdate_dim, frequency = frequency, 
                                  na.rm = na.rm, ncores = ncores)
  data$dims <- dim(data$data)
  if (!is.null(start) & !is.null(end)) {
    Dates <- SelectPeriodOnDates(dates = Dates, start = start, end = end, 
                                 time_dim = time_dim, ncores = ncores)
    data$attrs$Dates <- Dates 
  }
  if (is.null(rollwidth)) {
    data$coords[[time_dim]] <- 1 : length(data$dims[[time_dim]])
    if (!is.null(dim(Dates))) {
      # Create time_bounds
      time_bounds <- NULL
      time_bounds$start <- Subset(Dates, time_dim, 1, drop = FALSE)
      time_bounds$end <- Subset(Dates, time_dim, dim(Dates)[time_dim], drop = FALSE)

      # Add Dates in attrs
      data$attrs$Dates <- time_bounds$start
      data$attrs$time_bounds <- time_bounds
    }
  }

  return(data)
}

#'Period Accumulation on multidimensional array objects
#'
#'Period Accumulation computes the sum (accumulation) of a given variable in a 
#'period. Providing precipitation data, two agriculture indices can be obtained 
#'by using this function:
#'\itemize{
#'  \item{'SprR', Spring Total Precipitation: The total precipitation from 
#'        April 21th to June 21st.}
#'  \item{'HarR', Harvest Total Precipitation: The total precipitation from 
#'        August 21st to October 21st.}
#'}
#' 
#'There are two possible ways of performing the accumulation. The default one 
#'is by accumulating a variable over a dimension specified with 'time_dim'. To 
#'chose a specific time period, 'start' and 'end' must be used. The other method 
#'is by using 'rollwidth' parameter. When this parameter is a positive integer, 
#'the cumulative backward sum is applied to the time dimension. If it is 
#'negative, the rolling sum is applied backwards.
#'
#'@param data A multidimensional array with named dimensions.
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
#'@param time_dim A character string indicating the name of the dimension to 
#'  compute the indicator. By default, it is set to 'time'. 
#'@param rollwidth An optional parameter to indicate the number of time 
#'  steps the rolling sum is applied to. If it is positive, the rolling sum is 
#'  applied backwards 'time_dim', if it is negative, it will be forward it. When 
#'  this parameter is NULL, the sum is applied over all 'time_dim', in a 
#'  specified period. It is NULL by default.
#'@param sdate_dim (Only needed when rollwidth is used). A character string 
#'  indicating the name of the start date dimension to compute the rolling 
#'  accumulation. By default, it is set to 'sdate'.  
#'@param frequency (Only needed when rollwidth is used). A character string 
#'  indicating the time frequency of the data to apply the rolling accumulation.  
#'  It can be 'daily' or 'monthly'. If it is set to 'monthly', values from
#'  continuous months will be accumulated; if it is 'daliy', values from 
#'  continuous days will be accumulated. It is set to 'monthly' by default.
#'@param na.rm A logical value indicating whether to ignore NA values (TRUE) or 
#'  not (FALSE). 
#'@param ncores An integer indicating the number of cores to use in parallel 
#'  computation.
#'@return A multidimensional array with named dimensions containing the 
#'accumulated data in the element \code{data}. If parameter 'rollwidth' is 
#'not used, it will have the dimensions of the input 'data' except the dimension 
#'where the accumulation has been computed (specified with 'time_dim'). If 
#''rollwidth' is used, it will be of same dimensions as input data.
#'
#'@examples
#'exp <- array(rnorm(216)*200, dim = c(dataset = 1, member = 2, sdate = 3, 
#'             ftime = 9, lat = 2, lon = 2))
#'TP <- PeriodAccumulation(exp, time_dim = 'ftime')
#'data <- array(rnorm(5 * 3 * 214 * 2),
#'              c(memb = 5, sdate = 3, ftime = 214, lon = 2)) 
#'Dates <- c(seq(as.Date("01-05-2000", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2000", format = "%d-%m-%Y"), by = 'day'),
#'           seq(as.Date("01-05-2001", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2001", format = "%d-%m-%Y"), by = 'day'),
#'           seq(as.Date("01-05-2002", format = "%d-%m-%Y"), 
#'               as.Date("30-11-2002", format = "%d-%m-%Y"), by = 'day'))
#'dim(Dates) <- c(sdate = 3, ftime = 214)
#'SprR <- PeriodAccumulation(data, dates = Dates, start = list(21, 4), 
#'                           end = list(21, 6), time_dim = 'ftime')
#'HarR <- PeriodAccumulation(data, dates = Dates, start = list(21, 8), 
#'                           end = list(21, 10), time_dim = 'ftime')
#'
#'@import multiApply
#'@importFrom zoo rollapply
#'@importFrom stats setNames
#'@export
PeriodAccumulation <- function(data, dates = NULL, start = NULL, end = NULL, 
                               time_dim = 'time', rollwidth = NULL, 
                               sdate_dim = 'sdate', frequency = 'monthly', 
                               na.rm = FALSE, ncores = NULL) {
  # Initial checks
  ## data
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (!is.numeric(data)) {
    stop("Parameter 'data' must be numeric.")
  }
  ## time_dim
  if (!is.character(time_dim) | length(time_dim) != 1) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!is.array(data)) {
    dim(data) <- length(data)
    names(dim(data)) <- time_dim
  }
  dimnames <- names(dim(data))
  if (!time_dim %in% names(dim(data))) {
    stop("Parameter 'time_dim' is not found in 'data' dimension.")
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
        data <- SelectPeriodOnData(data, dates, start, end, 
                                   time_dim = time_dim, ncores = ncores)
        if (!is.null(rollwidth)) {
          dates <- SelectPeriodOnDates(dates = dates, start = start, end = end, 
                                       time_dim = time_dim, ncores = ncores)
        }
      } else {
        warning("Parameter 'dates' must have named dimensions if 'start' and ",
                "'end' are not NULL. All data will be used.")
      }
    }
  }

  if (is.null(rollwidth)) {
    # period accumulation
    total <- Apply(list(data), target_dims = time_dim, fun = function(...) {sum(...)},
                   na.rm = na.rm, ncores = ncores)$output1
  } else {
    # rolling accumulation
    ## dates
    if (is.null(dates)) {
      stop("Parameter 'dates' is NULL. Cannot compute the rolling accumulation.")
    }
    
    ## rollwidth
    if (!is.numeric(rollwidth)) {
      stop("Parameter 'rollwidth' must be a numeric value.")
    }
    if (abs(rollwidth) > dim(data)[time_dim]) {
      stop(paste0("Cannot compute accumulation of ", rollwidth, " months because ", 
                  "loaded data has only ", dim(data)[time_dim], " months."))
    }
    ## sdate_dim
    if (!is.character(sdate_dim) | length(sdate_dim) != 1) {
      stop("Parameter 'sdate_dim' must be a character string.")
    }
    if (!sdate_dim %in% names(dim(data))) {
      stop("Parameter 'sdate_dim' is not found in 'data' dimension.")
    }
    ## frequency
    if (!is.character(frequency)) {
      stop("Parameter 'frequency' must be a character string.")
    }
    
    forwardroll <- FALSE
    if (rollwidth < 0) {
      rollwidth <- abs(rollwidth)
      forwardroll <- TRUE
    }

    mask_dates <- .datesmask(dates, frequency = frequency)
    total <- Apply(data = list(data), 
                   target_dims = list(data = c(time_dim, sdate_dim)), 
                   fun = .rollaccumulation, 
                   mask_dates = mask_dates,
                   rollwidth = rollwidth,
                   forwardroll = forwardroll, na.rm = na.rm,
                   output_dims = c(time_dim, sdate_dim), 
                   ncores = ncores)$output1

    pos <- match(dimnames, names(dim(total)))
    total <- aperm(total, pos)
  }
  dim(total) <- c(dim(total), setNames(1, time_dim))
  return(total)
}

.rollaccumulation <- function(data, mask_dates, rollwidth = 1, 
                              forwardroll = FALSE, na.rm = FALSE) {
  dims <- dim(data)
  data_vector <- array(NA, dim = length(mask_dates))
  count <- 1
  for (dd in 1:length(mask_dates)) {
    if (mask_dates[dd] == 1) {
      data_vector[dd] <- as.vector(data)[count]
      count <- count + 1
    }
  }

  data_accum <- rollapply(data = data_vector, width = rollwidth,
                          FUN = function(...) {sum(...)},
                          na.rm = na.rm)
  if (!forwardroll) {
    data_accum <- c(rep(NA, rollwidth-1), data_accum) 
  } else {
    data_accum <- c(data_accum, rep(NA, rollwidth-1)) 
  }
  
  data_accum <- data_accum[which(mask_dates == 1)] 
  data_accum <- array(data_accum, dim = c(dims))
  return(data_accum)
}
