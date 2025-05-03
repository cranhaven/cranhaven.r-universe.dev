#'NetCDF file data reader for 'startR'
#'
#'This is a data reader function for NetCDF files, intended for use as parameter 
#'file_data_reader in a Start() call. This function complies with the 
#'input/output interface required by Start() defined in the documentation for 
#'the parameter 'file_data_reader'.\cr\cr
#'This function uses the function NcToArray() in the package 'easyNCDF', which 
#'in turn uses nc_var_get() in the package 'ncdf4'.
#'
#'@param file_path A character string indicating the path to the data file to 
#'  read. See details in the documentation of the parameter 'file_data_reader' 
#'  of the function Start(). The default value is NULL.
#'@param file_object An open connection to a NetCDF file, optionally with 
#'  additional header information. See details in the documentation of the 
#'  parameter 'file_data_reader' of the function Start(). The default value is 
#'  NULL.
#'@param file_selectors A named list containing the information of the path of 
#'  the file to read data from. It is automatically provided by Start(). See 
#'  details in the documentation of the parameter 'file_data_reader' of the 
#'  function Start(). The default value is NULL.
#'@param inner_indices A named list of numeric vectors indicating the indices 
#'  to take from each of the inner dimensions in the requested file. It is 
#'  automatically provided by Start(). See details in the documentation of the 
#'  parameter 'file_data_reader' of the function Start(). The default value is 
#'  NULL.
#'@param synonims A named list indicating the synonims for the dimension names 
#'  to look for in the requested file, exactly as provided in the parameter 
#'  'synonims' in a Start() call. See details in the documentation of the 
#'  parameter 'file_data_reader' of the function Start().
#'
#'@return A multidimensional data array with the named dimensions and indices 
#'  requested in 'inner_indices', potentially with the attribute 'variables' 
#'  with additional auxiliary data. See details in the documentation of the 
#'  parameter 'file_data_reader' of the function Start(). 
#'@examples
#'  data_path <- system.file('extdata', package = 'startR', mustWork = TRUE)
#'  file_to_open <- file.path(data_path, 'obs/monthly_mean/tos/tos_200011.nc')
#'  file_selectors <- c(dat = 'dat1', var = 'tos', sdate = '200011')
#'  first_round_indices <- list(time = 1, latitude = 1:8, longitude = 1:16)
#'  synonims <- list(dat = 'dat', var = 'var', sdate = 'sdate', time = 'time',
#'                   latitude = 'latitude', longitude = 'longitude')
#'  sub_array <- NcDataReader(file_to_open, NULL, file_selectors,
#'                            first_round_indices, synonims)
#'@seealso \code{\link{NcOpener}} \code{\link{NcDimReader}} 
#'  \code{\link{NcCloser}} \code{\link{NcVarReader}}
#'@import easyNCDF PCICt
#'@export
NcDataReader <- function(file_path = NULL, file_object = NULL, 
                         file_selectors = NULL, inner_indices = NULL,
                         synonims) {
  close <- FALSE
  if (!is.null(file_object)) {
    file_to_read <- file_object
    file_path <- file_object$filename
  } else if (!is.null(file_path)) {
    file_to_read <- NcOpener(file_path)
    close <- TRUE
  } else {
    stop("Either 'file_path' or 'file_object' must be provided.")
  }

  if (is.null(file_to_read)) {
    return(NULL)
  }
  var_requested <- is.null(inner_indices)

  drop_var_dim <- FALSE
  if (any(c('var', 'variable') %in% names(file_selectors))) {
    if (!any(c('var', 'variable') %in% names(inner_indices))) {
      inner_indices <- c(inner_indices,
                         list(var = file_selectors[[which(names(file_selectors) %in% 
                                                    c('var', 'variable'))[1]]]))
      drop_var_dim <- TRUE
    }
  }

  vars_in_file <- easyNCDF::NcReadVarNames(file_to_read)
  if (any(names(inner_indices) %in% c('var', 'variable'))) {
    position_of_var <- which(names(inner_indices) %in% c('var', 'variable'))[1]
  } else if (length(vars_in_file) == 1) {
    inner_indices <- c(inner_indices,
                       list(var = vars_in_file))
    drop_var_dim <- TRUE
    position_of_var <- length(inner_indices)
  } else {
    stop("A 'var'/'variable' file dimension or inner dimension must be ",
         "requested for NcDataReader() to read NetCDF files with more than ",
         "one variable.")
  }

  inner_indices[[position_of_var]] <- sapply(inner_indices[[position_of_var]],
    function(x) {
      if (x %in% names(synonims)) {
        x_in_file <- which(synonims[[x]] %in% vars_in_file)
        if (length(x_in_file) < 1) {
          stop("Could not find variable '", x, "' (or its synonims if ",
               "specified) in the file ", file_path)
        }
        if (length(x_in_file) > 1) {
          stop("Found more than one matches for the synonims of the ",
               "variable '", x, "' in the file ", file_path)
        }
        synonims[[x]][x_in_file]
      } else {
        if (is.character(x) && !(x %in% c('all', 'first', 'last'))) {
          if (!(x %in% vars_in_file)) {
            stop("Could not find variable '", x, "' (or its synonims if ",
                 "specified) in the file ", file_path)
          }
        }
        x
      }
    })
  #inner_indices[[position_of_var]] <- SelectorChecker(inner_indices[[position_of_var]], vars_in_file)
  dims_in_file <- NcDimReader(NULL, file_to_read, NULL, 
                              inner_indices[position_of_var], synonims)
  names(inner_indices) <- sapply(names(inner_indices), 
    function(x) {
      if (x %in% names(synonims)) {
        x_in_file <- which(synonims[[x]] %in% names(dims_in_file))
        if (length(x_in_file) < 1) {
          stop("Could not find dimension '", x, "' (or its synonims if ",
               "specified) in the file ", file_path)
        }
        if (length(x_in_file) > 1) {
          stop("Found more than one matches for the synonims of the ",
               "dimension '", x, "' in the file ", file_path)
        }
        synonims[[x]][x_in_file]
      } else {
        if (!(x %in% names(dims_in_file))) {
          stop("Could not find dimension '", x, "' (or its synonims if ",
               "specified) in the file ", file_path)
        }
        x
      }
    })
  if (drop_var_dim) {
    dims_in_file <- dims_in_file[-which(names(dims_in_file) %in% c('var', 'variable'))]
  }
  singleton_unspecified_dims <- which((dims_in_file == 1) & 
                                      !(names(dims_in_file) %in% names(inner_indices)))
  if (length(singleton_unspecified_dims) > 0) {
    dims_in_file <- dims_in_file[-singleton_unspecified_dims]
  }
  if (var_requested) {
    result <- easyNCDF::NcToArray(file_to_read, inner_indices, drop_var_dim = drop_var_dim,
                                  expect_all_indices = FALSE, allow_out_of_range = TRUE)
  } else {
    if (any(!(names(dims_in_file) %in% names(inner_indices)))) {
      expected_dim_names <- names(inner_indices)
      if (drop_var_dim) {
        expected_dim_names <- expected_dim_names[-position_of_var]
      }
      stop("Unexpected extra dimensions (of length > 1) in the file.\nExpected: ",
           paste(expected_dim_names, collapse = ', '), "\n",
           "Found: ", paste(names(dims_in_file), collapse = ', '), "\n",
           file_path)
    }
    result <- easyNCDF::NcToArray(file_to_read, inner_indices, drop_var_dim = drop_var_dim,
                                  expect_all_indices = TRUE, allow_out_of_range = TRUE)
  }
  if (!is.null(dim(result))) {
    names(dim(result)) <- sapply(names(dim(result)),
      function(x) {
        which_entry <- which(sapply(synonims, function(y) x %in% y))
        if (length(which_entry) > 0) {
          names(synonims)[which_entry]
        } else {
          x
        }
      })
  }
  if (!is.null(result)) {
    names(attr(result, 'variables')) <- sapply(names(attr(result, 'variables')),
      function(x) {
        which_entry <- which(sapply(synonims, function(y) x %in% y))
        if (length(which_entry) > 0) {
          names(synonims)[which_entry]
        } else {
          x
        }
      })

    if (length(names(attr(result, 'variables'))) == 1) {
      # The 1st condition is for implicit time dim (if time length = 1, it is 
      # allowed to not be defined in Start call. Therefore, it is not in the list
      # of synonims);
      # the 2nd condition is for the normal case; the 3rd one is that if return_vars
      # has a variable that is not 'time'. The only way to know if it should be time
      # is to check calendar.
      # All these conditions are to prevent the variables with time-like units but 
      # actually not a time variable, e.g., drought period [days].
      if (names(attr(result, 'variables')) == 'time' |
          'time' %in% synonims[[names(attr(result, 'variables'))]] |
          'calendar' %in% names(attr(result, 'variables')[[1]])) {
        var_name <- names(attr(result, 'variables'))
        units <- attr(result, 'variables')[[var_name]][['units']]

        if (units %in% c('seconds', 'minutes', 'hours', 'days', 'weeks', 'months', 'years')) {
          if (units == 'seconds') {
#            units <- 'secs'
          } else if (units == 'minutes') {
#            units <- 'mins'
            result <- result * 60  # min to sec
          }
          result[] <- paste(result[], units)

        } else if (grepl(' since ', units)) {
          # Find the calendar
          calendar <- attr(result, 'variables')[[var_name]]$calendar
          # Calendar types recognized by as.PCICt()
          cal.list <- c("365_day", "365", "noleap", "360_day", "360", "gregorian",  "standard", "proleptic_gregorian")

          if (is.null(calendar)) {
            warning("Calendar is missing. Use the standard calendar to calculate time values.")
            calendar <- 'gregorian'
          } else if (!calendar %in% cal.list) {
            # if calendar is not recognized by as.PCICt(), forced it to be standard
            warning("The calendar type '", calendar, "' is not recognized by NcDataReader(). It is forced to be standard type.")
            calendar <- 'gregorian'
          }
          if (calendar == 'standard') calendar <- 'gregorian'

          parts <- strsplit(units, ' since ')[[1]]
          units <- parts[1]

          if (units %in% c('second', 'seconds')) {
#            units <- 'secs'
          } else if (units %in% c('minute', 'minutes')) {
#            units <- 'mins'
            result <- result * 60  # min to sec
          } else if (units %in% c('hour', 'hours')) {
            result <- result * 60 * 60 # hour to sec
          } else if (units %in% c('day', 'days')) {
#            units <- 'days'
            result <- result * 24 * 60 * 60  # day to sec
          } else if (units %in% c('month', 'months')) {
            # define day in each month
            leap_month_day <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
            no_leap_month_day <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

            # If calendar is gregorian, we get the result date directly instead of calculating how many seconds we have. 
            # The other calendar type can also do this but then we need to calculate each date in for loop.
            #TODO: Try to use 'clock' to calculate the date (but dependency will be added)
            if (calendar == 'gregorian') {
              # Origin year and month and day
              ori_year <- as.numeric(substr(parts[2], 1, 4))
              ori_month <- as.numeric(substr(parts[2], 6, 7))
              ori_day <- as.numeric(substr(parts[2], 9, 10))
              if (is.na(ori_month)) {
                ori_month <- as.numeric(substr(parts[2], 6, 6))
                ori_day <- as.numeric(substr(parts[2], 8, 8))
              }
              if (!is.numeric(ori_year) | !is.numeric(ori_month) | !is.numeric(ori_day)) {
                stop(paste0("The time unit attribute format is not 'YYYY-MM-DD' or 'YYYY-M-D'. ",
                            "Check the file or contact the maintainer."))
              }
              result_vec <- rep(NA, length = length(result))
              for (result_i in 1:length(result)) {
                yr_num <- floor(result[result_i] / 12)
                month_left <- result[result_i] - yr_num * 12
                result_year <- ori_year + yr_num
                result_month <- ori_month + floor(month_left)
                result_day <- ori_day
                #NOTE: Assumption that hour down is 00
                result_hour <- 0
                if (result_month > 12) {
                  result_year <- result_year + 1
                  result_month <- result_month - 12
                }
                if (month_left %% 1 != 0) {
                  if (result_month == 2) {
                    day_in_month <- ifelse(s2dv::LeapYear(result_year), 29, 28)
                  } else {
                    day_in_month <- no_leap_month_day[result_month]
                  }
                  result_day <- ori_day + (month_left - floor(month_left)) * day_in_month
                  if (result_day > day_in_month) {
                    result_month <- result_month + 1
                    result_day <- result_day - day_in_month
                  }
                  if (result_month > 12) {
                    result_year <- result_year + 1
                    result_month <- result_month - 12
                  }
                  # if there is hour left
                  if (result_day %% 1 != 0) {
                    result_hour <- (result_day - floor(result_day)) * 24
                    result_day <- floor(result_day)
                  }
                  if (result_hour %% 1 != 0) {
                    warning("The time value is not correct below 'hour'.")
                    result_hour <- round(result_hour)
                  }
                }
                result_month <- sprintf("%02d", result_month)
                result_day <- sprintf("%02d", result_day)
                result_hour <- sprintf("%02d", result_hour)
                # Combine all the parts into one string
                tmp <- paste(result_year, result_month, result_day, sep = '-')
                tmp <- paste0(tmp, ' ', result_hour, ':00:00')
                result_vec[result_i] <- tmp
              }
              # Transfer the strings to time class
              new_array <- PCICt::as.PCICt(result_vec, cal = 'gregorian')
              new_array <- suppressWarnings(PCICt::as.POSIXct.PCICt(new_array, tz = "UTC"))

#            if (calendar == 'gregorian') {
#              # Find how many years + months 
#              yr_num <- floor(result / 12)
#              month_left <- result - yr_num * 12
#              # Find the leap years we care
#              if (ori_month <= 2) {
#                leap_num <- length(which(sapply(ori_year:(ori_year + yr_num - 1), s2dv::LeapYear)))
#              } else {
#                leap_num <- length(which(sapply((ori_year + 1):(ori_year + yr_num), s2dv::LeapYear)))
#              }
#              total_days <- leap_num * 366 + (yr_num - leap_num) * 365  # not include month_left yet
#
#              if (month_left != 0) {
##TODO: This part until result <- total_days* 24 ... is not correct. It doesn't consider ori_day
#                if ((ori_month + month_left - 1) <= 12) { # the last month is still in the same last yr
#                  # Is the last year a leap year?
#                  last_leap <- s2dv::LeapYear(ori_year + yr_num)
#                  if (last_leap) {
#                    month_day_vector <- leap_month_day
#                  } else {
#                    month_day_vector <- no_leap_month_day
#                  }
#                  if (month_left >= 1) { # Only a few days in Jan. only, directly go to the next "if"
#                    total_days <- total_days + sum(month_day_vector[ori_month:(ori_month + month_left - 1)])
#                  }
#                  if ((month_left %% 1) != 0) {
#                    # month_left has decimal point like 11.5
#                    total_days <- total_days + (month_left - floor(month_left)) * month_day_vector[ceiling(month_left)]
#                  }
#                } else { # the last month ends in the next yr
#                  if (ori_month == 2) { # e.g., 2005-02-16 + 11mth = 2006-01-16
#                    last_leap <- s2dv::LeapYear(ori_year + yr_num) # still consider 2005
#                    if (last_leap) {
#                      total_days <- total_days + sum(leap_month_day[2:12])
#                    } else {
#                      total_days <- total_days + sum(no_leap_month_day[2:12])
#                    }
#                  } else { # e.g., 2005-04-16 + 11mth = 2006-03-16
#                    last_leap <- s2dv::LeapYear(ori_year + yr_num + 1)
#                    needed_month <- c(ori_month:12, 1:(ori_month + month_left - 12 - 1))
#                    if (last_leap) {
#                      total_days <- total_days + sum(leap_month_day[needed_month])
#                    } else {
#                      total_days <- total_days + sum(no_leap_month_day[needed_month])
#                    }
#                  }
#                }
#              }
#              result <- total_days * 24 * 60 * 60 # day to sec

            } else if (calendar %in% c('365_day',' 365', 'noleap')) {
              yr_num <- floor(result / 12)
              month_left <- result - yr_num * 12
              total_days <- 365 * yr_num + sum(no_leap_month_day[ori_month:(month_left - 1)])
              result <- total_days * 24 * 60 * 60  # day to sec

            } else if (calendar %in% c('360_day', '360')) {
              result <- result * 30 * 24 * 60 * 60  # day to sec

            } else {  #old code. The calendar is not in any of the above.
              #NOTE: Should not have a chance to be used because the calendar types are forced to be standard above already.
              result <- result * 30.5
              result <- result * 24 * 60 * 60  # day to sec
            }
          }

          if (!(units %in% c('month', 'months') & calendar == 'gregorian')) {
            new_array <- PCICt::as.PCICt(result, cal = calendar, origin = parts[2])[]
            new_array <- suppressWarnings(PCICt::as.POSIXct.PCICt(new_array, tz = "UTC"))
          }
          #new_array <- seq(as.POSIXct(parts[2]), 
          #                  length = max(result, na.rm = TRUE) + 1, 
          #                  by = units)[result[] + 1]
          dim(new_array) <- dim(result)
          attr(new_array, 'variables') <- attr(result, 'variables')
          result <- new_array
        }
      }
    } 
  }

  if (close) {
    NcCloser(file_to_read)
  }

  result
}
