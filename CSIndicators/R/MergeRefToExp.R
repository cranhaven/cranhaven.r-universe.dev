#'Merge a Reference To Experiments 
#'
#'Some indicators are defined for specific temporal periods (e.g.: summer from 
#'June 21st to September 21st). If the initialization forecast date is later 
#'than the one required for the indicator (e.g.: July 1st), the user may want to 
#'merge past observations, or other references, to the forecast (or hindcast) to 
#'compute the indicator. If the forecast simulation doesn't cover the required
#'period because it is initialized too early (e.g.: Initialization on November
#'1st the forecast covers until the beginning of June next year), a climatology
#'(or other references) could be added at the end of the forecast lead time to 
#'cover the desired period (e.g.: until the end of summer).
#' 
#'This function is created to merge observations and forecasts, known as the 
#'‘blending’ strategy (see references). The basis for this strategy is that the 
#'predictions are progressively replaced with observational data as soon as they 
#'become available (i.e., when entering the indicator definition period). This 
#'key strategy aims to increase users’ confidence in the reformed predictions.
#'
#'@param data1 An 's2dv_cube' object with the element 'data' being a  
#'  multidimensional array with named dimensions. All dimensions must be 
#'  equal to 'data2' dimensions except for the ones specified with 'memb_dim' 
#'  and 'time_dim'. If 'start1' and 'end1' are used to subset a period, the
#'  Dates must be stored in element '$attrs$Dates' of the object. Dates must 
#'  have same time dimensions as element 'data'.
#'@param data2 An 's2dv_cube' object with the element 'data' being a 
#'  multidimensional array of named dimensions matching the dimensions of 
#'  parameter 'data1'. All dimensions must be equal to 'data1' except for the 
#'  ones specified with 'memb_dim' and 'time_dim'. If 'start2' and 'end2' are 
#'  used to subset a period, the Dates must be stored in element '$attrs$Dates' 
#'  of the object. Dates must have same time dimensions as element 'data'.
#'@param start1 A list to define the initial date of the period to select from 
#'  'data1' by providing a list of two elements: the initial date of the period 
#'  and the initial month of the period. 
#'@param end1 A list to define the final date of the period to select from 
#'  'data1' by providing a list of two elements: the final day of the period and 
#'  the final month of the period.
#'@param start2 A list to define the initial date of the period to select from 
#'  'data2' by providing a list of two elements: the initial date of the period 
#'  and the initial month of the period.
#'@param end2 A list to define the final date of the period to select from 
#'  'data2' by providing a list of two elements: the final day of the period and 
#'  the final month of the period.
#'@param time_dim A character string indicating the name of the temporal 
#'  dimension that will be used to combine the two arrays. By default, it is set 
#'  to 'time'. Also, it will be used to subset the data in a requested 
#'  period.
#'@param memb_dim A character string indicating the name of the member 
#'  dimension. If the data are not ensemble ones, set as NULL. The default 
#'  value is 'member'.
#'@param ncores An integer indicating the number of cores to use in parallel 
#'  computation.
#'@return An 's2dv_cube' object containing the indicator in the element 
#'\code{data}. The element \code{data} will be a multidimensional array created 
#'from the combination of 'data1' and 'data2'. The resulting array will contain 
#'the following dimensions: the original dimensions of the input data, which are 
#'common to both arrays and for the 'time_dim' dimension, the sum of the 
#'corresponding dimension of 'data1' and 'data2'. If 'memb_dim' is not null, 
#'regarding member dimension, two different situations can occur: (1) in the 
#'case that one of the arrays does not have member dimension or is equal to 1 
#'and the other array has multiple member dimension, the result will contain the 
#'repeated values of the array one up to the lenght of member dimension of array 
#'two; (2) in the case that both arrays have member dimension and is greater 
#'than 1, all combinations of member dimension will be returned. The other 
#'elements of the 's2dv_cube' will be updated with the combined information of 
#'both datasets.
#' 
#'@references Chou, C., R. Marcos-Matamoros, L. Palma Garcia, N. Pérez-Zanón, 
#'M. Teixeira, S. Silva, N. Fontes, A. Graça, A. Dell'Aquila, S. Calmanti and 
#'N. González-Reviriego (2023). Advanced seasonal predictions for vine 
#'management based on bioclimatic indicators tailored to the wine sector. 
#'Climate Services, 30, 100343, \doi{10.1016/j.cliser.2023.100343}.
#'
#'@examples
#'data_dates <- c(seq(as.Date("01-07-1993", "%d-%m-%Y", tz = 'UTC'),
#'                    as.Date("01-12-1993","%d-%m-%Y", tz = 'UTC'), "day"),
#'                seq(as.Date("01-07-1994", "%d-%m-%Y", tz = 'UTC'),
#'                    as.Date("01-12-1994","%d-%m-%Y", tz = 'UTC'), "day"))
#'dim(data_dates) <- c(time = 154, sdate = 2)
#'data <- NULL
#'data$data <- array(1:(2*154*2), c(time = 154, sdate = 2, member = 2))
#'data$attrs$Dates<- data_dates
#'class(data) <- 's2dv_cube'
#'ref_dates <- seq(as.Date("01-01-1993", "%d-%m-%Y", tz = 'UTC'),
#'                 as.Date("01-12-1994","%d-%m-%Y", tz = 'UTC'), "day")
#'dim(ref_dates) <- c(time = 350, sdate = 2)
#'ref <- NULL
#'ref$data <- array(1001:1700, c(time = 350, sdate = 2))
#'ref$attrs$Dates <- ref_dates
#'class(ref) <- 's2dv_cube'
#'new_data <- CST_MergeRefToExp(data1 = ref, data2 = data, 
#'                              start1 = list(21, 6), end1 = list(30, 6),
#'                              start2 = list(1, 7), end2 = list(21, 9))
#'
#'@export
CST_MergeRefToExp <- function(data1, data2, start1 = NULL, end1 = NULL, 
                              start2 = NULL, end2 = NULL,
                              time_dim = 'time', memb_dim = 'member', 
                              ncores = NULL) {
  # Check 's2dv_cube'
  if (!inherits(data1, 's2dv_cube')) {
    stop("Parameter 'data1' must be of the class 's2dv_cube'.")
  }
  if (!inherits(data2, 's2dv_cube')) {
    stop("Parameter 'data2' must be of the class 's2dv_cube'.")
  }
  # Dates subset of data1
  if (!is.null(start1) && !is.null(end1)) {
    if (is.null(dim(data1$attrs$Dates))) {
      warning("Dimensions in 'data1' element 'attrs$Dates' are missed and ",
              "all data would be used.")
      start1 <- NULL
      end1 <- NULL
    }
  }
  # Dates subset of data2
  if (!is.null(start2) && !is.null(end2)) {
    if (is.null(dim(data2$attrs$Dates))) {
      warning("Dimensions in 'data2' element 'attrs$Dates' are missed and ",
              "all data would be used.")
      start2 <- NULL
      end2 <- NULL
    }
  }

  dates1 <- data1$attrs$Dates
  dates2 <- data2$attrs$Dates

  # data
  data1$data <- MergeRefToExp(data1 = data1$data, dates1 = dates1,
                              start1 = start1, end1 = end1,
                              data2 = data2$data, dates2 = dates2,
                              start2, end2, time_dim = time_dim, 
                              memb_dim = memb_dim, ncores = ncores)
  # dims
  data1$dims <- dim(data1$data)

  # coords
  for (i_dim in names(dim(data1$data))) {
    if (length(data1$coords[[i_dim]]) != dim(data1$data)[i_dim]) {
      data1$coords[[i_dim]] <- NULL
      data1$coords[[i_dim]] <- 1:dim(data1$data)[i_dim]
      attr(data1$coords[[i_dim]], 'indices') <- TRUE
    } else if (length(data1$coords[[i_dim]]) == length(data2$coords[[i_dim]])) {
      if (any(as.vector(data1$coords[[i_dim]]) != as.vector(data2$coords[[i_dim]]))) {
        data1$coords[[i_dim]] <- NULL
        data1$coords[[i_dim]] <- 1:dim(data1$data)[i_dim]
        attr(data1$coords[[i_dim]], 'indices') <- TRUE
      } else if (!identical(attributes(data1$coords[[i_dim]]), 
                            attributes(data2$coords[[i_dim]]))) {
        attributes(data1$coords[[i_dim]]) <- NULL
      }
    } else {
      data1$coords[[i_dim]] <- NULL
      data1$coords[[i_dim]] <- 1:dim(data1$data)[i_dim]
      attr(data1$coords[[i_dim]], 'indices') <- TRUE
    }
  }

  # Dates
  if (!is.null(dates1)) {
    if (!is.null(start1) && !is.null(end1)) {
      dates1 <- SelectPeriodOnDates(dates1, start = start1, end = end1,
                                    time_dim = time_dim)
    }
  }
  if (!is.null(dates2)) {
    if ((!is.null(start2) && !is.null(end2))) {
      dates2 <- SelectPeriodOnDates(dates2, start = start2,
                                    end = end2, time_dim = time_dim)
    }
  }

  remove_dates_dim <- FALSE

  if (!is.null(dates1) & !is.null(dates2)) {
    if (is.null(dim(dates1))) {
      remove_dates_dim <- TRUE
      dim(dates1) <- length(dates1)
      names(dim(dates1)) <- time_dim
    }
    if (is.null(dim(dates2))) {
      remove_dates_dim <- TRUE
      dim(dates2) <- length(dates2)
      names(dim(dates2)) <- time_dim
    }
  }
  res <- Apply(list(dates1, dates2),
               target_dims = time_dim,
               fun = function(x, ...) {c(x, ...)},
               output_dims = time_dim, ncores = ncores)$output1

  if (inherits(dates1, 'Date')) {
    data1$attrs$Dates <- as.Date(res, origin = '1970-01-01')
  } else {
    data1$attrs$Dates <- as.POSIXct(res, origin = '1970-01-01', tz = 'UTC')
  }

  if (remove_dates_dim) {
    dim(data1$attrs$Dates) <- NULL
  }

  # Variable
  data1$attrs$Variable$varName <- unique(data1$attrs$Variable$varName, 
                                         data2$attrs$Variable$varName)
  names_metadata <- names(data1$attrs$Variable$metadata)
  data1$attrs$Variable$metadata <- intersect(data1$attrs$Variable$metadata, 
                                             data2$attrs$Variable$metadata)
  names(data1$attrs$Variable$metadata) <- names_metadata

  # source_files
  data1$attrs$source_files <- unique(c(data1$attrs$source_files, data2$attrs$source_files))

  # Datasets
  data1$attrs$Datasets <- unique(c(data1$attrs$Datasets, data2$attrs$Datasets))

  # when
  data1$attrs$when <- Sys.time()

  # load_parameters (TO DO: remove with CST_Start)
  if (!is.null(c(data1$attrs$load_parameters, data2$attrs$load_parameters))) {
    data1$attrs$load_parameters <- list(data1 = data1$attrs$load_parameters,
                                        data2 = data2$attrs$load_parameters)
  }
  
  return(data1)
}

#'Merge a Reference To Experiments 
#'
#'Some indicators are defined for specific temporal periods (e.g.: summer from 
#'June 21st to September 21st). If the initialization forecast date is later 
#'than the one required for the indicator (e.g.: July 1st), the user may want to 
#'merge past observations, or other references, to the forecast (or hindcast) to 
#'compute the indicator. If the forecast simulation doesn't cover the required 
#'period because it is initialized too early (e.g.: Initialization on November 
#'1st the forecast covers until the beginning of June next year), a climatology 
#'(or other references) could be added at the end of the forecast lead time to 
#'cover the desired period (e.g.: until the end of summer).
#' 
#'This function is created to merge observations and forecasts, known as the 
#'‘blending’ strategy (see references). The basis for this strategy is that the 
#'predictions are progressively replaced with observational data as soon as they 
#'become available (i.e., when entering the indicator definition period). This 
#'key strategy aims to increase users’ confidence in the reformed predictions.
#'
#'@param data1 A multidimensional array with named dimensions. All dimensions 
#'  must be equal to 'data2' dimensions except for the ones specified with 
#'  'memb_dim' and 'time_dim'. 
#'@param dates1 A multidimensional array of dates with named dimensions matching 
#'  the temporal dimensions of parameter 'data1'. The common dimensions must be 
#'  equal to 'data1' dimensions.
#'@param data2 A multidimensional array of named dimensions matching the 
#'  dimensions of parameter 'data1'. All dimensions must be equal to 'data1' 
#'  except for the ones specified with 'memb_dim' and 'time_dim'.
#'@param dates2 A multidimensional array of dates with named dimensions matching 
#'  the temporal dimensions on parameter 'data2'. The common dimensions must be 
#'  equal to 'data2' dimensions.
#'@param start1 A list to define the initial date of the period to select from 
#'  'data1' by providing a list of two elements: the initial date of the period 
#'  and the initial month of the period. The initial date of the period must be 
#'  included in the 'dates1' array.
#'@param end1 A list to define the final date of the period to select from 
#'  'data1' by providing a list of two elements: the final day of the period and 
#'  the final month of the period. The final date of the period must be 
#'  included in the 'dates1' array.
#'@param start2 A list to define the initial date of the period to select from 
#'  'data2' by providing a list of two elements: the initial date of the period 
#'  and the initial month of the period. The initial date of the period must be 
#'  included in the 'dates2' array.
#'@param end2 A list to define the final date of the period to select from 
#'  'data2' by providing a list of two elements: the final day of the period and 
#'  the final month of the period. The final date of the period must be 
#'  included in the 'dates2' array.
#'@param time_dim A character string indicating the name of the temporal 
#'  dimension that will be used to combine the two arrays. By default, it is set 
#'  to 'time'. Also, it will be used to subset the data in a requested 
#'  period.
#'@param memb_dim A character string indicating the name of the member 
#'  dimension. If the 'data1' and 'data2' have no member dimension, set it as 
#'  NULL. It is set as 'member' by default.
#'@param ncores An integer indicating the number of cores to use in parallel 
#'  computation.
#'
#'@return A multidimensional array created from the combination of 'data1' and 
#''data2'. The resulting array will contain the following dimensions: the 
#'original dimensions of the input data, which are common to both arrays and for 
#'the 'time_dim' dimension, the sum of the corresponding dimension of 'data1' 
#'and 'data2'. If 'memb_dim' is not null, regarding member dimension, two 
#'different situations can occur: (1) in the case that one of the arrays does 
#'not have member dimension or is equal to 1 and the other array has multiple 
#'member dimension, the result will contain the repeated values of the array one 
#'up to the lenght of member dimension of array two; (2) in the case that both 
#'arrays have member dimension and is greater than 1, all combinations of member 
#'dimension will be returned.
#' 
#'@references Chou, C., R. Marcos-Matamoros, L. Palma Garcia, N. Pérez-Zanón, 
#'M. Teixeira, S. Silva, N. Fontes, A. Graça, A. Dell'Aquila, S. Calmanti and 
#'N. González-Reviriego (2023). Advanced seasonal predictions for vine 
#'management based on bioclimatic indicators tailored to the wine sector. 
#'Climate Services, 30, 100343, \doi{10.1016/j.cliser.2023.100343}.
#' 
#'@examples
#'data_dates <- c(seq(as.Date("01-07-1993", "%d-%m-%Y", tz = 'UTC'),
#'                    as.Date("01-12-1993","%d-%m-%Y", tz = 'UTC'), "day"),
#'                seq(as.Date("01-07-1994", "%d-%m-%Y", tz = 'UTC'),
#'                    as.Date("01-12-1994","%d-%m-%Y", tz = 'UTC'), "day"))
#'dim(data_dates) <- c(time = 154, sdate = 2)
#'ref_dates <- seq(as.Date("01-01-1993", "%d-%m-%Y", tz = 'UTC'),
#'                 as.Date("01-12-1994","%d-%m-%Y", tz = 'UTC'), "day")
#'dim(ref_dates) <- c(time = 350, sdate = 2)
#'ref <- array(1001:1700, c(time = 350, sdate = 2))
#'data <- array(1:(2*154*2), c(time = 154, sdate = 2, member = 2))
#'new_data <- MergeRefToExp(data1 = ref, dates1 = ref_dates, start1 = list(21, 6),
#'                          end1 = list(30, 6), data2 = data, dates2 = data_dates,
#'                          start2 = list(1, 7), end = list(21, 9), 
#'                          time_dim = 'time')
#'
#'@import multiApply
#'@export
MergeRefToExp <- function(data1, data2, dates1 = NULL, dates2 = NULL, 
                          start1 = NULL, end1 = NULL, start2 = NULL, end2 = NULL,
                          time_dim = 'time', memb_dim = 'member', 
                          ncores = NULL) {
  # Input checks
  ## data1 and data2
  if (!is.array(data1) | !is.array(data2)) {
    stop("Parameters 'data1' and 'data2' must be arrays.")
  }
  if (is.null(names(dim(data1))) | is.null(names(dim(data2)))) {
    stop("Parameters 'data1' and 'data2' must have named dimensions.")
  }
  ## time_dim
  if (!is.character(time_dim)) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!time_dim %in% names(dim(data1)) | !time_dim %in% names(dim(data2))) {
    stop("Parameter 'time_dim' is not found in 'data1' or 'data2' dimension ",
         "names.")
  }
  ## memb_dim
  data1dims <- names(dim(data1))
  data2dims <- names(dim(data2))
  if (!is.null(memb_dim)) {
    if (!is.character(memb_dim)) {
      stop("Parameter 'memb_dim' must be a character string.")
    }
    if (!memb_dim %in% names(dim(data1)) & !memb_dim %in% names(dim(data2))) {
      stop("Parameter 'memb_dim' is not found in 'data1' or 'data2' dimension. ",
           "Set it to NULL if there is no member dimension.")
    }
    if ((memb_dim %in% names(dim(data1)) & memb_dim %in% names(dim(data2)))) {
      if (dim(data1)[memb_dim] != dim(data2)[memb_dim]) {
        if (dim(data1)[memb_dim] == 1) {
          data1 <- array(data1, dim = dim(data1)[-which(names(dim(data1)) == memb_dim)])
        } else if (dim(data2)[memb_dim] == 1) {
          data2 <- array(data2, dim = dim(data2)[-which(names(dim(data2)) == memb_dim)])
        } else {
          memb_dim1 <- dim(data1)[memb_dim]
          data1 <- Apply(list(data1), target_dims = memb_dim, 
                         fun = function(x, memb_rep) {
                           return(rep(x, each = memb_rep))
                         }, memb_rep = dim(data2)[memb_dim], 
                         output_dims  = memb_dim, ncores = ncores)$output1
          data2 <- Apply(list(data2), target_dims = memb_dim, 
                         fun = function(x, memb_rep) {
                           return(rep(x, memb_rep))
                         }, memb_rep = memb_dim1, 
                         output_dims  = memb_dim, ncores = ncores)$output1
        }
      }
    }
  }
  ## data1 and data2 (2)
  name_data1 <- sort(names(dim(data1)))
  name_data2 <- sort(names(dim(data2)))

  name_data1 <- name_data1[-which(name_data1 %in% c(time_dim, memb_dim))]
  name_data2 <- name_data2[-which(name_data2 %in% c(time_dim, memb_dim))]

  if (!identical(length(name_data1), length(name_data2)) |
      !identical(dim(data1)[name_data1], dim(data2)[name_data2])) {
    stop(paste0("Parameter 'data1' and 'data2' must have same length of ",
                "all dimensions except 'memb_dim'."))
  }
  ## dates1
  if (!is.null(start1) & !is.null(end1)) {
    if (is.null(dates1)) {
      warning("Parameter 'dates' is NULL and the average of the ",
              "full data provided in 'data' is computed.")
    } else if (!all(c(is.list(start1), is.list(end1)))) {
      warning("Parameter 'start1' and 'end1' must be lists indicating the ",
              "day and the month of the period start and end. Full data ",
              "will be used.")
    } else {
      if (!is.null(dim(dates1))) {
        data1 <- SelectPeriodOnData(data = data1, dates = dates1, start = start1, 
                                    end = end1, time_dim = time_dim, 
                                    ncores = ncores)
      } else {
        warning("Parameter 'dates1' must have named dimensions if 'start' and ",
                "'end' are not NULL. All 'data1' will be used.")
      }
    }
  }
  ## dates2
  if (!is.null(start2) & !is.null(end2)) {
    if (is.null(dates2)) {
      warning("Parameter 'dates2' is NULL and the average of the ",
              "full data provided in 'data' is computed.")
    } else  if (!all(c(is.list(start2), is.list(end2)))) {
      warning("Parameter 'start2' and 'end2' must be lists indicating the ",
              "day and the month of the period start and end. Full data ",
              "will be used.")
    } else {
      if (!is.null(dim(dates2))) {
        data2 <- SelectPeriodOnData(data = data2, dates = dates2, start = start2, 
                                    end = end2, time_dim = time_dim, 
                                    ncores = ncores)
      } else {
        warning("Parameter 'dates2' must have named dimensions if 'start2' and ",
                "'end2' are not NULL. All 'data2' will be used.")
      }
    }
  }

  data1 <- Apply(list(data1, data2), target_dims = time_dim,
                 fun = function(x, ...) {c(x, ...)},
                 output_dims = time_dim, ncores = ncores)$output1

  if (all(names(dim(data1)) %in% data1dims)) {
    pos <- match(data1dims, names(dim(data1)))
    data1 <- aperm(data1, pos)
  } else if (all(names(dim(data1)) %in% data2dims)) {
    pos <- match(data2dims, names(dim(data1)))
    data1 <- aperm(data1, pos)
  }
  return(data1)
}
