#'Compute seasonal mean or other calculations
#'
#'Compute the seasonal mean (or other methods) on monthly time series along
#'one dimension of a named multi-dimensional arrays. Partial season is not 
#'accounted.
#'
#'@param data A named numeric array with at least one dimension 'time_dim'. 
#'@param monini An integer indicating what the first month of the time series is. 
#'  It can be from 1 to 12.
#'@param moninf An integer indicating the starting month of the seasonal
#'  calculation. It can be from 1 to 12.
#'@param monsup An integer indicating the end month of the seasonal calculation. 
#'  It can be from 1 to 12.
#'@param time_dim A character string indicating the name of dimension along  
#'  which the seasonal mean or other calculations are computed. The default
#'  value is 'ftime'.
#'@param method An R function to be applied for seasonal calculation. For
#'  example, 'sum' can be used for total precipitation. The default value is mean.
#'@param na.rm A logical value indicating whether to remove NA values along 
#'  'time_dim' when calculating climatology (TRUE) or return NA if there is NA 
#'  along 'time_dim' (FALSE). The default value is TRUE.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return An array with the same dimensions as data except along the 'time_dim' 
#'  dimension, of which the length changes to the number of seasons.
#'
#'@examples
#'set.seed(1)
#'dat1 <- array(rnorm(144 * 3), dim = c(member = 2, sdate = 2, ftime = 12*3, lon = 3))
#'res <- Season(data = dat1, monini = 1, moninf = 1, monsup = 2)
#'res <- Season(data = dat1, monini = 10, moninf = 12, monsup = 2)
#'dat2 <- dat1
#'set.seed(2)
#'na <- floor(runif(30, min = 1, max = 144 * 3))
#'dat2[na] <- NA
#'res <- Season(data = dat2, monini = 3, moninf = 1, monsup = 2)
#'res <- Season(data = dat2, monini = 3, moninf = 1, monsup = 2, na.rm = FALSE)
#'@import multiApply
#'@export
Season <- function(data, monini, moninf, monsup, time_dim = 'ftime', 
                   method = mean, na.rm = TRUE, ncores = NULL) {

  # Check inputs 
  ## data
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (!is.numeric(data)) {
    stop("Parameter 'data' must be a numeric array.")
  }
  if (is.null(dim(data))) {  #is vector
    dim(data) <- c(length(data))
    names(dim(data)) <- time_dim
  }
  if (any(is.null(names(dim(data)))) | any(nchar(names(dim(data))) == 0)) {
    stop("Parameter 'data' must have dimension names.")
  }
  ## time_dim
  if (!is.character(time_dim) | length(time_dim) > 1) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!time_dim %in% names(dim(data))) {
    stop("Parameter 'time_dim' is not found in 'data' dimension.")
  }
  ## monini
  if (!is.numeric(monini)) {
    stop("Parameter 'monini' must be a positive integer between 1 and 12.")
  } else {
    if (monini %% 1 != 0 | monini < 1 | monini > 12 | length(monini) > 1) {
      stop("Parameter 'monini' must be a positive integer between 1 and 12.")
    }
  }
  ## moninf
  if (!is.numeric(moninf)) {
    stop("Parameter 'moninf' must be a positive integer between 1 and 12.")
  } else {
    if (moninf %% 1 != 0 | moninf < 1 | moninf > 12 | length(moninf) > 1) {
      stop("Parameter 'moninf' must be a positive integer between 1 and 12.")
    }
  }
  ## monsup
  if (!is.numeric(monsup)) {
    stop("Parameter 'monsup' must be a positive integer between 1 and 12.")
  } else {
    if (monsup %% 1 != 0 | monsup < 1 | monsup > 12 | length(monsup) > 1) {
      stop("Parameter 'monsup' must be a positive integer between 1 and 12.")
    }
  }
  ## time_dim, monini, moninf, monsup
  mon_gap <- ifelse(moninf >= monini, moninf - monini, moninf + 12 - monini)
  if ((mon_gap + 1) > dim(data)[time_dim]) {
    stop("Parameter 'moninf' is out of the range because 'monini' is ", monini,
         " and time dimenision length is ", as.numeric(dim(data)[time_dim]), ".")
  }
  mon_diff <- ifelse(monsup >= moninf, monsup - moninf, monsup + 12 - moninf)
  if ((mon_gap + mon_diff + 1) > dim(data)[time_dim]) {
    stop("The chosen month length exceeds the time dimension of 'data'.")
  } 
  ## method
  if (!is.function(method)) {
    stop("Parameter 'method' should be an existing R function, e.g., mean or sum.")
  }
  ## na.rm
  if (!is.logical(na.rm) | length(na.rm) > 1) {
    stop("Parameter 'na.rm' must be one logical value.")
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  } 

  ###############################
  # Calculate Season

    # Correction need if monini is not January: 
    moninf <- moninf - monini + 1
    monsup <- monsup - monini + 1
    moninf <- ifelse(moninf <= 0, moninf + 12, moninf)
    monsup <- ifelse(monsup <= 0, monsup + 12, monsup)

    while (monsup < moninf) {
        monsup <- monsup + 12
    }

# if (ncores == 1 | is.null(ncores)) { use apply } else { use Apply }
    if (!is.null(ncores)) {
      if (ncores == 1) {
        use_apply <- TRUE
      } else {
        use_apply <- FALSE
      }
    } else {
        use_apply <- TRUE
    }

    if (use_apply) {
      if (length(dim(data)) == 1) {
        res <- .Season(data, monini = monini, moninf = moninf, monsup = monsup,
                       method = method, na.rm = na.rm)
        names(dim(res)) <- time_dim
      } else {
        time_dim_ind <- match(time_dim, names(dim(data)))
        res <- apply(data, c(seq_along(dim(data)))[-time_dim_ind], .Season,
                     monini = monini, moninf = moninf, monsup = monsup,
                     method = method, na.rm = na.rm)
        if (is.null(dim(res))) {
          res <- array(res, dim = dim(data)[-time_dim_ind])
        }
        if (length(dim(res)) < length(dim(data))) {
          res <- InsertDim(res, posdim = 1, lendim = 1, name = time_dim)
        } else {
          names(dim(res))[1] <- time_dim
        }
      }
    } else {
    res <- Apply(list(data), 
                 target_dims = time_dim, 
                 output_dims = time_dim,
                 fun = .Season, 
                 monini = monini, moninf = moninf, monsup = monsup,
                 method = method, na.rm = na.rm, ncores = ncores)$output1

    }

    return(res)
}

.Season <- function(x, monini, moninf, monsup, method = mean, na.rm = TRUE) {

    #### Create position index:
    # Basic index:
    pos <- moninf : monsup
    # Extended index for all period:
    if (length(x) > pos[length(pos)]) {
        pos2 <- lapply(pos, function(y) {
                              seq(y, length(x), 12)
                              })
    } else {
        pos2 <- pos
    }
    # Correct if the final season is not complete:
    maxyear <- min(unlist(lapply(pos2, length)))
    pos2 <- lapply(pos2, function(y) {
                           y[1 : maxyear]
                           })
    # Convert to array:
    pos2 <- unlist(pos2)
    dim(pos2) <- c(year = maxyear, month = length(pos2) / maxyear)

    timeseries <- apply(pos2, 1, function(y) {
                                   method(x[y], na.rm = na.rm)
                                   })
    timeseries <- as.array(timeseries)

    return(timeseries)
}

