#'Compute the Standardization of Precipitation-Evapotranspiration Index
#' 
#'The Standardization of the data is the last step of computing the SPEI 
#'(Standarized Precipitation-Evapotranspiration Index). With this function the 
#'data is fit to a probability distribution to transform the original values to 
#'standardized units that are comparable in space and time and at different SPEI 
#'time scales.
#' 
#'Next, some specifications for the calculation of the standardization will be 
#'discussed. If there are NAs in the data and they are not removed with the 
#'parameter 'na.rm', the standardization cannot be carried out for those 
#'coordinates and therefore, the result will be filled with NA for the 
#'specific coordinates. When NAs are not removed, if the length of the data for 
#'a computational step is smaller than 4, there will not be enough data for 
#'standardization and the result will be also filled with NAs for those coordinates. 
#'About the distribution used to fit the data, there are only two possibilities: 
#''log-logistic' and 'Gamma'. The 'Gamma' method works only when precipitation 
#'is the sole variable provided, and all other variables are 0 because it is positive 
#'defined (SPI indicator). When only 'data' is provided ('data_cor' is NULL) the 
#'standardization is computed with cross validation. This function is built to 
#'be compatible with other tools in that work with 's2dv_cube' object 
#'class. The input data must be this object class. If you don't work with 
#''s2dv_cube', see PeriodStandardization. For more information on the SPEI 
#'indicator calculation, see CST_PeriodPET and CST_PeriodAccumulation.
#'
#'@param data An 's2dv_cube' that element 'data' stores a multidimensional 
#'  array containing the data to be standardized.
#'@param data_cor An 's2dv_cube' that element 'data' stores a multidimensional 
#'  array containing the data in which the standardization should be applied 
#'  using the fitting parameters from 'data'.
#'@param time_dim A character string indicating the name of the temporal 
#'  dimension. By default, it is set to 'syear'. 
#'@param leadtime_dim A character string indicating the name of the temporal 
#'  dimension. By default, it is set to 'time'. 
#'@param memb_dim A character string indicating the name of the dimension in 
#'  which the ensemble members are stored. When set it to NULL, threshold is 
#'  computed for individual members.
#'@param ref_period A list with two numeric values with the starting and end 
#'  points of the reference period used for computing the index. The default 
#'  value is NULL indicating that the first and end values in data will be 
#'  used as starting and end points.
#'@param params An optional parameter that needs to be a multidimensional array 
#'  with named dimensions. This option overrides computation of fitting 
#'  parameters. It needs to be of same time dimensions (specified in 'time_dim' 
#'  and 'leadtime_dim') of 'data' and a dimension named 'coef' with the length 
#'  of the coefficients needed for the used distribution (for 'Gamma' coef 
#'  dimension is of lenght 2, for 'log-Logistic' is of length 3). It also needs 
#'  to have a leadtime dimension (specified in 'leadtime_dim') of length 1. It 
#'  will only be used if 'data_cor' is not provided. 
#'@param handle_infinity A logical value wether to return infinite values (TRUE)
#'  or not (FALSE). When it is TRUE, the positive infinite values (negative 
#'  infinite) are substituted by the maximum (minimum) values of each 
#'  computation step, a subset of the array of dimensions time_dim, leadtime_dim 
#'  and memb_dim.
#'@param method A character string indicating the standardization method used. 
#'  If can be: 'parametric' or 'non-parametric'. It is set to 'parametric' by 
#'  default.
#'@param distribution A character string indicating the name of the distribution 
#'  function to be used for computing the SPEI. The accepted names are: 
#'  'log-Logistic' and 'Gamma'. It is set to 'log-Logistic' by default. The 
#'  'Gamma' method only works when only precipitation is provided and other 
#'   variables are 0 because it is positive defined (SPI indicator).
#'@param return_params A logical value indicating wether to return parameters 
#'  array (TRUE) or not (FALSE). It is FALSE by default.
#'@param na.rm A logical value indicating whether NA values should be removed 
#'  from data. It is FALSE by default. If it is FALSE and there are NA values, 
#'  standardization cannot be carried out for those coordinates and therefore, 
#'  the result will be filled with NA for the specific coordinates. If it is 
#'  TRUE, if the data from other dimensions except time_dim and leadtime_dim is 
#'  not reaching 4 values, it is not enough values to estimate the parameters 
#'  and the result will include NA.
#'@param ncores An integer value indicating the number of cores to use in 
#'  parallel computation.
#' 
#'@return An object of class \code{s2dv_cube} containing the standardized data. 
#'If 'data_cor' is provided the array stored in element data will be of the same 
#'dimensions as 'data_cor'. If 'data_cor' is not provided, the array stored in 
#'element data will be of the same dimensions as 'data'. The parameters of the 
#'standardization will only be returned if 'return_params' is TRUE, in this 
#'case, the output will be a list of two objects one for the standardized data 
#'and one for the parameters.
#' 
#'@examples 
#'dims <-  c(syear = 6, time = 3, latitude = 2, ensemble = 25)
#'data <- NULL
#'data$data <- array(rnorm(600, -204.1, 78.1), dim = dims)
#'class(data) <- 's2dv_cube'
#'SPEI <- CST_PeriodStandardization(data = data)
#'@export
CST_PeriodStandardization <- function(data, data_cor = NULL, time_dim = 'syear', 
                                      leadtime_dim = 'time', memb_dim = 'ensemble',
                                      ref_period = NULL,
                                      handle_infinity = FALSE, 
                                      method = 'parametric', 
                                      distribution = 'log-Logistic', 
                                      params = NULL, return_params = FALSE, 
                                      na.rm = FALSE, ncores = NULL) {
  # Check 's2dv_cube'
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (!inherits(data, 's2dv_cube')) {
    stop("Parameter 'data' must be of 's2dv_cube' class.")
  }
  if (!is.null(data_cor)) {
    if (!inherits(data_cor, 's2dv_cube')) {
      stop("Parameter 'data_cor' must be of 's2dv_cube' class.")
    }
  }
  res <- PeriodStandardization(data = data$data, data_cor = data_cor$data, 
                               dates = data$attrs$Dates,
                               time_dim = time_dim, leadtime_dim = leadtime_dim, 
                               memb_dim = memb_dim, 
                               ref_period = ref_period,
                               handle_infinity = handle_infinity, method = method, 
                               distribution = distribution, 
                               params = params, return_params = return_params, 
                               na.rm = na.rm, ncores = ncores)
  if (return_params) {
    std <- res$spei
    params <- res$params
  } else {
    std <- res
  }

  if (is.null(data_cor)) {
    data$data <- std
    data_longname <- data$attrs$Variable$metadata[[data$attrs$Variable$varName]]$longname
    if (!is.null(data_longname)) {
      data$attrs$Variable$metadata[[data$attrs$Variable$varName]]$longname <- paste(data_longname, 'standardized')
    }
    if (return_params) {
      return(list(spei = data, params = params))
    } else {
      return(data)
    }
  } else {
    data_cor$data <- std
    data_cor_longname <- data_cor$attrs$Variable$metadata[[data_cor$attrs$Variable$varName]]$longname
    if (!is.null(data_cor_longname)) {
      data_cor$attrs$Variable$metadata[[data_cor$attrs$Variable$varName]]$longname <- paste(data_cor_longname, 'standardized')
    }
    data_cor$attrs$Datasets <- c(data_cor$attrs$Datasets, data$attrs$Datasets)
    data_cor$attrs$source_files <- c(data_cor$attrs$source_files, data$attrs$source_files)
    return(data_cor)
  }
}

#'Compute the Standardization of Precipitation-Evapotranspiration Index
#' 
#'The Standardization of the data is the last step of computing the SPEI 
#'indicator. With this function the data is fit to a probability distribution to 
#'transform the original values to standardized units that are comparable in 
#'space and time and at different SPEI time scales.
#' 
#'Next, some specifications for the calculation of the standardization will be 
#'discussed. If there are NAs in the data and they are not removed with the 
#'parameter 'na.rm', the standardization cannot be carried out for those 
#'coordinates and therefore, the result will be filled with NA for the 
#'specific coordinates. When NAs are not removed, if the length of the data for 
#'a computational step is smaller than 4, there will not be enough data for 
#'standarize and the result will be also filled with NAs for that coordinates. 
#'About the distribution used to fit the data, there are only two possibilities: 
#''log-logistic' and 'Gamma'. The 'Gamma' method only works when only 
#'precipitation is provided and other variables are 0 because it is positive 
#'defined (SPI indicator). When only 'data' is provided ('data_cor' is NULL) the 
#'standardization is computed with cross validation. For more information about 
#'SPEI, see functions PeriodPET and PeriodAccumulation.
#'
#'@param data A multidimensional array containing the data to be standardized.
#'@param data_cor A multidimensional array containing the data in which the 
#'  standardization should be applied using the fitting parameters from 'data'.
#'@param dates An array containing the dates of the data with the same time 
#'  dimensions as the data. It is optional and only necessary for using the 
#'  parameter 'ref_period' to select a reference period directly from dates. 
#'@param time_dim A character string indicating the name of the temporal 
#'  dimension. By default, it is set to 'syear'. 
#'@param leadtime_dim A character string indicating the name of the temporal 
#'  dimension. By default, it is set to 'time'. 
#'@param memb_dim A character string indicating the name of the dimension in 
#'  which the ensemble members are stored. When set it to NULL, threshold is 
#'  computed for individual members.
#'@param ref_period A list with two numeric values with the starting and end 
#'  points of the reference period used for computing the index. The default 
#'  value is NULL indicating that the first and end values in data will be 
#'  used as starting and end points.
#'@param params An optional parameter that needs to be a multidimensional array 
#'  with named dimensions. This option overrides computation of fitting 
#'  parameters. It needs to be of same time dimensions (specified in 'time_dim' 
#'  and 'leadtime_dim') of 'data' and a dimension named 'coef' with the length 
#'  of the coefficients needed for the used distribution (for 'Gamma' coef 
#'  dimension is of lenght 2, for 'log-Logistic' is of length 3). It also needs 
#'  to have a leadtime dimension (specified in 'leadtime_dim') of length 1. It 
#'  will only be used if 'data_cor' is not provided. 
#'@param handle_infinity A logical value wether to return infinite values (TRUE)
#'  or not (FALSE). When it is TRUE, the positive infinite values (negative 
#'  infinite) are substituted by the maximum (minimum) values of each 
#'  computation step, a subset of the array of dimensions time_dim, leadtime_dim 
#'  and memb_dim.
#'@param method A character string indicating the standardization method used. 
#'  If can be: 'parametric' or 'non-parametric'. It is set to 'parametric' by 
#'  default.
#'@param distribution A character string indicating the name of the distribution 
#'  function to be used for computing the SPEI. The accepted names are: 
#'  'log-Logistic' and 'Gamma'. It is set to 'log-Logistic' by default. The 
#'  'Gamma' method only works when only precipitation is provided and other 
#'   variables are 0 because it is positive defined (SPI indicator).
#'@param return_params A logical value indicating wether to return parameters 
#'  array (TRUE) or not (FALSE). It is FALSE by default.
#'@param na.rm A logical value indicating whether NA values should be removed 
#'  from data. It is FALSE by default. If it is FALSE and there are NA values, 
#'  standardization cannot be carried out for those coordinates and therefore, 
#'  the result will be filled with NA for the specific coordinates. If it is 
#'  TRUE, if the data from other dimensions except time_dim and leadtime_dim is 
#'  not reaching 4 values, it is not enough values to estimate the parameters 
#'  and the result will include NA.
#'@param ncores An integer value indicating the number of cores to use in 
#'  parallel computation.
#' 
#'@return A multidimensional array containing the standardized data. 
#'If 'data_cor' is provided the array will be of the same dimensions as 
#''data_cor'. If 'data_cor' is not provided, the array will be of the same 
#'dimensions as 'data'. The parameters of the standardization will only be 
#'returned if 'return_params' is TRUE, in this case, the output will be a list 
#'of two objects one for the standardized data and one for the parameters.
#' 
#'@examples
#'dims <-  c(syear = 6, time = 2, latitude = 2, ensemble = 25)
#'dimscor <-  c(syear = 1, time = 2, latitude = 2, ensemble = 25)
#'data <- array(rnorm(600, -194.5, 64.8), dim = dims)
#'datacor <- array(rnorm(100, -217.8, 68.29), dim = dimscor)
#'
#'SPEI <- PeriodStandardization(data = data)
#'SPEIcor <- PeriodStandardization(data = data, data_cor = datacor)
#'@import multiApply
#'@importFrom ClimProjDiags Subset
#'@importFrom lmomco pwm.pp pwm.ub pwm2lmom are.lmom.valid parglo pargam parpe3 
#'@importFrom lmom cdfglo cdfgam cdfpe3 pelglo pelgam pelpe3
#'@importFrom SPEI parglo.maxlik
#'@importFrom stats qnorm sd window
#'@export
PeriodStandardization <- function(data, data_cor = NULL, dates = NULL, 
                                  time_dim = 'syear', leadtime_dim = 'time', 
                                  memb_dim = 'ensemble', 
                                  ref_period = NULL, handle_infinity = FALSE, 
                                  method = 'parametric', 
                                  distribution = 'log-Logistic', 
                                  params = NULL, return_params = FALSE, 
                                  na.rm = FALSE, ncores = NULL) {
  # Check inputs
  ## data
  if (!is.array(data)) {
    stop("Parameter 'data' must be a numeric array.")
  }
  if (is.null(names(dim(data)))) {
    stop("Parameter 'data' must have dimension names.")
  }
  ## data_cor
  if (!is.null(data_cor)) {
    if (!is.array(data_cor)) {
      stop("Parameter 'data_cor' must be a numeric array.")
    }
    if (is.null(names(dim(data_cor)))) {
      stop("Parameter 'data_cor' must have dimension names.")
    }
  }
  ## dates
  if (!is.null(dates)) {
    if (!any(inherits(dates, 'Date'), inherits(dates, 'POSIXct'))) {
      stop("Parameter 'dates' is not of the correct class, ", 
          "only 'Date' and 'POSIXct' classes are accepted.")
    }
    if (!time_dim %in% names(dim(dates)) | !leadtime_dim %in% names(dim(dates))) {
      stop("Parameter 'dates' must have 'time_dim' and 'leadtime_dim' ",
           "dimension.")
    }
    if (dim(data)[c(time_dim)] != dim(dates)[c(time_dim)]) {
      stop("Parameter 'dates' needs to have the same length of 'time_dim' ", 
           "as 'data'.")
    }
  }
  ## time_dim
  if (!is.character(time_dim) | length(time_dim) != 1) {
    stop("Parameter 'time_dim' must be a character string.")
  }
  if (!time_dim %in% names(dim(data))) {
    stop("Parameter 'time_dim' is not found in 'data' dimension.")
  }
  if (!is.null(data_cor)) {
    if (!time_dim %in% names(dim(data_cor))) {
      stop("Parameter 'time_dim' is not found in 'data_cor' dimension.")
    } 
  }
  ## leadtime_dim
  if (!is.character(leadtime_dim) | length(leadtime_dim) != 1) {
    stop("Parameter 'leadtime_dim' must be a character string.")
  }
  if (!leadtime_dim %in% names(dim(data))) {
    stop("Parameter 'leadtime_dim' is not found in 'data' dimension.")
  }
  if (!is.null(data_cor)) {
    if (!leadtime_dim %in% names(dim(data_cor))) {
      stop("Parameter 'leadtime_dim' is not found in 'data_cor' dimension.")
    } 
  }
  ## memb_dim
  if (!is.character(memb_dim) | length(memb_dim) != 1) {
    stop("Parameter 'memb_dim' must be a character string.")
  }
  if (!memb_dim %in% names(dim(data))) {
    stop("Parameter 'memb_dim' is not found in 'data' dimension.")
  }
  if (!is.null(data_cor)) {
    if (!memb_dim %in% names(dim(data_cor))) {
      stop("Parameter 'memb_dim' is not found in 'data_cor' dimension.")
    } 
  }
  ## data_cor (2)
  if (!is.null(data_cor)) {
    if (dim(data)[leadtime_dim] != dim(data_cor)[leadtime_dim]) {
      stop("Parameter 'data' and 'data_cor' have dimension 'leadtime_dim' ", 
           "of different length.")
    }
  }
  ## ref_period
  if (!is.null(ref_period)) {
    years_dates <- format(dates, "%Y")
    if (is.null(dates)) {
      warning("Parameter 'dates' is not provided so 'ref_period' can't be ", 
              "used.")
      ref_period <- NULL
    } else if (length(ref_period) != 2) {
      warning("Parameter 'ref_period' must be of length two indicating the ",
              "first and end years of the reference period. It will not ", 
              "be used.")
      ref_period <- NULL
    } else if (!all(sapply(ref_period, is.numeric))) {
      warning("Parameter 'ref_period' must be a numeric vector indicating the ", 
              "'start' and 'end' years of the reference period. It will not ",
              "be used.")
      ref_period <- NULL
    } else if (ref_period[[1]] > ref_period[[2]]) {
      warning("In parameter 'ref_period' 'start' cannot be after 'end'. It ",
              "will not be used.")
      ref_period <- NULL
    } else if (!all(unlist(ref_period) %in% years_dates)) {
      warning("Parameter 'ref_period' contains years outside the dates. ", 
              "It will not be used.")
      ref_period <- NULL
    } else {
      years <- format(Subset(dates, along = leadtime_dim, indices = 1), "%Y")
      ref_period[[1]] <- which(ref_period[[1]] == years)
      ref_period[[2]] <- which(ref_period[[2]] == years)
    }
  }
  ## handle_infinity
  if (!is.logical(handle_infinity)) {
    stop("Parameter 'handle_infinity' must be a logical value.")
  }
  ## method
  if (!(method %in% c('parametric', 'non-parametric'))) {
    stop("Parameter 'method' must be a character string containing one of ", 
         "the following methods: 'parametric' or 'non-parametric'.")
  }
  ## distribution
  if (!(distribution %in% c('log-Logistic', 'Gamma', 'PearsonIII'))) {
    stop("Parameter 'distribution' must be a character string containing one ", 
         "of the following distributions: 'log-Logistic', 'Gamma' or ", 
         "'PearsonIII'.")
  }
  ## params
  if (!is.null(params)) {
    if (!is.numeric(params)) {
      stop("Parameter 'params' must be numeric.")
    }
    if (!all(c(time_dim, leadtime_dim, 'coef') %in% names(dim(params)))) {
      stop("Parameter 'params' must be a multidimensional array with named ", 
           "dimensions: '", time_dim, "', '", leadtime_dim, "' and 'coef'.")
    }
    dims_data <- dim(data)[-which(names(dim(data)) == memb_dim)]
    dims_params <- dim(params)[-which(names(dim(params)) == 'coef')]
    if (!all(dims_data == dims_params)) {
      stop("Parameter 'data' and 'params' must have same common dimensions ", 
           "except 'memb_dim' and 'coef'.")
    }

    if (distribution == "Gamma") {
      if (dim(params)['coef'] != 2) {
        stop("For '", distribution, "' distribution, params array should have ",
             "'coef' dimension of length 2.")
      }
    } else {
      if (dim(params)['coef'] != 3) {
        stop("For '", distribution, "' distribution, params array should have ",
             "'coef' dimension of length 3.")
      }
    }
  }
  ## return_params 
  if (!is.logical(return_params)) {
    stop("Parameter 'return_params' must be logical.")
  }
  ## na.rm
  if (!is.logical(na.rm)) {
    stop("Parameter 'na.rm' must be logical.")
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | any(ncores %% 1 != 0) | any(ncores < 0) |
        length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  }

  if (is.null(ref_period)) {
    ref_start <- NULL
    ref_end <- NULL
  } else {
    ref_start <- ref_period[[1]]
    ref_end <- ref_period[[2]]
  }

  # Standardization
  if (is.null(data_cor)) {
    if (is.null(params)) {
      res <- Apply(data = list(data), 
                   target_dims = c(leadtime_dim, time_dim, memb_dim),
                   fun = .standardization, data_cor = NULL, params = NULL, 
                   leadtime_dim = leadtime_dim, time_dim = time_dim, 
                   ref_start = ref_start, ref_end = ref_end, 
                   handle_infinity = handle_infinity,
                   method = method, distribution = distribution, 
                   return_params = return_params,
                   na.rm = na.rm, ncores = ncores)
    } else {
      res <- Apply(data = list(data = data, params = params), 
                   target_dims = list(data = c(leadtime_dim, time_dim, memb_dim), 
                                      params = c(leadtime_dim, time_dim, 'coef')),
                   fun = .standardization, data_cor = NULL, 
                   leadtime_dim = leadtime_dim, time_dim = time_dim, 
                   ref_start = ref_start, ref_end = ref_end,
                   handle_infinity = handle_infinity,
                   method = method, distribution = distribution, 
                   return_params = return_params, 
                   na.rm = na.rm, ncores = ncores)
    }
  } else {
    res <- Apply(data = list(data = data, data_cor = data_cor), 
                 target_dims = c(leadtime_dim, time_dim, memb_dim),
                 fun = .standardization, params = NULL, 
                 leadtime_dim = leadtime_dim, time_dim = time_dim, 
                 ref_start = ref_start, ref_end = ref_end,
                 handle_infinity = handle_infinity,
                 method = method, distribution = distribution, 
                 return_params = return_params, 
                 na.rm = na.rm, ncores = ncores)
  }
  if (return_params) {
    spei <- res$spei
    params <- res$params
  } else {
    spei <- res$output1
  }

  if (is.null(data_cor)) {
    pos <- match(names(dim(data)), names(dim(spei)))
    spei <- aperm(spei, pos)
  } else {
    pos <- match(names(dim(data_cor)), names(dim(spei)))
    spei <- aperm(spei, pos)
  }

  if (return_params) {
    pos <- match(c(names(dim(spei))[-which(names(dim(spei)) == memb_dim)], 'coef'), 
                 names(dim(params)))
    params <- aperm(params, pos)
    return(list('spei' = spei, 'params' = params))
  } else {
    return(spei)
  }
}

.standardization <- function(data, data_cor = NULL, params = NULL, 
                             leadtime_dim = 'time', time_dim = 'syear', 
                             ref_start = NULL, ref_end = NULL, handle_infinity = FALSE, 
                             method = 'parametric', distribution = 'log-Logistic',
                             return_params = FALSE, na.rm = FALSE) {
  # data (data_cor): [leadtime_dim, time_dim, memb_dim]
  dims <- dim(data)[-1]
  fit = 'ub-pwm'

  coef = switch(distribution,
                "Gamma" = array(NA, dim = 2, dimnames = list(c('alpha', 'beta'))),
                "log-Logistic" = array(NA, dim = 3, dimnames = list(c('xi', 'alpha', 'kappa'))),
                "PearsonIII" = array(NA, dim = 3, dimnames = list(c('mu', 'sigma', 'gamma'))))

  if (is.null(data_cor)) {
    # cross_val = TRUE
    spei_mod <- data*NA
    if (return_params) {
      params_result <- array(dim = c(dim(data)[-length(dim(data))], coef = length(coef)))
    }
    for (ff in 1:dim(data)[leadtime_dim]) {
      data2 <- data[ff, , ]
      dim(data2) <- dims
      if (method == 'non-parametric') {
        bp <- matrix(0, length(data2), 1)
        for (i in 1:length(data2)) {
          bp[i,1] = sum(data2[] <= data2[i], na.rm = na.rm);  # Writes the rank of the data
        }
        std_index <- qnorm((bp - 0.44)/(length(data2) + 0.12))
        dim(std_index) <- dims
        spei_mod[ff, , ] <- std_index
      } else {
        if (!is.null(ref_start) && !is.null(ref_end)) {
          data_fit <- window(data2, ref_start, ref_end)	
        } else {
          data_fit <- data2
        }
        for (nsd in 1:dim(data)[time_dim]) {
          if (is.null(params)) {
            acu <- as.vector(data_fit[-nsd, ])
            if (na.rm) {
              acu_sorted <- sort.default(acu, method = "quick")
            } else {
              acu_sorted <- sort.default(acu, method = "quick", na.last = TRUE)
            }
            f_params <- NA
            if (!any(is.na(acu_sorted)) & length(acu_sorted) != 0) {
              acu_sd <- sd(acu_sorted)
              if (!is.na(acu_sd) & acu_sd != 0) {
                if (distribution != "log-Logistic") {
                  acu_sorted <- acu_sorted[acu_sorted > 0]
                }
                if (length(acu_sorted) >= 4) {
                  f_params <- .std(data = acu_sorted, fit = fit, 
                                   distribution = distribution)
                }
              }
            }
          } else {
            f_params <- params[ff, nsd, ]
          }
          if (all(is.na(f_params))) {
            cdf_res <- NA
          } else {
            f_params <- f_params[which(!is.na(f_params))]
            cdf_res = switch(distribution,
                             "log-Logistic" = lmom::cdfglo(data2, f_params),
                             "Gamma" = lmom::cdfgam(data2, f_params),
                             "PearsonIII" = lmom::cdfpe3(data2, f_params))
          }
          std_index_cv <- array(qnorm(cdf_res), dim = dims)
          spei_mod[ff, nsd, ] <- std_index_cv[nsd, ]
          if (return_params) params_result[ff, nsd, ] <- f_params
        }
      }
    }
  } else {
    # cross_val = FALSE
    spei_mod <- data_cor*NA
    dimscor <- dim(data_cor)[-1]
    if (return_params) {
      params_result <- array(dim = c(dim(data_cor)[-length(dim(data_cor))], coef = length(coef)))
    }
    for (ff in 1:dim(data)[leadtime_dim]) {
      data_cor2 <- data_cor[ff, , ]
      dim(data_cor2) <- dimscor
      if (method == 'non-parametric') {
        bp <- matrix(0, length(data_cor2), 1)
        for (i in 1:length(data_cor2)) {
          bp[i,1] = sum(data_cor2[] <= data_cor2[i], na.rm = na.rm);  # Writes the rank of the data
        }
        std_index <- qnorm((bp - 0.44)/(length(data_cor2) + 0.12))
        dim(std_index) <- dimscor
        spei_mod[ff, , ] <- std_index
      } else {
        data2 <- data[ff, , ]
        dim(data2) <- dims
        if (!is.null(ref_start) && !is.null(ref_end)) {
          data_fit <- window(data2, ref_start, ref_end)	
        } else {
          data_fit <- data2
        }
        acu <- as.vector(data_fit)
        if (na.rm) {
          acu_sorted <- sort.default(acu, method = "quick")
        } else {
          acu_sorted <- sort.default(acu, method = "quick", na.last = TRUE)
        }
        if (!any(is.na(acu_sorted)) & length(acu_sorted) != 0) {
          acu_sd <- sd(acu_sorted)
          if (!is.na(acu_sd) & acu_sd != 0) {
            if (distribution != "log-Logistic") {
              acu_sorted <- acu_sorted[acu_sorted > 0]
            }
            if (length(acu_sorted) >= 4) {
              f_params <- .std(data = acu_sorted, fit = fit, 
                               distribution = distribution)
            }
            if (all(is.na(f_params))) {
              cdf_res <- NA
            } else {
              f_params <- f_params[which(!is.na(f_params))]
              cdf_res = switch(distribution,
                              "log-Logistic" = lmom::cdfglo(data_cor2, f_params),
                              "Gamma" = lmom::cdfgam(data_cor2, f_params),
                              "PearsonIII" = lmom::cdfpe3(data_cor2, f_params))
            }
            std_index_cv <- array(qnorm(cdf_res), dim = dimscor)
            spei_mod[ff, , ] <- std_index_cv
            if (return_params) params_result[ff, , ] <- f_params
          }
        }
      }
    }
  }
  if (handle_infinity) { 
    # could also use "param_error" ?; we are giving it the min/max value of the grid point
    spei_mod[is.infinite(spei_mod) & spei_mod < 0] <- min(spei_mod[!is.infinite(spei_mod)],na.rm = TRUE)
    spei_mod[is.infinite(spei_mod) & spei_mod > 0] <- max(spei_mod[!is.infinite(spei_mod)],na.rm = TRUE) 
  }
  if (return_params) {
    return(list(spei = spei_mod, params = params_result))
  } else {
    return(spei_mod)
  }
}

.std <- function(data, fit = 'pp-pwm', distribution = 'log-Logistic') {
  pwm = switch(fit,
               'pp-pwm' = lmomco::pwm.pp(data, -0.35, 0, nmom = 3),
               lmomco::pwm.ub(data, nmom = 3)
               # TLMoments::PWM(data, order = 0:2)
               )
  lmom <- lmomco::pwm2lmom(pwm) 
  if (!any(!lmomco::are.lmom.valid(lmom), anyNA(lmom[[1]]), any(is.nan(lmom[[1]])))) {
    fortran_vec = c(lmom$lambdas[1:2], lmom$ratios[3])
    params_result = switch(distribution,
                    'log-Logistic' = tryCatch(lmom::pelglo(fortran_vec), 
                                              error = function(e){lmomco::parglo(lmom)$para}),
                    'Gamma' = tryCatch(lmom::pelgam(fortran_vec), 
                                       error = function(e){lmomco::pargam(lmom)$para}),
                    'PearsonIII' = tryCatch(lmom::pelpe3(fortran_vec), 
                                            error = function(e){lmomco::parpe3(lmom)$para}))
    if (distribution == 'log-Logistic' && fit == 'max-lik') {
      params_result = SPEI::parglo.maxlik(data, params_result)$para
    }
    return(params_result)
  } else {
    return(NA)
  }
}
