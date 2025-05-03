#'Forecast Calibration 
#'
#'@author Verónica Torralba, \email{veronica.torralba@bsc.es} 
#'@author Bert Van Schaeybroeck, \email{bertvs@meteo.be}
#'@description Five types of member-by-member bias correction can be performed. 
#'The \code{"bias"} method corrects the bias only, the \code{"evmos"} method 
#'applies a variance inflation technique to ensure the correction of the bias 
#'and the correspondence of variance between forecast and observation (Van 
#'Schaeybroeck and Vannitsem, 2011). The ensemble calibration methods 
#'\code{"mse_min"} and \code{"crps_min"} correct the bias, the overall forecast 
#'variance and the ensemble spread as described in Doblas-Reyes et al. (2005) 
#'and Van Schaeybroeck and Vannitsem (2015), respectively. While the 
#'\code{"mse_min"} method minimizes a constrained mean-squared error using three 
#'parameters, the \code{"crps_min"} method features four parameters and 
#'minimizes the Continuous Ranked Probability Score (CRPS). The 
#'\code{"rpc-based"} method adjusts the forecast variance ensuring that the 
#'ratio of predictable components (RPC) is equal to one, as in Eade et al. 
#'(2014). It is equivalent to function \code{Calibration} but for objects 
#'of class \code{s2dv_cube}.
#'
#'@param exp An object of class \code{s2dv_cube} as returned by \code{CST_Start} 
#'  function with at least 'sdate' and 'member' dimensions, containing the  
#'  seasonal hindcast experiment data in the element named \code{data}. The 
#'  hindcast is used to calibrate the forecast in case the forecast is provided; 
#'  if not, the same hindcast will be calibrated instead.
#'@param obs An object of class \code{s2dv_cube} as returned by \code{CST_Start} 
#'  function with at least 'sdate' dimension, containing the observed data in 
#'  the element named \code{$data}.
#'@param exp_cor An optional object of class \code{s2dv_cube} as returned by 
#'  \code{CST_Start} function with at least 'sdate' and 'member' dimensions, 
#'  containing the seasonal forecast experiment data in the element named 
#'  \code{data}. If the forecast is provided, it will be calibrated using the 
#'  hindcast and observations; if not, the hindcast will be calibrated instead. 
#'  If there is only one corrected dataset, it should not have dataset dimension. 
#'  If there is a corresponding corrected dataset for each 'exp' forecast, the 
#'  dataset dimension must have the same length as in 'exp'. The default value 
#'  is NULL.
#'@param cal.method A character string indicating the calibration method used, 
#'  can be either \code{bias}, \code{evmos}, \code{mse_min}, \code{crps_min} or 
#'  \code{rpc-based}. Default value is \code{mse_min}.
#'@param eval.method A character string indicating the sampling method used, it 
#'  can be either \code{in-sample} or \code{leave-one-out}. Default value is the 
#'  \code{leave-one-out} cross validation. In case the forecast is provided, any 
#'  chosen eval.method is over-ruled and a third option is used.
#'@param multi.model A boolean that is used only for the \code{mse_min} 
#'  method. If multi-model ensembles or ensembles of different sizes are used, 
#'  it must be set to \code{TRUE}. By default it is \code{FALSE}. Differences 
#'  between the two approaches are generally small but may become large when 
#'  using small ensemble sizes. Using multi.model when the calibration method is 
#'  \code{bias}, \code{evmos} or \code{crps_min} will not affect the result.
#'@param na.fill A boolean that indicates what happens in case calibration is 
#'  not possible or will yield unreliable results. This happens when three or 
#'  less forecasts-observation pairs are available to perform the training phase 
#'  of the calibration. By default \code{na.fill} is set to true such that NA 
#'  values will be returned. If \code{na.fill} is set to false, the uncorrected 
#'  data will be returned. 
#'@param na.rm A boolean that indicates whether to remove the NA values or not. 
#'  The default value is \code{TRUE}. See Details section for further 
#'  information about its use and compatibility with \code{na.fill}.
#'@param apply_to A character string that indicates whether to apply the 
#'  calibration to all the forecast (\code{"all"}) or only to those where the 
#'  correlation between the ensemble mean and the observations is statistically
#'  significant (\code{"sign"}). Only useful if \code{cal.method == "rpc-based"}.
#'@param alpha A numeric value indicating the significance level for the 
#'  correlation test. Only useful if \code{cal.method == "rpc-based" & apply_to 
#'  == "sign"}.
#'@param memb_dim A character string indicating the name of the member dimension.
#'  By default, it is set to 'member'.
#'@param sdate_dim A character string indicating the name of the start date 
#'  dimension. By default, it is set to 'sdate'.
#'@param dat_dim A character string indicating the name of dataset dimension. 
#'  The length of this dimension can be different between 'exp' and 'obs'. 
#'  The default value is NULL.
#'@param ncores An integer that indicates the number of cores for parallel 
#'  computations using multiApply function. The default value is one.
#' 
#'@return An object of class \code{s2dv_cube} containing the calibrated 
#'forecasts in the element \code{data} with the dimensions nexp, nobs and same 
#'dimensions as in the 'exp' object. nexp is the number of experiment 
#'(i.e., 'dat_dim' in exp), and nobs is the number of observation (i.e., 
#''dat_dim' in obs). If dat_dim is NULL, nexp and nobs are omitted. If 'exp_cor' 
#'is provided the returned array will be with the same dimensions as 'exp_cor'.
#'
#'@details Both the \code{na.fill} and \code{na.rm} parameters can be used to 
#'indicate how the function has to handle the NA values. The \code{na.fill} 
#'parameter checks whether there are more than three forecast-observations pairs 
#'to perform the computation. In case there are three or less pairs, the 
#'computation is not carried out, and the value returned by the function depends 
#'on the value of this parameter (either NA if \code{na.fill == TRUE} or the 
#'uncorrected value if \code{na.fill == TRUE}). On the other hand, \code{na.rm} 
#'is used to indicate the function whether to remove the missing values during 
#'the computation of the parameters needed to perform the calibration.
#' 
#'@references Doblas-Reyes F.J, Hagedorn R, Palmer T.N. The rationale behind the 
#'success of multi-model ensembles in seasonal forecasting-II calibration and 
#'combination. Tellus A. 2005;57:234-252. \doi{10.1111/j.1600-0870.2005.00104.x}
#'@references Eade, R., Smith, D., Scaife, A., Wallace, E., Dunstone, N., 
#'Hermanson, L., & Robinson, N. (2014). Do seasonal-to-decadal climate 
#'predictions underestimate the predictability of the read world? Geophysical 
#'Research Letters, 41(15), 5620-5628. \doi{10.1002/2014GL061146}
#'@references Van Schaeybroeck, B., & Vannitsem, S. (2011). Post-processing 
#'through linear regression. Nonlinear Processes in Geophysics, 18(2), 
#'147. \doi{10.5194/npg-18-147-2011}
#'@references Van Schaeybroeck, B., & Vannitsem, S. (2015). Ensemble 
#'post-processing using member-by-member approaches: theoretical aspects. 
#'Quarterly Journal of the Royal Meteorological Society, 141(688), 807-818.  
#'\doi{10.1002/qj.2397}
#' 
#'@seealso \code{\link{CST_Start}}
#' 
#'@examples
#'# Example 1:
#'mod1 <- 1 : (1 * 3 * 4 * 5 * 6 * 7)
#'dim(mod1) <- c(dataset = 1, member = 3, sdate = 4, ftime = 5, lat = 6, lon = 7)
#'obs1 <- 1 : (1 * 1 * 4 * 5 * 6 * 7)
#'dim(obs1) <- c(dataset = 1, member = 1, sdate = 4, ftime = 5, lat = 6, lon = 7)
#'lon <- seq(0, 30, 5)
#'lat <- seq(0, 25, 5)
#'coords <- list(lat = lat, lon = lon)
#'exp <- list(data = mod1, coords = coords)
#'obs <- list(data = obs1, coords = coords)
#'attr(exp, 'class') <- 's2dv_cube'
#'attr(obs, 'class') <- 's2dv_cube'
#'a <- CST_Calibration(exp = exp, obs = obs, cal.method = "mse_min", eval.method = "in-sample")
#'
#'# Example 2:
#'mod1 <- 1 : (1 * 3 * 4 * 5 * 6 * 7)
#'mod2 <- 1 : (1 * 3 * 1 * 5 * 6 * 7)
#'dim(mod1) <- c(dataset = 1, member = 3, sdate = 4, ftime = 5, lat = 6, lon = 7)
#'dim(mod2) <- c(dataset = 1, member = 3, sdate = 1, ftime = 5, lat = 6, lon = 7)
#'obs1 <- 1 : (1 * 1 * 4 * 5 * 6 * 7)
#'dim(obs1) <- c(dataset = 1, member = 1, sdate = 4, ftime = 5, lat = 6, lon = 7)
#'lon <- seq(0, 30, 5)
#'lat <- seq(0, 25, 5)
#'coords <- list(lat = lat, lon = lon)
#'exp <- list(data = mod1, coords = coords)
#'obs <- list(data = obs1, coords = coords)
#'exp_cor <- list(data = mod2, lat = lat, lon = lon)
#'attr(exp, 'class') <- 's2dv_cube'
#'attr(obs, 'class') <- 's2dv_cube'
#'attr(exp_cor, 'class') <- 's2dv_cube'
#'a <- CST_Calibration(exp = exp, obs = obs, exp_cor = exp_cor, cal.method = "evmos")
#' 
#'@importFrom s2dv InsertDim Reorder
#'@import multiApply
#'@importFrom ClimProjDiags Subset
#'@export
CST_Calibration <- function(exp, obs, exp_cor = NULL, cal.method = "mse_min", 
                            eval.method = "leave-one-out", multi.model = FALSE, 
                            na.fill = TRUE, na.rm = TRUE, apply_to = NULL, 
                            alpha = NULL, memb_dim = 'member', sdate_dim = 'sdate', 
                            dat_dim = NULL, ncores = NULL) {
  # Check 's2dv_cube'
  if (!inherits(exp, "s2dv_cube") || !inherits(obs, "s2dv_cube")) {
        stop("Parameter 'exp' and 'obs' must be of the class 's2dv_cube'.")
  }
  if (!is.null(exp_cor)) {
    if (!inherits(exp_cor, "s2dv_cube")) {
        stop("Parameter 'exp_cor' must be of the class 's2dv_cube'.")
    }
  }

  Calibration <- Calibration(exp = exp$data, obs = obs$data, exp_cor = exp_cor$data, 
                             cal.method = cal.method, eval.method = eval.method, 
                             multi.model =  multi.model, na.fill = na.fill, 
                             na.rm = na.rm, apply_to = apply_to, alpha = alpha, 
                             memb_dim = memb_dim, sdate_dim = sdate_dim, 
                             dat_dim = dat_dim, ncores = ncores)

  if (is.null(exp_cor)) {
    exp$data <- Calibration
    exp$attrs$Datasets <- c(exp$attrs$Datasets, obs$attrs$Datasets)
    exp$attrs$source_files <- c(exp$attrs$source_files, obs$attrs$source_files)
    
    return(exp)

  } else {
    exp_cor$data <- Calibration
    exp_cor$attrs$Datasets <- c(exp_cor$attrs$Datasets, exp$attrs$Datasets, obs$attrs$Datasets)
    exp_cor$attrs$source_files <- c(exp_cor$attrs$source_files, exp$attrs$source_files, obs$attrs$source_files)

    return(exp_cor)
  } 
}

#'Forecast Calibration 
#'
#'@author Verónica Torralba, \email{veronica.torralba@bsc.es} 
#'@author Bert Van Schaeybroeck, \email{bertvs@meteo.be}
#'@description Five types of member-by-member bias correction can be performed. 
#'The \code{"bias"} method corrects the bias only, the \code{"evmos"} method 
#'applies a variance inflation technique to ensure the correction of the bias 
#'and the correspondence of variance between forecast and observation (Van 
#'Schaeybroeck and Vannitsem, 2011). The ensemble calibration methods 
#'\code{"mse_min"} and \code{"crps_min"} correct the bias, the overall forecast 
#'variance and the ensemble spread as described in Doblas-Reyes et al. (2005) 
#'and Van Schaeybroeck and Vannitsem (2015), respectively. While the 
#'\code{"mse_min"} method minimizes a constrained mean-squared error using three 
#'parameters, the \code{"crps_min"} method features four parameters and 
#'minimizes the Continuous Ranked Probability Score (CRPS). The 
#'\code{"rpc-based"} method adjusts the forecast variance ensuring that the 
#'ratio of predictable components (RPC) is equal to one, as in Eade et al. 
#'(2014). Both in-sample or our out-of-sample (leave-one-out cross 
#'validation) calibration are possible.
#'
#'@param exp A multidimensional array with named dimensions (at least 'sdate' 
#'  and 'member') containing the seasonal hindcast experiment data. The hindcast 
#'  is used to calibrate the forecast in case the forecast is provided; if not, 
#'  the same hindcast will be calibrated instead.
#'@param obs A multidimensional array with named dimensions (at least 'sdate') 
#'  containing the observed data.
#'@param exp_cor An optional multidimensional array with named dimensions (at 
#'  least 'sdate' and 'member') containing the seasonal forecast experiment 
#'  data. If the forecast is provided, it will be calibrated using the hindcast 
#'  and observations; if not, the hindcast will be calibrated instead. If there  
#'  is only one corrected dataset, it should not have dataset dimension. If there 
#'  is a corresponding corrected dataset for each 'exp' forecast, the dataset 
#'  dimension must have the same length as in 'exp'. The default value is NULL.
#'@param cal.method A character string indicating the calibration method used, 
#'  can be either \code{bias}, \code{evmos}, \code{mse_min}, \code{crps_min} 
#'  or \code{rpc-based}. Default value is \code{mse_min}.
#'@param eval.method A character string indicating the sampling method used, 
#'  can be either \code{in-sample} or \code{leave-one-out}. Default value is 
#'  the \code{leave-one-out} cross validation. In case the forecast is 
#'  provided, any chosen eval.method is over-ruled and a third option is 
#'  used.
#'@param multi.model A boolean that is used only for the \code{mse_min} 
#'  method. If multi-model ensembles or ensembles of different sizes are used, 
#'  it must be set to \code{TRUE}. By default it is \code{FALSE}. Differences 
#'  between the two approaches are generally small but may become large when 
#'  using small ensemble sizes. Using multi.model when the calibration method 
#'  is \code{bias}, \code{evmos} or \code{crps_min} will not affect the result.
#'@param na.fill A boolean that indicates what happens in case calibration is 
#'  not possible or will yield unreliable results. This happens when three or 
#'  less forecasts-observation pairs are available to perform the training phase
#'  of the calibration. By default \code{na.fill} is set to true such that NA 
#'  values will be returned. If \code{na.fill} is set to false, the uncorrected 
#'  data will be returned. 
#'@param na.rm A boolean that indicates whether to remove the NA values or 
#'  not. The default value is \code{TRUE}.
#'@param apply_to A character string that indicates whether to apply the 
#'  calibration to all the forecast (\code{"all"}) or only to those where the 
#'  correlation between the ensemble mean and the observations is statistically 
#'  significant (\code{"sign"}). Only useful if \code{cal.method == "rpc-based"}.
#'@param alpha A numeric value indicating the significance level for the 
#'  correlation test. Only useful if \code{cal.method == "rpc-based" & apply_to == 
#'  "sign"}.
#'@param memb_dim A character string indicating the name of the member 
#'  dimension. By default, it is set to 'member'.
#'@param sdate_dim A character string indicating the name of the start date 
#'  dimension. By default, it is set to 'sdate'.
#'@param dat_dim A character string indicating the name of dataset dimension. 
#'  The length of this dimension can be different between 'exp' and 'obs'. 
#'  The default value is NULL.
#'@param ncores An integer that indicates the number of cores for parallel 
#'  computation using multiApply function. The default value is NULL (one core).
#' 
#'@return An array containing the calibrated forecasts with the dimensions 
#'nexp, nobs and same dimensions as in the 'exp' array. nexp is the number of 
#'experiment (i.e., 'dat_dim' in exp), and nobs is the number of observation 
#'(i.e., 'dat_dim' in obs). If dat_dim is NULL, nexp and nobs are omitted. 
#'If 'exp_cor' is provided the returned array will be with the same dimensions as 
#''exp_cor'.
#' 
#'@details Both the \code{na.fill} and \code{na.rm} parameters can be used to 
#'indicate how the function has to handle the NA values. The \code{na.fill} 
#'parameter checks whether there are more than three forecast-observations pairs 
#'to perform the computation. In case there are three or less pairs, the 
#'computation is not carried out, and the value returned by the function depends 
#'on the value of this parameter (either NA if \code{na.fill == TRUE} or the 
#'uncorrected value if \code{na.fill == TRUE}). On the other hand, \code{na.rm} 
#'is used to indicate the function whether to remove the missing values during 
#'the computation of the parameters needed to perform the calibration.
#'
#'@references Doblas-Reyes F.J, Hagedorn R, Palmer T.N. The rationale behind the 
#'success of multi-model ensembles in seasonal forecasting-II calibration and 
#'combination. Tellus A. 2005;57:234-252. doi:10.1111/j.1600-0870.2005.00104.x
#'@references Eade, R., Smith, D., Scaife, A., Wallace, E., Dunstone, N., 
#'Hermanson, L., & Robinson, N. (2014). Do seasonal-to-decadal climate 
#'predictions underestimate the predictability of the read world? Geophysical 
#'Research Letters, 41(15), 5620-5628. \doi{10.1002/2014GL061146}
#'@references Van Schaeybroeck, B., & Vannitsem, S. (2011). Post-processing 
#'through linear regression. Nonlinear Processes in Geophysics, 18(2), 
#'147. \doi{10.5194/npg-18-147-2011}
#'@references Van Schaeybroeck, B., & Vannitsem, S. (2015). Ensemble 
#'post-processing using member-by-member approaches: theoretical aspects. 
#'Quarterly Journal of the Royal Meteorological Society, 141(688), 807-818.  
#'\doi{10.1002/qj.2397}
#' 
#'@seealso \code{\link{CST_Start}}
#' 
#'@examples
#'mod1 <- 1 : (1 * 3 * 4 * 5 * 6 * 7)
#'dim(mod1) <- c(dataset = 1, member = 3, sdate = 4, ftime = 5, lat = 6, lon = 7)
#'obs1 <- 1 : (1 * 1 * 4 * 5 * 6 * 7)
#'dim(obs1) <- c(dataset = 1, member = 1, sdate = 4, ftime = 5, lat = 6, lon = 7)
#'a <- Calibration(exp = mod1, obs = obs1)
#' 
#'@importFrom s2dv InsertDim Reorder
#'@import multiApply
#'@importFrom ClimProjDiags Subset
#'@export
Calibration <- function(exp, obs, exp_cor = NULL, 
                        cal.method = "mse_min", eval.method = "leave-one-out",  
                        multi.model = FALSE, na.fill = TRUE, 
                        na.rm = TRUE, apply_to = NULL, alpha = NULL,
                        memb_dim = 'member', sdate_dim = 'sdate', dat_dim = NULL, 
                        ncores = NULL) {

  # Check inputs
  ## exp, obs
  if (!is.array(exp) || !is.numeric(exp)) {
    stop("Parameter 'exp' must be a numeric array.")
  }
  if (!is.array(obs) || !is.numeric(obs)) {
    stop("Parameter 'obs' must be a numeric array.")
  }
  expdims <- names(dim(exp))
  obsdims <- names(dim(obs))
  if (is.null(expdims)) {
    stop("Parameter 'exp' must have dimension names.")
  }
  if (is.null(obsdims)) {
    stop("Parameter 'obs' must have dimension names.")
  }
  if (any(is.na(exp)))  {
    warning("Parameter 'exp' contains NA values.")
  }
  if (any(is.na(obs))) {
    warning("Parameter 'obs' contains NA values.")
  }
  ## exp_cor
  if (!is.null(exp_cor)) {
    # if exp_cor is provided, it will be calibrated: "calibrate forecast instead of hindcast"
    # if exp_cor is provided, eval.method is overruled (because if exp_cor is provided, the 
    # train data will be all data of "exp" and the evalutaion data will be all data of "exp_cor"; 
    # no need for "leave-one-out" or "in-sample")
    eval.method <- "hindcast-vs-forecast"
    expcordims <- names(dim(exp_cor))
    if (is.null(expcordims)) {
      stop("Parameter 'exp_cor' must have dimension names.")
    }
    if (any(is.na(exp_cor)))  {
      warning("Parameter 'exp_cor' contains NA values.")
    }
  }
  ## dat_dim
  if (!is.null(dat_dim)) {
    if (!is.character(dat_dim) | length(dat_dim) > 1) {
      stop("Parameter 'dat_dim' must be a character string.")
    }
    if (!dat_dim %in% names(dim(exp)) | !dat_dim %in% names(dim(obs))) {
      stop("Parameter 'dat_dim' is not found in 'exp' or 'obs' dimension.",
           " Set it as NULL if there is no dataset dimension.")
    }
  }
  ## sdate_dim and memb_dim
  if (!is.character(sdate_dim)) {
    stop("Parameter 'sdate_dim' should be a character string indicating the",
         "name of the dimension where start dates are stored in 'exp'.")   
  }
  if (length(sdate_dim) > 1) {
    sdate_dim <- sdate_dim[1]
    warning("Parameter 'sdate_dim' has length greater than 1 and only",
            " the first element will be used.")
  }
  if (!is.character(memb_dim)) {
    stop("Parameter 'memb_dim' should be a character string indicating the",
         "name of the dimension where members are stored in 'exp'.")
  }
  if (length(memb_dim) > 1) {
    memb_dim <- memb_dim[1]
    warning("Parameter 'memb_dim' has length greater than 1 and only",
            " the first element will be used.")
  }

  target_dims_exp <- c(memb_dim, sdate_dim, dat_dim)
  target_dims_obs <- c(sdate_dim, dat_dim)
  
  if (!all(target_dims_exp %in% expdims)) {
    stop("Parameter 'exp' requires 'sdate_dim' and 'memb_dim' dimensions.")
  }
  if (!all(target_dims_obs %in% obsdims)) {
    stop("Parameter 'obs' must have the dimension defined in sdate_dim ",
         "parameter.")
  }
  if (memb_dim %in% obsdims) {
    if (dim(obs)[memb_dim] != 1) {
      warning("Parameter 'obs' has dimension 'memb_dim' with length larger",
              " than 1. Only the first member dimension will be used.")
    }
    obs <- Subset(obs, along = memb_dim, indices = 1, drop = "selected")
  }
  if (!is.null(exp_cor)) {
    if (!memb_dim %in% names(dim(exp_cor))) {
      exp_cor <- InsertDim(exp_cor, posdim = 1, lendim = 1, name = memb_dim)
      exp_cor_remove_memb <- TRUE
    } else {
      exp_cor_remove_memb <- FALSE
    }
  } else {
    exp_cor_remove_memb <- FALSE
  }
  ## exp, obs, and exp_cor (2)
  name_exp <- sort(names(dim(exp)))
  name_obs <- sort(names(dim(obs)))
  name_exp <- name_exp[-which(name_exp == memb_dim)]
  if (!is.null(dat_dim)) {
    name_exp <- name_exp[-which(name_exp == dat_dim)]
    name_obs <- name_obs[-which(name_obs == dat_dim)]
  }
  if (!identical(length(name_exp), length(name_obs)) |
      !identical(dim(exp)[name_exp], dim(obs)[name_obs])) {
    stop("Parameter 'exp' and 'obs' must have same length of all ", 
         "dimensions except 'memb_dim' and 'dat_dim'.")
  }
  if (!is.null(exp_cor)) {
    name_exp_cor <- sort(names(dim(exp_cor)))
    name_exp <- sort(names(dim(exp)))
    if (!is.null(dat_dim)) {
      if (dat_dim %in% expcordims) {
        if (!identical(dim(exp)[dat_dim], dim(exp_cor)[dat_dim])) {
          stop("If parameter 'exp_cor' has dataset dimension, it must be", 
               " equal to dataset dimension of 'exp'.")
        }
        name_exp_cor <- name_exp_cor[-which(name_exp_cor == dat_dim)]
        target_dims_cor <- c(memb_dim, sdate_dim, dat_dim)
      } else {
        target_dims_cor <- c(memb_dim, sdate_dim)
      }
    } else {
      target_dims_cor <- c(memb_dim, sdate_dim)
    }
    name_exp <- name_exp[-which(name_exp %in% target_dims_exp)]
    name_exp_cor <- name_exp_cor[-which(name_exp_cor %in% target_dims_cor)]
    if (!identical(length(name_exp), length(name_exp_cor)) |
        !identical(dim(exp)[name_exp], dim(exp_cor)[name_exp_cor])) {
      stop("Parameter 'exp' and 'exp_cor' must have the same length of ",
           "all common dimensions except 'dat_dim', 'sdate_dim' and 'memb_dim'.")
    }
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | ncores %% 1 != 0 | ncores <= 0 |
      length(ncores) > 1) {
      stop("Parameter 'ncores' must be either NULL or a positive integer.")
    }
  }
  ## na.rm
  if (!inherits(na.rm, "logical")) {
    stop("Parameter 'na.rm' must be a logical value.")
  }
  ## na.fill
  if (!inherits(na.fill, "logical")) {
    stop("Parameter 'na.fill' must be a logical value.")
  }
  ## cal.method, apply_to, alpha
  if (!any(cal.method %in% c('bias', 'evmos', 'mse_min', 'crps_min', 'rpc-based'))) {
    stop("Parameter 'cal.method' must be a character string indicating the calibration method used.")
  }
  if (cal.method == 'rpc-based') {
    if (is.null(apply_to)) {
      apply_to <- 'sign'
      warning("Parameter 'apply_to' cannot be NULL for 'rpc-based' method so it ", 
              "has been set to 'sign', as in Eade et al. (2014).")
    } else if (!apply_to %in% c('all','sign')) {
      stop("Parameter 'apply_to' must be either 'all' or 'sign' when 'rpc-based' ", 
           "method is used.")
    }
    if (apply_to == 'sign') {
      if (is.null(alpha)) {
        alpha <- 0.1
        warning("Parameter 'alpha' cannot be NULL for 'rpc-based' method so it ", 
                "has been set to 0.1, as in Eade et al. (2014).")
      } else if (!is.numeric(alpha) | alpha <= 0 | alpha >= 1) {
        stop("Parameter 'alpha' must be a number between 0 and 1.")
      }
    }
  }
  ## eval.method
  if (!any(eval.method %in% c('in-sample', 'leave-one-out', 'hindcast-vs-forecast'))) {
    stop(paste0("Parameter 'eval.method' must be a character string indicating ", 
                "the sampling method used ('in-sample', 'leave-one-out' or ", 
                "'hindcast-vs-forecast')."))
  }
  ## multi.model
  if (!inherits(multi.model, "logical")) {
    stop("Parameter 'multi.model' must be a logical value.")
  }
  if (multi.model & !(cal.method == "mse_min")) {
	  warning(paste0("The 'multi.model' parameter is ignored when using the ", 
                   "calibration method '", cal.method, "'."))
  }
  ## data sufficiently large
  data.set.sufficiently.large.out <- 
    Apply(data = list(exp = exp, obs = obs),
      target_dims = list(exp = target_dims_exp, obs = target_dims_obs),
      fun = .data.set.sufficiently.large, dat_dim = dat_dim, 
      ncores = ncores)$output1

  if (!all(data.set.sufficiently.large.out)) {		
  	if (na.fill) {
      warning("Some forecast data could not be corrected due to data lack",
              " and is replaced with NA values.")
  	} else {
      warning("Some forecast data could not be corrected due to data lack",
              " and is replaced with uncorrected values.")
  	 }
  }

  if (is.null(exp_cor)) {
    calibrated <- Apply(data = list(exp = exp, obs = obs), dat_dim = dat_dim, 
                        cal.method = cal.method, eval.method = eval.method, multi.model = multi.model,
                        na.fill = na.fill, na.rm = na.rm, apply_to = apply_to, alpha = alpha,
                        target_dims = list(exp = target_dims_exp, obs = target_dims_obs),
                        ncores = ncores, fun = .cal)$output1
  } else {
    calibrated <- Apply(data = list(exp = exp, obs = obs, exp_cor = exp_cor),
                        dat_dim = dat_dim, cal.method = cal.method, eval.method = eval.method,
                        multi.model = multi.model, na.fill = na.fill, na.rm = na.rm, 
                        apply_to = apply_to, alpha = alpha,
                        target_dims = list(exp = target_dims_exp, obs = target_dims_obs, 
                                           exp_cor = target_dims_cor),
                        ncores = ncores, fun = .cal)$output1
  }

  if (!is.null(dat_dim)) {
    pos <- match(c(names(dim(exp))[-which(names(dim(exp)) == dat_dim)], 'nexp', 'nobs'), 
                 names(dim(calibrated)))
    calibrated <- aperm(calibrated, pos)
  } else {
    pos <- match(c(names(dim(exp))), names(dim(calibrated)))
    calibrated <- aperm(calibrated, pos)
  }

  if (exp_cor_remove_memb) {
    dim(calibrated) <- dim(calibrated)[-which(names(dim(calibrated)) == memb_dim)]
  }

  dims <- dim(calibrated)
  if (is.logical(calibrated)) {
    calibrated <- array(as.numeric(calibrated), dim = dims)
  }

  return(calibrated)
}


.data.set.sufficiently.large <- function(exp, obs, dat_dim = NULL) {
  amt.min.samples <- 3
  if (is.null(dat_dim)) {
    amt.good.pts <- sum(!is.na(obs) & !apply(exp, c(2), function(x) all(is.na(x))))
    return(amt.good.pts > amt.min.samples)
  } else {
    nexp <- as.numeric(dim(exp)[dat_dim])
    nobs <- as.numeric(dim(obs)[dat_dim])
    amt.good.pts <- NULL
    for (i in 1:nexp) {
      for (j in 1:nobs) {
        agp <- sum(!is.na(obs[, j, drop = FALSE]) & 
                            !apply(exp[, , i, drop = FALSE], c(2), 
                                   function(x) all(is.na(x))))
        amt.good.pts <- c(amt.good.pts, agp)
      }
    }
    return(amt.good.pts > amt.min.samples)
  }
}

.make.eval.train.dexes <- function(eval.method, amt.points, amt.points_cor) { 
  if (eval.method == "leave-one-out") {
    dexes.lst <- lapply(seq(1, amt.points), function(x) return(list(eval.dexes = x,
                        train.dexes = seq(1, amt.points)[-x])))
  } else if (eval.method == "in-sample") {
    dexes.lst <- list(list(eval.dexes = seq(1, amt.points), 
                           train.dexes = seq(1, amt.points)))
  } else if (eval.method == "hindcast-vs-forecast") {
    dexes.lst <- list(list(eval.dexes = seq(1,amt.points_cor),
                           train.dexes = seq(1, amt.points)))
  } else {
    stop(paste0("unknown sampling method: ", eval.method))
  }
  return(dexes.lst)
}

.cal <- function(exp, obs, exp_cor = NULL, dat_dim = NULL, cal.method = "mse_min", 
                 eval.method = "leave-one-out", multi.model = FALSE, na.fill = TRUE, 
                 na.rm = TRUE, apply_to = NULL, alpha = NULL) {

  # exp: [memb, sdate, (dat)]
  # obs: [sdate (dat)]
  # exp_cor: [memb, sdate, (dat)] or NULL

  if (is.null(dat_dim)) {
    nexp <- 1
    nobs <- 1
    exp <- InsertDim(exp, posdim = 3, lendim = 1, name = 'dataset')
    obs <- InsertDim(obs, posdim = 2, lendim = 1, name = 'dataset')
  } else {
    nexp <- as.numeric(dim(exp)[dat_dim])
    nobs <- as.numeric(dim(obs)[dat_dim])
  }

  if (is.null(exp_cor)) {
    # generate a copy of exp so that the same function can run for both cases
    exp_cor <- exp
    cor_dat_dim <- TRUE
  } else {
    if (length(dim(exp_cor)) == 2) { # exp_cor: [memb, sdate]
      cor_dat_dim <- FALSE
    } else {                         # exp_cor: [memb, sdate, dat]
      cor_dat_dim <- TRUE
    }  
  }

  expdims <- dim(exp)
  expdims_cor <- dim(exp_cor)
  memb <- expdims[1] # memb
  sdate <- expdims[2] # sdate
  sdate_cor <- expdims_cor[2]

  var.cor.fc <- array(dim = c(dim(exp_cor)[1:2], nexp = nexp, nobs = nobs))
  
  for (i in 1:nexp) {
    for (j in 1:nobs) {
      exp_data <- exp[, , i]
      dim(exp_data) <- dim(exp)[1:2]
      obs_data <- as.vector(obs[, j])
      if (!.data.set.sufficiently.large(exp = exp_data, obs = obs_data)) {
        if (!na.fill) {
          exp_subset <- exp[, , i]
          var.cor.fc[, , i, j] <- exp_subset
        }
      } else {
        # Subset data for dataset dimension
        if (cor_dat_dim) {
          expcor_data <- exp_cor[, , i]
          dim(expcor_data) <- dim(exp_cor)[1:2]
        } else {
          expcor_data <- exp_cor
        }

        eval.train.dexeses <- .make.eval.train.dexes(eval.method = eval.method, 
                                                     amt.points = sdate, 
                                                     amt.points_cor = sdate_cor)
        amt.resamples <- length(eval.train.dexeses)
        for (i.sample in seq(1, amt.resamples)) {
          # defining training (tr) and evaluation (ev) subsets
          # fc.ev is used to evaluate (not train; train should be done with exp (hindcast))
          eval.dexes <- eval.train.dexeses[[i.sample]]$eval.dexes
          train.dexes <- eval.train.dexeses[[i.sample]]$train.dexes
          fc.ev <- expcor_data[, eval.dexes, drop = FALSE]
          fc.tr <- exp_data[, train.dexes]
          obs.tr <- obs_data[train.dexes, drop = FALSE] 
          
          if (cal.method == "bias") {
            var.cor.fc[, eval.dexes, i, j] <- fc.ev + mean(obs.tr, na.rm = na.rm) - mean(fc.tr, na.rm = na.rm)
            # forecast correction implemented
          } else if (cal.method == "evmos") {
            # forecast correction implemented
            # ensemble and observational characteristics
            quant.obs.fc.tr <- .calc.obs.fc.quant(obs = obs.tr, fc = fc.tr, na.rm = na.rm)
            # calculate value for regression parameters
            init.par <- c(.calc.evmos.par(quant.obs.fc.tr, na.rm = na.rm))
            # correct evaluation subset
            var.cor.fc[, eval.dexes, i, j] <- .correct.evmos.fc(fc.ev , init.par, na.rm = na.rm)
          } else if (cal.method == "mse_min") {
            quant.obs.fc.tr <- .calc.obs.fc.quant(obs = obs.tr, fc = fc.tr, na.rm = na.rm)
            init.par <- .calc.mse.min.par(quant.obs.fc.tr, multi.model, na.rm = na.rm)
            var.cor.fc[, eval.dexes, i, j] <- .correct.mse.min.fc(fc.ev , init.par, na.rm = na.rm)      
          } else if (cal.method == "crps_min") {
            quant.obs.fc.tr <- .calc.obs.fc.quant.ext(obs = obs.tr, fc = fc.tr, na.rm = na.rm)
            init.par <- c(.calc.mse.min.par(quant.obs.fc.tr, na.rm = na.rm), 0.001)
            init.par[3] <- sqrt(init.par[3])
            # calculate regression parameters on training dataset
            optim.tmp <- optim(par = init.par, fn = .calc.crps.opt, gr = .calc.crps.grad.opt, 
                               quant.obs.fc = quant.obs.fc.tr, na.rm = na.rm, method = "BFGS")
            mbm.par <- optim.tmp$par
            var.cor.fc[, eval.dexes, i, j] <- .correct.crps.min.fc(fc.ev , mbm.par, na.rm = na.rm)
          } else if (cal.method == 'rpc-based') {
            # Ensemble mean
            ens_mean.ev <- Apply(data = fc.ev, target_dims = names(memb), fun = mean, na.rm = na.rm)$output1
            ens_mean.tr <- Apply(data = fc.tr, target_dims = names(memb), fun = mean, na.rm = na.rm)$output1
            # Ensemble spread
            ens_spread.tr <- Apply(data = list(fc.tr, ens_mean.tr), target_dims = names(sdate), fun = "-")$output1
            # Mean (climatology)
            exp_mean.tr <- mean(fc.tr, na.rm = na.rm) 
            # Ensemble mean variance
            var_signal.tr <- var(ens_mean.tr, na.rm = na.rm) 
            # Variance of ensemble members about ensemble mean (= spread)
            var_noise.tr <- var(as.vector(ens_spread.tr), na.rm = na.rm)
            # Variance in the observations 
            var_obs.tr <- var(obs.tr, na.rm = na.rm)
            # Correlation between observations and the ensemble mean 
            r.tr <- cor(x = ens_mean.tr, y = obs.tr, method = 'pearson', 
                        use = ifelse(test = isTRUE(na.rm), yes = "pairwise.complete.obs", no = "everything"))
            if ((apply_to == 'all') || (apply_to == 'sign' && 
                cor.test(ens_mean.tr, obs.tr, method = 'pearson', alternative = 'greater')$p.value < alpha)) {
              ens_mean_cal <- (ens_mean.ev - exp_mean.tr) * r.tr * sqrt(var_obs.tr) / sqrt(var_signal.tr) + exp_mean.tr
              var.cor.fc[, eval.dexes, i, j] <- Reorder(data = Apply(data = list(exp = fc.ev, ens_mean = ens_mean.ev, 
                                                                     ens_mean_cal = ens_mean_cal), 
                                                                     target_dims = names(sdate), fun = .CalibrationMembersRPC, 
                                                                     var_obs = var_obs.tr, var_noise = var_noise.tr, r = r.tr)$output1, 
                                                        order = names(expdims)[1:2])
            } else {
              # no significant -> replacing with observed climatology
              var.cor.fc[, eval.dexes, i, j] <- array(data = mean(obs.tr, na.rm = na.rm), dim = dim(fc.ev))
            }
          }
        }
      }
    }
  }

  if (is.null(dat_dim)) {
    dim(var.cor.fc) <- dim(exp_cor)[1:2]
  }
  return(var.cor.fc)
}

# Function to calculate different quantities of a series of ensemble forecasts and corresponding observations
.calc.obs.fc.quant <- function(obs, fc, na.rm) { 
  if (is.null(dim(fc))) {
    dim(fc) <- c(length(fc), 1)
  }
  amt.mbr <- dim(fc)[1]
  obs.per.ens <- InsertDim(obs, posdim = 1, lendim = amt.mbr, name = 'amt.mbr')
  fc.ens.av <- apply(fc, c(2), mean, na.rm = na.rm)
  cor.obs.fc <- cor(fc.ens.av, obs, use = "complete.obs")
  obs.av <- mean(obs, na.rm = na.rm)
  obs.sd <- sd(obs, na.rm = na.rm)
  return(
    append(
      .calc.fc.quant(fc = fc, na.rm = na.rm),
      list(
        obs.per.ens = obs.per.ens,
        cor.obs.fc = cor.obs.fc,
        obs.av = obs.av,
        obs.sd = obs.sd
      )
    )
  )
}

# Extended function to calculate different quantities of a series of ensemble forecasts and corresponding observations
.calc.obs.fc.quant.ext <- function(obs, fc, na.rm){ 
  amt.mbr <- dim(fc)[1]
  obs.per.ens <- InsertDim(obs, posdim = 1, lendim = amt.mbr, name = 'amt.mbr')
  fc.ens.av <- apply(fc, c(2), mean, na.rm = na.rm)
  cor.obs.fc <- cor(fc.ens.av, obs, use = "complete.obs")
  obs.av <- mean(obs, na.rm = na.rm)
  obs.sd <- sd(obs, na.rm = na.rm)

  return(
    append(
      .calc.fc.quant.ext(fc = fc, na.rm = na.rm),
      list(
        obs.per.ens = obs.per.ens,
        cor.obs.fc = cor.obs.fc,
        obs.av = obs.av,
        obs.sd = obs.sd
      )
    )
  )
}

# Function to calculate different quantities of a series of ensemble forecasts
.calc.fc.quant <- function(fc, na.rm) { 
  amt.mbr <- dim(fc)[1]
  fc.ens.av <- apply(fc, c(2), mean, na.rm = na.rm)
  fc.ens.av.av <- mean(fc.ens.av, na.rm = na.rm)
  fc.ens.av.sd <- sd(fc.ens.av, na.rm = na.rm)
  fc.ens.av.per.ens <- InsertDim(fc.ens.av, posdim = 1, lendim = amt.mbr, name = 'amt.mbr')
  fc.ens.sd <- apply(fc, c(2), sd, na.rm = na.rm)
  fc.ens.var.av.sqrt <- sqrt(mean(fc.ens.sd^2, na.rm = na.rm))
  fc.dev <- fc - fc.ens.av.per.ens
  fc.dev.sd <- sd(fc.dev, na.rm = na.rm)
  fc.av <- mean(fc, na.rm = na.rm)
  fc.sd <- sd(fc, na.rm = na.rm)
  return(
    list(
      fc.ens.av = fc.ens.av,
      fc.ens.av.av = fc.ens.av.av,
      fc.ens.av.sd = fc.ens.av.sd,
      fc.ens.av.per.ens = fc.ens.av.per.ens,
      fc.ens.sd = fc.ens.sd,
      fc.ens.var.av.sqrt = fc.ens.var.av.sqrt,
      fc.dev = fc.dev,
      fc.dev.sd = fc.dev.sd,
      fc.av = fc.av,
      fc.sd = fc.sd
    )
  )
}

# Extended function to calculate different quantities of a series of ensemble forecasts
.calc.fc.quant.ext <- function(fc, na.rm) { 
  amt.mbr <- dim(fc)[1]
  repmat1.tmp <- InsertDim(fc, posdim = 1, lendim = amt.mbr, name = 'amt.mbr')
  repmat2.tmp <- aperm(repmat1.tmp, c(2, 1, 3))
  spr.abs <- apply(abs(repmat1.tmp - repmat2.tmp), c(3), mean, na.rm = na.rm)
  spr.abs.per.ens <- InsertDim(spr.abs, posdim = 1, lendim = amt.mbr, name = 'amt.mbr')

  return(
    append(.calc.fc.quant(fc, na.rm = na.rm),
	  list(spr.abs = spr.abs, spr.abs.per.ens = spr.abs.per.ens))
  )
}

# Below are the core or elementary functions to calculate the regression parameters for the different methods
.calc.mse.min.par <- function(quant.obs.fc, multi.model = F, na.rm) {
  par.out <- rep(NA, 3)
  if (multi.model) {
    par.out[3] <- with(quant.obs.fc, obs.sd * sqrt(1. - cor.obs.fc^2) / fc.ens.var.av.sqrt)
  } else {
    par.out[3] <- with(quant.obs.fc, obs.sd * sqrt(1. - cor.obs.fc^2) / fc.dev.sd)
  }
  par.out[2] <- with(quant.obs.fc, abs(cor.obs.fc) * obs.sd / fc.ens.av.sd)
  par.out[1] <- with(quant.obs.fc, obs.av - par.out[2] * fc.ens.av.av, na.rm = na.rm)
  
  return(par.out)
}

.calc.evmos.par <- function(quant.obs.fc, na.rm) {
  par.out <- rep(NA, 2)
  par.out[2] <- with(quant.obs.fc, obs.sd / fc.sd)
  par.out[1] <- with(quant.obs.fc, obs.av - par.out[2] * fc.ens.av.av, na.rm = na.rm)
  return(par.out)
}

# Below are the core or elementary functions to calculate the functions necessary for the minimization of crps
.calc.crps.opt <- function(par, quant.obs.fc, na.rm){
  return( 
    with(quant.obs.fc, 
      mean(abs(obs.per.ens - (par[1] + par[2] * fc.ens.av.per.ens +
	    ((par[3])^2 + par[4] / spr.abs.per.ens) * fc.dev)), na.rm = na.rm) -
        mean(abs((par[3])^2 * spr.abs + par[4]) / 2., na.rm = na.rm)
    )
  )
}

.calc.crps.grad.opt <- function(par, quant.obs.fc, na.rm) {
  sgn1 <- with(quant.obs.fc,sign(obs.per.ens - (par[1] + par[2] * fc.ens.av.per.ens +
               ((par[3])^2 + par[4] / spr.abs.per.ens) * fc.dev)))
  sgn2 <- with(quant.obs.fc, sign((par[3])^2 + par[4] / spr.abs.per.ens))
  sgn3 <- with(quant.obs.fc,sign((par[3])^2 * spr.abs + par[4]))
  deriv.par1 <- mean(sgn1, na.rm = na.rm)
  deriv.par2 <- with(quant.obs.fc, mean(sgn1 * fc.dev, na.rm = na.rm))
  deriv.par3 <- with(quant.obs.fc, 
    mean(2* par[3] * sgn1 * sgn2 * fc.ens.av.per.ens, na.rm = na.rm) -
    mean(spr.abs * sgn3, na.rm = na.rm) / 2.)
  deriv.par4 <- with(quant.obs.fc,
    mean(sgn1 * sgn2 * fc.ens.av.per.ens / spr.abs.per.ens, na.rm = na.rm) -
    mean(sgn3, na.rm = na.rm) / 2.)
  return(c(deriv.par1, deriv.par2, deriv.par3, deriv.par4))
}

# Below are the core or elementary functions to correct the evaluation set based on the regression parameters
.correct.evmos.fc <- function(fc, par, na.rm) {
  quant.fc.mp <- .calc.fc.quant(fc = fc, na.rm = na.rm)
  return(with(quant.fc.mp, par[1] + par[2] * fc))
}
.correct.mse.min.fc <- function(fc, par, na.rm) {
  quant.fc.mp <- .calc.fc.quant(fc = fc, na.rm = na.rm)
  return(with(quant.fc.mp, par[1] + par[2] * fc.ens.av.per.ens + fc.dev * par[3]))
}
.correct.crps.min.fc <- function(fc, par, na.rm) {
  quant.fc.mp <- .calc.fc.quant.ext(fc = fc, na.rm = na.rm)
  return(with(quant.fc.mp, par[1] + par[2] * fc.ens.av.per.ens + fc.dev * abs((par[3])^2 + par[4] / spr.abs)))
}

# Function to calibrate the individual members with the RPC-based method
.CalibrationMembersRPC <- function(exp, ens_mean, ens_mean_cal, var_obs, var_noise, r) {
  member_cal <- (exp - ens_mean) * sqrt(var_obs) * sqrt(1 - r^2) / sqrt(var_noise) + ens_mean_cal
  return(member_cal)
}
