#'Multivariate Root Mean Square Error (RMSE) 
#'
#'@author Deborah Verfaillie, \email{deborah.verfaillie@bsc.es}
#'@description This function calculates the RMSE from multiple variables, as the 
#'mean of each variable's RMSE scaled by its observed standard deviation. 
#'Variables can be weighted based on their relative importance (defined by the 
#'user).
#'
#'@param exp A list of objects, one for each variable, of class \code{s2dv_cube} 
#'  as returned by \code{CST_Anomaly} function, containing the anomaly of the 
#'  seasonal forecast experiment data in the element named \code{$data}.
#'@param obs A list of objects, one for each variable (in the same order than 
#'  the input in 'exp') of class \code{s2dv_cube} as returned by 
#'  \code{CST_Anomaly} function, containing the observed anomaly data in the 
#'  element named \code{$data}.
#'@param weight (optional) A vector of weight values to assign to each variable. 
#'  If no weights are defined, a value of 1 is assigned to every variable.
#'@param memb_dim A character string indicating the name of the member 
#'  dimension. It must be one dimension in 'exp' and 'obs'. The default value is 
#'  'member'.
#'@param dat_dim A character string indicating the name of the dataset 
#'  dimension. It must be one dimension in 'exp' and 'obs'. If there is no 
#'  dataset dimension, it can be NULL. The default value is 'dataset'.
#'@param sdate_dim A character string indicating the name of the start date 
#'  dimension. It must be one dimension in 'exp' and 'obs'. The default value is 
#'  'sdate'.
#'@param ftime_dim A character string indicating the name of the forecast time 
#'  dimension. It must be one dimension in 'exp' and 'obs'. The default value is 
#'  'ftime'.
#'
#'@return An object of class \code{s2dv_cube} containing the RMSE in the element 
#'  \code{$data} which is an array with two datset dimensions equal to the 
#'  'dataset' dimension in the \code{exp$data} and \code{obs$data} inputs. An 
#'  array with dimensions: c(number of exp, number of obs, 1 (the multivariate 
#'  RMSE value), number of lat, number of lon)
#'
#'@seealso \code{\link[s2dv]{RMS}} and \code{\link{CST_Load}}
#'@examples
#'# Example with 2 variables
#'mod1 <- abs(rnorm(1 * 3 * 4 * 5 * 6 * 7))
#'mod2 <- abs(rnorm(1 * 3 * 4 * 5 * 6 * 7))
#'dim(mod1) <- c(dataset = 1, member = 3, sdate = 4, ftime = 5, lat = 6, lon = 7)
#'dim(mod2) <- c(dataset = 1, member = 3, sdate = 4, ftime = 5, lat = 6, lon = 7)
#'obs1 <- abs(rnorm(1 * 1 * 4 * 5 * 6 * 7))
#'obs2 <- abs(rnorm(1 * 1 * 4 * 5 * 6 * 7))
#'dim(obs1) <- c(dataset = 1, member = 1, sdate = 4, ftime = 5, lat = 6, lon = 7)
#'dim(obs2) <- c(dataset = 1, member = 1, sdate = 4, ftime = 5, lat = 6, lon = 7)
#'lon <- seq(0, 30, 5)
#'lat <- seq(0, 25, 5)
#'coords <- list(lat = lat, lon = lon)
#'exp1 <- list(data = mod1, coords = coords, 
#'             attrs = list(Datasets = "EXP1", source_files = "file1", 
#'                          Variable = list(varName = 'pre')))
#'exp2 <- list(data = mod2, coords = coords, 
#'             attrs = list(Datasets = "EXP2", source_files = "file2", 
#'                          Variable = list(varName = 'tas')))
#'obs1 <- list(data = obs1, coords = coords, 
#'             attrs = list(Datasets = "OBS1", source_files = "file1", 
#'                          Variable = list(varName = 'pre')))
#'obs2 <- list(data = obs2, coords = coords, 
#'             attrs = list(Datasets = "OBS2", source_files = "file2", 
#'                          Variable = list(varName = 'tas')))
#'attr(exp1, 'class') <- 's2dv_cube'
#'attr(exp2, 'class') <- 's2dv_cube'
#'attr(obs1, 'class') <- 's2dv_cube'
#'attr(obs2, 'class') <- 's2dv_cube'
#'anom1 <- CST_Anomaly(exp1, obs1, cross = TRUE, memb = TRUE)
#'anom2 <- CST_Anomaly(exp2, obs2, cross = TRUE, memb = TRUE)
#'ano_exp <- list(anom1$exp, anom2$exp)
#'ano_obs <- list(anom1$obs, anom2$obs)
#'a <- CST_MultivarRMSE(exp = ano_exp, obs = ano_obs, weight = c(1, 2))
#'@importFrom s2dv RMS MeanDims
#'@export
CST_MultivarRMSE <- function(exp, obs, weight = NULL, memb_dim = 'member', 
                             dat_dim = 'dataset', sdate_dim = 'sdate', 
                             ftime_dim = 'ftime') {

  # s2dv_cube
  if (!is.list(exp) | !is.list(obs)) {
    stop("Parameters 'exp' and 'obs' must be lists of 's2dv_cube' objects")
  }
  if (!(all(sapply(exp, inherits, 's2dv_cube')))) {
    stop("Elements of the list in parameter 'exp' must be of the class ",
        "'s2dv_cube', as output by CSTools::CST_Load.")
  }
  if (!(all(sapply(obs, inherits, 's2dv_cube')))) {
    stop("Elements of the list in parameter 'obs' must be of the class ",
        "'s2dv_cube', as output by CSTools::CST_Load.")
  }
  if (length(exp) != length(obs)) {
    stop("Parameters 'exp' and 'obs' must be of the same length.")
  }

  nvar <- length(exp)
  if (nvar < 2) {
    stop("Parameters 'exp' and 'obs'  must contain at least two", 
         " s2dv objects for two different variables.")
  }
  for (j in 1 : nvar) {
    if (!is.null(names(dim(exp[[j]]$data))) & !is.null(names(dim(obs[[j]]$data)))) {
      if (all(names(dim(exp[[j]]$data)) %in% names(dim(obs[[j]]$data)))) {
        dimnames <- names(dim(exp[[j]]$data))
      } else {
        stop("Dimension names of element 'data' from parameters 'exp'",
             " and 'obs' should be equal.")
      }
    } else {
       stop("Element 'data' from parameters 'exp' and 'obs'",
            " should have dimmension names.")
    }
  }
  # weight
  if (is.null(weight)) {
    weight <- c(rep(1, nvar))
  } else if (!is.numeric(weight)) {
    stop("Parameter 'weight' must be numeric.")
  } else if (length(weight) != nvar){
    stop("Parameter 'weight' must have a length equal to the number ",
         "of variables.")
  }
  # memb_dim
  if (!is.null(memb_dim)) {
    if (!is.character(memb_dim)) {
      stop("Parameter 'memb_dim' must be a character string.")
    }
    if (!memb_dim %in% names(dim(exp[[1]]$data)) | !memb_dim %in% names(dim(obs[[1]]$data))) {
      stop("Parameter 'memb_dim' is not found in 'exp' or in 'obs' dimension.")
    }
  } else {
    stop("Parameter 'memb_dim' cannot be NULL.")
  }
  # dat_dim
  if (!is.null(dat_dim)) {
    if (!is.character(dat_dim)) {
      stop("Parameter 'dat_dim' must be a character string.")
    }
    if (!dat_dim %in% names(dim(exp[[1]]$data)) | !dat_dim %in% names(dim(obs[[1]]$data))) {
      stop("Parameter 'dat_dim' is not found in 'exp' or in 'obs' dimension.")
    }
  }
  # ftime_dim
  if (!is.null(ftime_dim)) {
    if (!is.character(ftime_dim)) {
      stop("Parameter 'ftime_dim' must be a character string.")
    }
    if (!ftime_dim %in% names(dim(exp[[1]]$data)) | !ftime_dim %in% names(dim(obs[[1]]$data))) {
      stop("Parameter 'ftime_dim' is not found in 'exp' or in 'obs' dimension.")
    }
  } else {
    stop("Parameter 'ftime_dim' cannot be NULL.")
  }
  # sdate_dim
  if (!is.null(sdate_dim)) {
    if (!is.character(sdate_dim)) {
      stop("Parameter 'sdate_dim' must be a character string.")
    }
    if (!sdate_dim %in% names(dim(exp[[1]]$data)) | !sdate_dim %in% names(dim(obs[[1]]$data))) {
      stop("Parameter 'sdate_dim' is not found in 'exp' or in 'obs' dimension.")
    }
  } else {
    stop("Parameter 'sdate_dim' cannot be NULL.")
  }
  # Variables
  obs_var <- unlist(lapply(exp, function(x) {
                    x$attrs$Variable$varName}))

  exp_var <- unlist(lapply(exp, function(x) {
                    x$attrs$Variable$varName}))
  
  if (all(exp_var != obs_var)) {
    stop("Variables in parameters 'exp' and 'obs' must be in the same order.")
  }

  mvrmse <- 0
  sumweights <- 0

  for (j in 1 : nvar) {
    # seasonal average of anomalies
    AvgExp <- MeanDims(exp[[j]]$data, c(memb_dim, ftime_dim), na.rm = TRUE)
    AvgObs <- MeanDims(obs[[j]]$data, c(memb_dim, ftime_dim), na.rm = TRUE)
    # multivariate RMSE (weighted) 
    rmse <- RMS(AvgExp, AvgObs, dat_dim = dat_dim, time_dim = sdate_dim,
                conf = FALSE)$rms
    stdev <- sd(AvgObs)
    mvrmse <- mvrmse + (rmse / stdev * as.numeric(weight[j]))
    sumweights <- sumweights + as.numeric(weight[j])
  }
  mvrmse <- mvrmse / sumweights 
  
  # names(dim(mvrmse)) <- c(dimnames[1], dimnames[1], 'statistics', dimnames[5 : 6])
  exp_Datasets <- unlist(lapply(exp, function(x) {
                         x$attrs[[which(names(x$attrs) == 'Datasets')]]}))
  exp_source_files <- unlist(lapply(exp, function(x) {
                         x$attrs[[which(names(x$attrs) == 'source_files')]]}))
  obs_Datasets <- unlist(lapply(obs, function(x) {
                         x$attrs[[which(names(x$attrs) == 'Datasets')]]}))
  obs_source_files <- unlist(lapply(obs, function(x) {
                         x$attrs[[which(names(x$attrs) == 'source_files')]]}))

  exp1 <- exp[[1]]
  exp1$data <- mvrmse
  exp1$attrs$Datasets <- c(exp_Datasets, obs_Datasets)
  exp1$attrs$source_files <- c(exp_source_files, obs_source_files)
  exp1$attrs$Variable$varName <- as.character(exp_var)
  exp1$attrs$Variable$metadata <- c(exp1$attrs$Variable$metadata, exp[[2]]$attrs$Variable$metadata)
  return(exp1)
}
