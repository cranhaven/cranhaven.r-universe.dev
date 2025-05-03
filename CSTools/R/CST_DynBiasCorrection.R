#'@rdname CST_DynBiasCorrection
#'@title Performing a Bias Correction conditioned by the dynamical
#'properties of the data.
#'
#'@author Carmen Alvarez-Castro, \email{carmen.alvarez-castro@cmcc.it}
#'@author Maria M. Chaves-Montero, \email{mdm.chaves-montero@cmcc.it}
#'@author Veronica Torralba, \email{veronica.torralba@cmcc.it}
#'@author Davide Faranda, \email{davide.faranda@lsce.ipsl.fr}
#'
#'@description This function perform a bias correction conditioned by the 
#'dynamical properties of the dataset. This function internally uses the functions 
#''Predictability' to divide in terciles the two dynamical proxies 
#'computed with 'CST_ProxiesAttractor'. A bias correction
#'between the model and the observations is performed using the division into
#'terciles of the local dimension 'dim' and inverse of the persistence 'theta'.
#'For instance, model values with lower 'dim' will be corrected with observed 
#'values with lower 'dim', and the same for theta. The function gives two options
#'of bias correction: one for 'dim' and/or one for 'theta'
#'
#'@references Faranda, D., Alvarez-Castro, M.C., Messori, G., Rodriguez, D., 
#'and Yiou, P. (2019). The hammam effect or how a warm ocean enhances large 
#'scale atmospheric predictability.Nature Communications, 10(1), 1316. 
#'\doi{10.1038/s41467-019-09305-8}"
#'@references Faranda, D., Gabriele Messori and Pascal Yiou. (2017).
#' Dynamical proxies of North Atlantic predictability and extremes. 
#' Scientific Reports, 7-41278, 2017.
#'
#'@param exp An s2v_cube object with the experiment data.
#'@param obs An s2dv_cube object with the reference data.
#'@param method A character string indicating the method to apply bias 
#'  correction among these ones: "PTF","RQUANT","QUANT","SSPLIN".
#'@param wetday Logical indicating whether to perform wet day correction 
#'  or not OR a numeric threshold below which all values are set to zero (by 
#'  default is set to 'FALSE').
#'@param proxy A character string indicating the proxy for local dimension
#'  'dim' or inverse of persistence 'theta' to apply the dynamical 
#'  conditioned bias correction method. 
#'@param quanti A number lower than 1 indicating the quantile to perform 
#'  the computation of local dimension and theta.
#'@param ncores The number of cores to use in parallel computation.
#'
#'@return dynbias An s2dvcube object with a bias correction performed 
#'conditioned by local dimension 'dim' or inverse of persistence 'theta'.
#'
#'@examples
#'expL <- rnorm(1:2000)
#'dim(expL) <- c(time = 100, lat = 4, lon = 5)
#'obsL <- c(rnorm(1:1980), expL[1, , ] * 1.2)
#'dim(obsL) <- c(time = 100, lat = 4, lon = 5)
#'time_obsL <- as.POSIXct(paste(rep("01", 100), rep("01", 100), 1920:2019, sep = "-"), 
#'                        format = "%d-%m-%y")
#'time_expL <- as.POSIXct(paste(rep("01", 100), rep("01", 100), 1929:2019, sep = "-"), 
#'                        format = "%d-%m-%y")
#'lon <- seq(-1, 5, 1.5)
#'lat <- seq(30, 35, 1.5)
#'# qm = 0.98 #'too high for this short dataset, it is possible that doesn't
#'# get the requirement, in that case it would be necessary select a lower qm
#'# for instance qm = 0.60
#'expL <- s2dv_cube(data = expL, coords = list(lon = lon, lat = lat),
#'                  Dates = time_expL)
#'obsL <- s2dv_cube(data = obsL, coords = list(lon = lon, lat = lat),
#'                  Dates = time_obsL)
#'# to use DynBiasCorrection
#'dynbias1 <- DynBiasCorrection(exp = expL$data, obs = obsL$data, proxy= "dim",
#'                              quanti = 0.6)
#'# to use CST_DynBiasCorrection
#'dynbias2 <- CST_DynBiasCorrection(exp = expL, obs = obsL, proxy= "dim",
#'                                  quanti = 0.6)
#'
#'@export
CST_DynBiasCorrection<- function(exp, obs, method = 'QUANT', wetday=FALSE,
                                 proxy = "dim", quanti,
                                 ncores = NULL) {
  # Check 's2dv_cube'
  if (!inherits(obs, 's2dv_cube')) {
    stop("Parameter 'obs' must be of the class 's2dv_cube', ",
         "as output by CSTools::CST_Load.")
  }
  if (!inherits(exp, 's2dv_cube')) {
    stop("Parameter 'exp' must be of the class 's2dv_cube', ",
         "as output by CSTools::CST_Load.")
  }
  exp$data <- DynBiasCorrection(exp = exp$data, obs = obs$data, method = method,
                                wetday = wetday,
                                proxy = proxy, quanti = quanti, ncores = ncores)
  return(exp)
}
#'@rdname DynBiasCorrection
#'@title Performing a Bias Correction conditioned by the dynamical
#'properties of the data.
#'
#'@author Carmen Alvarez-Castro, \email{carmen.alvarez-castro@cmcc.it}
#'@author Maria M. Chaves-Montero, \email{mdm.chaves-montero@cmcc.it}
#'@author Veronica Torralba, \email{veronica.torralba@cmcc.it}
#'@author Davide Faranda, \email{davide.faranda@lsce.ipsl.fr}
#'
#'@description This function perform a bias correction conditioned by the 
#'dynamical properties of the dataset. This function used the functions 
#''CST_Predictability' to divide in terciles the two dynamical proxies 
#'computed with 'CST_ProxiesAttractor'. A bias correction
#'between the model and the observations is performed using the division into
#'terciles of the local dimension 'dim' and inverse of the persistence 'theta'.
#'For instance, model values with lower 'dim' will be corrected with observed 
#'values with lower 'dim', and the same for theta. The function gives two options
#'of bias correction: one for 'dim' and/or one for 'theta'
#'
#'@references Faranda, D., Alvarez-Castro, M.C., Messori, G., Rodriguez, D., 
#'and Yiou, P. (2019). The hammam effect or how a warm ocean enhances large 
#'scale atmospheric predictability.Nature Communications, 10(1), 1316. 
#'\doi{10.1038/s41467-019-09305-8}"
#'@references Faranda, D., Gabriele Messori and Pascal Yiou. (2017).
#' Dynamical proxies of North Atlantic predictability and extremes. 
#' Scientific Reports, 7-41278, 2017.
#'
#'@param exp A multidimensional array with named dimensions with the 
#'  experiment data.
#'@param obs A multidimensional array with named dimensions with the 
#'  observation data.
#'@param method A character string indicating the method to apply bias 
#'  correction among these ones:
#'  "PTF", "RQUANT", "QUANT", "SSPLIN".
#'@param wetday Logical indicating whether to perform wet day correction 
#'  or not OR a numeric threshold below which all values are set to zero (by 
#'  default is set to 'FALSE').
#'@param proxy A character string indicating the proxy for local dimension 
#'  'dim' or inverse of persistence 'theta' to apply the dynamical conditioned 
#'  bias correction method. 
#'@param quanti A number lower than 1 indicating the quantile to perform the 
#'  computation of local dimension and theta.
#'@param ncores The number of cores to use in parallel computation.
#'
#'@return A multidimensional array with named dimensions with a bias correction 
#'performed conditioned by local dimension 'dim' or inverse of persistence 'theta'.
#'
#'@import multiApply
#'@importFrom ClimProjDiags Subset
#'@import qmap
#'@examples
#'expL <- rnorm(1:2000)
#'dim (expL) <- c(time =100,lat = 4, lon = 5)
#'obsL <- c(rnorm(1:1980),expL[1,,]*1.2)
#'dim (obsL) <- c(time = 100,lat = 4, lon = 5)
#'dynbias <- DynBiasCorrection(exp = expL, obs = obsL, method='QUANT',
#'                             proxy= "dim", quanti = 0.6)
#'@export
DynBiasCorrection<- function(exp, obs, method = 'QUANT',wetday=FALSE, 
                             proxy = "dim", quanti, ncores = NULL){
  if (is.null(obs)) {
    stop("Parameter 'obs' cannot be NULL.")
  }  
  if (is.null(exp)) {
    stop("Parameter 'exp' cannot be NULL.")
  }    
  if (is.null(method)) {
    stop("Parameter 'method' cannot be NULL.")
  }  
  if (is.null(quanti)) {
    stop("Parameter 'quanti' cannot be NULL.")
  }   
  if (is.null(proxy)) {
    stop("Parameter 'proxy' cannot be NULL.")
  } 
  dims <- dim(exp)

  attractor.obs <- ProxiesAttractor(data = obs, quanti = quanti)
  predyn.obs <- Predictability(dim = attractor.obs$dim,
                               theta = attractor.obs$theta)
  attractor.exp <- ProxiesAttractor(data = exp, quanti = quanti)
  predyn.exp <- Predictability(dim = attractor.exp$dim,
                               theta = attractor.exp$theta)
 
  if (!(any(names(dim(exp)) %in% 'time'))) { 
    if (any(names(dim(exp)) %in% 'sdate')) {
      if (any(names(dim(exp)) %in% 'ftime')) {
        exp <- MergeDims(exp, merge_dims = c('ftime', 'sdate'),
                         rename_dim = 'time')
      }
    }
  }
  if (!(any(names(dim(obs)) %in% 'time'))) { 
    if (any(names(dim(obs)) %in% 'sdate')) {
      if (any(names(dim(obs)) %in% 'ftime')) {
        obs <- MergeDims(obs, merge_dims = c('ftime', 'sdate'),
                         rename_dim = 'time')
      }
    }
  }
 
  dim_exp <- dim(exp) 
  names_to_check <- names(dim_exp)[which(names(dim_exp) %in% 
                          c('time', 'lat', 'lon', 'sdate') == FALSE)]
  if (length(names_to_check) > 0) {
    dim_obs <- dim(obs)
    if (any(names(dim_obs) %in% names_to_check)) {
      if (any(dim_obs[which(names(dim_obs) %in% names_to_check)] != 
          dim_exp[which(names(dim_exp) %in% names_to_check)])) {
        for (i in names_to_check) {
          pos <- which(names(dim_obs) == i)
          names(dim(obs))[pos] <- ifelse(dim_obs[pos] != 
                                  dim_exp[which(names(dim_exp) == i)], 
                                  paste0('obs_', names(dim_obs[pos])), 
                                  names(dim(obs)[pos])) 
        }
        warning("Common dimension names with different length are renamed.")
      }
    }
  } 
  
  if (proxy == "dim") {
    adjusted <- Apply(list(exp, obs), target_dims = 'time',
                      fun = .dynbias, method, wetday,
                      predyn.exp = predyn.exp$pred.dim$pos.d,
                      predyn.obs = predyn.obs$pred.dim$pos.d,
                      ncores = ncores, output_dims = 'time')$output1
  } else if (proxy == "theta") {
    adjusted <- Apply(list(exp, obs), target_dims = 'time',
                      fun = .dynbias, method, wetday,
                      predyn.exp = predyn.exp$pred.theta$pos.t,
                      predyn.obs = predyn.obs$pred.theta$pos.t,
                      ncores = ncores, output_dims = 'time')$output1
  } else {
    stop ("Parameter 'proxy' must be set as 'dim' or 'theta'.")
  }
  
  if (any(names(dim(adjusted)) %in% 'memberObs')) {
    if (dim(adjusted)['memberObs'] == 1) {
      adjusted <- Subset(adjusted, along = 'memberObs', indices=1, drop = 'selected')
    } else {
      print('Dimension member in obs changed to memberObs')
    }
  }
  
  if (any(names(dim(adjusted)) %in% 'datasetObs')) {
    if (dim(adjusted)['datasetObs'] == 1) {
      adjusted <- Subset(adjusted, along = 'datasetObs', indices = 1, drop = 'selected')
    } else {
      print('Dimension dataset in obs changed to datasetObs')
    }
  }
  return(adjusted)
}

.dynbias <- function(exp, obs, method, wetday, predyn.exp, predyn.obs) {
   result <- array(rep(NA, length(exp)))
   res <- lapply(1:3, function(x) {
     exp_sub <- exp[predyn.exp[[x]]]
     obs_sub <- obs[predyn.obs[[x]]]
     adjust <- .qbiascorrection(exp_sub, obs_sub, method,wetday)
     result[predyn.exp[[x]]] <<- adjust
     return(NULL)
   })
   return(result)
}   
.qbiascorrection <- function(expX, obsX, method, wetday) {
  ## functions fitQmap and doQmap
  if (method == "PTF") {
    qm.fit <- fitQmap(obsX, expX, method = "PTF", transfun = "expasympt",
                      cost = "RSS", wet.day = wetday)
    qmap <- doQmap(expX, qm.fit)
  } else if (method == "QUANT") {
    qm.fit <- fitQmap(obsX, expX, method = "QUANT", qstep = 0.01, wet.day = wetday)
    qmap <- doQmap(expX, qm.fit, type = "tricub")
  } else if (method == "RQUANT") {
    qm.fit <- fitQmap(obsX, expX, method = "RQUANT", qstep = 0.01,wet.day = wetday)
    qmap <- doQmap(expX, qm.fit, type = "linear")
  } else if (method == "SSPLIN") {
    qm.fit <- fitQmap(obsX, expX, qstep = 0.01, method = "SSPLIN",wet.day = wetday)
    qmap <- doQmap(expX, qm.fit)
  } else {
    stop ("Parameter 'method' doesn't match any of the available methods.")
  }
  return(qmap)
}
