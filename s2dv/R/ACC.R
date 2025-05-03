#'Compute the spatial anomaly correlation coefficient between the forecast and 
#'corresponding observation
#'
#'Calculate the spatial anomaly correlation coefficient (ACC) for the ensemble
#'mean of each model and the corresponding references over a spatial domain. It
#'can return a forecast time series if the data contain forest time dimension, 
#'and also the ACC mean over one dimension, e.g., start date dimension.
#'The domain of interest can be specified by providing the list of longitudes/
#'latitudes of the data together with the corners of the domain: lonlatbox = 
#'c(lonmin, lonmax, latmin, latmax). The data will be adjusted to have a spatial
#'mean of zero, then area weighting is applied. The formula is referenced from 
#'Wilks (2011; section 7.6.4; https://doi.org/10.1016/B978-0-12-385022-5.00008-7).
#'
#'@param exp A numeric array of experimental anomalies with named dimensions.
#'  The dimension must have at least 'lat_dim' and 'lon_dim'.
#'@param obs A numeric array of observational anomalies with named dimensions.
#'  The dimension should be the same as 'exp' except the length of 'dat_dim' 
#'  and 'memb_dim'.
#'@param dat_dim A character string indicating the name of dataset (nobs/nexp) 
#'  dimension. The default value is NULL (no dataset).
#'@param lat_dim A character string indicating the name of the latitude
#'  dimension of 'exp' and 'obs' along which ACC is computed. The default value
#'  is 'lat'. 
#'@param lon_dim A character string indicating the name of the longitude
#'  dimension of 'exp' and 'obs' along which ACC is computed. The default value
#'  is 'lon'. 
#'@param avg_dim A character string indicating the name of the dimension to be
#'  averaged, which is usually the time dimension. If no need to calculate mean
#'  ACC, set as NULL. The default value is 'sdate'.
#'@param memb_dim A character string indicating the name of the member 
#'  dimension. If the data are not ensemble ones, set as NULL. The default 
#'  value is 'member'.
#'@param lat A vector of the latitudes of the exp/obs grids. It is used for
#'  area weighting and when the domain of interested 'lonlatbox' is specified.
#'@param lon A vector of the longitudes of the exp/obs grids. Only required when
#'  the domain of interested 'lonlatbox' is specified. The default value is 
#'  NULL.
#'@param lonlatbox A numeric vector of 4 indicating the corners of the domain of
#'  interested: c(lonmin, lonmax, latmin, latmax). The default value is NULL 
#'  and the whole data will be used.
#'@param alpha A numeric indicating the significance level for the statistical
#'  significance test. The default value is 0.05. 
#'@param pval A logical value indicating whether to compute the p-value or not.
#'  The default value is TRUE.
#'@param sign A logical value indicating whether to retrieve the statistical
#'  significance of the test Ho: ACC = 0 based on 'alpha'. The default value is
#'  FALSE.
#'@param conf A logical value indicating whether to retrieve the confidence 
#'  intervals or not. The default value is TRUE.
#'@param conftype A charater string of "parametric" or "bootstrap". 
#'  "parametric" provides a confidence interval for the ACC computed by a 
#'  Fisher transformation and a significance level for the ACC from a one-sided
#'  student-T distribution. "bootstrap" provides a confidence interval for the
#'  ACC and MACC computed from bootstrapping on the members with 100 drawings 
#'  with replacement. To guarantee the statistical robustness of the result, 
#'  make sure that your experiment and observation always have the same number
#'  of members. "bootstrap" requires 'memb_dim' has value. The default value is
#'  'parametric'.
#'@param ncores An integer indicating the number of cores to use for parallel 
#'  computation. The default value is NULL.
#'
#'@return 
#'A list containing the numeric arrays:\cr 
#'\item{acc}{
#'  The ACC with the dimensions c(nexp, nobs, the rest of the dimension except 
#'  lat_dim, lon_dim and memb_dim). nexp is the number of experiment (i.e., dat_dim in
#'  exp), and nobs is the number of observation (i.e., dat_dim in obs). If 
#'  dat_dim is NULL, nexp and nobs are omitted.
#'}
#'\item{macc}{
#'  The mean anomaly correlation coefficient with dimensions
#'  c(nexp, nobs, the rest of the dimension except lat_dim, lon_dim, memb_dim, and 
#'  avg_dim). Only present if 'avg_dim' is not NULL. If dat_dim is NULL, nexp 
#'  and nobs are omitted.
#'}
#'\item{conf.lower (if conftype = "parametric") or acc_conf.lower (if 
#'      conftype = "bootstrap")}{
#'  The lower confidence interval of ACC with the same dimensions as ACC. Only
#'  present if \code{conf = TRUE}.
#'}
#'\item{conf.upper (if conftype = "parametric") or acc_conf.upper (if 
#'      conftype = "bootstrap")}{
#'  The upper confidence interval of ACC with the same dimensions as ACC. Only 
#'  present if \code{conf = TRUE}.
#'}
#'\item{p.val}{
#'  The p-value with the same dimensions as ACC. Only present if 
#'  \code{pval = TRUE} and \code{conftype = "parametric"}.  
#'}
#'\item{$sign}{
#'  The statistical significance. Only present if \code{sign = TRUE}.
#'}
#'\item{macc_conf.lower}{
#'  The lower confidence interval of MACC with the same dimensions as MACC. 
#'  Only present if \code{conftype = "bootstrap"}.
#'}
#'\item{macc_conf.upper}{
#'  The upper confidence interval of MACC with the same dimensions as MACC. 
#'  Only present if \code{conftype = "bootstrap"}.
#'}
#'
#'@examples
#'  \dontshow{
#'startDates <- c('19851101', '19901101', '19951101', '20001101', '20051101')
#'sampleData <- s2dv:::.LoadSampleData('tos', c('experiment'),
#'                                                c('observation'), startDates,
#'                                                leadtimemin = 1,
#'                                                leadtimemax = 4,
#'                                                output = 'lonlat',
#'                                                latmin = 27, latmax = 48,
#'                                                lonmin = -12, lonmax = 40)
#'  }
#'sampleData$mod <- Season(sampleData$mod, monini = 11, moninf = 12, monsup = 2)
#'sampleData$obs <- Season(sampleData$obs, monini = 11, moninf = 12, monsup = 2) 
#'clim <- Clim(sampleData$mod, sampleData$obs)
#'ano_exp <- Ano(sampleData$mod, clim$clim_exp)
#'ano_obs <- Ano(sampleData$obs, clim$clim_obs)
#'acc <- ACC(ano_exp, ano_obs, lat = sampleData$lat, dat_dim = 'dataset')
#'acc_bootstrap <- ACC(ano_exp, ano_obs, conftype = 'bootstrap', 
#'                     lat = sampleData$lat, dat_dim = 'dataset')
#'# Combine acc results for PlotACC
#'res <- array(c(acc$conf.lower, acc$acc, acc$conf.upper, acc$p.val), 
#'             dim = c(dim(acc$acc), 4))
#'res_bootstrap <- array(c(acc$acc_conf.lower, acc$acc, acc$acc_conf.upper, acc$p.val),
#'                       dim = c(dim(acc$acc), 4))
#'  \donttest{
#'PlotACC(res, startDates)
#'PlotACC(res_bootstrap, startDates)
#'  }
#'@references Joliffe and Stephenson (2012). Forecast Verification: A 
#'  Practitioner's Guide in Atmospheric Science. Wiley-Blackwell.; 
#'  Wilks (2011; section 7.6.4; https://doi.org/10.1016/B978-0-12-385022-5.00008-7).
#'@import multiApply
#'@importFrom abind abind
#'@importFrom stats qt qnorm quantile
#'@importFrom ClimProjDiags Subset
#'@export
ACC <- function(exp, obs, dat_dim = NULL, lat_dim = 'lat', lon_dim = 'lon',
                avg_dim = 'sdate', memb_dim = 'member', 
                lat = NULL, lon = NULL, lonlatbox = NULL, alpha = 0.05, 
                pval = TRUE, sign = FALSE, conf = TRUE, conftype = "parametric",
                ncores = NULL) {

  # Check inputs 
  ## exp and obs (1)
  if (is.null(exp) | is.null(obs)) {
    stop("Parameter 'exp' and 'obs' cannot be NULL.")
  }
  if (!is.numeric(exp) | !is.numeric(obs)) {
    stop("Parameter 'exp' and 'obs' must be a numeric array.")
  }
  if (is.null(dim(exp)) | is.null(dim(obs))) {
    stop("Parameter 'exp' and 'obs' must have at least dimensions ",
         "lat_dim and lon_dim.")
  }
  if (any(is.null(names(dim(exp)))) | any(nchar(names(dim(exp))) == 0) |
      any(is.null(names(dim(obs)))) | any(nchar(names(dim(obs))) == 0)) {
    stop("Parameter 'exp' and 'obs' must have dimension names.")
  }
  ## dat_dim
  if (!is.null(dat_dim)) {
    if (!is.character(dat_dim) | length(dat_dim) > 1) {
      stop("Parameter 'dat_dim' must be a character string or NULL.")
    }
    if (!dat_dim %in% names(dim(exp)) | !dat_dim %in% names(dim(obs))) {
      stop("Parameter 'dat_dim' is not found in 'exp' or 'obs' dimension. ",
           "Set it as NULL if there is no dataset dimension.")
    }
  }
  ## lat_dim
  if (!is.character(lat_dim) | length(lat_dim) != 1) {
    stop("Parameter 'lat_dim' must be a character string.")
  }
  if (!lat_dim %in% names(dim(exp)) | !lat_dim %in% names(dim(obs))) {
    stop("Parameter 'lat_dim' is not found in 'exp' or 'obs' dimension.")
  }
  ## lon_dim
  if (!is.character(lon_dim) | length(lon_dim) != 1) {
    stop("Parameter 'lon_dim' must be a character string.")
  }
  if (!lon_dim %in% names(dim(exp)) | !lon_dim %in% names(dim(obs))) {
    stop("Parameter 'lon_dim' is not found in 'exp' or 'obs' dimension.")
  }
  ## avg_dim
  if (!is.null(avg_dim)) {
    if (!is.character(avg_dim) | length(avg_dim) > 1) {
      stop("Parameter 'avg_dim' must be a character string.")
    } 
    if (!avg_dim %in% names(dim(exp)) | !avg_dim %in% names(dim(obs))) {
      stop("Parameter 'avg_dim' is not found in 'exp' or 'obs' dimension.")
    }
  }
  ## memb_dim
  if (!is.null(memb_dim)) {
    if (!is.character(memb_dim) | length(memb_dim) > 1) {
      stop("Parameter 'memb_dim' must be a character string.")
    }
    if (!memb_dim %in% names(dim(exp)) & !memb_dim %in% names(dim(obs))) {
      stop("Parameter 'memb_dim' is not found in 'exp' nor 'obs' dimension. ",
           "Set it as NULL if there is no member dimension.")
    }
    # Add [member = 1] 
    if (memb_dim %in% names(dim(exp)) & !memb_dim %in% names(dim(obs))) {
      dim(obs) <- c(dim(obs), 1)
      names(dim(obs))[length(dim(obs))] <- memb_dim
    }
    if (!memb_dim %in% names(dim(exp)) & memb_dim %in% names(dim(obs))) {
      dim(exp) <- c(dim(exp), 1)
      names(dim(exp))[length(dim(exp))] <- memb_dim
    }
  }
  ## lat
  if (is.null(lat)) {
    stop("Parameter 'lat' cannot be NULL. It is required for area weighting.")
  }
  if (!is.numeric(lat) | length(lat) != dim(exp)[lat_dim]) {
    stop("Parameter 'lat' must be a numeric vector with the same ",
         "length as the latitude dimension of 'exp' and 'obs'.")
  }
  ## lon
  if (!is.null(lon)) {
    if (!is.numeric(lon) | length(lon) != dim(exp)[lon_dim]) {
      stop("Parameter 'lon' must be a numeric vector with the same ",
           "length as the longitude dimension of 'exp' and 'obs'.")
    }
  }
  ## lonlatbox
  if (!is.null(lonlatbox)) {
    if (!is.numeric(lonlatbox) | length(lonlatbox) != 4) {
      stop("Parameter 'lonlatbox' must be a numeric vector of 4.")
    }
    if (is.null(lon)) {
      stop("Parameter 'lat' and 'lon' are required if 'lonlatbox' is specified.")
    }
    select_lonlat <- TRUE
  } else {
    select_lonlat <- FALSE
  }
  ## alpha
  if (!is.numeric(alpha) | any(alpha < 0) | any(alpha > 1) | length(alpha) > 1) {
    stop("Parameter 'alpha' must be a numeric number between 0 and 1.")
  }
  ## pval
  if (!is.logical(pval) | length(pval) > 1) {
    stop("Parameter 'pval' must be one logical value.")
  }
  ## sign
  if (!is.logical(sign) | length(sign) > 1) {
    stop("Parameter 'sign' must be one logical value.")
  }
  ## conf
  if (!is.logical(conf) | length(conf) > 1) {
    stop("Parameter 'conf' must be one logical value.")
  }
  if (conf) {
    ## conftype 
    if (!conftype %in% c('parametric', 'bootstrap')) {
      stop("Parameter 'conftype' must be either 'parametric' or 'bootstrap'.")
    }
    if (conftype == 'bootstrap' & is.null(memb_dim)) {
      stop("Parameter 'memb_dim' cannot be NULL when parameter 'conftype' is 'bootstrap'.")
    }
  }
  ## ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | any(ncores %% 1 != 0) | any(ncores < 0) |
        length(ncores) > 1) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  }
  ## exp and obs (2)
  name_exp <- sort(names(dim(exp)))
  name_obs <- sort(names(dim(obs)))
  if (!all(name_exp %in% name_obs) | !all(name_obs %in% name_exp)) {
    stop("Parameter 'exp' and 'obs' must have same dimension names.")
  }
  if (!is.null(dat_dim)) {
    name_exp <- name_exp[-which(name_exp == dat_dim)]
    name_obs <- name_obs[-which(name_obs == dat_dim)]
  }
  if (!is.null(memb_dim)) {
    name_exp <- name_exp[-which(name_exp == memb_dim)]
    name_obs <- name_obs[-which(name_obs == memb_dim)]
  }
  if (!identical(dim(exp)[name_exp], dim(obs)[name_obs])) {
    stop("Parameter 'exp' and 'obs' must have same length of ",
         "all the dimensions except 'dat_dim' and 'memb_dim'.")
  }

#-----------------------------------------------------------------


  ###############################
  # Sort dimension
  name_exp <- names(dim(exp))
  name_obs <- names(dim(obs))
  order_obs <- match(name_exp, name_obs)
  obs <- Reorder(obs, order_obs)
  ###############################

  # Select the domain 
  if (select_lonlat) {
    for (jind in 1:2) {
      while (lonlatbox[jind] < 0) {
        lonlatbox[jind] <- lonlatbox[jind] + 360
      }
      while (lonlatbox[jind] > 360) {
        lonlatbox[jind] <- lonlatbox[jind] - 360
      }
    }
    indlon <- which((lon >= lonlatbox[1] & lon <= lonlatbox[2]) | 
                    (lonlatbox[1] > lonlatbox[2] & (lon > lonlatbox[1] | lon < lonlatbox[2])))
    indlat <- which(lat >= lonlatbox[3] & lat <= lonlatbox[4])

    exp <- ClimProjDiags::Subset(exp, c(lat_dim, lon_dim), list(indlat, indlon), drop = FALSE)
    obs <- ClimProjDiags::Subset(obs, c(lat_dim, lon_dim), list(indlat, indlon), drop = FALSE)
  }

   # Ensemble mean
  if (!is.null(memb_dim)) {
    if (conftype == 'bootstrap') {
      exp_ori <- exp
      obs_ori <- obs
    }
    exp <- MeanDims(exp, memb_dim, na.rm = TRUE)
    obs <- MeanDims(obs, memb_dim, na.rm = TRUE)
  }


  if (is.null(avg_dim)) {
    target_dims <- list(c(lat_dim, lon_dim, dat_dim), c(lat_dim, lon_dim, dat_dim))
  } else {
    target_dims <- list(c(lat_dim, lon_dim, avg_dim, dat_dim),
                        c(lat_dim, lon_dim, avg_dim, dat_dim))
  }
  res <- Apply(list(exp, obs),
                 target_dims = target_dims,
                 fun = .ACC,
                 dat_dim = dat_dim, avg_dim = avg_dim,
                 lat = lat,
                 conftype = conftype, pval = pval, conf = conf, alpha = alpha,
                 sign = sign, ncores = ncores)

  # If bootstrap, calculate confidence level
  if (conftype == 'bootstrap') {
    if (is.null(avg_dim)) {
      target_dims_bs <- list(c(memb_dim, dat_dim, lat_dim, lon_dim),
                             c(memb_dim, dat_dim, lat_dim, lon_dim))
    } else {
      target_dims_bs <- list(c(memb_dim, dat_dim, avg_dim, lat_dim, lon_dim),
                             c(memb_dim, dat_dim, avg_dim, lat_dim, lon_dim))
    }
    res_conf <- Apply(list(exp_ori, obs_ori),
                      target_dims = target_dims_bs, 
                      fun = .ACC_bootstrap, 
                      dat_dim = dat_dim, memb_dim = memb_dim, avg_dim = avg_dim,
                      lat = lat,
                      conftype = conftype, pval = pval, conf = conf, alpha = alpha,
                      sign = sign, ncores = ncores)
    #NOTE: pval?
    res <- list(acc = res$acc,
                acc_conf.lower = res_conf$acc_conf.lower,
                acc_conf.upper = res_conf$acc_conf.upper,
                macc = res$macc,
                macc_conf.lower = res_conf$macc_conf.lower,
                macc_conf.upper = res_conf$macc_conf.upper)
  }

  return(res)
}

.ACC <- function(exp, obs, lat, dat_dim = NULL, avg_dim = 'sdate', alpha = 0.05,
                 pval = TRUE, sign = FALSE, conf = TRUE, conftype = "parametric") {
  # .ACC() should use all the spatial points to calculate ACC. It returns [nexp, nobs].
  # If dat_dim = NULL, it returns a number.

  # if (is.null(avg_dim)) 
  ## exp: [lat, lon, (dat_exp)]
  ## obs: [lat, lon, (dat_obs)]
  # if (!is.null(avg_dim)) 
  ## exp: [lat, lon, avg_dim, (dat_exp)]
  ## obs: [lat, lon, avg_dim, (dat_obs)]

  # Add dat_dim temporarily if dat_dim = NULL
  if (is.null(dat_dim)) {
    nexp <- 1
    nobs <- 1
  } else {
    nexp <- as.numeric(dim(exp)[dat_dim])
    nobs <- as.numeric(dim(obs)[dat_dim])
  }

  if (is.null(avg_dim)) {
    acc <- array(dim = c(nexp = nexp, nobs = nobs))
    if (pval) p.val <- array(dim = c(nexp = nexp, nobs = nobs))
    if (sign) signif <- array(dim = c(nexp = nexp, nobs = nobs))
    if (conf) {
      conf.upper <- array(dim = c(nexp = nexp, nobs = nobs))
      conf.lower <- array(dim = c(nexp = nexp, nobs = nobs))
    }
    
  } else {
    acc <- array(dim = c(nexp = nexp, nobs = nobs, dim(exp)[avg_dim]))
    names(dim(acc))[3] <- avg_dim
    macc <- array(dim = c(nexp = nexp, nobs = nobs))
    if (pval) p.val <- array(dim = c(nexp = nexp, nobs = nobs, dim(exp)[avg_dim]))
    if (sign) signif <- array(dim = c(nexp = nexp, nobs = nobs, dim(exp)[avg_dim]))
    if (conf) {
        conf.upper <- array(dim = c(nexp = nexp, nobs = nobs, dim(exp)[avg_dim]))
        conf.lower <- array(dim = c(nexp = nexp, nobs = nobs, dim(exp)[avg_dim]))
    }
  }  
  
  # centralize & area weighted
  ## spatial centralization for each [avg_dim, dat]
  dim_exp <- dim(exp)
  dim_obs <- dim(obs)
  wt <- cos(lat * pi / 180)
  wt <- rep(wt, times = prod(dim_exp[2:length(dim_exp)]))

  if (is.null(avg_dim) & is.null(dat_dim)) {  #[lat, lon]
    # turn exp and obs into vector, first latitudes and then longitudes
    exp <- as.vector(exp)
    obs <- as.vector(obs)
    exp <- array(sqrt(wt) * (exp - mean(exp, na.rm = TRUE)), dim = dim_exp)
    obs <- array(sqrt(wt) * (obs - mean(obs, na.rm = TRUE)), dim = dim_obs)
  } else { # [lat, lon, dat], [lat, lon, avg_dim], or [lat, lon, avg_dim, dat]
    # exp
    exp <- array(exp, dim = c(prod(dim_exp[1:2]), dim_exp[3:length(dim_exp)]))
    mean_exp <- colMeans(exp, na.rm = TRUE)  # [avg_dim, (dat)]
    mean_exp <- rep(as.vector(mean_exp), each = prod(dim_exp[1:2]))
    exp <- array(sqrt(wt) * (as.vector(exp) - mean_exp), dim = dim_exp)
    # obs
    obs <- array(obs, dim = c(prod(dim_obs[1:2]), dim_obs[3:length(dim_obs)]))
    mean_obs <- colMeans(obs, na.rm = TRUE)  # [avg_dim, (dat)]
    mean_obs <- rep(as.vector(mean_obs), each = prod(dim_obs[1:2]))
    obs <- array(sqrt(wt) * (as.vector(obs) - mean_obs), dim = dim_obs)
  }

  # Per-paired exp and obs. NAs should be in the same position in both exp and obs
  for (iobs in 1:nobs) {
    for (iexp in 1:nexp) {
      if (!is.null(dat_dim)) {
        exp_sub <- ClimProjDiags::Subset(exp, dat_dim, iexp, drop = 'selected')
        obs_sub <- ClimProjDiags::Subset(obs, dat_dim, iobs, drop = 'selected')
      } else {
        exp_sub <- exp
        obs_sub <- obs
      }
      # dim: [lat, lon, (avg_dim)]

      # Variance(iexp) should not take into account any point 
      # that is not available in iobs and therefore not accounted for 
      # in covariance(iexp, iobs) and vice-versa 
      exp_sub[is.na(obs_sub)] <- NA
      obs_sub[is.na(exp_sub)] <- NA

      if (is.null(avg_dim)) {
        # ACC
        top <- sum(exp_sub * obs_sub, na.rm = TRUE)  #a number
        bottom <- sqrt(sum(exp_sub^2, na.rm = TRUE) * sum(obs_sub^2, na.rm = TRUE))
        acc[iexp, iobs] <- top / bottom #a number
        # handle bottom = 0
        if (is.infinite(acc[iexp, iobs])) acc[iexp, iobs] <- NA

        # pval, sign, and conf
        if ((pval | conf | sign) && conftype == "parametric") {
          # calculate effective sample size
          eno <- .Eno(as.vector(obs_sub), na.action = na.pass)

          if (pval | sign) {
            t <- qt(1 - alpha, eno - 2)  # a number
            p.value <- sqrt(t^2 / (t^2 + eno - 2))
            if (pval) p.val[iexp, iobs] <- p.value
            if (sign) signif[iexp, iobs] <- !is.na(p.value) & p.value <= alpha
          }
          if (conf) {
            conf.upper[iexp, iobs] <- tanh(atanh(acc[iexp, iobs]) + 
                                        qnorm(1 - alpha / 2) / sqrt(eno - 3))
            conf.lower[iexp, iobs] <- tanh(atanh(acc[iexp, iobs]) + 
                                        qnorm(alpha / 2) / sqrt(eno - 3))
          }
        }

      } else {  #avg_dim is not NULL
        # exp_sub and obs_sub: [lat, lon, avg_dim]

        # MACC
        top <- sum(exp_sub * obs_sub, na.rm = TRUE)  #a number
        bottom <- sqrt(sum(exp_sub^2, na.rm = TRUE) * sum(obs_sub^2, na.rm = TRUE))
        macc[iexp, iobs] <- top / bottom #a number

        # handle bottom = 0
        if (is.infinite(macc[iexp, iobs])) macc[iexp, iobs] <- NA

        # ACC
        for (i in seq_len(dim(acc)[3])) {
          exp_sub_i <- exp_sub[, , i]
          obs_sub_i <- obs_sub[, , i]
          top <- sum(exp_sub_i * obs_sub_i, na.rm = TRUE)  #a number
          bottom <- sqrt(sum(exp_sub_i^2, na.rm = TRUE) * sum(obs_sub_i^2, na.rm = TRUE))
          acc[iexp, iobs, i] <- top / bottom #a number
          # handle bottom = 0
          if (is.infinite(acc[iexp, iobs, i])) acc[iexp, iobs, i] <- NA
        }

        # pval, sign, and conf
        if ((pval | sign | conf) && conftype == "parametric") {
          # calculate effective sample size along lat_dim and lon_dim 
          # combine lat_dim and lon_dim into one dim first
          obs_tmp <- array(obs_sub, 
                           dim = c(space = prod(dim(obs_sub)[1:2]), 
                                   dim(obs_sub)[3]))
          eno <- apply(obs_tmp, 2, .Eno, na.action = na.pass)  # a vector of avg_dim
          if (pval | sign) {
            t <- qt(1 - alpha, eno - 2)  # a vector of avg_dim
            p.value <- sqrt(t^2 / (t^2 + eno - 2))
            if (pval) p.val[iexp, iobs, ] <- p.value
            if (sign) signif[iexp, iobs, ] <- !is.na(p.value) & p.value <= alpha
          }
          if (conf) {
            conf.upper[iexp, iobs, ] <- tanh(atanh(acc[iexp, iobs, ]) + 
                                          qnorm(1 - alpha / 2) / sqrt(eno - 3))
            conf.lower[iexp, iobs, ] <- tanh(atanh(acc[iexp, iobs, ]) + 
                                          qnorm(alpha / 2) / sqrt(eno - 3))
          }
        }
      }  # if avg_dim is not NULL

    }
  }

#------------------------------------------------
  # Remove nexp and nobs if dat_dim = NULL
  if (is.null(dat_dim)) {
    if (is.null(avg_dim)) {
      acc <- as.vector(acc)
      if (conf) {
        conf.lower <- as.vector(conf.lower)
        conf.upper <- as.vector(conf.upper)
      }   
      if (pval) p.val <- as.vector(p.val)
      if (sign) signif <- as.vector(signif)

    } else {
      dim(acc) <- dim(acc)[3:length(dim(acc))]
      macc <- as.vector(macc)
      if (conf) {
        dim(conf.lower) <- dim(conf.lower)[3:length(dim(conf.lower))]
        dim(conf.upper) <- dim(conf.upper)[3:length(dim(conf.upper))]
      }
      if (pval) dim(p.val) <- dim(p.val)[3:length(dim(p.val))]
      if (sign) dim(signif) <- dim(signif)[3:length(dim(signif))]
    }
  }

  # Return output
  if (is.null(avg_dim)) {
    output <- list(acc = acc)
  } else {
    output <- list(acc = acc, macc = macc)
  }
  if (conf) output <- c(output, list(conf.lower = conf.lower, conf.upper = conf.upper))
  if (pval) output <- c(output, list(p.val = p.val))
  if (sign) output <- c(output, list(sign = signif))

  return(output)
}


.ACC_bootstrap <- function(exp, obs, lat, dat_dim = NULL, 
                           avg_dim = 'sdate', memb_dim = NULL, alpha = 0.05,
                           pval = TRUE, sign = FALSE, conf = TRUE, conftype = "parametric") {
# if (is.null(avg_dim)) 
  # exp: [memb_exp, (dat_exp), lat, lon]
  # obs: [memb_obs, (dat_obs), lat, lon]
# if (!is.null(avg_dim)) 
  # exp: [memb_exp, (dat_exp), avg_dim, lat, lon]
  # obs: [memb_obs, (dat_obs), avg_dim, lat, lon]

  if (is.null(dat_dim)) {
    nexp <- 1
    nobs <- 1
    dim(exp) <- c(dim(exp)[1], dat = 1, dim(exp)[-1])
    dim(obs) <- c(dim(obs)[1], dat = 1, dim(obs)[-1])
    dat_dim <- 'dat'
    remove_dat_dim <- TRUE
  } else {
    nexp <- as.numeric(dim(exp)[dat_dim])
    nobs <- as.numeric(dim(obs)[dat_dim])
    remove_dat_dim <- FALSE
  }

  nmembexp <- as.numeric(dim(exp)[1])
  nmembobs <- as.numeric(dim(obs)[1])

  ndraw <- 100
  if (is.null(avg_dim)) {
    acc_draw <- array(dim = c(nexp = nexp, nobs = nobs, ndraw))
  } else {
    acc_draw <- array(dim = c(nexp = nexp, nobs = nobs, dim(exp)[3], ndraw))
    macc_draw <- array(dim = c(nexp = nexp, nobs = nobs, ndraw))
  }

  for (jdraw in 1:ndraw) {
    #choose a randomly member index for each point of the matrix 
    indexp <- array(sample(nmembexp, 
                           size = prod(dim(exp)[-c(length(dim(exp)) - 1, length(dim(exp)))]),
                           replace = TRUE), 
                    dim = dim(exp))
    indobs <- array(sample(nmembobs, 
                           size = prod(dim(obs)[-c(length(dim(obs)) - 1, length(dim(obs)))]),
                           replace = TRUE),
                     dim = dim(obs))

      #combine maxtrix of data and random index
      varindexp <- abind::abind(exp, indexp, along = length(dim(exp)) + 1)
      varindobs <- abind::abind(obs, indobs, along = length(dim(obs)) + 1)

    #select randomly the members for each point of the matrix
#    if (is.null(avg_dim)) {

    drawexp <- array( 
                    apply(varindexp, 2:length(dim(exp)), function(x) x[, 1][x[, 2]]),
                          dim = dim(exp)) 
    drawobs <- array(
                    apply(varindobs, 2:length(dim(obs)), function(x) x[, 1][x[, 2]]),
                          dim = dim(obs))

    # ensemble mean before .ACC
    drawexp <- MeanDims(drawexp, memb_dim, na.rm = TRUE)
    drawobs <- MeanDims(drawobs, memb_dim, na.rm = TRUE)
    # Reorder
    if (is.null(avg_dim)) {
      drawexp <- Reorder(drawexp, c(2, 3, 1))
      drawobs <- Reorder(drawobs, c(2, 3, 1))
    } else {
      drawexp <- Reorder(drawexp, c(3, 4, 2, 1))
      drawobs <- Reorder(drawobs, c(3, 4, 2, 1))
    } 
   
    #calculate the ACC of the randomized field
    tmpACC <- .ACC(drawexp, drawobs, conf = FALSE, pval = FALSE, sign = FALSE, 
                   avg_dim = avg_dim, lat = lat, dat_dim = dat_dim)
    if (is.null(avg_dim)) {
      acc_draw[, , jdraw] <- tmpACC$acc
    } else {
      acc_draw[, , , jdraw] <- tmpACC$acc
      macc_draw[, , jdraw] <- tmpACC$macc
    }
  }

  #calculate the confidence interval
  if (is.null(avg_dim)) {
    acc_conf.upper <- apply(acc_draw, c(1, 2),
                            function (x) {
                              quantile(x, 1 - alpha / 2, na.rm = TRUE)})
    acc_conf.lower <- apply(acc_draw, c(1, 2),
                            function (x) {
                              quantile(x, alpha / 2, na.rm = TRUE)})

  } else {
    acc_conf.upper <- apply(acc_draw, c(1, 2, 3), 
                            function (x) {
                              quantile(x, 1 - alpha / 2, na.rm = TRUE)})
    acc_conf.lower <- apply(acc_draw, c(1, 2, 3),
                            function (x) {
                              quantile(x, alpha / 2, na.rm = TRUE)})
    macc_conf.upper <- apply(macc_draw, c(1, 2),
                            function (x) {
                              quantile(x, 1 - alpha / 2, na.rm = TRUE)})
    macc_conf.lower <- apply(macc_draw, c(1, 2),
                            function (x) {
                              quantile(x, alpha / 2, na.rm = TRUE)})
  }

  if (remove_dat_dim) {
    if (is.null(avg_dim)) {
      dim(acc_conf.lower) <- NULL
      dim(acc_conf.upper) <- NULL
    } else {
      dim(acc_conf.lower) <- dim(acc_conf.lower)[-c(1, 2)]
      dim(acc_conf.upper) <- dim(acc_conf.upper)[-c(1, 2)]
      dim(macc_conf.lower) <- NULL
      dim(macc_conf.upper) <- NULL
    }
  }

  # Return output
  if (is.null(avg_dim)) {
      return(list(acc_conf.lower = acc_conf.lower,
                  acc_conf.upper = acc_conf.upper))
  } else {
      return(list(acc_conf.lower = acc_conf.lower,
                  acc_conf.upper = acc_conf.upper,
                  macc_conf.lower = macc_conf.lower,
                  macc_conf.upper = macc_conf.upper))
  }
                  
}
