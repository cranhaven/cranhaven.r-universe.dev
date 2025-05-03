#'Synthetic forecast generator imitating seasonal to decadal forecasts. The
#'components of a forecast: (1) predictabiltiy (2) forecast error (3) 
#'non-stationarity and (4) ensemble generation. The forecast can be computed 
#'for real observations or observations generated artifically.
#'
#'The toymodel is based on the model presented in Weigel et al. (2008) QJRS
#'with an extension to consider non-stationary distributions prescribing a
#'linear trend. The toymodel allows to generate an aritifical forecast
#'based on obsevations provided by the input (from Load) or artificially
#'generated observations based on the input parameters (sig, trend). 
#'The forecast can be specfied for any number of start-dates, lead-time and 
#'ensemble members. It imitates components of a forecast: (1) predictabiltiy
#'(2) forecast error (3) non-stationarity and (4) ensemble generation. 
#'The forecast can be computed for real observations or observations generated
#'artifically.
#'
#'@param alpha Predicabiltiy of the forecast on the observed residuals 
#'  Must be a scalar 0 < alpha < 1.
#'@param beta Standard deviation of forecast error 
#'  Must be a scalar 0 < beta < 1.
#'@param gamma Factor on the linear trend to sample model uncertainty. Can be 
#'  a scalar or a vector of scalars -inf < gammay < inf.
#'  Defining a scalar results in multiple forecast, corresponding to different 
#'  models with different trends.
#'@param sig Standard deviation of the residual variability of the forecast.
#'  If observations are provided 'sig' is computed from the observations. 
#'@param trend Linear trend of the forecast. The same trend is used for each 
#'  lead-time. If observations are provided the 'trend' is computed from the 
#'  observations, with potentially different trends for each lead-time. The 
#'  trend has no unit and needs to be defined according to the time vector 
#'  [1,2,3,... nstartd].
#'@param nstartd Number of start-dates of the forecast. 
#'  If observations are provided the 'nstartd' is computed from the observations. 
#'@param nleadt Number of lead-times of the forecats.
#'  If observations are provided the 'nleadt' is computed from the observations.
#'@param nmemb Number of members of the forecasts.
#'@param obsini Observations that can be used in the synthetic forecast coming 
#'  from Load (anomalies are expected). If no observations are provided 
#'  artifical observations are generated based on Gaussian variaiblity with 
#'  standard deviation from 'sig' and linear trend from 'trend'.
#'@param fxerr Provides a fixed error of the forecast instead of generating 
#'  one from the level of beta. This allows to perform pair of forecasts with 
#'  the same conditional error as required for instance in an attribution context.
#'
#'@return List of forecast with $mod including the forecast and $obs the 
#'  observations. The dimensions correspond to 
#'  c(length(gamma), nmemb, nstartd, nleadt)
#'
#'@examples
#'# Example 1: Generate forecast with artifical observations
#'# Seasonal prediction example
#'a <- 0.1
#'b <- 0.3
#'g <- 1
#'sig <- 1
#'t <- 0.02
#'ntd <- 30
#'nlt <- 4
#'nm <- 10
#'toyforecast <- ToyModel(alpha = a, beta = b, gamma = g, sig = sig, trend = t, 
#'                        nstartd = ntd, nleadt = nlt, nmemb = nm)
#'
#'# Example 2: Generate forecast from loaded observations
#'# Decadal prediction example
#'  \dontrun{
#'data_path <- system.file('sample_data', package = 's2dv')
#'expA <- list(name = 'experiment', path = file.path(data_path,
#'             'model/$EXP_NAME$/$STORE_FREQ$_mean/$VAR_NAME$_3hourly',
#'             '$VAR_NAME$_$START_DATE$.nc'))
#'obsX <- list(name = 'observation', path = file.path(data_path,
#'             '$OBS_NAME$/$STORE_FREQ$_mean/$VAR_NAME$',
#'             '$VAR_NAME$_$YEAR$$MONTH$.nc'))
#'
#'# Now we are ready to use Load().
#'startDates <- c('19851101', '19901101', '19951101', '20001101', '20051101')
#'sampleData <- Load('tos', list(expA), list(obsX), startDates,
#'                   output = 'areave', latmin = 27, latmax = 48,
#'                   lonmin = -12, lonmax = 40)
#'  }
#'  \dontshow{
#'startDates <- c('19851101', '19901101', '19951101', '20001101', '20051101')
#'sampleData <- s2dv:::.LoadSampleData('tos', c('experiment'),
#'                                                c('observation'), startDates,
#'                                                output = 'areave',
#'                                                latmin = 27, latmax = 48,
#'                                                lonmin = -12, lonmax = 40)
#'  }
#'
#'a <- 0.1
#'b <- 0.3
#'g <- 1
#'nm <- 10
#'
#'toyforecast <- ToyModel(alpha = a, beta = b, gamma = g, nmemb = nm, 
#'                        obsini = sampleData$obs, nstartd = 5, nleadt = 60)
#'## Add PlotAno() back when this function is included!!
#'#  \donttest{
#'#PlotAno(toyforecast$mod, toyforecast$obs, startDates, 
#'#        toptitle = c("Synthetic decadal temperature prediction"), 
#'#        fileout = "ex_toymodel.eps")
#'#  }
#'
#'@importFrom stats rnorm
#'@export
ToyModel <- function(alpha = 0.1, beta = 0.4, gamma = 1, sig = 1, 
  trend = 0, nstartd = 30, nleadt = 4, nmemb = 10, obsini = NULL, 
  fxerr = NULL) {
  # Check alpha, beta, gamma, sig, trend, nstartd, nleadt, nmemb
  if (any(!is.numeric(c(alpha, beta, gamma, sig, trend, nstartd, 
                      nleadt, nmemb)))) {
    stop(paste("Parameters 'alpha', 'beta', 'gamma', 'sig', 'trend', 'nstartd',",
               "nleadt and nmemb must be numeric."))
  }
  nstartd <- round(nstartd)
  nleadt <- round(nleadt)
  nmemb <- round(nmemb)

  # Check obsini
  if (!is.null(obsini)) {
    if (!is.numeric(obsini) || !is.array(obsini)) {
      stop("Parameter 'obsini' must be a numeric array.")
    }
    if (length(dim(obsini)) != 4) {
      stop("Parameter 'obsini' must be an array with dimensions c(1, 1, nleadt, nstartd).")
    }
    if (dim(obsini)[3] != nstartd || dim(obsini)[4] != nleadt) {
      stop(paste0("The dimensions of parameter 'obsini' and the parameters 'nleadt' and 'nstartd' must match:\n  dim(obsini) = c(", 
           dim(obsini)[3], ", ", dim(obsini)[4], ")\n  nstartd = ", nstartd, "  nleadt = ", nleadt))
    }
  }

  # Check fxerr
  if (!is.null(fxerr)) {
    if (!is.numeric(fxerr)) {
      stop("Parameter 'fxerr' must be numeric.")
    }
  }

  time <- seq(1, nstartd)  # time vector, generated from 1 -> nstard
  
  if (!(sig^2 - alpha^2 - beta^2 > 0)) {
    stop("Model variability not constrained: respect condition \"sig^2-alpha^2-beta^2 > 0\")")
  }
  
  if (nstartd < 0) {
    stop("Number of start dates must be positive")
  }
  
  if (nleadt < 0) {
    stop("Number of lead-times must be positive")
  }
  
  if (nmemb < 0) {
    stop("Number of members must be positive")
  }
  
  
  if (!is.null(obsini)) {
    # Observations provided by input compute forecast parameters
    # from observations
    obs_ano <- obsini
  } else {
    # Create observations for selected period
    obs_ano <- array(rnorm(nleadt * nstartd, mean = 0, sd = sig), 
      dim = c(1, 1, nstartd, nleadt))
    obs_trend <- array(t(time) * rep(trend, times = nstartd), 
      , dim = c(1, 1, nstartd, nleadt))
    obs <- obs_ano + obs_trend
    
    trend <- rep(c(trend), times = nleadt)  # same trend in all lead-times
    sig <- rep(c(sig), times = nleadt)  # same variability in all lead-times
  }
  
  forecast <- array(dim = c(length(gamma), nmemb, nstartd, 
    nleadt))
  
  # Allocate observations and forecast according to
  # s2dv standards
  for (j in 1:nstartd) {
    for (f in 1:nleadt) {
      for (g in 1:length(gamma)) {
        # Generate forecasts with different trends
        auto_term <- alpha * obs_ano[1, 1, j, f]  # Predictability term
        if (is.numeric(fxerr)) {
          conf_term <- fxerr  # Forecast error term, fixed by input
        } else {
          conf_term <- rnorm(1, mean = 0, sd = beta)  # Forecast error term, random
        }
        trend_term <- gamma[g] * trend[f] * j  # Trend term
        var_corr <- rnorm(nmemb, mean = 0, sd = sqrt(sig[f] - 
          alpha^2 - beta^2))
        forecast[g, , j, f] <- matrix(auto_term, c(nmemb, 
          1)) + matrix(conf_term, c(nmemb, 1)) + matrix(trend_term, 
          c(nmemb, 1)) + var_corr
      }
    }
  }
  
  list(mod = forecast, obs = obs_ano)
}
