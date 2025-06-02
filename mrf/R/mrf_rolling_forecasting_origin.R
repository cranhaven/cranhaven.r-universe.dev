mrf_rolling_forecasting_origin <- function(UnivariateData, Aggregation,
                                           CoefficientCombination=NULL,
                                           Horizon = 2, Window = 3,
                                           Method = "r", NumClusters = 1,
                                           Threshold="hard",Lambda=0.05){
  # DESCRIPTION
  # This function computes a rolling forecasting origin. The training of the
  # model and the computation of the procedure itself will be executed on the
  # given data. The part of data determined for the rolling forecasting origin
  # computation will be the last part of the time series and its size is computed
  # by the method itself depending on the size of the given parameter 'Window'
  # (which determines the number of steps for which a forecast should be
  # computed) and on the forecast horizon (specific by parameter 'Horizon')
  #
  # INPUT
  # UnivariateData[1:n]                   Numerical vector with n time series
  #                                       values.
  # CoefficientCombination[1:Scales+1]    Numerical vector with numbers which
  #                                       are associated with wavelet levels.
  #                                       The last number is associated with the
  #                                       smooth level. Each number determines
  #                                       the number of coefficient used per
  #                                       wavelet/smooth approximation level.
  #                                       The selection follows a specific
  #                                       scheme.
  # Aggregation[1:Scales]    Numerical vector carrying numbers whose index is
  #                          associated with the wavelet level. The numbers
  #                          indicate the number of time in points used for
  #                          aggregation from the original time series.
  #
  # OPTIONAL
  # Horizon          Number indicating forecast horizon. Horizon = 1 means
  #                  one-step forecast and Horizon > 1 means a one-step forecast
  #                  and all multi-step forecasts from horizon 2 to 'Horizon'.
  # Window           Number indicating how many time points are used as origin
  #                  for computing forecasts.
  # Method           String indicating which method to use
  #                  Available methods: 'r'  = Autoregression
  #                                     'nn' = Neural Network
  # NumClusters      Number of clusters used for parallel computing.
  # Threshold                Character indicating if Thresholding is done on the
  #                          wavelet decomposition or not.
  #                          Default: Threshold="hard". Possible entries:
  #                          Threshold = "hard" for hard thresholding.
  #                          Threshold = "soft" for soft thresholding.
  #                          Any other input indicates no thresholding.
  # Lambda                   Numeric value indicating the threshold for
  #                          computing a hard or soft threshold on the wavelet
  #                          decomposition.
  #
  # OUTPUT
  # Error[1:Window,1:Horizon]       Numerical Matrix with 'Window' many rows
  #                                 entries indicating one time point with
  #                                 'Horizon' many forecast errors.
  # Forecast[1:Window,1:Horizon]    Numerical Matrix with 'Window' many rows
  #                                 entries indicating one time point with
  #                                 'Horizon' many forecasts.
  #
  # Author: QS, 02/2021
  if(!is.vector(UnivariateData)){
    message("Data must be of type vector")
    return()
  }
  if(!is.vector(Aggregation)){
    message("agg_per_lvl must be of type vector")
    return()
  }

  if(!is.double(Horizon)){
    message("horizon must be of type double")
    return()
  }
  if(!is.double(Window)){
    message("window_size must be of type double")
    return()
  }
  if(!is.character(Method)){
    message("method must be of type character")
    return()
  }
  if(Method %in% c("r", "nn")){
    if(!is.vector(CoefficientCombination)){
      message("ccps must be of type vector")
      return()
    }
  }
  if(is.null(CoefficientCombination) && (Method %in% c("r", "nn"))){
    message("CoefficientCombination must be given for all methods except 'elm' and 'nnetar'.")
    return()
  }
  #if(!is.double(numClusters)){
  #  message("numClusters must be of type double")
  #  return()
  #}
  # Non parallel
  if(NumClusters == 1){
    int_total_length  = length(UnivariateData)                        # Length time series
    matError = rbind()
    matForecast = rbind()
    for(i in 1:(Window)){

      int_CFCP = int_total_length - Window - Horizon + i # Current Forecast Position
      dfTrain  = UnivariateData[1:int_CFCP]
      dfTest   = UnivariateData[int_CFCP+1:Horizon]
      forecast = mrf_multi_step_forecast(UnivariateData = dfTrain,
                                         Horizon = Horizon,
                                         CoefficientCombination = CoefficientCombination,
                                         Aggregation = Aggregation,
                                         Method = Method,
                                         Threshold = Threshold,
                                         Lambda = Lambda)
      arr_Error = as.numeric(forecast) - dfTest
      #arr_Error = forecast - dfTest
      matError = rbind(matError, arr_Error)
      matForecast = rbind(matForecast, dfTest)
      #print("Training X")
      #print(dfTrain[(int_CFCP-5):int_CFCP],)
      #print("Training Y")
      #print(dfTest)
    }
    Error = matrix(matError, ncol = Horizon, byrow = TRUE)
    Forecast = matrix(matForecast, ncol = Horizon, byrow = TRUE)
  }
  else{ # Parallel

    if (!requireNamespace('parallel', quietly = TRUE)) {
      message(
        "Package parallel is missing in function rolling_window
      No computations are performed.
      Please install the packages which are defined in 'Suggests'"
      )
      return()
    }

    available_cores = parallel::detectCores()    # Number of cores available
    cores = available_cores[1]-1                 # Do not overuse => av_cores-1
    if(NumClusters != "max"){
      if(NumClusters > cores){
        message(paste0("There are only ", cores, " cores available. Falling back to maximum available number of cores."))
        #message("There are not enough cores. Note that only maximum of detectCores() - 1 is allowed as maximum.")
        #return()
      }
      if(NumClusters < 1){
        message("Input smaller 1 is not allowed!")
        return()
      }
    }
    cl <- parallel::makeCluster(cores)
    #parallel::clusterEvalQ(cl, source("wavelet_decomposition.R"))
    #parallel::clusterEvalQ(cl, source("training.R"))
    #parallel::clusterEvalQ(cl, source("multi_step.R"))
    #parallel::clusterEvalQ(cl, source("onestep.R"))
    #parallel::clusterEvalQ(cl, source("neuralnet_one_step.R"))
    #parallel::clusterEvalQ(cl, source("prediction_scheme.R"))
    #parallel::clusterEvalQ(cl, source("regression_one_step.R"))
    #parallel::clusterEvalQ(cl, source("regression_lsm_optimization.R"))
    #parallel::clusterEvalQ(cl, library("mrf"))
    lst_forecast = parallel::parLapply(cl, 1:Window, help_function,
                                       data = UnivariateData, ccps = CoefficientCombination,
                                       agg_per_lvl = Aggregation,
                                       horizon = Horizon,
                                       window_size = Window,
                                       method = Method,
                                       Threshold = Threshold,
                                       Lambda = Lambda)

    RawVector = unlist(lst_forecast)
    RawMatrix = matrix(RawVector, ncol = Horizon*2, byrow = TRUE)
    Error = RawMatrix[,1:Horizon]
    Forecast = RawMatrix[,(Horizon+1):(2*Horizon)]
    parallel::stopCluster(cl)
  }
  return(list("Error"=Error, "Forecast"=Forecast))
}


help_function <- function(i, data, ccps, agg_per_lvl, horizon = 14,
                          window_size = 365, method = "r", Threshold = "hard",
                          Lambda = 0.05){
  int_total_length  = length(data)                        # Length time series
  int_CFCP = int_total_length - window_size - horizon + i # Current Forecast Position
  dfTrain  = data[1:int_CFCP]
  dfTest   = data[int_CFCP+1:horizon]
  forecast = mrf_multi_step_forecast(UnivariateData = dfTrain,
                                     Horizon = horizon,
                                     CoefficientCombination = ccps,
                                     Aggregation = agg_per_lvl,
                                     Method = method,
                                     Threshold = Threshold,
                                     Lambda = Lambda)
  arr_Error = as.numeric(forecast) - dfTest
  return(list("Error" = arr_Error, "Forecast"=dfTest))
}

