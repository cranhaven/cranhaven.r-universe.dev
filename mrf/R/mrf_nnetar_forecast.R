mrf_nnetar_forecast = function(UnivariateData, Horizon, Aggregation,
                             Threshold="hard", Lambda = 0.05){
  # DESCRIPTION
  # Computes a one-step forecast on given Data using a redundant Haar wavelet
  # transform and a specific selection of coefficients with an extreme learning
  # machine on each wavelet and the last smooth approximation level separately
  # combining them in the end via reconstruction scheme.
  #
  # INPUT
  # UnivariateData[1:n]      Numerical vector with n values
  # Horizon                  Number indicating horizon for forecast from 1 to
  #                          horizon.
  # Aggregation[1:Scales]    Numerical vector carrying numbers whose index is
  #                          associated with the wavelet level. The numbers
  #                          indicate the number of time in points used for
  #                          aggregation from the original time series.
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
  # Forecast                 Numerical value with one step forecast
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
  dec_res <- wavelet_decomposition(UnivariateData, Aggregation,Threshold,Lambda)  # Decomposition
  if (!requireNamespace('nnfor', quietly = TRUE)) {
    message(
      "Package nnfor is missing in function mrf_xlm_one_step_forecast
      No computations are performed.
      Please install the packages which are defined in 'Suggests'"
    )
    return()
  }else{
  Cut = mrf_requirement(UnivariateData=UnivariateData,
                        CoefficientCombination = rep(0,length(Aggregation)+1),
                        Aggregation = Aggregation)
  Cut = Cut$StartTraining
  LenData = length(UnivariateData)
  NumLevels = length(Aggregation)+1
  Forecast = rbind()
  for(i in 1:NumLevels){
    currForecast = 0
    if(i < NumLevels){
      model = forecast::nnetar(y = stats::as.ts(dec_res$WaveletCoefficients[i,]))
      tmpForecast = forecast::forecast(model,h=Horizon)
      currForecast = as.vector(tmpForecast$mean)
    }else{
      model = forecast::nnetar(y = as.ts(dec_res$SmoothCoefficients[i-1,]))
      tmpForecast = forecast::forecast(model,h=Horizon)
      currForecast = as.vector(tmpForecast$mean)
    }
    Forecast = rbind(Forecast, currForecast)
  }
  FinalForecast = colSums(Forecast)
  return(FinalForecast)
  }
}

#
#
#
#
#
