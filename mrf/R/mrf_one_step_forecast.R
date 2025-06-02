mrf_one_step_forecast <- function(UnivariateData, Aggregation,
                                  CoefficientCombination=NULL, Method="r",
                                  Threshold="hard", Lambda = 0.05){
  # DESCRIPTION
  # This function creates a one step forecast using the multiresolution
  # forecasting framework.
  #
  # INPUT
  # UnivariateData[1:n]      Vector with n time series values.
  # Aggregation              Vector carrying numbers whose index is associated with the
  #                          wavelet level. The numbers indicate the number of time in
  #                          points used for aggregation from the original time series.
  #
  # OPTIONAL
  # CoefficientCombination   Vector with numbers which are associated with wavelet levels.
  #                          The last number is associated with the smooth level.
  #                          Each number determines the number of coefficient used per level.
  #                          The selection follows a specific scheme.
  # Method           String indicating which method to use
  #                  Available methods: 'r'  = Autoregression
  #                                     'nn' = Neural Network
  #                                     'elm' = Extreme Learning Machine
  #                                     'nnetar' = forecast::nnetar
  # Threshold                Character indicating if Thresholding is done on the
  #                          wavelet decomposition or not.
  #                          Default: Threshold="hard". Possible entries:
  #                          Threshold = "hard" for hard thresholding.
  #                          Threshold = "soft" for soft thresholding.
  #                          Any other input indicates no thresholding.
  # Lambda                   Numeric value indicating the threshold for
  #                          computing a hard or soft threshold on the wavelet
  #                          decomposition.
  # OUTPUT
  # forecast    Numerical value with one step forecast
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
  if(Method %in% c("r", "nn")){
    if(!is.vector(CoefficientCombination)){
      message("ccps must be of type vector")
      return()
    }
  }
  if(is.null(CoefficientCombination) && (Method %in% c("r", "nn"))){
    message("CoefficientCombination must be given for all methods except 'elm'.")
    return()
  }

  if(Method == "r"){
    Forecast = mrf_regression_one_step_forecast(UnivariateData=UnivariateData,
                                                CoefficientCombination=CoefficientCombination,
                                                Aggregation=Aggregation,
                                                Threshold=Threshold,
                                                Lambda=Lambda)
  }else if(Method == "nn"){
    Forecast = mrf_neuralnet_one_step_forecast(UnivariateData=UnivariateData,
                                               CoefficientCombination=CoefficientCombination,
                                               Aggregation=Aggregation,
                                               Threshold=Threshold,
                                               Lambda=Lambda)
  }else if(Method == "elm"){
    Forecast = mrf_elm_forecast(UnivariateData, Horizon = 1,
                                Aggregation,Threshold,Lambda)
  }else if(Method == "nnetar"){
    Forecast = mrf_nnetar_forecast(UnivariateData, Horizon = 1,
                                Aggregation,Threshold,Lambda)
  }else{
    print("No valid methodname given => Returning.")
    Forecast = 0
  }
  return(Forecast)
}



#
#
#
#
#
