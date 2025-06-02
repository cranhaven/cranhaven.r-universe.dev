mrf_forecast <- function(Model, Horizon=1){
  # DESCRIPTION
  # Computes multi-step forecasts with a given multiresolution model.
  #
  # INPUT
  # Model        List containing model specifications from mrf_train().
  #
  # OPTIONAL
  # Horizon      Number indicating forecast horizon. Horizon = 1 means
  #              one-step forecast and Horizon > 1 means a one-step forecast
  #              and all multi-step forecasts from horizon 2 to 'Horizon'.
  #
  # OUTPUT
  # List of 2 elements:
  # Forecast[1:Horizon]    Numerical vector with forecast with 'Horizon' many
  #                        entries.
  # Model                  List containing model specifications from mrf_train().
  #
  # Author: QS, 08/2021
  Data         = Model$Data
  Coefficients = Model$CoefficientCombination
  Aggregation  = Model$Aggregation
  Method       = Model$Method
  Threshold    = Model$Threshold
  Lambda       = Model$Lambda
  if(!is.vector(Data)){
    message("Data must be of type vector")
    return()
  }
  if(!is.double(Horizon)){
    message("Horizon must be of type double")
    return()
  }
  if(!is.vector(Aggregation)){
    message("Aggregation must be of type vector")
    return()
  }
  if(!is.character(Method)){
    message("Method must be of type character")
    return()
  }
  if(Method %in% c("r", "nn")){
    if(!is.vector(Coefficients)){
      message("ccps must be of type vector")
      return()
    }
  }
  if(is.null(Coefficients) && (Method %in% c("r", "nn"))){
    message("CoefficientCombination must be given for all methods except 'elm' or 'nnetar'.")
    return()
  }
  if(Horizon != Model$Horizon){
    message("Forecasting horizon must be the same as the horizon for which the model was trained for.")
    return()
  }
  Forecast = mrf_multi_step_forecast(UnivariateData=Data, Horizon=Horizon,
                                     Aggregation=Aggregation,
                                     CoefficientCombination=Coefficients,
                                     Method=Method,
                                     Threshold=Threshold, Lambda=Lambda)
  return(list("Forecast"=Forecast,
              "Model"=Model))
}


#
#
#
#
#
