mrf_train <- function(Data, Horizon=1, Aggregation="auto", Method = "r",
                      TimeSteps4ModelSelection=2, crit="AIC", InSample=FALSE,
                      Threshold="hard", Lambda=0.05,
                      NumClusters=1, itermax=1){
  # DESCRIPTION
  # Computes the best model on data on given data selecting
  # the best method on the training data based on the last
  # TimeSteps4ModelSelection time steps.
  #
  # INPUT
  # Data[1:n]                Numerical vector with n time series values.
  #
  # OPTIONAL
  # Horizon          Number indicating forecast horizon. Horizon = 1 means
  #                  one-step forecast and Horizon > 1 means a one-step forecast
  #                  and all multi-step forecasts from horizon 2 to 'Horizon'.
  # Aggregation[1:Scales]    Numerical vector carrying numbers whose index is
  #                          associated with the wavelet level. The numbers
  #                          indicate the number of time in points used for
  #                          aggregation from the original time series.
  #                          If set to "auto", wavelet models from levels 1 to 5
  #                          are used for model selection.
  # Method           String indicating which method to use
  #                  Available methods: 'r'  = Autoregression
  #                                     'nn' = Neural Network
  #                                     'elm' = Extreme Learning Machine
  #                                     'nnetar' = forecast::nnetar
  # TimeSteps4ModelSelection Number of time steps of data (newest part) on which
  #                          a model selection is performed.
  # crit                     String with criterion.
  #                          Available criterions: "AIC" = Akaikes Info. Crit.
  #                                                "MAE" = Mean Abs. Error
  #                                                "MRE" = Mean Root Error
  # InSampleMAE              Boolean, deciding if in-sample-forecast based on
  #                          rolling forecasting origin is computed or not.
  #                          TRUE = Computation of in-sample-forecast.
  #                          FALSE = No computation.
  # Threshold                Character indicating if Thresholding is done on the
  #                          wavelet decomposition or not.
  #                          Default: Threshold="hard". Possible entries:
  #                          Threshold = "hard" for hard thresholding.
  #                          Threshold = "soft" for soft thresholding.
  #                          Any other input indicates no thresholding.
  # Lambda                   Numeric value indicating the threshold for
  #                          computing a hard or soft threshold on the wavelet
  #                          decomposition.
  # NumClusters      Number of clusters used for parallel computing.
  # itermax          Number of iterations for evolutionary optimization method.
  #
  # OUTPUT
  # Model    List of 7 elements containing model specifications:
  # Data[1:n]                Numerical vector with n time series values.
  # Method           String indicating which method to use
  #                  Available methods: 'r'  = Autoregression
  #                                     'nn' = Neural Network
  #                                     'elm' = Extreme Learning Machine
  #                                     'nnetar' = forecast::nnetar
  # Aggregation[1:Scales]    Numerical vector with best aggregation scheme found.
  # CoefficientCombination[1:Scales+1]    Numerical vector with best combination
  #                          of coefficients found for fixed aggregation scheme.
  # Horizon          Number indicating forecast horizon. Horizon = 1 means
  #                  one-step forecast and Horizon > 1 means a one-step forecast
  #                  and all multi-step forecasts from horizon 2 to 'Horizon'.
  # ModelError[1:TimeSteps4ModelSelection, 1:Horizon]    Numerical matrix with
  #                          one-/multi-steps in columns and the time steps
  #                          rowwise. The error is according to the scheme of a
  #                          rolling forecasting origin. The length depends on
  #                          the minimum required length for constructing the
  #                          wavelet model and the length of data. The newer
  #                          part of the data is used for the model fit
  #                          truncating the oldest data according to the minimum
  #                          required length for constructing the model.
  # ModelMAE                 Integer: Mean Absolute Error (MAE) computed for the
  #                          in-sample-forecast resulting from a rolling
  #                          forecasting origin.
  #
  # Author: QS, 08/2021
  if(!is.vector(Data)){
    message("Data must be of type vector")
    return()
  }
  if(!is.double(Horizon)){
    message("steps must be of type double")
    return()
  }
  len_Data = length(Data)
  if(Aggregation == "auto"){
    TryAggregations = list(c(2,4), c(2,4,8), c(2,4,8,16), c(2,4,8,16,32))
    AllAggregations = list()
    for(i in 1:4){
      numCoeffs = length(TryAggregations[[i]]) + 1
      if(Method=="r"){
        upper_limit <- rep(15,numCoeffs)
      }else{
        upper_limit <- rep(8,numCoeffs)
      }
      req = mrf_requirement(Data, upper_limit, TryAggregations[[i]])
      if(len_Data > req$MinLen){
        AllAggregations[[i]] = TryAggregations[[i]]
      }
    }
  }else if(!is.vector(Aggregation)){
    message("Aggregation must be of type vector")
    return()
  }else{
    req = mrf_requirement(Data, upper_limit, Aggregation)
    if(len_Data > req$MinLen){
      AllAggregations = list(Aggregation)
    }
  }
  if(length(AllAggregations) == 0){
    message("Data is too short the multiresolution forecasting method.")
    return()
  }
  lst_Results = list()
  selectMAE = c()
  counter = 1
  for(i in 1:length(AllAggregations)){
    lower_limit = 1
    if(Method=="r"){
      upper_limit = 15
    }else{
      upper_limit = 8
    }
    Aggregation = AllAggregations[[i]]
    ccps = NULL
    if(Method %in% c("r", "nn")){
      scales = length(Aggregation)
      len_ccps = scales + 1
      lower <- rep(lower_limit,len_ccps)
      upper <- rep(upper_limit,len_ccps)

      res = mrf_model_selection(UnivariateData = Data,
                                Aggregation=Aggregation,
                                Horizon = Horizon,
                                Window = TimeSteps4ModelSelection,
                                Method = Method,
                                crit = crit,
                                itermax = itermax,
                                lower_limit = lower,
                                upper_limit = upper,
                                NumClusters = NumClusters,
                                Threshold = Threshold,
                                Lambda = Lambda)
      ccps = res$CoefficientCombination
      lst_Results[[i]] = ccps
    }
    res2 = mrf_rolling_forecasting_origin(Data,
                                          Aggregation=Aggregation,
                                          CoefficientCombination=ccps,
                                          Horizon = Horizon,
                                          Window = TimeSteps4ModelSelection,
                                          Method = Method,
                                          NumClusters = NumClusters,
                                          Threshold = Threshold,
                                          Lambda = Lambda)
    Error     = res2$Error
    MAE       = sum(abs(res2$Error))/(TimeSteps4ModelSelection*Horizon)
    selectMAE = c(selectMAE, MAE)
  }
  IdxMin      = which(selectMAE==min(selectMAE))
  Aggregation = AllAggregations[[IdxMin]]
  Best        = NULL
  if(Method %in% c("r", "nn")){
    Best = lst_Results[[IdxMin]]
  }
  # In-sample-forecast
  InSampleMAE = Inf
  Error = Inf
  if(InSample==TRUE){
    res3   = mrf_requirement(Data, Best, Aggregation)
    minLen = res3$MinLen
    Window = length(Data)-minLen-Horizon
    res4 = mrf_rolling_forecasting_origin(Data,
                                          CoefficientCombination = Best,
                                          Aggregation            = Aggregation,
                                          Horizon                = Horizon,
                                          Window                 = Window,
                                          Method                 = Method,
                                          NumClusters            = NumClusters,
                                          Threshold              = Threshold,
                                          Lambda                 = Lambda)
    InSampleMAE = sum(abs(res4$Error))/(Window*Horizon)
    Error = res4$Error
  }
  return(list("Data"=Data,
              "Method"=Method,
              "Aggregation"=Aggregation,
              "CoefficientCombination"=Best,
              "Horizon"=Horizon,
              "ModelError"=Error,
              "ModelMAE"=InSampleMAE,
              "Threshold"=Threshold,
              "Lambda"=Lambda))
}

#
#
#
#
#
