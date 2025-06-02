mrf_evaluation <- function(UnivariateData, Horizon = 14, TestLength=2,
                            TimeSteps4ModelSelection=2, Method="r", MultivariateData=NULL,
                            NumMV=1, NumClusters=1,Threshold="hard",Lambda=0.05){
  # DESCRIPTION
  # Evaluates a best performing model on given univariate time series data.
  # Splits data into 3 parts: Train, test and evaluation dataset. Trains
  # multiple models on training data. Selects best model on test dataset.
  # Evaluates forecasting performance of best model on evaluation dataset.
  # User can control size of all 3 datasets with EvaluationLength and TimeSteps4ModelSelection
  #
  #
  # INPUT
  # UnivariateData[1:n]      Numerical vector with n values
  #
  # OPTIONAL
  # Horizon             Number indicating horizon for forecast from 1 to horizon.
  # TestLength          Number of forecasts on test data.
  # TimeSteps4ModelSelection    Number indicating how many datapoints are used
  #                             in order to select a trained model on the last
  #                             'TimeSteps4ModelSelection' many time steps.
  # Method              String indicating which method to use
  #                     Available methods: 'r'  = Autoregression
  #                                        'nn' = Neural Network
  # MultivariateData    Not implemented yet.
  # NumMV               Not implemented yet.
  # NumClusters         Number of clusters used for parallel computing.
  #
  #
  # OUTPUT
  # Best[1:Scales+1]                Numerical vector with integers associated
  #                                 with the best found number of coefficients
  #                                 per wavelet scale (1:Scales) and number of
  #                                 coefficients for the smooth approximation
  #                                 level in the last entry.
  # Error[1:Window,1:Horizon]       Numerical Matrix with 'Window' many rows
  #                                 entries indicating one time point with
  #                                 'Horizon' many forecast errors.
  # Forecast[1:Window,1:Horizon]    Numerical Matrix with 'Window' many rows
  #                                 entries indicating one time point with
  #                                 'Horizon' many forecasts.
  #
  # Author: QS, 02/2021
  DataLength = length(UnivariateData)
  # Create Test data
  # => There is no need for explicitly defining an evaluation dataset in case of a split in test and evaluation data.
  TestUnivariateData = UnivariateData[0:(DataLength - TestLength)]


  if (length(TestUnivariateData) != (DataLength - TestLength)){
    message("Something went wrong when splitting Data in Test and Evaluation part in modelSelection.py!")
  }
  if(length(TestUnivariateData) == 0){
    message("modelSelection: There is no Testdata for computing a model!")
  }
  #if(typeof(MultivariateData) == np.ndarray){
  #  TestMultivariateData = MultivariateData[0:(DataLength - EvaluationLength)]
  #}
  #if(TestMultivariateData.shape[0] != (DataLength - EvaluationLength)){
  #  print("Something went wrong when splitting MultivariateData in Test and Evaluation part in nested_cross_validation.R!")
  #}

  #if(length(TestUnivariateData) == 0){
  #  print("nested_cross_validation.R: There is no Testdata (Multivariate data part) for computing a model!")
  #}else{
  #  TestMultivariateData = NULL
  #}

  AllAggregations = list(c(2,4), c(2,4,8), c(2,4,8,16), c(2,4,8,16,32))

  lst_Results = list()
  selectMAE = c()
  counter = 1
  for(i in 1:length(AllAggregations)){
    #NumCoeffs = length(Aggregation) + 1
    lower_limit = 1
    if(Method=="r"){
      upper_limit = 15
    }else{
      upper_limit = 8
    }
    Aggregation = AllAggregations[[i]]
    res = model_selection_part(UnivariateData = TestUnivariateData,
                               Aggregation=Aggregation,
                               Horizon = Horizon,
                               Window = TimeSteps4ModelSelection,
                               Method = Method,
                               crit = "AIC",
                               itermax = 1, lower_limit = lower_limit,
                               upper_limit = upper_limit,
                               NumClusters = NumClusters,
                               Threshold=Threshold,
                               Lambda=Lambda)
    MAE = sum(abs(res$Error))/(TimeSteps4ModelSelection*Horizon)
    lst_Results[[i]] = res$Best
    selectMAE = c(selectMAE, MAE)
  }
  IdxMin = which(selectMAE==min(selectMAE))
  Best = lst_Results[[IdxMin]]
  Aggregation = AllAggregations[[IdxMin]]
  res = mrf_rolling_forecasting_origin(UnivariateData = UnivariateData,
                                       CoefficientCombination = Best,
                                       Aggregation = Aggregation,
                                       Horizon = Horizon,
                                       Window = TestLength,
                                       Method = Method,
                                       NumClusters = NumClusters,
                                       Threshold = Threshold,
                                       Lambda = Lambda)
  return(list("Best"=Best, "Error"=res$Error, "Forecast"=res$Forecast))
}

model_selection_part <- function(UnivariateData, Aggregation, Horizon = 1,
                             Window = 2, Method = "r", crit = "AIC",
                             itermax = 1, lower_limit = 1, upper_limit = 2,
                             NumClusters = 1,Threshold="hard", Lambda=0.05){
  # DESCRIPTION
  # Computes best model for a fixed aggregation scheme on two datasets.
  # Training and test.
  #
  # INPUT
  # UnivariateData[1:n]      Numerical vector with n values
  # Aggregation[1:Scales]    Numerical vector carrying numbers whose index is
  #                          associated with the wavelet level. The numbers
  #                          indicate the number of time in points used for
  #                          aggregation from the original time series.
  #
  #
  # OPTIONAL
  # Horizon          Number indicating horizon for forecast from 1 to horizon.
  # Window           Number indicating how many points are used for cross validation.
  # Method           String indicating which method to use
  #                  Available methods: 'r'  = Autoregression
  #                                     'nn' = Neural Network
  # crit             String indicating which criterion to use.
  #                  Available criterion: AIC = Akaikes Information Criterion
  #                                       MRE = Mean Root Error
  # itermax          Number of iterations for evolutionary optimization method.
  # lower_limit      Lower limit for coefficients selected for each level.
  # upper_limit      Higher limit for coefficients selected for each level.
  # NumClusters      Number of clusters used for parallel computing.
  #
  #
  # OUTPUT
  # Error[1:Window,1:Horizon]       Numerical Matrix with 'Window' many rows
  #                                 entries indicating one time point with
  #                                 'Horizon' many forecast errors.
  # Best[1:Scales+1]                Numerical vector with integers associated
  #                                 with the best found number of coefficients
  #                                 per wavelet scale (1:Scales) and number of
  #                                 coefficients for the smooth approximation
  #                                 level in the last entry.
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
  if(!is.character(crit)){
    message("crit must be of type character")
    return()
  }
  if(!is.double(itermax)){
    message("itermax must be of type double")
    return()
  }
  if(!is.double(lower_limit)){
    message("lower_limit must be of type double")
    return()
  }
  if(!is.double(upper_limit)){
    message("upper_limit must be of type double")
    return()
  }
  #if(!is.double(numClusters)){
  #  message("numClusters must be of type double")
  #  return()
  #}
  len_data = length(UnivariateData)
  scales = length(Aggregation)
  len_ccps = scales + 1
  lower <- rep(lower_limit,len_ccps)
  upper <- rep(upper_limit,len_ccps)

  if (!requireNamespace('DEoptim', quietly = TRUE)) {
    message(
      "Package DEoptim is missing in function evolutionary_optim.
      No computations are performed.
      Please install the packages which are defined in 'Suggests'"
    )
    return()
  }
  res = DEoptim::DEoptim(crit_rolling_window, lower, upper, fnMap = round,
                         UnivariateData = UnivariateData, Aggregation = Aggregation,
                         Window = Window,
                         Horizon = Horizon, Method = Method, NumClusters = NumClusters,
                         Threshold=Threshold, Lambda=Lambda,
                         crit = crit,
                         control = DEoptim::DEoptim.control(itermax = itermax))
  ccps = as.numeric(res$optim$bestmem)
  res = mrf_rolling_forecasting_origin(UnivariateData,
                                       CoefficientCombination = ccps,
                                       Aggregation = Aggregation,
                                       Horizon = Horizon,
                                       Window = Window,
                                       Method = Method,
                                       NumClusters = NumClusters,
                                       Threshold = Threshold,
                                       Lambda = Lambda)
  mat_error = res$Error
  return(list("Error"=mat_error, "Best"=ccps))
}

crit_rolling_window <- function(CoefficientCombination, Aggregation, UnivariateData, Window, Horizon,
                                Method, crit = "AIC", NumClusters = "max"){
  res = mrf_rolling_forecasting_origin(UnivariateData = UnivariateData,
                                       CoefficientCombination = CoefficientCombination,
                                       Aggregation = Aggregation,
                                       Horizon = Horizon,
                                       Window = Window,
                                       Method = Method,
                                       NumClusters = NumClusters,
                                       Threshold=Threshold,
                                       Lambda=Lambda)
  mat_error = res$Error
  MAE = sum(abs(mat_error))/(Window*Horizon)
  AIC = length(UnivariateData) * log(MAE^2) + 2*(sum(CoefficientCombination)+1)
  if(crit == "MAE"){
    return(MAE)
  }
  if(crit == "AIC"){
    return(AIC)
  }
  if(crit == "MRE"){
    SRE = (sum(sqrt(as.complex(((-1)*mat_error)))))
    MRE = SRE/(Window*Horizon)
    a = Re(MRE)
    b = Im(MRE)
    magnitude = abs(MRE)
    gamma = -1
    if(a > 0){
      gamma = atan(b/a)
    }else{
      if((a == 0) && (b > 0)){
        gamma = pi/2
      }
      else{
        if((a==0) && (b==0)){
          gamma = 0
        }
      }
    }
    kappa = 1 - ((4*gamma)/pi)
    qm_vec = c(magnitude, kappa)
    pareto = norm(as.matrix(qm_vec))
    return(pareto)
  }
}

#
#
#
#
#
