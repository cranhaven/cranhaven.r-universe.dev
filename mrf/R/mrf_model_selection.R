mrf_model_selection <- function(UnivariateData, Aggregation, Horizon = 1,
                                Window = 2, Method = "r", crit = "AIC",
                                itermax = 1, lower_limit = 1, upper_limit = 2,
                                NumClusters = 1, Threshold="hard",Lambda=0.05){
  # DESCRIPTION
  # Computes best model for a fixed aggregation scheme on given data selecting
  # the best method on the training data based on the last Window=2 time steps.
  # This function does not treat Method="elm"
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
  # crit             String with criterion.
  #                  Available criterions: "AIC" = Akaikes Info. Crit.
  #                                        "MAE" = Mean Abs. Error
  #                                        "MRE" = Mean Root Error
  # itermax          Number of iterations for evolutionary optimization method.
  # lower_limit      Numeric vector: Lower limit for coefficients selected for each level.
  # upper_limit      Numeric vector: Higher limit for coefficients selected for each level.
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
  #
  # OUTPUT
  # Aggregation[1:Scales]    Numerical vector carrying numbers whose index is
  #                          associated with the wavelet level. The numbers
  #                          indicate the number of time in points used for
  #                          aggregation from the original time series.
  # CoefficientCombination[1:Scales+1]    Numerical vector with numbers which
  #                                       are associated with wavelet levels.
  #                                       The last number is associated with the
  #                                       smooth level. Each number determines
  #                                       the number of coefficient used per
  #                                       level. The selection follows a
  #                                       specific scheme.
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
  if(Method %in% c("elm", "nnetar")){
    message("This function does not work with 'elm'.")
    return()
  }
  NumCoeff = length(Aggregation)+1
  if(length(lower_limit) != NumCoeff){
    lower_limit = rep(lower_limit, NumCoeff)
  }
  if(length(upper_limit) != NumCoeff){
    upper_limit = rep(upper_limit, NumCoeff)
  }

  #if(!is.double(numClusters)){
  #  message("numClusters must be of type double")
  #  return()
  #}
  len_data = length(UnivariateData)
  if (!requireNamespace('DEoptim', quietly = TRUE)) {
    message(
      "Package DEoptim is missing in function evolutionary_optim.
      No computations are performed.
      Please install the packages which are defined in 'Suggests'"
    )
    return()
  }

  res = DEoptim::DEoptim(crit_rolling_window, lower_limit, upper_limit, fnMap = round,
                         UnivariateData = UnivariateData, Aggregation = Aggregation,
                         Window = Window,
                         Horizon = Horizon, Method = Method, NumClusters = NumClusters,
                         Threshold=Threshold,Lambda=Lambda,
                         crit = crit,
                         control = DEoptim::DEoptim.control(itermax = itermax))
  ccps = as.numeric(res$optim$bestmem)
  return(list("Aggregation"=Aggregation, "CoefficientCombination"=ccps))
}

crit_rolling_window <- function(CoefficientCombination, Aggregation,
                                UnivariateData, Window, Horizon,
                                Method, crit = "AIC", NumClusters = "max",
                                Threshold="hard",Lambda=0.05){
  res = mrf_rolling_forecasting_origin(UnivariateData = UnivariateData,
                                       Aggregation = Aggregation,
                                       CoefficientCombination = CoefficientCombination,
                                       Horizon = Horizon,
                                       Window = Window,
                                       Method = Method,
                                       NumClusters = NumClusters,
                                       Threshold = Threshold,
                                       Lambda = Lambda)
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
