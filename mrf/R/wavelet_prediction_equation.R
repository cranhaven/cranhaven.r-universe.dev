wavelet_prediction_equation <- function(WaveletCoefficients, SmoothCoefficients,
                                        CoefficientCombination, Aggregation){
  # INPUT
  # WaveletCoefficients[Scales, n]    Matrix with 'Scales' many wavelet scales
  #                                   row-wise with n columns corresponding
  #                                   to the time domain of a time series
  # SmoothCoefficients[Scales, n]     Matrix with 'Scales' many smooth
  #                                   approximation scales row-wise with n
  #                                   columns corresponding to the time domain
  #                                   of a time series
  # CoefficientCombination[1:Scales+1]    Numerical vector with numbers which
  #                                       are associated with wavelet levels.
  #                                       The last number is associated with the
  #                                       smooth level. Each number determines
  #                                       the number of coefficient used per
  #                                       level. The selection follows a
  #                                       specific scheme.
  # Aggregation[1:Scales]    Numerical vector carrying numbers whose index is
  #                          associated with the wavelet level. The numbers
  #                          indicate the number of time in points used for
  #                          aggregation from the original time series.
  #
  # OUTPUT
  # future_point    Numerical value carrying one step forecast.
  #
  # Author: QS, 02/2021

  #if(!is.vector(weights)){
  #  message("weights must be of type vector")
  #  return()
  #}
  #if(!is.matrix(wmatrix)){
  #  message("wmatrix must be of type matrix")
  #  return()
  #}
  #if(!is.matrix(rhwtCoeff)){
  #  message("rhwtCoeff must be of type matrix")
  #  return()
  #}
  #if(!is.vector(ccps)){
  #  message("ccps must be of type vector")
  #  return()
  #}
  #if(!is.vector(agg_per_lvl)){
  #  message("agg_per_lvl must be of type vector")
  #  return()
  #}

  time = dim(WaveletCoefficients)[2]
  future_point = c()
  scales = length(Aggregation)
  for(s in 0:scales){
    for(k in 0:(CoefficientCombination[s+1]-1)){
      if(s != scales){
        index = time - k*Aggregation[s+1]
        future_point = c(future_point, WaveletCoefficients[s+1, index])
      }
      else{
        index = time - k*Aggregation[s]
        future_point = c(future_point, SmoothCoefficients[s, index])
      }
    }
  }
  return(future_point)
}

#
#
#
#
#
