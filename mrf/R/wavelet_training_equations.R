wavelet_training_equations <- function(UnivariateData, WaveletCoefficients,
                                       SmoothCoefficients, Scales,
                                       CoefficientCombination, Aggregation){
  # INPUT
  # UnivariateData[1:n]                   Numerical vector with n time series
  #                                       values.
  # WaveletCoefficients[Scales, n]    Matrix with 'Scales' many wavelet scales
  #                                   row-wise with n columns corresponding
  #                                   to the time domain of a time series
  # SmoothCoefficients[Scales, n]     Matrix with 'Scales' many smooth
  #                                   approximation scales row-wise with n
  #                                   columns corresponding to the time domain
  #                                   of a time series.
  # Scales                            Number of wavelet levels.
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
  #
  # OUTPUT
  # points_in_future[1:n] n many values of the time series, for which there
  #                       is an equation from a prediction scheme.
  # lsmatrix[m,n]         Matrix carrying predictive equations associated with a
  #                       specific value of the time series.
  #
  # Author: QS, 02/2021
  if(!is.vector(UnivariateData)){
    message("arrValues must be of type vector")
    return()
  }
  if(!is.matrix(WaveletCoefficients)){
    message("wmatrix must be of type matrix")
    return()
  }
  if(!is.matrix(SmoothCoefficients)){
    message("rhwtCoeff must be of type matrix")
    return()
  }
  #if(!is.double(scales)){
  #  message("scales must be of type double")
  #  return()
  #}
  if(!is.vector(CoefficientCombination)){
    message("ccps must be of type vector")
    return()
  }
  if(!is.vector(Aggregation)){
    message("agg_per_lvl must be of type vector")
    return()
  }
  if(length(CoefficientCombination) != (Scales + 1)){
    message("Length of ccps must be number of scales from decomposition + 1")
    return()
  }
  maxConLen = max(Aggregation)                                          # Range needed for constructing decomposition
  maxReqLen = get_required_training_length(Scales, CoefficientCombination, Aggregation)   # Range needed for constructing model
  startTraining = maxReqLen + maxConLen                                 # Offset needed at the start for choosing coefficients
  len_data = length(UnivariateData)                                     # Length of swt dec
  intNumEquations = round((len_data - startTraining-1), 0)              # Number of equations available for training
  numberWeights = sum(CoefficientCombination)                           # Number of equations needed
  if(numberWeights > intNumEquations){
    message("There are not enough equations for training. Your time series is too short!")
    return()
  }
  lsmatrix = matrix(data = 0, nrow = intNumEquations, ncol = numberWeights)    # Matrix for equations
  for(i in 1:intNumEquations){
    time = startTraining + i
    counter = 1
    for(s in 0:(Scales)){
      for(k in 0:(CoefficientCombination[s+1]-1)){
        if(s != (Scales)){
          index = time - k*Aggregation[s+1]
          lsmatrix[i, counter] = WaveletCoefficients[s+1, index]
          counter = counter +  1
        }
        else{
          index = time - k*Aggregation[s]
          lsmatrix[i, counter] = SmoothCoefficients[s, index]
          counter = counter + 1
        }
      }
    }
  }
  #target_vector = arrValues[(length(arrValues)-intNumEquations+1):length(arrValues)]
  target_vector = as.vector(UnivariateData[(length(UnivariateData)-intNumEquations+1):length(UnivariateData)])
  result = list("points_in_future" = target_vector, "lsmatrix" = lsmatrix)
  return(result)
}

get_required_training_length = function(scales, ccps, agg_per_lvl = NULL){
  minCut = numeric(length(ccps))
  for(i in 1:length(ccps)){
    if(i != length(ccps)){
      minCut[i] = ccps[i]*agg_per_lvl[i]
    }else{
      minCut[i] = ccps[i]*agg_per_lvl[i-1]
    }
  }
  return(max(minCut))
}


#
#
#
#
#
