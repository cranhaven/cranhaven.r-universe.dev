wavelet_decomposition <- function(UnivariateData, Aggregation = c(2,4,8,16,32),
                                  Threshold="hard", Lambda=0.05){
  # DESCRIPTION
  # This function decomposes a time series in its wavelet and smooth
  # coefficients using the redundant Haar wavelet transform.
  #
  # INPUT
  # UnivariateData[1:n]           Numerical vector with n time series values.
  #
  # OPTIONAL
  # Aggregation[1:Scales]         Numerical vector carrying numbers whose index
  #                               is associated with the wavelet level. The
  #                               numbers indicate the number of values used for
  #                               aggregation from the original time series.
  #
  # OUTPUT
  # UnivariateData[1:n]               Numerical vector with n time series values.
  # WaveletCoefficients[Scales, n]    Numerical matrix with wavelet coefficients.
  # SmoothCoefficients[Scales, n]     Numerical matrix with smooth coefficients.
  # Scales                            Number of wavelet levels.
  #
  # DETAILS
  #
  #
  # Author: QS, 02/2021
  if(!is.vector(UnivariateData)){
    message("Data must be of type vector!")
    return()
  }
  if(!is.vector(Aggregation)){
    message("agg_per_lvl must be of type vector")
    return()
  }
  intLenTS = length(UnivariateData)
  if(intLenTS < max(Aggregation)){
    message("The length of the time series or data is not long enough for given aggregation")
    return()
  }
  Scales = length(Aggregation)
  sprintf("Decomposition is built for %i levels", Scales)
  SmoothCoefficients = dynamic_aggregation(UnivariateData, Aggregation)
  WaveletCoefficients   = compute_wavelet_matrix(UnivariateData, intLenTS, Scales, SmoothCoefficients, WaveletCoefficients)
  if(is.character(Threshold)){
    if(Threshold=="hard"){
      WaveletCoefficients = hard_thresholding(WaveletCoefficients, Lambda)
    }else if(Threshold=="soft"){
      WaveletCoefficients = soft_thresholding(WaveletCoefficients, Lambda)
    }else{
      message("No thresholding is performed.")
    }
  }
  return(list("UnivariateData" = UnivariateData,
              "WaveletCoefficients" = WaveletCoefficients,
              "SmoothCoefficients" = SmoothCoefficients,
              "Scales" = Scales))
}

hard_thresholding = function(WaveletCoefficients, Lambda){
  NumLevels = dim(WaveletCoefficients)[1]
  HardThreshold = c()
  for(i in 1:NumLevels){
    tmpVar = quantile(abs(WaveletCoefficients[i,]), probs = Lambda)
    HardThreshold = c(HardThreshold, tmpVar)
  }
  HardThreshold = as.vector(HardThreshold)
  for(i in 1:NumLevels){
    WaveletCoefficients[i,abs(WaveletCoefficients[i,]) < HardThreshold[i]] = 0
  }
  return(WaveletCoefficients)
}

soft_thresholding = function(WaveletCoefficients, Lambda){
  NumLevels = dim(WaveletCoefficients)[1]
  HardThreshold = c()
  for(i in 1:NumLevels){
    tmpVar = stats::quantile(abs(WaveletCoefficients[i,]), probs = Lambda)
    HardThreshold = c(HardThreshold, tmpVar)
  }
  HardThreshold = as.vector(HardThreshold)
  Length = dim(WaveletCoefficients)[2]
  for(i in 1:NumLevels){
    WaveletCoefficients[i,abs(WaveletCoefficients[i,]) < HardThreshold[i]] = 0
  }
  for(i in 1:NumLevels){
    for(j in 1:Length){
      if(abs(WaveletCoefficients[i,j])>=HardThreshold[i]){
        WaveletCoefficients[i,j] = sign(WaveletCoefficients[i,j])*(abs(WaveletCoefficients[i,j]-HardThreshold[i]))
      }
    }
  }
  return(WaveletCoefficients)
}

first_level_aggregation <- function(data, time, number_aggregation_points){
  aggregation_sum = 0
  for(i in 0:(number_aggregation_points-1)){
    aggregation_sum = aggregation_sum + data[time-i]
  }
  averaged_aggregation = aggregation_sum/number_aggregation_points
  return(averaged_aggregation)
}

dynamic_aggregation <- function(data, agg_per_lvl){
  intLenTS = length(data)
  scales = length(agg_per_lvl)
  rhwtCoeff = matrix(0L, nrow = scales, ncol = intLenTS)    # Smooth coefficients
  for(level in 1:scales){
    start = agg_per_lvl[level]
    for(time in 1:intLenTS){
      if(time >= start){                                   # Construct only those time points, for which there is data (depends on scale)
        rhwtCoeff[level, time] = first_level_aggregation(data, time, agg_per_lvl[level])
      }
    }
  }
  return(rhwtCoeff)
}

compute_wavelet_matrix <- function(data, intLenTS, scales, rhwtCoeff, wmatrix){
  wmatrix   = matrix(0L, nrow = scales, ncol = intLenTS)    # Wavelet coefficients (Differences)
  for(time in 1:intLenTS){    # Consider all possible time points
    for(scale in 1:scales){   # Build all difference level
      if(scale == 1){
        wmatrix[scale, time] = data[time] - rhwtCoeff[scale, time]
      }
      else{
        wmatrix[scale, time] = rhwtCoeff[scale-1, time] - rhwtCoeff[scale, time]
      }
    }
  }
  return(wmatrix)
}



#
#
#
#
#
