mrf_requirement <- function(UnivariateData, CoefficientCombination, Aggregation){
  # DESCRIPTION
  # Computes the minimum size of data required for constructing given wavelet
  # forecasting model.
  #
  # INPUT
  # UnivariateData[1:n]                   Numerical vector with n time series
  #                                       values.
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
  # MinLen           Integer minimum required length for model
  # StartTraining    Integer indicating the index of time series at which
  #                  the training equations can be built up
  # NumberWeights    Number of weights required for building model
  # NumberEquations  Number of equations which can be built with given data.
  #
  # Author: QS, 08/2021
  if(!is.vector(UnivariateData)){
    message("Data must be of type vector")
    return()
  }
  maxConLen     = max(Aggregation)                                     # Req. to construct decompositon
  tmpVAR        = c(Aggregation, Aggregation[length(Aggregation)])
  minReqLen     = max(CoefficientCombination * tmpVAR)                 # Req. to construct model
  startTraining = minReqLen + maxConLen                                # Offset at start of time series
  len_data      = length(UnivariateData)                               # Length of swt dec
  intNumEq      = round((len_data - startTraining - 1), 0)             # Number of equations available for training
  numberWeights = sum(CoefficientCombination)                          # Number of equations needed
  MinLen        = startTraining + numberWeights
  Requirements  = TRUE
  if(numberWeights > intNumEq){     # There must be enough equations, otherwise no  optimization is performed
    Requirements = F
  }
  if(len_data < MinLen){
    Requirements = F
  }
  if(Requirements!=TRUE){
    MinLen = Inf
    startTraining = Inf
    numberWeights = Inf
    intNumEq = Inf
  }
  return(list("MinLen"=MinLen, "StartTraining"=startTraining, "NumberWeights"=numberWeights, "NumberEquations"=intNumEq))
}


#
#
#
#
#
