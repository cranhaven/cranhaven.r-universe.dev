#' Set up datasets for (A)CMTF input
#'
#' @param datasets List of arrays of datasets. Multi-way and two-way may be combined.
#' @param modes Numbered modes per dataset in a list. Example element 1: 1 2 3 and element 2: 1 4 for the X tensor and Y matrix case with a shared subject mode.
#' @param normalize Boolean specifying if the datasets should be normalized to Frobenium norm 1.
#'
#' Note: this function puts zeroes in positions with missing values.
#' The indices of missing data are conserved in the output.
#'
#' @return Z, a list with "object" listing the datasets, "sizes" with their size, "norms" with their norms and "missing" stating the missing data.
#' @export
#'
#' @examples
#' set.seed(123)
#' A = array(rnorm(108*2), c(108, 2))
#' B = array(rnorm(100*2), c(100, 2))
#' C = array(rnorm(10*2), c(10, 2))
#' D = array(rnorm(100*2), c(100,2))
#' E = array(rnorm(10*2), c(10,2))
#'
#' df1 = reinflateTensor(A, B, C)
#' df2 = reinflateTensor(A, D, E)
#' datasets = list(df1, df2)
#' modes = list(c(1,2,3), c(1,4,5))
#' Z = setupCMTFdata(datasets, modes, normalize=FALSE)
setupCMTFdata = function(datasets, modes, normalize=TRUE){

  numDatasets = length(datasets)
  numModes = max(unlist(modes))
  missing = list()
  sizes = list()

  tensors = list()
  for(p in 1:numDatasets){

    # Find and set missing values to zero, conserve indices
    missingMask = is.na(datasets[[p]])
    convertedDataset = datasets[[p]]
    convertedDataset[missingMask] = 0
    missing[[p]] = rTensor::as.tensor(array(as.numeric(!missingMask), dim(datasets[[p]])))
    tensors[[p]] = rTensor::as.tensor(convertedDataset)
  }

  # Calculate norms and normalize if requested
  norms = rep(1,numDatasets)
  for(p in 1:numDatasets){
    norms[p] = rTensor::fnorm(tensors[[p]])

    if(normalize == TRUE){
      tensors[[p]] = tensors[[p]] / rTensor::fnorm(tensors[[p]])
    }
  }

  # Fix sizes to only state unique dimensions corresponding to indices in modes
  sizes = rep(0, numModes)
  for(i in 1:numModes){
    for(p in 1:numDatasets){
      if(i %in% modes[[p]]){
        sizes[i] = dim(datasets[[p]])[modes[[p]] == i]
      }
    }
  }

  Z = list("object"=tensors, "modes"=modes, "sizes"=sizes, "norms"=norms, "missing"=missing)
  return(Z)
}
