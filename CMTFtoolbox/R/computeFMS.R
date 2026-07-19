#' Compute Factor Match Score for two models.
#'
#' @param Fac1 A list of matrices corresponding to found components per mode in model 1.
#' @param Fac2 A list of matrices corresponding to found components per mode in model 2.
#' @param modes List of modes per dataset.
#'
#' @return Vector of FMS scores, one per dataset.
#' @export
#'
#' @examples
#' A = array(rnorm(108*2), c(108, 2))
#' B = array(rnorm(100*2), c(100, 2))
#' C = array(rnorm(10*2), c(10, 2))
#' D = array(rnorm(100*2), c(100, 2))
#' E = array(rnorm(10*2), c(10, 2))
#'
#' Fac1 = list(A,B,C,D,E)
#' Fac2 = Fac1 # identical models for the purposes of demonstration
#' modes = list(c(1,2,3), c(1,4,5))
#
#' FMS_result = computeFMS(Fac1, Fac2, modes) # FMS_result = c(1,1)
computeFMS = function(Fac1, Fac2, modes){

  # Make robust towards 1-component case
  Fac1 = lapply(Fac1, as.matrix)
  Fac2 = lapply(Fac2, as.matrix)

  # Setup
  numComponents = ncol(Fac1[[1]])
  numDatasets = length(modes)

  # Recognize which modes are shared between all datasets
  inCommon = findSharedModes(modes)

  # Exclude shared modes from FMS computation
  modesToCheck = list()
  for(i in 1:numDatasets){
    modesToCheck[[i]] = setdiff(modes[[i]], inCommon)
  }

  # Calculate FMS per dataset
  FMS_result = rep(0, numDatasets)
  for(i in 1:numDatasets){
    allModesToCheck = modesToCheck[[i]]
    numModes = length(allModesToCheck)
    FMS = 0
    numComparisons = 0

    for(j in 1:numComponents){
      FMSproposal = list()
      for(k in 1:numComponents){
        total = 1

        for(m in 1:numModes){
          mode = allModesToCheck[m]
          vect1 = as.matrix(Fac1[[mode]][,j])
          vect2 = as.matrix(Fac2[[mode]][,k])

          term = abs(t(vect1) %*% vect2) / (norm(vect1,"F")*norm(vect2,"F"))
          total = total * term
        }
        FMSproposal[[k]] = total
      }
      FMS = FMS + max(unlist(FMSproposal))
      numComparisons = numComparisons + 1
    }

    if(numComparisons > 0){
      FMS_result[i] = FMS / numComparisons
    }
    else{
      FMS_result[i] = NA
    }
  }

  return(FMS_result)
}
