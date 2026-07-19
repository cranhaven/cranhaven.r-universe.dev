#' Compute Factor Match Score for two models.
#'
#' @param Fac1 A list of matrices corresponding to found components per mode in model 1.
#' @param Fac2 A list of matrices corresponding to found components per mode in model 2.
#'
#' @return Scalar of FMS value
#' @export
#'
#' @examples
#' set.seed(123)
#'
#' I = 10
#' J = 5
#' K = 3
#' df = array(rnorm(I*J*K), c(I,J,K))
#' datasets = list(df, df)
#' modes = list(c(1,2,3), c(1,4,5))
#' Z = setupCMTFdata(datasets, modes)
#'
#' model1 = acmtf_opt(Z, 1)
#'
#' Fac1 = model1$Fac[1:3]
#' Fac2 = Fac1 # identical models for the purposes of demonstration
#' result = FMS_random(Fac1, Fac2) # [1] 1
FMS_random = function(Fac1, Fac2){

  # Make robust towards 1-component case
  Fac1 = lapply(Fac1, as.matrix)
  Fac2 = lapply(Fac2, as.matrix)

  # Setup
  numComponents = ncol(Fac1[[1]])
  numModes = length(Fac1)

  stopifnot(length(Fac1) == length(Fac2))

  FMSresult = matrix(1L, nrow=numComponents, ncol=numComponents)
  for(i in 1:numModes){
    similarityMatrix = matrix(0L, nrow=numComponents, ncol=numComponents)

    for(j in 1:numComponents){
      for(k in 1:numComponents){
        vect1 = as.matrix(Fac1[[i]][,j])
        vect2 = as.matrix(Fac2[[i]][,k])
        similarityMatrix[j,k] = abs(t(vect1) %*% vect2) / (norm(vect1, "F") * norm(vect2, "F"))
      }
    }
    FMSresult = FMSresult * similarityMatrix
  }

  # Find best combination
  mapping = clue::solve_LSAP(FMSresult, maximum=TRUE)

  # Find mapping matrix and calculate FMS
  mappingMatrix = cbind(seq_along(mapping), mapping)

  result = (sum(FMSresult[mappingMatrix])) / numComponents

  return(result)
}
