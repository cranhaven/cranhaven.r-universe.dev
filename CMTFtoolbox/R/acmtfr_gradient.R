#' Calculate gradient of ACMTF model.
#'
#' @inheritParams acmtfr_fun
#'
#' @return Vectorized gradient of the ACMTF regression model.
#' @export
#'
#' @examples
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
#' Y = A[,1]
#'
#' init = initializeACMTF(Z, 2, output="vect")
#' g = acmtfr_gradient(init, Z, Y)
acmtfr_gradient = function(x, Z, Y, alpha=1, beta=rep(1e-3, length(Z$object)), epsilon=1e-8, pi=0.5, mu=1e-6){

  numDatasets = length(Z$object)
  numModes = max(unlist(Z$modes))
  Fac = vect_to_fac(x, Z)
  numComponents = ncol(Fac[[1]])
  reinflatedBlocks = reinflateFac(Fac, Z, returnAsTensor=TRUE)
  gradient = list()

  # Gradients per mode stored in a list, will be vectorized at the end.
  for(i in 1:numModes){
    gradient[[i]] = array(0L, dim(Fac[[i]]))

    # Gradient as generated per dataset
    # Note: this is different from CMTF because it multiplies the residuals by the lambdas
    for(p in 1:numDatasets){
      modes = Z$modes[[p]]

      if(i %in% modes){
        idx = which(modes==i)
        otherModes = modes[-idx]

        unfoldedX = rTensor::k_unfold(Z$missing[[p]], idx) * rTensor::k_unfold(Z$object[[p]], idx)
        unfoldedXhat = rTensor::k_unfold(Z$missing[[p]], idx) * rTensor::k_unfold(reinflatedBlocks[[p]], idx)
        lambdas = Fac[[numModes+1]][p,]

        if(length(modes) == 3){
          gradientMode = (unfoldedXhat - unfoldedX)@data %*% multiway::krprod(t(lambdas), multiway::krprod(Fac[[otherModes[2]]], Fac[[otherModes[1]]]))
        } else if(length(modes) == 2){
          gradientMode = (unfoldedXhat - unfoldedX)@data %*% Fac[[otherModes[1]]] %*% diag(x=lambdas, nrow=length(lambdas), ncol=length(lambdas))
        }
        else{
          stop(paste0("Number of modes is incorrect for block ", p))
        }

        gradient[[i]] = gradient[[i]] + gradientMode
      }
    }
  }

  # Gradient of A is dependent on pi
  gradient[[1]] = pi * gradient[[1]]

  # Gradient of norm 1 restriction
  for(i in 1:numModes){
    gradient[[i]] = gradient[[i]] + alpha * (Fac[[i]] - removeTwoNormCol(Fac[[i]]))
  }

  # Gradient of A related to Y
  A = Fac[[1]]
  coefs = safeSolve(t(A) %*% A, mu) %*% t(A) %*% Y
  Yhat = A %*% coefs
  gradient[[1]] = gradient[[1]] + (1 - pi) * (Yhat - Y) %*% t(coefs)

  # Gradient of the lambdas
  gradient[[numModes+1]] = array(0L, dim(Fac[[numModes+1]]))
  for(i in 1:numDatasets){
    modes = Z$modes[[i]]

    for(j in 1:numComponents){
      lambda_r = Fac[[numModes+1]][i,j]
      residuals = reinflatedBlocks[[i]] - Z$object[[i]]
      residuals = Z$missing[[i]] * residuals

      for(k in 1:length(modes)){
        residuals = rTensor::ttm(residuals, t(as.matrix(Fac[[modes[k]]][,j])), k)
      }

      gradient[[numModes+1]][i,j] = residuals@data[1] + ((beta[i]/2) * (lambda_r / (sqrt(lambda_r^2+epsilon))))
    }
  }

  g = fac_to_vect(gradient)
  return(g)
}
