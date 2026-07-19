#' Predict Y for new data by projecting the data onto the latent space defined by an ACMTF-R model.
#'
#' @param model ACMTF-R model
#' @param newX List object of new data, where each element corresponds to a block
#' @param Z Original input data used for the model
#' @param sharedMode Shared mode between the blocks (default 1).
#'
#' @return Ypred: the predicted value of Y for the new data
#' @export
#'
#' @examples
#' set.seed(123)
#' A = array(rnorm(108*2), c(108, 2))
#' B = array(rnorm(100*2), c(100, 2))
#' C = array(rnorm(10*2), c(10, 2))
#' D = array(rnorm(100*2), c(100, 2))
#' E = array(rnorm(10*2), c(10, 2))
#'
#' df1 = reinflateTensor(A, B, C)
#' df2 = reinflateTensor(A, D, E)
#' datasets = list(df1, df2)
#' modes = list(c(1,2,3), c(1,4,5))
#' Z = setupCMTFdata(datasets, modes)
#' Y = matrix(A[,1])

#' # Remove a sample and define
#' i = 1
#' Xtest = lapply(Z$object, function(x){x@data[i,,]})
#' Ytest = Y[i]

#' Xtrain = lapply(Z$object, function(x){x@data[-i,,]})
#' Ytrain = Y[-i]
#' Ztrain = setupCMTFdata(Xtrain, Z$modes)

#' model = acmtfr_opt(Ztrain,Ytrain,1,initialization="random",pi=1, nstart=1, max_iter=10)
#' Ypred = npred(model, Xtest, Ztrain, sharedMode=1)
npred = function(model, newX, Z, sharedMode=1){

  # 20250617 found another approach that is faster and easier:
  # TODO: implement
  #
  # Z1 = rTensor::khatri_rao(model$Fac[[3]], model$Fac[[2]]) * model$Fac[[6]][1,]
  # Z2 = rTensor::khatri_rao(model$Fac[[5]], model$Fac[[4]]) * model$Fac[[6]][2,]
  # G = crossprod(Z1) + crossprod(Z2)
  #
  # vectX1 = c(Z$object[[1]][1,,]@data)
  # vectX2 = c(Z$object[[2]][1,,]@data)
  # b = crossprod(Z1, vectX1) + crossprod(Z2, vectX2)
  #
  # a = solve(G,b)
  # a %*% model$rho

  numDatasets = length(Z$object)
  numComponents = length(model$rho)
  numModes = max(unlist(Z$modes))
  Fac = model$Fac

  # Find a projection matrix Zproj
  Zproj = list()
  for(p in 1:numDatasets){
    modes = Z$modes[[p]]
    idx = which(modes==sharedMode)
    otherModes = modes[-idx]

    lambdas = Fac[[numModes+1]][p,]

    if(length(modes)==3){
      Zproj[[p]] = multiway::krprod(t(lambdas), multiway::krprod(Fac[[otherModes[2]]], Fac[[otherModes[1]]]))
    } else{
      Zproj[[p]] = multiway::krprod(t(lambdas), Fac[[otherModes[1]]])
    }

  }

  # Combine Zproj elements and vectorize newX
  vectZ = do.call(rbind, Zproj)

  # Check if X has multiple samples
  if (length(dim(newX[[1]])) > 2){
    numSamples = dim(newX[[1]])[1]
  } else{
    numSamples = 1
  }
  Ypred = rep(NA, numSamples)

  # Calculate Ypred per sample
  # This is needed because you need to mask based on missing values per sample
  for(i in 1:numSamples){

    if(numSamples > 1){
      # newX_small = lapply(newX, function(x){x[i,,]}) # old approach: breaks for tensor-matrix case

      newX_small = list()
      for(p in 1:length(newX)){
        if(length(dim(newX[[p]]))==3){
          newX_small[[p]] = newX[[p]][i,,]
        } else{
          newX_small[[p]] = newX[[p]][i,]
        }
      }
      vectX = as.matrix(unlist(lapply(newX_small, c)))
    } else{
      vectX = as.matrix(unlist(lapply(newX, c)))
    }

    # Identify missing values in X, remove those from the calculation
    mask = !is.na(vectX)
    vectZ_small = vectZ[mask,]
    vectX_small = vectX[mask,]

    # Project vectX onto the latent space to obtain scores per component
    Zplus = safePseudoInverse(as.matrix(vectZ_small))
    newA = Zplus %*% vectX_small

    # Predict Y
    Ypred[i] = sum(newA * model$rho)
  }

  return(Ypred)
}
