#' Generate the artificial dataset
#'
#' This function generates simulated datasets with different attributes
#'
#' @param responseType The type of the dataset. The types include \code{multidim.nocorrel}, \code{multidim.withcorrel}, \code{discriminate}, \code{multidim.within}, and \code{testlets}.
#' @param theta A vector of the ability parameters range value, \code{c(min.theta,max.theta)}. It applies when the \code{randtype = "uniform"}.
#' @param ntheta The number of the observations.
#' @param sdtheta Standard deviation which is used to generate theta values using [stats::rnorm()] with \code{n = ntheta}, \code{mean = 0}, and \code{sd = sdtheta}.It applies when the \code{randtype = "normal"}.
#' @param beta A vector of the item difficulty parameters range value, \code{c(min.beta,max.beta)}. It applies when the \code{randtype = "uniform"}.
#' @param nitem The number of the items in each subgroup.
#' @param sdbeta Standard deviation which is used to generate item location values using [stats::rnorm()] with \code{n = nitem}, \code{mean = 0}, and \code{sd = sdbeta}.It applies when the \code{randtype = "normal"}.
#' @param ncat The number of the response categories
#' @param thGap The difference between adjacent threshold.
#' @param alpha A vector of the discrimination parameters apply to each items.
#' @param sdlambda A vector of the standard deviation to simulate the testlet (local dependency) effect. The effect is added using [stats::rnorm()] with \code{n = ntheta}, \code{mean = 0}, and \code{sd = sdlambda}
#' @param randtype The randomize type. This includes \code{uniform} and \code{normal}.
#' @param ndim The number of subgroups (dimensions/testlets) created.
#' @param dim.members The list of item members in each dimension.
#' @param corLevel The correlation between the two dimensions.
#' @param seed Integer seed for reproducibility.
#'
#' @return
#' The generated dataset as a \code{data.frame}.
#'
#' @import stats
#'
#' @examples
#' # 1. Multidimensional Polytomous Dataset with 0.2 Correlation
#' # Generate multidimensional dataset which having correlation of 0.2 between the dimensions
#' correl02_multidim <- generate_data(
#'   responseType = "multidim.withcorrel", corLevel = 0.2, seed = 2021
#' )
#'
#' # 2.  Within-item Multidimensional Polytomous Dataset
#' # Generate multidimensional dataset with some items relate to more than one
#' # dimension.
#' withinItem_multidim <- generate_data(
#'   responseType = "multidim.within", ndim = 3,
#'   dim.members = list(c(1:6,13),c(3,7:12),c(5,13:18)), seed = 2021
#' )
#'
#' # 3. Multi-testlets Polytomous Dataset
#' # Generate dataset which consist of two bundle items with different level of
#' # local dependency effect.
#' testlets_dataset <- generate_data(
#'   responseType = "testlets", ndim = 2, sdlambda = c(0,4), seed = 2021
#' )
#'
#' # 4a. Inhomogenous Dichotomous Dataset
#' # Generate dataset with binary type responses containing three subsets
#' # with different discrimination values.
#'
#' dicho_inh_dset <- generate_data(
#'   responseType = "discriminate", ncat = 2, seed = 2021,
#'   alpha = c(0.04,0.045,0.05,0.055,0.06,0.065,0.2,0.25,0.3,0.35,0.4,0.45,
#'             2.6,2.65,2.7,2.75,2.8,2.85)
#' )
#'
#' # 4b. Inhomogenous Polytomous Dataset
#' # Generate dataset with polytomous responses (five categories) containing
#' # three subsets with different discrimination values.
#'
#' poly_inh_dset <- generate_data(
#'   responseType = "discriminate", ncat = 5, seed = 2021,
#'   alpha = c(0.04,0.045,0.05,0.055,0.06,0.065,0.2,0.25,0.3,0.35,0.4,0.45,
#'             2.6,2.65,2.7,2.75,2.8,2.85)
#' )
#'
#' # 4c. Shorter Inhomogenous Polytomous Dataset
#' short_poly_data <- generate_data(
#'   alpha = c(0.02,0.5,2), nitem = 3, ndim = 3, ncat = 5,
#'   theta = c(-6,6), beta = c(-4,4), ntheta = 151, seed = 2021
#' )
#'
#' # 4d. Short Dataset containing DIF items
#' # Generate dataset with polytomous responses (five categories) containing
#' # three subsets with different discrimination values and two DIF-items.
#' seed <- c(54748,96765)
#' difset_short1 <- generate_data(responseType = "discriminate", ncat = 3,
#'                                 ntheta = 50, nitem = 3, ndim = 1,
#'                                 seed = seed[1], alpha = c(2))
#' difset_short2 <- generate_data(responseType = "discriminate", ncat = 3,
#'                                 ntheta = 50, nitem = 2, ndim = 1,
#'                                 seed = seed[2], alpha = c(0.8),
#'                                 beta = c(-2.5,2.5))
#' shortDIF <- cbind(rbind(difset_short1,difset_short1),
#'                    c(difset_short2[,1],difset_short2[,2]))
#'
#' # 5a. Uncorrelated Multidimensional Dichotomous Dataset
#' # Generate dataset with binary type responses containing three subsets which
#' # represent different uncorrelated dimensions.
#' dicho_md_dset <- generate_data(
#'   responseType = "multidim.nocorrel", ncat = 2, seed = 2021
#' )
#'
#' # 5b. Uncorrelated Multidimensional Polytomous Dataset
#' # Generate dataset with polytomous responses (five categories) containing
#' # three subsets which represent different uncorrelated dimensions.
#' poly_md_dset <- generate_data(
#'   responseType = "multidim.nocorrel", ncat = 5, seed = 2021
#' )
#'

#' @export
generate_data <- function(responseType = "multidim.nocorrel", theta = c(-3,3), sdtheta = 6, ntheta = 301, beta = c(-2.5,2.5), sdbeta = 4, nitem = 6,
                          alpha = c(1), sdlambda = 1, ncat = 5, thGap = 0.8, ndim = 3, randtype = "uniform", corLevel = 0,
                          dim.members = c(), seed = NULL){

  set.seed(seed)

  mB <- mA <- c()

  if(randtype == "uniform"){
    B <- seq(theta[1],theta[2],(sum(abs(theta))/(ntheta-1)))  #Make a sequence of ability score
  } else {
    B <- rnorm(ntheta, mean = 0, sd = sdtheta)
  }
  B.mat <- B

  if(responseType == "multidim.nocorrel" | responseType == "multidim.within"){
    for(i in 2:ndim){
      B.mat <- rbind(B.mat,sample(B,length(B)))
    }
  } else if(responseType == "multidim.withcorrel"){
    ndim <- 2
    for(i in 2:2){
      B.mat <- rbind(B.mat,sample(B,length(B)))
    }
    B.mat.hat <- B.mat
    av <- sqrt(0.5+(0.5*corLevel))
    bv <- sqrt(0.5-(0.5*corLevel))
    B.mat.hat[1,] <- av*B.mat[1,] + bv*B.mat[2,]
    B.mat.hat[2,] <- av*B.mat[1,] - bv*B.mat[2,]

    B.mat <- B.mat.hat

  } else if(randtype == "normal" & responseType == "random"){ # Multidimensional dataset is created by shuffling the ability score
    for(i in 2:ndim){
      B.mat <- rbind(B.mat,rnorm(ntheta, mean = 0, sd = sdtheta))
    }
  } else if(responseType == "discriminate" | responseType == "testlets"){ # Multidimensional dataset is created by shuffling the ability score
    for(i in 2:ndim){
      B.mat <- matrix(rep(B, each = (ndim)), nrow = ndim)
    }
  }


  if(randtype == "uniform"){
    D <- rep(seq(beta[1],beta[2],(sum(abs(beta))/((nitem)-1))),ndim) ## creating the global position of item diffictulty
  } else {
    D <- rep(rnorm(nitem, mean = 0, sd = sdbeta),ndim) #Make a sequence of difficulty score
  }


  if(length(thGap) == 1){
  if(ncat > 2){
    D.mat <- matrix(NA, ncol = (ncat-1), nrow = length(D)) #Create a set of thersholds scores

    tempCat <- 0
    if((ncat-1)%%2 == 0){
      j <- (ncat-1)/2+1
      for(i in ((ncat-1)/2):1){
        if(i == (ncat-1)/2){
          tempCat <- (thGap/2)
          D.mat[,i] <- D-tempCat
          D.mat[,j] <- D+tempCat
        } else {
          tempCat <- tempCat + thGap
          D.mat[,i] <- D-tempCat
          D.mat[,j] <- D+tempCat
        }
        j <- j+1
      }
    } else {
      j <- (ncat-1)/2+1
      for(i in ((ncat-1)/2):1){
        if(i == (ncat-1)/2){
          D.mat[,i] <- D
        } else {
          tempCat <- tempCat + thGap
          D.mat[,i] <- D-tempCat
          D.mat[,j] <- D+tempCat
          j <- j+1
        }
      }
    }
  } else {
    D.mat <- matrix(D, ncol = (ncat-1), nrow = length(D)) #Create a set of thersholds scores

  }
  }

  tempth <- c(0)
  if(length(thGap) > 1){
    for(i in 1:(ncat-2)){
      tempth <- c(tempth,tempth[i]+thGap[i])
    }

    D.mat <- c()
    tempth.mean <- mean(tempth)
    for(i in 1:length(D)){
      dif.th <- D[i] - tempth.mean
      D.mat <- rbind(D.mat, tempth + dif.th)
    }
  }

  temp.mx <- c()
  pmat.mx <- c()

  nthDim <- nitem*(ncat-1)
  nthres <- nthDim*ndim

  if(!is.null(dim.members)){

    test.mat <- c()
    for(i in seq_along(dim.members)){
      temp <- rep(0,(nitem*ndim))
      temp[dim.members[[i]]] <- 1
      temp <- rep(temp, each=(ncat-1))
      test.mat <- cbind(test.mat, temp)
    }
    mB <- test.mat
  } else {
    if(responseType == "multidim.withcorrel"){
      items <- c(1:(nitem*ndim))
      mB1.idx <- c(1:nitem)
      mB2.idx <- c((nitem+1):(nitem*2))
      mB1 <- mB2 <- rep(0,nitem*ndim)
      mB1[mB1.idx] <- 1
      mB1 <- rep(mB1,each = ncat-1)
      mB2[mB2.idx] <- 1
      mB2 <- rep(mB2,each = ncat-1)
      mB <- cbind(mB1,mB2)

    } else {
      if(is.null(mB)){
        mBvec <- c()
        if(ndim > 1){
          for(i in 1:(ndim-1)){
            mBvec <- c(mBvec,rep(1,nthDim),rep(0,nthres))
          }
          mB <- matrix(c(mBvec,rep(1,nthDim)),ncol = ndim,byrow = FALSE)
          # mB <- matrix(c(rep(1,nthDim),rep(0,nthres),rep(1,nthDim),rep(0,nthres),rep(1,nthDim)),ncol = ndim,byrow = FALSE)
        } else {
          mB <- matrix(1,ncol = 1,nrow = nthres)
        }
      }
    }
  }

  if(is.null(mA)){
    mA <- diag(1, nrow = nthres)
  }

  D.vector <- as.vector(t(D.mat))

  B.mult <- mB %*% B.mat
  D.mult <- mA %*% D.vector
  D.mult <- matrix(rep(diag(1,nrow = nthres)%*%D.mult,ntheta), nrow = nthres)

  mt_vek <- ncol(D.mat)
  mt_ind <- rep(seq_len(nrow(D.mat)),each = mt_vek)

  if(length(alpha) < (nitem*ndim)){
    if(length(alpha) == 1){
      alpha <- alpha[1]
      alphas <- rep(alpha,length(D.vector))
    } else {
      alphas <- c()
      for(i in 1:ndim){
        alphas <- c(alphas,rep(alpha[i],nthDim))
      }
    }
  } else if(length(alpha) == (nitem*ndim)){
    alphas <- rep(alpha,each = (ncat-1))
  } else {
    stop("The length of alpha is larger than the number of items!")
  }

  diff <- t(B.mult) - t(D.mult)

  if(responseType == "testlet"){
    mat.lambda <- c()
    for(i in seq_along(sdlambda)){
      lambda <- rnorm(ntheta, mean = 0, sd = sdlambda[i])
      lambda <- rep(lambda, (nitem*(ncat-1)))
      mat.lambda.temp <- matrix(lambda, nrow = ntheta, ncol = (nitem*(ncat-1)))
      mat.lambda <- cbind(mat.lambda,mat.lambda.temp)
    }

    diff <- diff + mat.lambda
  }

  if(ncat > 2){
    pmat.l <- tapply(seq_along(D.vector), mt_ind, function(xin){

      discr.diff <- t(diff[,xin])*(alphas[xin])

      cat.0 <- rep(0, ncol(discr.diff))
      discr.diff.0 <- rbind(cat.0, discr.diff)

      sum.discr.diff <- discr.diff.0

      for(i in 2:nrow(discr.diff.0)){
        sum.discr.diff[i,] <- colSums(discr.diff.0[1:i,], na.rm = TRUE)
      }

      sum.discr.diff.exp <- exp(sum.discr.diff)

      l1 <- sum.discr.diff.exp

      l2.temp <- colSums(sum.discr.diff.exp, na.rm = TRUE)
      l2 <- matrix(rep(l2.temp, nrow(sum.discr.diff)), ncol = ncol(l1), byrow = TRUE)

      pmat.part <- l1/l2

      return(t(pmat.part))
    })

    pmat <- matrix(unlist(pmat.l), nrow = length(B), byrow = FALSE)

    mt_ind <- rep(seq_len(nrow(D.mat)), each = (mt_vek+1))

    datagen <- apply(pmat, 1, function(pmat.r) {                       #runs over missing structures
      pmat.t <- pmat.r
      p.dat <- tapply(pmat.t,mt_ind,function(indx) {     #matrices of expected prob as list (over items)
        temp <- runif(1)
        i <- 1
        while(i < length(indx)){
          if(temp < indx[1]){
            part.data <- 0
          } else if(temp > sum(indx[1:i])){
            part.data <- (i)
          }
          i <- i + 1
        }
        return(part.data)
      })
      pdattemp <- p.dat
      return(pdattemp)
    })

    temp.mx <- cbind(temp.mx,t(datagen))
    pmat.mx <- cbind(pmat.mx,pmat)

    mxpmat <- as.data.frame(pmat.mx)

  } else {

    for(i in 1:ndim){
      idx <- c(((nitem*(i-1))+1):(nitem*i))
      X <- c()

      disc.diff <- exp(t(diff[,idx])*alphas[idx])
      Pr <- disc.diff/(1+disc.diff)

      for(i in seq_along(Pr)){
        X <- cbind(X,rbinom(1,1,Pr[i]))
      }

      temp.mx <- cbind(temp.mx,t(matrix(X,nrow = nitem)))
    }

    mxpmat <- c()

  }

  mxdat <- as.data.frame(temp.mx)
  colnames(mxdat) <- c(1:(nitem*ndim))
  colnames(mxdat) <- paste("I", colnames(mxdat), sep = "")

  return(mxdat)

}

