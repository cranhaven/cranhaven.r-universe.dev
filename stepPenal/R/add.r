#'
#' Simulate data with normally distributed predictors and binary response
#' @name SimData
#' @param N sample size
#' @param beta coefficients (effect of informative predictors)
#' @param noise variables (effect of uninformative predictors)
#' @param corr Logical, if FALSE the function generates uncorrelated predictors,
#' if TRUE  the correlation between predictors is 0.5 by default and the user can supply a different value in
#' the corr.effect argument.
#' @param corr.effect the correlation between informative predictors.
#' @return A data frame N x p, where p is the total number of informative and uninformative predictors.
#' The first column of the dataframe is the binary response variable y
#' @details The response y follows a Binomial distribution with probability= exp(X*beta)/(1+exp(X*beta))
#' @export
#' @examples
#' # simulate data with N=100 (sample size) and 23 predictors; 4 informative and 20 noise
#'
#' set.seed(14)
#' beta    <- c(3, 2, -1.6, -4)
#' noise <- 5
#' N     <- 100
#' simData <- SimData(N=N, beta=beta, noise=noise, corr=FALSE)
#'

SimData <- function(N, beta, noise, corr=TRUE, corr.effect=0.5){
  #install.packages("mvtnorm")
  beta <- c(1,beta)

  if (corr==TRUE){
    sigma <- (diag((length(beta)-1))+corr.effect)- diag((length(beta)-1))*corr.effect
    X     <- mvtnorm::rmvnorm(N, mean = rep(0,(length(beta)-1)), sigma=sigma)
    X     <- cbind(rep(1,N), X)
    Xb    <- X %*% beta
    sigma2 <- (diag(noise)+corr.effect)- diag(noise)*corr.effect
    u     <- mvtnorm::rmvnorm(N, mean = rep(0,noise), sigma=sigma2)
  }
  if (corr==FALSE){
    X     <- matrix( stats::rnorm(N*(length(beta)-1)) , N, length(beta)-1)
    X     <- cbind(rep(1,N), X)
    Xb    <- X %*% beta
    u     <- matrix(stats::rnorm(N*noise), N, noise)
  }
  expit   <- function(x) { exp(x)/(1+exp(x))}
  prob    <- expit(Xb)
  y       <- stats::rbinom(N,1,prob)
  mydata  <- data.frame(y, X[,-1], u)
  names(mydata)[-1] <- c(paste0('X', 1:(length(beta)-1)), paste0('u', 1:noise))
  return(mydata)
}


#' Objective function
#'
#' Objective (non-convex) function to minimize (objFun=-logL+ lamda*CL, CL= (1-w)L0 + wL1)
#' @name objFun
#' @param x input matrix, of dimension nobs x nvars. Each row is an observation vector.
#' @param y binary response variable
#' @param lamda a tuning penalty parameter
#' @param w the weighting parameter for L1; then (1-w) is the weight for L0
#' @param beta coefficients
#' @param epsilon the continuity parameter
#' @return the value of the objective function evaluated at the given points.
#' @export
#' @examples
#'
#' set.seed(14)
#' beta    <- c(3, 2, -1.6, -1)
#' noise   <- 5
#' simData <- SimData(N=100, beta=beta, noise=noise, corr=FALSE)
#'
#' x  <- as.matrix(simData[,-1][,1])
#' y  <- as.matrix(simData$y)
#' betapoints <- seq(-2,2,0.01)
#'
#' lamda <- 1
#' w     <- 0.6
#' epsilon <- 0.1
#'
#' out <- numeric(length(betapoints))
#' for(i in 1:length(betapoints)){
#'  out[i]<- objFun(x, y, lamda=lamda, w=w, beta=betapoints[i], epsilon=epsilon)
#' }
#' plot(betapoints, out, type="l", ylab="objFun")
#'


objFun <- function(x, y, lamda, w, beta, epsilon) {

  logL <- function(x, y, beta){
    loglik   <- - sum( -y*log(1 + exp(-(x%*%beta))) - (1-y)*log(1 + exp(x%*%beta)))
    return(loglik)
  }
f1 <- logL(x, y,beta)

CL <- function(beta, w, epsilon) {
  # begin L0
  L0 <- function(beta, epsilon) {
    p <- length(beta)
    L0_j <- function(beta_j, epsilon) {
      y <- NULL
      if (abs(beta_j) >= epsilon) {
        y <- 1
      } else {
        y <- abs(beta_j) / epsilon
      }
      return(y)
    }
    y <- sum(sapply(beta, L0_j, epsilon))
    return(y)
  }
  # end L0
  beta <- as.matrix(beta)
  y <- (1-w) * L0(beta, epsilon) + w * norm(beta, type="1")
  return(y)
}
f  <- f1 + lamda*CL(beta, w, epsilon)
return(f)
}



#' Stepwise forward variable selection using penalized regression.
#'
#' Stepwise forward variable selection based on the combination of L1 and L0 penalties.
#' The optimization is done using the "BFGS" method in stats::optim
#' @name  StepPenal
#' @param Data should have the following structure: the first column must be the binary response variable y.
#' @param lamda the tuning penalty parameter
#' @param standardize Logical flag for the predictors' standardization, prior to fitting the model.
#' Default is standardize=TRUE
#' @param w the weight parameter for the sum (1-w)L0+ wL1
#' @return a list with the shrinked coefficients and the names of the selected variables, i.e those variables with
#' an estimated coefficient different from zero. It also returns the value of the objective function, evaluated for the
#' values of the coefficients.
#' @details lamda and w  parameters need to be tuned by cross-Validation using stepPenal::tuneParam
#' @references Vradi E, Brannath W, Jaki T, Vonk R. Model selection based on combined penalties for biomarker
#' identification. Journal of biopharmaceutical statistics. 2018 Jul 4;28(4):735-49.
#' @seealso \code{\link[stats]{optim}}
#' @export
#' @examples
#' # use the StepPenal function on a simulated dataset, with given lamda and w.
#'
#' set.seed(14)
#' beta    <- c(3, 2, -1.6, -1)
#' noise   <- 5
#' simData <- SimData(N=100, beta=beta, noise=noise, corr=FALSE)
#' \dontrun{
#' before <- Sys.time()
#' stepPenal<- StepPenal(Data=simData, lamda=1.5, w=0.3)
#' after <- Sys.time()
#' after-before
#'
#' (varstepPenal<- stepPenal$coeffP)
#' }

StepPenal <- function(Data, lamda,  w, standardize=TRUE) {
  if (standardize==TRUE){
    Data<- data.frame(cbind(y=Data$y,scale(Data[,-1])))
  }else{
    Data<- Data
  }
  expit   <- function(x) { exp(x)/(1+exp(x))}
  epsilon<- 0.01
  z    <- colnames(Data)[-1]
  # selected variables by mod.AIC: sz
  sz <- NULL
  n <- length(z)
  for(i in 1:n) {
    res.AIC <- NULL
    res.beta<- list()
    for (j in 1:n) {
      #generate model formula
      if(i==1){
        mod.form <- stats::as.formula(paste0('y~',z[j]))
        var <- c(paste0('y~',z[j]))
      } else {
        mod.form <- stats::as.formula(paste0('y~',paste(c(sz, z[j]), collapse='+')))
        var <- c(paste0('y~', paste(c(sz, z[j]), collapse='+')))
      }
      y     <- as.matrix(Data$y)
      x     <- as.matrix(Data[,-1][,c(sz, z[j]) ])


          objFun<- function(x, y, lamda, w, beta, epsilon) {

            logL <- function(x, y, beta){
              loglik   <- - sum( -y*log(1 + exp(-(x%*%beta))) - (1-y)*log(1 + exp(x%*%beta)))
              return(loglik)
            }
            f1 <- logL(x, y,beta)

            CL <- function(beta, w, epsilon) {
              # begin L0
              L0 <- function(beta, epsilon) {
                p <- length(beta)
                L0_j <- function(beta_j, epsilon) {
                  y <- NULL
                  if (abs(beta_j) >= epsilon) {
                    y <- 1
                  } else {
                    y <- abs(beta_j) / epsilon
                  }
                  return(y)
                }
                y <- sum(sapply(beta, L0_j, epsilon))
                return(y)
              }
              # end L0
              beta <- as.matrix(beta)
              y <- (1-w) * L0(beta, epsilon) + w * norm(beta, type="1")
              return(y)
            }
            f  <- f1 + lamda*CL(beta, w, epsilon)
            return(f)
          }

      useoptim <-  tryCatch({
        stats::optim(par=c(rep(1,(length(z[j])+length(sz)))),
              fn=objFun,
              lamda=lamda, w=w, epsilon=epsilon, x=x, y=y,
              method="BFGS", control=list(maxit=5000))
              # lower= rep(-100,(length(z[j])+length(sz))),
              # upper= rep(100,(length(z[j])+length(sz))) )
      }, error = function(err) {
        return(NA)
      })
  suppressWarnings(if(!is.na(useoptim)){
    res.AIC <- rbind.data.frame(res.AIC,
                data.frame(mod.AIC=useoptim$value, stringsAsFactors=FALSE))
    res.beta[[i]] <- round(useoptim$par,4)
      } else{
     res.AIC <- rbind.data.frame(res.AIC,
                                 data.frame(mod.AIC=c(999),stringsAsFactors=FALSE))
     res.beta[[i]] <- c(9999)
        })
    }
    # break if AIC is greater in last round i
    if(i>1 && !any(res.AIC$mod.AIC < best.AIC))
      break
    idx.min  <- which.min(res.AIC$mod.AIC)
    best.AIC <- res.AIC$mod.AIC[idx.min]
    sz       <- c(sz, z[idx.min])
    z        <- z[-idx.min]
    n        <- length(z)
    finalVar  <- c(z[j],sz)[-1]
    finalBeta <- unlist(res.beta)
    fvalue <- best.AIC
    names(finalBeta)<- finalVar
  }
 return(list(VarP=finalVar, coeffP=finalBeta, value=fvalue))
}



#' Stepwise forward variable selection using penalized regression.
#'
#' Stepwise forward variable selection based on the combination of L2 and L0 penalties.
#' The optimization is done using the "BFGS" method in stats::optim
#' @name  StepPenalL2
#' @param Data should have the following structure: the first column must be the binary response variable y.
#' @param lamda the tuning penalty parameter
#' @param standardize Logical flag for the predictors' standardization, prior to fitting the model.
#' Default is standardize=TRUE
#' @param w the weight parameter for the sum (1-w)L0+ wL2
#' @return a list with the shrinked coefficients and the names of the selected variables, i.e those variables with
#' an estimated coefficient different from zero.
#' @details lamda and w  parameters need to be tuned by cross-Validation using stepPenal::tuneParam
#' @seealso \code{\link[stats]{optim}}
#' @export
#' @examples
#' # use the StepPenal function on a simulated dataset, with given lamda and w.
#'
#' set.seed(14)
#' beta    <- c(3, 2, -1.6, -1)
#' noise   <- 5
#' simData <- SimData(N=100, beta=beta, noise=noise, corr=TRUE)
#' \dontrun{
#' before <- Sys.time()
#' stepPenalL2 <- StepPenalL2(Data=simData, lamda=1.5, w=0.6)
#' after <- Sys.time()
#' after-before
#'
#' (varstepPenal<- stepPenalL2$coeffP)
#' }

StepPenalL2<- function(Data, lamda,  w, standardize=TRUE) {
  if (standardize==TRUE){
    Data<- data.frame(cbind(y=Data[,1],scale(Data[,-1])))
  }else{
    Data<- Data
  }
  expit   <- function(x) { exp(x)/(1+exp(x))}

  objFun2 <- function(x, y, lamda, w, beta, epsilon) {
    logL <- function(beta, x, y){
      loglik   <- - sum( -y*log(1 + exp(-(x%*%beta))) - (1-y)*log(1 + exp(x%*%beta)))
      return(loglik)
    }

    L0 <- function(beta, epsilon) {
      p <- length(beta)
      L0_j <- function(beta_j, epsilon) {
        y <- NULL
        if (abs(beta_j) >= epsilon) {
          y <- 1
        } else {
          y <- abs(beta_j) / epsilon
        }
        return(y)
      }
      y <- sum(sapply(beta, L0_j, epsilon))
      return(y)
    }

    CL2 <- function(beta, w, epsilon) {
      beta <- as.matrix(beta)
      y <- (1-w) * L0(beta, epsilon) + w * norm(beta, type="f")
      return(y)
    }
    f1 <- logL(beta, x, y)
    f  <- f1 + lamda*CL2(beta, w, epsilon)
    return(f)
  }

  epsilon<- 0.01
  z    <- colnames(Data)[-1]
  # selected variables by mod.AIC: sz
  sz <- NULL
  n <- length(z)
  for(i in 1:n) {
    res.AIC <- NULL
    res.beta<- list()
    for (j in 1:n) {
      #generate model formula
      if(i==1){
        mod.form <- stats::as.formula(paste0('y~',z[j]))
        var <- c(paste0('y~',z[j]))
      } else {
        mod.form <- stats::as.formula(paste0('y~',paste(c(sz, z[j]), collapse='+')))
        var <- c(paste0('y~', paste(c(sz, z[j]), collapse='+')))
      }
      y     <- as.matrix(Data$y)
      x     <- as.matrix(Data[,-1][,c(sz, z[j]) ])

      useoptim <-  tryCatch({
        stats::optim(par=c(rep(1,(length(z[j])+length(sz)))),
              fn=objFun2,
              lamda=lamda, w=w, epsilon=epsilon, x=x, y=y,
              method="BFGS", control=list(maxit=5000))
              # lower= rep(-100,(length(z[j])+length(sz))),
              # upper= rep(100,(length(z[j])+length(sz))) )
      }, error = function(err) {
        return(NA)
      })
      suppressWarnings(if(!is.na(useoptim)){
        res.AIC <- rbind.data.frame(res.AIC,
                                    data.frame(mod.AIC=useoptim$value, stringsAsFactors=FALSE))
        res.beta[[i]] <- round(useoptim$par,4)
      } else{
        res.AIC <- rbind.data.frame(res.AIC,
                                    data.frame(mod.AIC=c(999),stringsAsFactors=FALSE))
        res.beta[[i]] <- c(999)
      })
    }
    # break if AIC is greater in last round i
    if(i>1 && !any(res.AIC$mod.AIC < best.AIC))
      break
    idx.min  <- which.min(res.AIC$mod.AIC)
    best.AIC <- res.AIC$mod.AIC[idx.min]
    sz       <- c(sz, z[idx.min])
    z        <- z[-idx.min]
    n        <- length(z)
    finalVar  <- c(z[j],sz)[-1]
    finalBeta <- unlist(res.beta)
    fvalue <- best.AIC
    names(finalBeta)<- finalVar
  }
  return(list(VarP=finalVar, coeffP=finalBeta, value=fvalue))
}


#' Variable selection based on the combined penalty CL= (1-w)L0 + wL1
#'
#' Methods to use for optimization include Hooke-Jeeves derivative-free minimization algorithm (hjk),
#' or the BFGS method (modified Quasi-Newton). This method does variable selection by shrinking
#' the coefficients towards zero using the combined penalty (CL= (1-w)L0 + wL1).
#'
#' @name  optimPenaLik
#' @param Data should have the following structure: the first column must be the response variable y
#' @param lamda tuning penalty parameter
#' @param w the weight parameter for the sum (1-w)L0+ wL1
#' @param standardize standardize Logical flag for x variable standardization, prior to fitting the model sequence.
#' The coefficients are always returned on the original scale. Default is standardize=TRUE
#' @param algorithms select between BFGS ('QN') or Hooke-Jeeves (hjk) algorithm.
#' @details it is recommended to use the tuneParam function to tune parameters lamda and w prior
#' using the optimPenaLik function.
#' @seealso \code{\link[stats]{optim}}
#' @return a list with the shrinked coefficients and the names of the selected variables, i.e those variables with
#' estimated coefficient different from zero.
#' @export
#' @examples
#' # use the optimPenaLik function on a simulated dataset, with given lamda and w.
#' \dontrun{
#' set.seed(14)
#' beta    <- c(3, 2, -1.6, -1)
#' noise   <- 5
#' simData <- SimData(N=100, beta=beta, noise=noise, corr=TRUE)
#'
#' # use BFGS
#'
#' before  <- Sys.time()
#' PenalQN <- optimPenaLik(Data=simData, lamda=1.5, w=0.7,
#'                      algorithms=c("QN"))
#' (tot <- Sys.time()-before)
#' PenalQN
#'
#'
#
#' # use Hooke-Jeeves algorithm
#'
#' before  <- Sys.time()
#' Penalhjk <- optimPenaLik(Data=simData, lamda=1.5, w=0.7,
#'                        algorithms=c("hjk"))
#' (totRun  <- Sys.time() - before)
#' # total run of approx 0.25sec
#'
#' Penalhjk
#'}
#'

optimPenaLik <-  function(Data, lamda, w, standardize=TRUE,
                          algorithms=c("QN","hjk") ){
  #install.packages("dfoptim")
  if (standardize==TRUE){
    Data<- data.frame(cbind(y=Data[,1],scale(Data[,-1])))
  }else{
    Data<- Data
  }
  epsilon <- 0.01

  #begin objFun
  objFun <- function(x, y, lamda, w, beta, epsilon) {
    logL <- function(x, y, beta){
      loglik   <- - sum( -y*log(1 + exp(-(x%*%beta))) - (1-y)*log(1 + exp(x%*%beta)))
      return(loglik)
    }
    f1 <- logL(x, y,beta)
    CL <- function(beta, w, epsilon) {
      # begin L0
      L0 <- function(beta, epsilon) {
        p <- length(beta)
        L0_j <- function(beta_j, epsilon) {
          y <- NULL
          if (abs(beta_j) >= epsilon) {
            y <- 1
          } else {
            y <- abs(beta_j) / epsilon
          }
          return(y)
        }
        y <- sum(sapply(beta, L0_j, epsilon))
        return(y)
      }
      # end L0
      beta <- as.matrix(beta)
      y <- (1-w) * L0(beta, epsilon) + w * norm(beta, type="1")
      return(y)
    }
    f  <- f1 + lamda*CL(beta, w, epsilon)
    return(f)
  }
  #end objFun

  y     <- as.matrix(Data$y)
  x     <- as.matrix(Data[,-1])
  out <- list()

  # ##  Optimization using Hooke-Jeeves #
  if("hjk" %in% algorithms){
    useoptimhjk <- tryCatch({
      dfoptim::hjk(par=c(rep(1,length(Data[,-1]))), fn=objFun,
          x=x, y=y , lamda=lamda, epsilon=epsilon, w=w)
      # lower= rep(-50, length(Data[,-1])),
      # upper= rep(50, length(Data[,-1])))
    }, error = function(err) {
      return(NA)
    })
    suppressWarnings(if(!is.na(useoptimhjk)){
      beta0   <- which(round(abs(useoptimhjk$par),3)>0.001)
      coefL2  <- useoptimhjk$par[abs(useoptimhjk$par)>0.001]
      varL2   <- colnames(Data)[-1][beta0]
      names(coefL2)<- varL2
      out <- c(out,list(varhjk=coefL2,value=useoptimhjk$value))
    }else {
      stop(paste("The optimization failed"))
    })
  }
  #
  ## Optimization using modified quasi-Newton #
  if("QN" %in% algorithms){
    useoptimQN <- tryCatch({
      stats::optim(fn=objFun,
            par=c(rep(1,length(Data[,-1]))),
            # lower=c(rep(-20,length(Data[,-1]))),
            # upper=c(rep(20,length(Data[,-1]))),
            x=x, y=y,
            method="BFGS", control=list(maxit=5000),
            lamda=lamda, epsilon=epsilon, w=w)
    }, error = function(err) {
      return(NA)
    })
    suppressWarnings(if(!is.na(useoptimQN)){
      betaQN  <- which(round(abs(useoptimQN$par),3)> 0.001)
      coefQN  <- round(useoptimQN$par[abs(useoptimQN$par)> 0.001],3)
      names(coefQN)<- colnames(Data[,-1][betaQN])
      out <- c(out,list(varQN= coefQN, valueQN=useoptimQN$value))
    }else {
      paste("The (QN) optimization failed and simulated annealing will be used instead")
      useoptimSA <- tryCatch({
        stats::optim(par=c(rep(1,length(Data[,-1]))),fn=objFun,
              x=x, y=y, method="SANN",
              lamda=lamda, epsilon=epsilon, w=w,
              control=list(maxit = 20000, temp = 800, tmax=800) )
      }, error = function(err) {
        return(NA)
      })
      suppressWarnings(if(!is.na(useoptimSA)){
        beta0   <- which(round(abs(useoptimSA$par),3)>0.001)
        coefL2  <- useoptimSA$par[abs(useoptimSA$par)>0.001]
        varL2   <- colnames(Data)[-1][beta0]
        names(coefL2)<- varL2
        out <- c(out,list(varSA=coefL2,value=useoptimSA$value))
      }else {
        paste("The (SA) optimization failed")
      })
    })
  }
  return(out)
}


#' Variable selection based on the combined penalty CL2= (1-w)L0 + wL2
#'
#' Methods to use for optimization include the
#' Hooke-Jeeves derivative-free minimization algorithm (hjk),
#' and the BFGS method (modified Quasi-Newton). This algorithm does variable selection
#' by shrinking the coefficients towards zero using the combined penalty (CL2= (1-w)L0 + wL2).
#'
#' @name  optimPenaLikL2
#' @param Data should have the following structure: the first column must be the response variable y
#' @param lamda tuning penalty parameter
#' @param w the weight parameter for the sum (1-w)L0+ wL2
#' @param standardize standardize Logical flag for x variable standardization, prior to fitting the model sequence.
#' The coefficients are always returned on the original scale. Default is standardize=TRUE
#' @param algorithms select between Simulated annealing or Differential evolution
#' @details it is recommended to use the tuneParam function to tune parameters lamda and w prior
#' using the optimPenaLik function.
#' @return a list with the shrinked coefficients and the names of the selected variables, i.e those variables with
#' estimated coefficient different from zero.
#' @export
#' @examples
#' \dontrun{
#' # use the optimPenaLik function on a simulated dataset, with given lamda and w.
#' set.seed(14)
#' beta    <- c(3, 2, -1.6, -1)
#' noise   <- 5
#' simData <- SimData(N=100, beta=beta, noise=noise, corr=TRUE)
#'
#' # example with Quasi-Newton:
#' before <- Sys.time()
#' PenalQN <- optimPenaLikL2(Data=simData, lamda=2, w=0.6,
#'                      algorithms=c("QN"))
#' after <- Sys.time()
#' after-before
#' PenalQN
#'}
#'
optimPenaLikL2 <-  function(Data, lamda, w, standardize=TRUE,
                            algorithms=c("QN","hjk") ){
  if (standardize==TRUE){
    Data<- data.frame(cbind(y=Data[,1],scale(Data[,-1])))
  }else{
    Data<- Data
  }
  epsilon <- 0.01

  #begin objFun
  objFun2 <- function(x, y, lamda, w, beta, epsilon) {
    logL <- function(x, y, beta){
      loglik   <- - sum( -y*log(1 + exp(-(x%*%beta))) - (1-y)*log(1 + exp(x%*%beta)))
      return(loglik)
    }
    f1 <- logL(x, y,beta)
    ###
    CL <- function(beta, w, epsilon) {
      # begin L0
      L0 <- function(beta, epsilon) {
        p <- length(beta)
        L0_j <- function(beta_j, epsilon) {
          y <- NULL
          if (abs(beta_j) >= epsilon) {
            y <- 1
          } else {
            y <- abs(beta_j) / epsilon
          }
          return(y)
        }
        y <- sum(sapply(beta, L0_j, epsilon))
        return(y)
      }
      # end L0
      beta <- as.matrix(beta)
      y <- (1-w) * L0(beta, epsilon) + w * norm(beta, type="f")
      return(y)
    }
    ###
    f  <- f1 + lamda*CL(beta, w, epsilon)
    return(f)
  }
  #end objFun

  y     <- as.matrix(Data$y)
  x     <- as.matrix(Data[,-1])
  out <- list()

  # ##  Optimization using Hooke-Jeeves #
  if("hjk" %in% algorithms){
    useoptimhjk <- tryCatch({
      dfoptim::hjk(par=c(rep(1,length(Data[,-1]))),
          fn=objFun2,
          x=x, y=y , lamda=lamda, epsilon=epsilon, w=w)
      # lower= rep(-50, length(Data[,-1])),
      # upper= rep(50, length(Data[,-1])))
    }, error = function(err) {
      return(NA)
    })
    suppressWarnings(if(!is.na(useoptimhjk)){
      beta0   <- which(round(abs(useoptimhjk$par),3)>0.001)
      coefL2  <- round(useoptimhjk$par[abs(useoptimhjk$par)>0.001],3)
      varL2   <- colnames(Data)[-1][beta0]
      names(coefL2)<- varL2
      out <- c(out,list(varhjk=coefL2,value=useoptimhjk$value))
    }else {
      stop(paste("The optimization failed"))
    })
  }

  ## Optimization using quasi-Newton #
  if("QN" %in% algorithms){
    useoptimQN <- tryCatch({
      stats::optim(fn=objFun2,
            par=c(rep(1,length(Data[,-1]))),
            # lower=c(rep(-10,length(Data[,-1]))),
            # upper=c(rep(10,length(Data[,-1]))),
            x=x, y=y,
            method="BFGS", control = list(maxit=5000),
            lamda=lamda, epsilon=epsilon, w=w)
    }, error = function(err) {
      return(NA)
    })
    suppressWarnings(if(!is.na(useoptimQN)){
      betaQN  <- which(round(abs(useoptimQN$par),3)> 0.001)
      coefQN  <- round(useoptimQN$par[abs(useoptimQN$par)> 0.001],3)
      names(coefQN)<- colnames(Data[,-1][betaQN])
      out <- c(out,list(varQN= coefQN, valueQN=useoptimQN$value))
    }else {
      paste("The (QN) optimization failed and simulated annealing will be used instead")
      useoptimSA <- tryCatch({
        stats::optim(par=c(rep(1,length(Data[,-1]))),fn=objFun2,
              x=x, y=y, method="SANN",
              lamda=lamda, epsilon=epsilon, w=w,
              control=list(maxit = 20000, temp = 800, tmax=800) )
      }, error = function(err) {
        return(NA)
      })
      suppressWarnings(if(!is.na(useoptimSA)){
        beta0   <- which(round(abs(useoptimSA$par),3)>0.001)
        coefL2  <- useoptimSA$par[abs(useoptimSA$par)>0.001]
        varL2   <- colnames(Data)[-1][beta0]
        names(coefL2)<- varL2
        out <- c(out,list(varSA=coefL2,value=useoptimSA$value))
      }else {
        paste("The (SA) optimization failed")
      })

    })
  }
  return(out)
}


#'
#' Tune parameters w and lamda using the CL penalty
#'
#' Does k-fold cross-validation with the function optimPenalLik and returns
#' the values of lamda and w that maximize the area under the ROC.
#'
#' @name  tuneParam
#' @param Data a data frame, as a first column should have the response variable y and the other columns the predictors
#' @param nfolds the number of folds used for cross-validation. OBS! nfolds>=2
#' @param grid a grid (data frame) with values of lamda and w that will be used for tuning
#' to tune the model. It is created by expand.grid see example below
#' @param algorithm choose between BFGS ("QN") and hjk (Hooke-Jeeves optimization free) to be used for optmization
#' @return A matrix with the following: the average (over folds) cross-validated AUC, the totalVariables selected on the training set,
#' and the standard deviation of the AUC over the nfolds.
#' @details It supports the BFGS optimization method ('QN') from the optim {stats} function, the Hooke-Jeeves derivative-free
#' minimization algorithm ('hjk')
#' The value of lamda and w that yield the maximum AUC on the
#' cross-validating data set is selected.
#'
#' @export
#' @examples
#' \dontrun{
#' set.seed(14)
#' beta    <- c(3, 2, -1.6, -4)
#' noise   <- 5
#' simData <- SimData(N=100, beta=beta, noise=noise, corr=TRUE)
#'
#' nfolds  <- 3
#' grid <- expand.grid(w = c( 0.3, 0.7),
#'                    lamda = c(1.5))
#'
#' before <- Sys.time()
#' paramCV <- tuneParam(simData, nfolds, grid, algorithm=c("QN"))
#' (totalTime <- Sys.time() - before)
#'
#'
#' maxAUC    <- paramCV[which.max(paramCV$AUC),]$AUC
#' allmaxAUC <- paramCV[which(paramCV$AUC==maxAUC),] # checks if the value of AUC
#' # is unique; if is not unique then it will take the combination of lamda and
#' # w where lamda has the largest value- thus achieving higher sparsity
#'
#' runQN   <- optimPenaLik(simData, lamda= allmaxAUC[nrow(allmaxAUC),]$lamda,
#'                          w= allmaxAUC[nrow(allmaxAUC),]$w,
#'                          algorithms=c("QN"))
#' (coefQN  <- runQN$varQN)
#'
#'
#' # check the robustness of the choice of lamda
#'
#' runQN2   <- optimPenaLik(simData, lamda= allmaxAUC[1,]$lamda,
#'                          w= allmaxAUC[1,]$w,
#'                          algorithms=c("QN"))
#' (coefQN2  <- runQN2$varQN)
#'
#' }

tuneParam <- function(Data, nfolds=nfolds, grid, algorithm=c("hjk", "QN")){
  if (nfolds==1){
    stop(paste(nfolds,"You need to supply nfolds with a value >=2"))
  }

# begin function to calculatethe CV AUC
  findAUC <- function(Data, valiData, coefPenal){
    #expit <- function(x) { exp(x)/(1+exp(x))}

    if (length(coefPenal)<1){
      aucpenalTr <- c(0)
      aucpenalV  <- c(0)
      out  <- list(aucpenalTr=aucpenalTr,
                   aucpenalV=aucpenalV)
    }else{
      aucTr <- tryCatch({
        Xmat    <- Data[,-1]
        Xpenal  <- Xmat[,names(coefPenal)]
      }, error = function(err) {
        return(NA)
      })
      suppressWarnings(if(!is.na(aucTr)){
        Xmat  <- Data[,-1]
        Xpenal    <- Xmat[,names(coefPenal)]
        Xbpenal   <- round(as.matrix(Xpenal)%*%coefPenal,4)
        aucpenalTr <- pROC::roc(Data[,1], c(Xbpenal))

        XpenalV   <- valiData[,-1][,names(coefPenal)]
        # XbpenalV  <- round(expit(as.matrix(XpenalV)%*%coefPenal),4)
        XbpenalV  <- round(as.matrix(XpenalV)%*%coefPenal,4)
        aucpenalV <- pROC::roc(valiData[,1], c(XbpenalV))

        # Results:
        out  <- list(aucpenalTr=aucpenalTr$auc,
                     aucpenalV=aucpenalV$auc,
                     senspenalV=aucpenalV$sensitivities,
                     specpenalV=aucpenalV$specificities)
      }else{
        out<- list(aucpenalTr=c(0),
                   aucpenalV=c(0),
                   senspenalV=c(0),
                   specpenalV=c(0))
      })
    }
    return(out)
  }
## end findAUC
  y     <- Data$y
  folds <- caret::createMultiFolds(y, k=nfolds, times=1)
  models <- vector("list", nrow(grid) )
  keepCV <- list()

  fun <- function(i){
    inTrain <- folds[[i]]
    trainData <- Data[inTrain,]
    cvData <- Data[-inTrain,]

    inloop <- lapply( 1:nrow(grid), function(p){
      modelQN <- optimPenaLik(trainData, lamda=grid$lamda[p],
                              w=grid$w[p],
                              algorithms= algorithm)
      if("hjk" %in% algorithm){
        coefModel <- modelQN$varhjk
      } else{
        coefModel <- modelQN$varQN
      }
      perfQN  <- findAUC(trainData, cvData, coefPenal=coefModel)

      modelCV <- data.frame(lamda= grid$lamda[p],
                            w= grid$w[p],
                            devQN= modelQN$value,
                            complex= length(coefModel),
                            aucQNTr= perfQN$aucpenalTr,
                            aucQNCV= perfQN$aucpenalV)
      models[[p]] <- modelCV
    })

    keepCV <- c(keepCV,list( models= inloop))
    return(keepCV)
  }

  outune <- lapply(1:length(folds),function(i) fun(i))

  fol0 <- lapply(1:nfolds, function(m) outune[[m]]$models)
  fol  <- lapply(fol0, function(x) {do.call(rbind, x)})
  tm   <- lapply(1:nfolds, function(i) { c(fol[[i]]$aucQNCV)})
  stepQNCV <- colMeans(do.call(rbind, tm))

  matr <- do.call(rbind, tm)
  sdAUC <- base::apply(matr, 2 , stats::sd)

  tVar   <- lapply(1:nfolds, function(i) { c(fol[[i]]$complex)})
  totVar <- base::colMeans(do.call(rbind, tVar))

  perfMat  <- data.frame(grid, AUC=round(stepQNCV,3),
                         sdAUC=sdAUC, totVar=totVar)

  return(perfMat)
}


#'
#' Tune parameters w and lamda using the CL2 penalty
#'
#' Does k-fold cross-validation with the function optimPenalLikL2 and returns
#' the values of lamda and w that maximize the area under the ROC.
#'
#' @name  tuneParamCL2
#' @param Data a data frame, as a first column should have the response variable y and the other columns the predictors
#' @param nfolds the number of folds used for cross-validation. OBS! nfolds>=2
#' @param grid a grid (data frame) with values of lamda and w that will be used for tuning
#' to tune the model. It is created by expand.grid see example below
#' @param algorithm choose between BFGS ("QN") and hjk (Hooke-Jeeves optimization free) to be used for optmization
#' @return A matrix with the following: the average (over folds) cross-validated AUC, the totalVariables selected on the training set,
#' and the standard deviation of the AUC over the nfolds
#' @details It supports the BFGS optimization method ('QN') from the optim {stats} function, the Hooke-Jeeves derivative-free
#' minimization algorithm ('hjk').
#' The value of lamda and w that yield the maximum AUC on the
#' cross-validating data set is selected. If more that one value of lamda nad w yield the same AUC,
#' then the biggest values of lamda and w are choosen.
#' @export
#'
#'
tuneParamCL2 <- function(Data, nfolds=nfolds, grid, algorithm=c("QN")){
  if (nfolds==1){
    stop(paste(nfolds,"You need to supply nfolds with a value >=2"))
  }

  # begin function to calculatethe CV AUC
  findAUC <- function(Data, valiData, coefPenal){
    #expit <- function(x) { exp(x)/(1+exp(x))}

    if (length(coefPenal)<1){
      aucpenalTr <- c(0)
      aucpenalV  <- c(0)
      out  <- list(aucpenalTr=aucpenalTr,
                   aucpenalV=aucpenalV)
    }else{
      aucTr <- tryCatch({
        Xmat    <- Data[,-1]
        Xpenal  <- Xmat[,names(coefPenal)]
      }, error = function(err) {
        return(NA)
      })
      suppressWarnings(if(!is.na(aucTr)){
        Xmat  <- Data[,-1]
        Xpenal    <- Xmat[,names(coefPenal)]
        Xbpenal   <- round(as.matrix(Xpenal)%*%coefPenal,4)
        aucpenalTr <- pROC::roc(Data[,1], c(Xbpenal))

        XpenalV   <- valiData[,-1][,names(coefPenal)]
        # XbpenalV  <- round(expit(as.matrix(XpenalV)%*%coefPenal),4)
        XbpenalV  <- round(as.matrix(XpenalV)%*%coefPenal,4)
        aucpenalV <- pROC::roc(valiData[,1], c(XbpenalV))

        # Results:
        out  <- list(aucpenalTr=aucpenalTr$auc,
                     aucpenalV=aucpenalV$auc,
                     senspenalV=aucpenalV$sensitivities,
                     specpenalV=aucpenalV$specificities)
      }else{
        out<- list(aucpenalTr=c(0),
                   aucpenalV=c(0),
                   senspenalV=c(0),
                   specpenalV=c(0))
      })
    }
    return(out)
  }
  ## end findAUC
  y     <- Data$y
  folds <- caret::createDataPartition(y, times=nfolds, p=0.7)
  models <- vector("list", nrow(grid) )
  keepCV <- list()

  fun <- function(i){
    inTrain <- folds[[i]]
    trainData <- Data[inTrain,]
    cvData <- Data[-inTrain,]

    inloop <- lapply( 1:nrow(grid), function(p){
      modelQN <- optimPenaLikL2(trainData, lamda=grid$lamda[p],
                                w=grid$w[p],standardize = FALSE,
                                algorithms= algorithm)

      if("hjk" %in% algorithm){
        coefModel <- modelQN$varhjk
      } else{
        coefModel <- modelQN$varQN
      }
      perfQN  <- findAUC(trainData, cvData, coefPenal=coefModel)
      #BrierQN  <- stepPenalBrier(cvData, coeffP=coefModel)

      modelCV <- data.frame(lamda= grid$lamda[p],
                            w=grid$w[p],
                            devQN=modelQN$value,
                            complex=length(coefModel),
                            #BrierQN=BrierQN,
                            aucQNTr= perfQN$aucpenalTr,
                            aucQNCV= perfQN$aucpenalV)
      models[[p]] <- modelCV
    })

    keepCV <- c(keepCV,list( models= inloop))
    return(keepCV)
  }

  # if (parallel==TRUE){
  #   doMC::registerDoMC(ncores)
  #   outune <- foreach::foreach(i=1:length(folds)) %dopar% {
  #     fun(i)}
  # } else {
    outune <- lapply(1:length(folds),function(i) fun(i))
 # }

  fol0 <- lapply(1:nfolds, function(m) outune[[m]]$models)
  fol  <- lapply(fol0, function(x) {do.call(rbind, x)})

  tm   <- lapply(1:nfolds, function(i) { c(fol[[i]]$aucQNCV)})
  stepQNCV <- base::colMeans(do.call(rbind, tm))
  matr <- do.call(rbind, tm)
  sdAUC <- base::apply(matr, 2 ,stats::sd)

  tVar   <- lapply(1:nfolds, function(i) { c(fol[[i]]$complex)})
  totVar <- colMeans(do.call(rbind, tVar))

  # brier   <- lapply(1:nfolds, function(i) { c(fol[[i]]$BrierQN)})
  # brierCV <- colMeans(do.call(rbind, brier))

  perfMat  <- data.frame(grid, AUC=round(stepQNCV,3),
                         sdAUC=round(sdAUC,3),
                         #brier=brierCV,
                         totVar=totVar)
  return(perfMat)
}



#'
#' Stepwise forward variable selection based on the AIC criterion
#'
#' It is a wrapper function over the step function in the buildin package stats
#'
#' @name   stepaic
#' @param  Data a data frame, as a first column should hava the response variable y
#' @param standardize Logical flag for x variable standardization, prior to fitting the model sequence.
#'  Default is standardize=TRUE
#' @return a list with the coefficients of the final model.
#' It also returns the in-sample AUC and the Brier score
#' @seealso \code{\link[stats]{step}}
#' @export
#' @examples
#' \dontrun{
#' set.seed(14)
#' beta    <- c(3, 2, -1.6, -4)
#' noise   <- 5
#' simData <- SimData(N=100, beta=beta, noise=noise, corr=FALSE)
#'
#' stepaicfit <- stepaic(Data=simData)
#' stepaicfit
#' }


stepaic <- function(Data, standardize=TRUE){
  if (standardize==TRUE){
    Data<- data.frame(cbind(y=Data[,1], scale(Data[,-1])))
  }else{
    Data<- Data
  }
  ## Step AIC :
  null    <- stats::glm( factor(y) ~ 1 , data= Data, family="binomial")
  full    <- stats::glm( factor(y) ~ . , data= Data, family="binomial")
  fitaic  <- stats::step(null, scope= list(lower=null, upper=full), direction="forward", trace=F)
  coefaic <- stats::coef(fitaic)[-1]

  if(is.null(names(coefaic))==TRUE){
    print("stepAIC method selects zero variables")
  }else {
    # Xstepaic  <- Data[,-1][,names(coefaic)]
    # Xbstepaic <- round(as.matrix(Xstepaic)%*%coefaic,4)
    Xbstepaic    <- stats::predict(fitaic, type="response")
    aucstepaicTr <- pROC::roc(Data[,1], c(Xbstepaic))
    brierstepaic <- sum( (1/nrow(Data))*(Xbstepaic - Data[,1])^2)
  }
  outstep <- list(coefaic = stats::coef(fitaic)[-1],
                  #VarStepaic= names(coef(fitaic))[-1],
                  aucstepaicTr= aucstepaicTr,
                  brierstepaic= brierstepaic)
  return(outstep)
}


#'
#' Fits a lasso model and a lasso followed by a stepAIC algorithm.
#'
#' @name  lassomodel
#' @param  Data a data frame, as a first column should have the response variable y
#' @param standardize Logical flag for variable standardization, prior to fitting the model.
#' Default is standardize=TRUE. If variables are in the same units already, you might not
#' wish to standardize.
#' @param measure loss to use for cross-validation. measure="auc" is for two-class logistic regression only,
#' and gives area under the ROC curve. measure="deviance", uses the deviance for logistic regression.
#' @param nfold number of folds - default is 5. Although nfolds can be as large as the sample size (leave-one-out CV),
#' it is not recommended for large datasets. Smallest value allowable is nfolds=3
#' @return a list with the coefficients in the final model for the lasso fit and also for the lasso followed by stepAIC.
#' @seealso \code{\link[glmnet]{glmnet}}
#' @details the function lassomodel is a wrapper function over the glmnet::glmnet. The parameter lambda is tuned
#' by 10-fold cross-validation with the glmnet::cv.glmnet function.
#' The selected lambda is the one that gives either the minimum deviance (measure="deviance")
#' or the maximum auc (measure="auc") or minimum misclassification error (measure="class")
#' @export
#' @examples
#' \dontrun{
#' set.seed(14)
#' beta    <- c(3, 2, -1.6, -4)
#' noise   <- 5
#' simData <- SimData(N=100, beta=beta, noise=noise, corr=FALSE)
#'
#' lassofit <- lassomodel(Data=simData, measure="auc")
#' lassofit
#'
#' lassofit2 <- lassomodel(Data=simData, measure="deviance")
#' lassofit2
#'}
#'

lassomodel <- function(Data, standardize=TRUE, measure=c("deviance"), nfold=5){
  if(standardize==TRUE){
    Data <- data.frame(cbind(y=Data[,1],scale(Data[,-1])))
    ## Lasso fit:
    lassofit  <- glmnet::cv.glmnet(x=data.matrix(Data[,-1]),y=factor(Data$y), alpha=1,
                           standardize=FALSE, nfolds = nfold,
                           family="binomial", type.logistic="modified.Newton",
                           type.measure = measure)
  } else{
  lassofit  <- glmnet::cv.glmnet(x=data.matrix(Data[,-1]),y=factor(Data$y), alpha=1,
                         standardize=FALSE, nfolds = nfold,
                         family="binomial",type.measure=measure)
  }
    coeff     <- stats::coef(lassofit,lassofit$lambda.min)
    coefb     <- coeff@x[-1]
    variables <- names(which(coeff[,1][-1]!=0))
    names(coefb)<- variables

    datatopVar   <- cbind(y=Data[,1], Data[,variables])
    datatopVar   <- as.data.frame(datatopVar)
    nullasso     <- stats::glm( factor(y) ~ 1 , data= datatopVar, family="binomial")
    fullasso     <- stats::glm( factor(y) ~ . , data= datatopVar, family="binomial")
    lassostepaic <- stats::step(nullasso, scope=list(lower=nullasso, upper=fullasso),
                         direction="forward", trace=FALSE)

  out <- list(coeflasso= coefb,
              #VarlassoStepaic= names(coef(lassostepaic)[-1]),
              #varlasso= variables,
              minlamda=lassofit$lambda.min,
              coeflassostepaic= stats::coef(lassostepaic)[-1]
              )
  return(out)
}


#'
#' Evaluation of the performance of risk prediction models with binary status response variable.
#'
#' @name   penalBrier
#' @param  Data a data matrix; in the first column there should be the response variable y.
#' If you give the training dataset it will calculate the Brier score.
#' @param  coeffP  a named vector of coefficients
#' @return  the Brier score (misclassification error)
#' @details Brier score is a measure for classification performance of a binary classifier.
#' Its values range between [0,1] and the closest is to 0 the better the classifier is.
#' The area under the curve and the Brier score is used to summarize and compare the performance.
#' @export
#' @references Brier, G. W. (1950). Verification of forecasts expressed in terms of probability.
#' Monthly Weather Review 78.
#' @examples
#' # use the penalBrier function on a simulated dataset, with given lamda and w.
#' \dontrun{
#' set.seed(14)
#' beta    <- c(3, 2, -1.6, -4)
#' noise   <- 5
#' simData <- SimData(N=100,beta=beta, noise=noise, corr=FALSE)
#'
#' before   <- Sys.time()
#' stepPenal<- StepPenal(Data=simData, lamda=1.2, w=0.4)
#' (totRun  <- Sys.time() - before)
#'
#' (coeff<- stepPenal$coeffP)
#'  me <- penalBrier(simData,coeff)
#' }

penalBrier <- function(Data, coeffP){
  if(is.null(names(coeffP))==TRUE){
    stop(paste(names(coeffP),"elements of the coeffP were not found."))
  }else {
    expit   <- function(x) { exp(x)/(1+exp(x))}
    Xpenal     <- Data[,-1][,names(coeffP)]
    Xbpenal    <- round(expit(as.matrix(Xpenal)%*%coeffP),4)
    brierpenal <- sum( (1/nrow(Data))*(Xbpenal-Data[,1])^2)
  }
  return(brierpenal)
}


#' Compute the area under the ROC curve
#'
#' This function computes the numeric value of area under the ROC curve (AUC) with the trapezoidal rule.
#' It is a wrapper function around the pRoc function in the roc package
#'
#' @name   findROC
#' @param  Data a data matrix; in the first column there should be the binary response variable y.
#' If you give the training dataset it will calculate the in-sample AUC.
#' If supplied with a new dataset then it will return the predictive AUC.
#' @param  coeff vector of coefficients
#' @return The area under the ROC curve, the sensitivity and specificity
#' @seealso \code{\link[pROC]{roc}}
#' @export
#' @examples
#' \dontrun{
#' set.seed(14)
#' beta    <- c(3, 2, -1.6, -4)
#' noise   <- 5
#' simData <- SimData(N=100,beta=beta, noise=noise, corr=FALSE)
#'
#' stepPenal<- StepPenal(Data=simData, lamda=1.2, w=0.7)
#'
#' (coeffP <- stepPenal$coeffP)
#'
#' findROC(simData, coeff=coeffP)
#' }

findROC  <- function(Data, coeff){
  if(is.null(names(coeff))==TRUE){
    print("There were zero variables")
  }else {
    expit   <- function(x) { exp(x)/(1+exp(x))}
    Xmat     <-  Data[,-1]
    Xpenal   <- Xmat[,names(coeff)]
    Xbpenal  <- round(expit(as.matrix(Xpenal)%*%coeff),4)
    findAUC <-  pROC::roc(Data[,1], c(Xbpenal))
  }
  return(findAUC)
}


