rvarsellcm <- function(varselResult){
  x <- rep(0, length(varselResult@model@omega))
  z <- sample(1:varselResult@model@g, 1, prob = varselResult@param@pi)
  cp <- 0
  for (j in 1:nrow(varselResult@param@paramContinuous@mu)){
    if (!any(is.na(varselResult@param@paramContinuous@mu))){
      cp <- cp + 1
      x[cp] <- rnorm(1, varselResult@param@paramContinuous@mu[j,z], varselResult@param@paramContinuous@sd[j,z])
    }
  }
  for (j in 1:nrow(varselResult@param@paramInteger@lambda)){
    if (!any(is.na(varselResult@param@paramInteger@lambda))){
      cp <- cp + 1
      x[cp] <- rpois(1, varselResult@param@paramInteger@lambda[j,z])
    }
  }
  for (j in 1:length(varselResult@param@paramCategorical@alpha)){
    cp <- cp + 1
    x[cp] <- sample(1:length(varselResult@param@paramCategorical@alpha[[j]][z,]), 1, prob = varselResult@param@paramCategorical@alpha[[j]][z,]) 
  }
  x
}

dlogtikvarsellcm <- function(x, varselResult){
  dlog <- log(varselResult@param@pi)
  cp <- 0
  for (j in 1:nrow(varselResult@param@paramContinuous@mu)){
    if (!any(is.na(varselResult@param@paramContinuous@mu))){
      cp <- cp + 1
      dlog <- dlog + dnorm(x[cp], varselResult@param@paramContinuous@mu[j,], varselResult@param@paramContinuous@sd[j,], log = TRUE)
    }
  }
  for (j in 1:nrow(varselResult@param@paramInteger@lambda)){
    if (!any(is.na(varselResult@param@paramInteger@lambda))){
      cp <- cp + 1
      dlog <- dlog + dpois(x[cp], varselResult@param@paramInteger@lambda[j,])
    }
  }
  for (j in 1:length(varselResult@param@paramCategorical@alpha)){
    cp <- cp + 1
    dlog <- dlog + log(varselResult@param@paramCategorical@alpha[[j]][,x[cp]])
  }
  
  dlog <- dlog - max(dlog)
  dlog <- dlog - log(sum(exp(dlog)))
  return(dlog)
}

rlogtikvarsellcm <- function(varselResult)
  dlogtikvarsellcm(rvarsellcm(varselResult), varselResult)




###################################################################################
##' This function estimates the parameters used for visualization of model-based clustering performs with R package Rmixmod. To achieve the parameter infernece, it automatically samples probabilities of classification from the model parameters
##'
##'
##' @param varselResult [\code{\linkS4class{VSLCMresults}}] It is an instance of class VSLCMresults returned by function VarSelCluster of R package VarSelLCM.
##' @param sample.size numeric. Number of probabilities of classification sampled for parameter inference.
##' @param maxit numeric. It limits the number of iterations for the Quasi-Newton algorithm (default 1000).
##' @param nbrandomInit numeric. It defines the number of random initialization of the Quasi-Newton algorithm.
##' @param nbcpu numeric. It specifies the number of CPU (only for linux).
##' @param loccont numeric. Index of the column containing continuous variables (only for mixed-type data).
##'
##' @return Returns a list
##' @examples
##' \dontrun{
##'
##'  # Package loading
##'  require(VarSelLCM)
##'
##'  # Data loading (categorical data)
##'  data("heart")
##'  # Model-based clustering with 3 components
##'  res <- VarSelCluster(heart[,-13], 3)
##'
##'  # Inference of the parameters used for results visualization
##'  # (specific for VarSelLCM results)
##'  # It is better because probabilities of classification are generated
##'  # by using the model parameters
##'  resvisu <- clusvisVarSelLCM(res)
##'
##'  # Component interpretation graph
##'  plotDensityClusVisu(resvisu)
##'
##'  # Scatter-plot of the observation memberships
##'  plotDensityClusVisu(resvisu,  add.obs = TRUE)
##'
##' }
##' @export
##'
##'
clusvisVarSelLCM <- function(varselResult, sample.size=5000, maxit=10**3, nbrandomInit=4*varselResult@model@g, nbcpu=1, loccont=NULL){
  if (class(varselResult)!="VSLCMresults") stop("Object varselResult must be an object returned by the function VarSelCluster of the R package VarSelLCM")
  logtik.estim <-  t(replicate(sample.size, rlogtikvarsellcm(varselResult)))
  out <- clusvis(logtik.estim,
                 prop=varselResult@param@pi,
                 logtik.obs=log(varselResult@partitions@tik),
                 maxit,
                 nbrandomInit, nbcpu)
  return(out)
}
