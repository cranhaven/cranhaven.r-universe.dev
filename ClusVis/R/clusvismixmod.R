rmixmod <- function(resmixmod){
  x <- NA
  if (resmixmod@dataType == "quantitative"){
    z <- sample(1:resmixmod@bestResult@nbCluster, 1, prob = resmixmod@bestResult@parameters@proportions)
    x <- rmvnorm(1, resmixmod@bestResult@parameters@mean[z,], resmixmod@bestResult@parameters@variance[[z]])
  }else if (resmixmod@dataType == "qualitative"){
    z <- sample(1:resmixmod@bestResult@nbCluster, 1, prob = resmixmod@bestResult@parameters@proportions)
    x <- sapply(1:ncol(resmixmod@bestResult@parameters@center),
                function(j, resmixmod, z){
                  p <- resmixmod@bestResult@parameters@scatter[[z]][j,]
                  p[resmixmod@bestResult@parameters@center[z,j]] <- 1 -  p[resmixmod@bestResult@parameters@center[z,j]]
                  sample(1:length(p), 1, prob = p)
                },
                resmixmod=resmixmod,
                z=z)
  }else if (resmixmod@dataType == "composite"){
    z <- sample(1:resmixmod@bestResult@nbCluster, 1, prob = resmixmod@bestResult@parameters@proportions)
    xcont <- rmvnorm(1, resmixmod@bestResult@parameters@g_parameter@mean[z,], resmixmod@bestResult@parameters@g_parameter@variance[[z]])
    xcat <- sapply(1:ncol(resmixmod@bestResult@parameters@m_parameter@center),
                   function(j, resmixmod, z){
                     p <- resmixmod@bestResult@parameters@m_parameter@scatter[[z]][j,]
                     p[resmixmod@bestResult@parameters@m_parameter@center[z,j]] <- 1 -  p[resmixmod@bestResult@parameters@m_parameter@center[z,j]]
                     sample(1:length(p), 1, prob = p)
                   },
                   resmixmod=resmixmod,
                   z=z)
    x <- list(xcont=xcont, xcat=xcat)
  }else{
    stop("type non programe")
  }
  x
}

dlogsinglequalimixmod <- function(x, j, resmixmod, z){
  p <- resmixmod@bestResult@parameters@scatter[[z]][j,]
  p[resmixmod@bestResult@parameters@center[z,j]] <- 1 -  p[resmixmod@bestResult@parameters@center[z,j]]
  log(p[x])
}

dlogsinglequalimixmodcompo <- function(x, j, resmixmod, z){
  p <- resmixmod@bestResult@parameters@m_parameter@scatter[[z]][j,]
  p[resmixmod@bestResult@parameters@m_parameter@center[z,j]] <- 1 -  p[resmixmod@bestResult@parameters@m_parameter@center[z,j]]
  log(p[x])
}

dlogtik <- function(x, resmixmod){
  dlog <- NA
  if (resmixmod@dataType == "quantitative"){
    dlog <-  sapply(1:resmixmod@bestResult@nbCluster,
                    function(z, x, resmixmod)
                      dmvnorm(x, resmixmod@bestResult@parameters@mean[z,], resmixmod@bestResult@parameters@variance[[z]], log=TRUE) + log(resmixmod@bestResult@parameters@proportions[z]),
                    x=x,
                    resmixmod=resmixmod)
  }else if (resmixmod@dataType == "qualitative"){
    dlog <- rowSums(sapply(1:length(x),
                           function(j, resmixmod, x)
                             sapply(1:resmixmod@bestResult@nbCluster, dlogsinglequalimixmod, x=x[j], j=j, resmixmod=resmixmod),
                           resmixmod=resmixmod,
                           x=x)) + log(resmixmod@bestResult@parameters@proportions)
  }else if (resmixmod@dataType == "composite"){
    dlog <- log(resmixmod@bestResult@parameters@proportions) +
      sapply(1:resmixmod@bestResult@nbCluster,
             function(z, x, resmixmod)
               dmvnorm(x, resmixmod@bestResult@parameters@g_parameter@mean[z,], resmixmod@bestResult@parameters@g_parameter@variance[[z]], log=TRUE),
             x=x$xcont,
             resmixmod=resmixmod) +
      rowSums(sapply(1:length(x$xcat),
                     function(j, resmixmod, x)
                       sapply(1:resmixmod@bestResult@nbCluster, dlogsinglequalimixmodcompo, x=x[j], j=j, resmixmod=resmixmod),
                     resmixmod=resmixmod,
                     x=x$xcat))
  }else{
    stop("this type of variable is not implemented")
  }
  dlog <- dlog - max(dlog)
  dlog <- dlog - log(sum(exp(dlog)))
  return(dlog)
}

rlogtikmixmod <- function(resmixmod){
  x <- rmixmod(resmixmod)
  dlogtik(x, resmixmod)
}



###################################################################################
##' This function estimates the parameters used for visualization of model-based clustering performs with R package Rmixmod. To achieve the parameter infernece, it automatically samples probabilities of classification from the model parameters
##'
##'
##' @param mixmodResult [\code{\linkS4class{MixmodCluster}}] It is an instance of class MixmodCluster returned by function mixmodCluster of R package Rmixmod.
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
##'  ## First example: R package Rmixmod
##'  # Package loading
##'  require(Rmixmod)
##'
##'  # Data loading (categorical data)
##'  data("congress")
##'  # Model-based clustering with 4 components
##'  set.seed(123)
##'  res <- mixmodCluster(congress[,-1], 4, strategy = mixmodStrategy(nbTryInInit = 500, nbTry=25))
##'
##'  # Inference of the parameters used for results visualization
##'  # (specific for Rmixmod results)
##'  # It is better because probabilities of classification are generated
##'  # by using the model parameters
##'  resvisu <- clusvisMixmod(res)
##'
##'  # Component interpretation graph
##'  plotDensityClusVisu(resvisu)
##'
##'  # Scatter-plot of the observation memberships
##'  plotDensityClusVisu(resvisu,  add.obs = TRUE)
##'
##'
##' ## Second example: R package Rmixmod
##' # Package loading
##' require(Rmixmod)
##'  
##' # Data loading (categorical data)
##' data(birds)
##'
##' # Model-based clustering with 3 components
##' resmixmod <- mixmodCluster(birds, 3)
##'
##' # Inference of the parameters used for results visualization (general approach)
##' # Probabilities of classification are not sampled from the model parameter,
##' # but observed probabilities of classification are used for parameter estimation
##' resvisu <- clusvis(log(resmixmod@bestResult@proba),
##'                    resmixmod@bestResult@parameters@proportions)
##'
##' # Inference of the parameters used for results visualization
##' # (specific for Rmixmod results)
##' # It is better because probabilities of classification are generated
##' # by using the model parameters
##' resvisu <- clusvisMixmod(resmixmod)
##'
##' # Component interpretation graph
##' plotDensityClusVisu(resvisu)
##'
##' # Scatter-plot of the observation memberships
##' plotDensityClusVisu(resvisu,  add.obs = TRUE)
##' }
##' @export
##'
##'
clusvisMixmod <- function(mixmodResult, sample.size=5000, maxit=10**3, nbrandomInit=4*mixmodResult@bestResult@nbCluster, nbcpu=1, loccont=NULL){
  if (mixmodResult@dataType != "composite"){
    tmp <- t(apply(mixmodResult@data, 1, dlogtik, resmixmod=mixmodResult))
  }else{
    loccat <- (1:ncol(mixmodResult@data))[-loccont]
    tmp <- t(sapply(1:nrow(mixmodResult@data), function(i) dlogtik(list(xcont=mixmodResult@data[i,loccont], xcat=mixmodResult@data[i,loccat]), mixmodResult)))
  }
  if (sample.size>0){
    logtik.estim <-  t(replicate(sample.size, rlogtikmixmod(mixmodResult)))
  }else{
    logtik.estim <- tmp
  }


  out <- clusvis(logtik.estim, prop=mixmodResult@bestResult@parameters@proportions, logtik.obs=tmp, maxit, nbrandomInit, nbcpu)
  if (out$error == FALSE) out$EM <- sum(exp(logtik.estim) * logtik.estim) / (log(length(mixmodResult@bestResult@parameters@proportions)) * sample.size)

  return(out)
}
