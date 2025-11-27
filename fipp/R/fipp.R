#'
#' @useDynLib fipp, .registration=TRUE
#' @import Rcpp
NULL

#' Prior pmf of the number of data clusters for three model types (static/dynamic
#' MFMs and DPM)
#'
#' \code{nClusters} is a closure that returns a function which computes a table
#' of probability masses for specified K+s. Arguments needed for the returned
#' function to evaluate are: prior distribution of the number of mixture
#' components and its parameters (see examples for details).
#'
#' @param Kplus a numeric value or vector. All values must be positive integers
#'   (that is 1,2,...). It specifies the range of the number of data clusters
#'   the user wants to evaluate the prior probabilities on.
#' @param N the number of observations in data
#' @param type the type of model considered. Three models (static/dynamic MFMs
#'   and DPM) are supported.
#' @param alpha,gamma hyperparameters for the symmetric Dirichlet prior. For
#'   static MFM, gamma should be specified, while alpha should be specified for
#'   all other models (that is, dynamic MFM and DPM).
#' @param maxK the maximum number of K (= the number of mixture components)
#'   considered. Only needed for static/dynamic MFMs.
#' @param log logical, indicating whether the returned probability should be
#'   logged or not
#' @return \code{nClusters} returns a function which takes two arguments:
#'
#'   \describe{ \item{priorK}{a function with support on the positive integers.
#'   The function serves as a prior on K (default = NULL which is for the DPM).}
#'   \item{priorKparams}{a named list of prior parameters for the function
#'   supplied in argument \code{priorK} (default = NULL which is for the
#'   DPM).}}
#' @examples
#' ## first, create the function pmf() for the dynamic MFM
#' ## with N = 100, K+ evaluated between 1 and 15 with alpha = 1,
#' ## we assume that K will be smaller than 30 by setting maxK  = 30,
#' ## please increase this value for more realistic analysis.
#' pmf <- nClusters(Kplus = 1:15, N = 100, type = "dynamic",
#' alpha = 1, maxK = 30)
#'
#' ## then, specifiy the prior for K so that the pmf can be evaluated
#' ## between K+ = 1 and K+ = 15
#' pmf(dgeom, list(prob = 0.1))
#'
#' ## we can also compare this result with a different prior setting
#' pmf(dpois, list(lambda = 1))
#'
#' @references Greve, J., Grün, B., Malsiner-Walli, G., and Frühwirth-Schnatter, S. (2020) 
#'  Spying on the Prior of the Number of Data Clusters and the Partition Distribution in Bayesian Cluster Analysis.
#'  \url{https://arxiv.org/abs/2012.12337}
#'  
#'  Escobar, M. D., and West, M. (1995) Bayesian Density Estimation and Inference Using Mixtures. 
#'  \emph{Journal of the American Statistical Association} \bold{90} (430), Taylor & Francis: 577-–88. 
#'  \url{https://www.tandfonline.com/doi/abs/10.1080/01621459.1995.10476550}
#'  
#'  Miller, J. W., and Harrison, M. T. (2018) Mixture Models with a Prior on the Number of Components.
#'  \emph{Journal of the American Statistical Association} \bold{113} (521), Taylor & Francis: 340-–56. 
#'  \url{https://www.tandfonline.com/doi/full/10.1080/01621459.2016.1255636}
#'  
#'  Frühwirth-Schnatter, S., Malsiner-Walli, G., and Grün, B. (2020) 
#'  Generalized mixtures of finite mixtures and telescoping sampling \url{https://arxiv.org/abs/2005.09918}
#' @importFrom stats setNames
#' @export

nClusters <- function(Kplus,N,type = c("DPM","static","dynamic"),
                         alpha = NULL,gamma = NULL,maxK = NULL,log = FALSE){
  type <- match.arg(type)
  # check that the user specified alpha exists for DPM/dynamic or gamma for static
  if((type != "static") && is.null(alpha)){
    stop(sprintf("for a %s model, alpha needs to be specified", type))
  }else if((type == "static") && is.null(gamma)){
    stop(sprintf("for a %s model, gamma needs to be specified", type))
  }
  
  # precompute values common across different prior specifications (namely the C_{N,k}^{K,gamma_K})
  CNs <- switch(type,
                DPM = {
                  condCompositions(N,0)
                },
                static = {
                  condCompositions(N,gamma)
                },
                dynamic = {
                  Ks <- min(Kplus):maxK
                  lapply(setNames(Ks,Ks),
                         function(K){
                             CNkKa <- condCompositions(N,alpha/K)
                             validKplus <- Kplus[Kplus<=K]
                             lapply(setNames(validKplus,validKplus),function(k) CNkKa(k)[1,])
                         }
                  )
                })
  # this is the function that computes the p(K+|N,gamma_K) using encapsulated variables in the closure
  function(priorK = NULL,priorKparams = NULL){
    if(type != "DPM"){
      priorKcdf <- Reduce("+",{
        sapply(1:maxK,function(k){
          do.call(priorK,args = c(list(x = k-1),priorKparams))
        })
      })
      if(priorKcdf<0.95){
        warning(sprintf("the CDF of p(K) at maxK = %s is %.3f, it may truncate the result too much, consider increasing maxK",maxK,priorKcdf))
      } 
    }
    if(type %in% c("static","dynamic") && any(c(is.null(priorK),is.null(priorKparams)))){
      stop(sprintf("for a %s MFM, both priorK and priorKparams must be specified",type))
    }
    lprob <- switch(type,
                    DPM = {
                      lapply(setNames(Kplus,Kplus),function(k){
                        lfactorial(N) - lfactorial(k) + k*log(alpha) + lgamma(alpha)-
                          lgamma(alpha + N) + CNs(k)[1,]
                      })
                    },
                    static = {
                      VNkg <- mfmCoefsRecursive(N,gamma,priorK,priorKparams,maxK)
                      lapply(setNames(Kplus,Kplus),function(k){
                        lfactorial(N) - lfactorial(k) + matrixStats::logSumExp(VNkg(k)) - k*lgamma(gamma) + CNs(k)[1,]
                      })
                    },
                    dynamic ={
                      lapply(setNames(Kplus,Kplus), function(k){
                        V1 <- lfactorial(N)-lfactorial(k)+k*log(alpha)+lgamma(alpha)-lgamma(alpha+N)
                        V2 <- matrixStats::logSumExp(sapply(k:maxK,function(K){
                          CNkKa <- do.call("$",
                                           list(do.call("$",list(CNs,as.character(K))),
                                                as.character(k)))
                          lpk <- do.call(priorK,args = c(list(x = K-1),priorKparams,list(log = TRUE)))
                          val <- lpk + CNkKa + sum(sapply(1:k,function(j){
                            log(K-j+1)-log(K)-lgamma(1+alpha/K)
                          }))
                          return(val)
                        }))
                        V1 + V2
                      })
                    })
    if(log){
      retval <- sapply(lprob,function(x) x)
    }else{
      retval <- sapply(lprob,function(x) exp(x))
    }
    return(retval)
  }
}

# A function that computes conditional EPPF for N_j
# 
# Intended for internal use by Exlfunc 
#
NjcondEPPF <- function(Nk,N,Kplus,CNs,type = c("DPM","static","dynamic"),priorK,priorKparams,
                                 alpha,gamma,maxK){
  type <- match.arg(type)
  # from C_N's with a parameter infty/gamma/alpha, compute Pr(N_j = n|N,K_+ = k,.)
  lprobs <- switch(type,
                   DPM = {
                     Cdenom <- CNs(Kplus)[1,]
                     Cnum <- CNs(Kplus-1)[1+Nk,]
                     Cnum - Cdenom - log(Nk)
                   },
                   static = {
                     Cdenom <- CNs(Kplus)[1,]
                     Cnum <- CNs(Kplus-1)[1+Nk,]
                     lgamma(Nk+gamma) - lgamma(Nk+1) + Cnum - Cdenom
                   },
                   dynamic = {
                     normalizerVec4W <- sapply(Kplus:(maxK), function(K){
                       Cval <- do.call(do.call("$",list(CNs,as.character(K))),list(Kplus))
                       wNkKa <- do.call(priorK,c(list(x = K-1),priorKparams,list(log = TRUE))) +
                         sum(sapply(1:Kplus,function(j) log1p(K-j)-log(K)-lgamma(1+alpha/K)))
                       return(wNkKa + Cval[1,])
                     })
                     normalizer4W <- matrixStats::logSumExp(normalizerVec4W)
                     unnormLprobs <- sapply(Nk,function(n){
                       lprobvec <-sapply(Kplus:(maxK), function(K){
                         Cval <- do.call(do.call("$",list(CNs,as.character(K))),list(Kplus-1))
                         wNkKa <- do.call(priorK,c(list(x = K-1),priorKparams,list(log = TRUE))) +
                           sum(sapply(1:Kplus,function(j) log1p(K-j)-log(K)-lgamma(1+alpha/K)))
                         return(wNkKa - normalizer4W + lgamma(n+alpha/K) - lgamma(n+1) + Cval[1+n,])
                       })
                       return(matrixStats::logSumExp(lprobvec[1:(length(lprobvec)-1)]))
                     })
                     unnormLprobs-matrixStats::logSumExp(unnormLprobs)
                   })
  return(lprobs)
}


Exlfunc <- function(lfunc,Kplus,N,type = c("DPM","static","dynamic"),
                    alpha = NULL,gamma = NULL,maxK = NULL, log = FALSE){
  if(!is.call(lfunc)){
    lfunc <- match.fun(lfunc)
  }
  type <- match.arg(type)
  # check that the user specified alpha exists for DPM/dynamic or gamma for static
  if((type == "dynamic") && is.null(alpha)){
    stop(sprintf("for a %s model, alpha needs to be specified",type))
  }else if((type == "static") && is.null(gamma)){
    stop(sprintf("for a %s model, gamma needs to be specified",type))
  }
  # for dynamic, it returns a list of CN's (with different alpha/K for each K)
  # this could potentially result in high memory usage depending on maxK
  CNs <- switch(type,
                DPM = {
                  condCompositions(N,0)
                },
                static = {
                  condCompositions(N,gamma)
                },
                dynamic = {
                  # expensive (both in terms of computation and memory) when maxK is large
                  lapply(setNames(Kplus:(maxK),Kplus:(maxK)),function(K){
                    condCompositions(N,alpha/K)
                  })
                })
  Nk <- 1:(N-Kplus+1)
  function(priorK = NULL,priorKparams = NULL){
    lprobs <- NjcondEPPF(Nk,N,Kplus,CNs,type,priorK,priorKparams,alpha,gamma,maxK)
    if(is.call(lfunc)){
      lval <- matrixStats::logSumExp(sapply(Nk,function(n) eval(lfunc)) + lprobs)
    }else{
      lval <- matrixStats::logSumExp(sapply(Nk,function(n) lfunc(n)) + lprobs)
    }
    return(ifelse(log,lval,exp(lval)))
  }
}


Excrosslfunc <- function(lfunc,Kplus,N,type = c("DPM","static","dynamic"),
                         alpha = NULL,gamma = NULL,maxK = NULL, log = FALSE){
  lfunc <- match.fun(lfunc)
  type <- match.arg(type)
  # check that the user specified alpha exists for DPM/dynamic or gamma for static
  if((type == "dynamic") && is.null(alpha)){
    stop(sprintf("for a %s model, alpha needs to be specified",type))
  }else if((type == "static") && is.null(gamma)){
    stop(sprintf("for a %s model, gamma needs to be specified",type))
  }
  CNs <- switch(type,
                DPM = {
                  condCompositions(N,0)
                },
                static = {
                  condCompositions(N,gamma)
                },
                dynamic = {
                  lapply(setNames(Kplus:(maxK),Kplus:(maxK)),function(K){
                    condCompositions(N,alpha/K)
                  })
                })
  function(priorK = NULL,priorKparams = NULL){
    lprobs <- switch(type,
                     DPM = {
                       sapply(Kplus:maxK,function(K){
                         wNkKa <- -log(maxK-Kplus+1)-CNs(Kplus)[1,]
                         aK <- sapply(1:(N-Kplus+1), function(n) lfunc(n)-log(n))
                         lcrossAkak <- rev(as.vector(logTopdot(aK,c(0,rev(aK)))))
                         Cval2 <- CNs(Kplus-2)
                         retval <- wNkKa + matrixStats::logSumExp(Cval2[3:length(Cval2),]+lcrossAkak)
                         return(retval)
                       }) 
                     },
                     static = {
                       W_normalizer <- matrixStats::logSumExp(sapply(Kplus:(maxK),function(KK){
                         wNkKa <- do.call(priorK,c(list(x = KK-1),priorKparams,list(log = TRUE)))+
                           Kplus*(log(gamma)) + lgamma(gamma*KK) + lfactorial(KK) - Kplus*lgamma(1+gamma) -
                           lgamma(gamma*KK+N) - lfactorial(KK-Kplus)
                         Cval <- CNs(Kplus)
                         return(wNkKa + Cval[1,])
                       }))
                       sapply(Kplus:maxK,function(K){
                         wNkKa <- do.call(priorK,c(list(x = K-1),priorKparams,list(log = TRUE))) +
                           Kplus*(log(gamma)) + lgamma(gamma*K) + lfactorial(K) - Kplus*lgamma(1+gamma) -
                           lgamma(gamma*K+N) - lfactorial(K-Kplus)
                         aK <- sapply(1:(N-Kplus+1), function(n) lfunc(n)+lgamma(n+gamma)-lgamma(n+1))
                         lcrossAkak <- rev(as.vector(logTopdot(aK,c(0,rev(aK)))))
                         Cval2 <- CNs(Kplus-2)
                         retval <- wNkKa - W_normalizer + matrixStats::logSumExp(Cval2[3:length(Cval2),]+lcrossAkak)
                         return(retval)
                       })
                     },
                     dynamic = {
                       sapply(Kplus:maxK,function(K){
                         W_normalizer <- matrixStats::logSumExp(lapply(Kplus:maxK,function(KK){
                           wNkKa <- do.call(priorK,c(list(x = KK-1),priorKparams,list(log = TRUE))) +
                             sum(sapply(1:Kplus,function(j) log(KK-j+1)-log(KK)-lgamma(1+alpha/KK)))
                           Cval <- do.call(do.call("$",list(CNs,as.character(KK))),list(Kplus))
                           return(wNkKa + Cval[1,])
                         }))
                         wNkKa <- do.call(priorK,c(list(x = K-1),priorKparams,list(log = TRUE))) +
                           sum(sapply(1:Kplus,function(j) log(K-j+1)-log(K)-lgamma(1+alpha/K)))
                         aK <- sapply(1:(N-Kplus+1), function(n) lfunc(n)+lgamma(n+alpha/K)-lgamma(n+1))
                         lcrossAkak <- rev(as.vector(logTopdot(aK,c(0,rev(aK)))))
                         Cval2 <- do.call(do.call("$",list(CNs,as.character(K))),list(Kplus-2))
                         retval <- wNkKa - W_normalizer + matrixStats::logSumExp(Cval2[3:length(Cval2),]+lcrossAkak)
                         return(retval)
                       })
                     })
    lval <- matrixStats::logSumExp(lprobs)
    return(ifelse(log,lval,exp(lval)))
  }
}
#' Moments of symmetric additive functional computed over the induced prior
#' partitions (static/dynamic MFMs and DPM)
#'
#'
#' \code{fipp} is a closure which returns a function that computes moments of a
#' user-specified functional over the induced prior partitions. Required
#' arguments are: prior distribution of the number of mixture components and its
#' parameters (see examples for details). Optional arguments are: the number of
#' moments to be evaluated (currently only up to 2 are implemented) and whether
#' the mean/variance or 1st/2nd moments should be printed out as a result of
#' computing the first two moments (default is set to print out mean/variance).
#'
#' @param lfunc a logged version of the additive symmetric functional intended
#'   to compute over the prior partition. The function should only accept one
#'   argument N_j (= number of observations in each partition).
#' @param Kplus a numeric value that represents the number of filled clusters in
#'   data
#' @param N the number of observation in data
#' @param type the type of model considered. Three models (static/dynamic MFMs
#'   and DPM) are supported.
#' @param alpha,gamma hyperparameters for the Dirichlet prior. For static MFM,
#'   gamma should be specified, while alpha should be specified for all other
#'   models (that is, for dynamic MFM and DPM).
#' @param maxK the maximum number of K (= the number of mixture components)
#'   considered. Only needed for static/dynamic MFMs.
#' @param log logical, indicating whether the probability should be logged or
#'   not
#' @return \code{fipp} returns a function which takes two required arguments
#'   (required only for static/dynamic MFMs) and 2 optional arguments:
#'
#'   \describe{ \item{priorK}{a function with support on the positive integers.
#'   The function serves as a prior of K (default = NULL which is for DPM).}
#'   \item{priorKparams}{a named list of prior parameters for the function
#'   supplied in argument \code{priorK} (default = NULL which is for
#'   DPM).}\item{order}{maximum number of moments to be evaluated by the
#'   function (default = 2)}\item{replace2ndwvar}{replace 2nd moment with
#'   variance (default = TRUE)}}
#' @examples
#' ## Determine mean/variance of the number of singleton clusters for dynamic 
#' ## MFM model conditional on K+ = 5, alpha = 1 with a sample size N = 100.
#' ## We assume that K will be smaller than 30 by setting maxK = 30, please
#' ## increase this value for more realistic analysis.
#' ## 
#' ## First create the function singletons():
#' singletons <- fipp(lfunc = function(n) log(n==1), Kplus = 5, N = 100,
#'   type = "dynamic", alpha = 1, maxK = 30)
#'
#' ## Then evaluate it using a Geom(0.1) prior:
#' singletons(dgeom, list(prob = 0.1))
#'
#' ## Try a different prior, the Poisson prior Pois(1):
#' singletons(dpois, list(lambda = 1))
#'
#' ## If mean is the only thing you are interested in, try the following:
#' singletons(dpois, list(lambda = 1), order = 1)
#'
#' ## Also, if you want 1st/2nd moments instead of mean/variance, try:
#' singletons(dpois, list(lambda = 1), replace2ndwvar = FALSE)
#' 
#' @references Greve, J., Grün, B., Malsiner-Walli, G., and Frühwirth-Schnatter, S. (2020) 
#'  Spying on the Prior of the Number of Data Clusters and the Partition Distribution in Bayesian Cluster Analysis.
#'  \url{https://arxiv.org/abs/2012.12337}
#'  
#'  Escobar, M. D., and West, M. (1995) Bayesian Density Estimation and Inference Using Mixtures. 
#'  \emph{Journal of the American Statistical Association} \bold{90} (430), Taylor & Francis: 577-–88. 
#'  \url{https://www.tandfonline.com/doi/abs/10.1080/01621459.1995.10476550}
#'  
#'  Miller, J. W., and Harrison, M. T. (2018) Mixture Models with a Prior on the Number of Components.
#'  \emph{Journal of the American Statistical Association} \bold{113} (521), Taylor & Francis: 340-–56. 
#'  \url{https://www.tandfonline.com/doi/full/10.1080/01621459.2016.1255636}
#'  
#'  Frühwirth-Schnatter, S., Malsiner-Walli, G., and Grün, B. (2020) 
#'  Generalized mixtures of finite mixtures and telescoping sampling \url{https://arxiv.org/abs/2005.09918}
#' @export

fipp <- function(lfunc,Kplus,N,type = c("DPM","static","dynamic"),
                 alpha = NULL,gamma = NULL,maxK = NULL, log = FALSE){
  lfunc <- match.fun(lfunc)
  type <- match.arg(type)
  # check that the user specified alpha for DPM/dynamic and gamma for static
  if((type == "dynamic") && is.null(alpha)){
    stop(sprintf("for a %s model, alpha needs to be specified",type))
  }else if((type == "static") && is.null(gamma)){
    stop(sprintf("for a %s model, gamma needs to be specified",type))
  }
  
  lfunc2 <- lfunc
  lfunc2call <- substitute(lfunc2(n)*2)
  Ex <- Exlfunc(lfunc,Kplus,N,type,alpha,gamma,maxK,log)
  if(Kplus == 2){
    lfuncXcall <- substitute(lfunc(n)+lfunc(N-n))
    Excross <- Exlfunc(lfuncXcall,Kplus,N,type,alpha,gamma,maxK,log)
  }else{
    Excross <- Excrosslfunc(lfunc,Kplus,N,type,alpha,gamma,maxK,log) 
  }
  Ex2 <- Exlfunc(lfunc2call,Kplus,N,type,alpha,gamma,maxK,log)
  Moments <- function(priorK = NULL,priorKparams = NULL,order = 2,replace2ndwvar = TRUE){
    if(type != "DPM"){
      priorKcdf <- Reduce("+",{
        sapply(1:maxK,function(k){
          do.call(priorK,args = c(list(x = k-1),priorKparams))
        })
      })
      if(priorKcdf<0.95){
        warning(sprintf("the CDF of p(K) at maxK = %s is %.3f, it may truncate the result too much, consider increasing maxK",maxK,priorKcdf))
      } 
    }
    if(order>2){
      stop("not implemented yet")
    }
    if(order == 1){
      first <- list(Kplus*Ex(priorK,priorKparams))
      if(replace2ndwvar){
        names(first) <- "E"
      }else{
        names(first) <- "Moment 1"
      }
      return(first)
    }
    # This part won't generalize fully to order >= 3
    # TODO: generalize this part
    cartesian <- expand.grid(replicate(order,1:Kplus,simplify = FALSE))
    multipliers <- sapply(1:ncol(cartesian),function(x){
        return(sum(apply(cartesian,1,function(row){
          length(unique(row)) == x
        })))
      })
    Ex <- Moments(priorK,priorKparams,order-1,replace2ndwvar)
    # for E[X^2] = E[X(X-1)]+E[X], in general E[X^r] can be recursively obtained
    factorialMean <- sum(sapply(1:order,function(order_idx){
      if(order_idx < order){
        return(-1*Ex[[1]])
      }else{
        return(sum(multipliers*c(Ex2(priorK,priorKparams),Excross(priorK,priorKparams))))
      }
    }))
    res <- c(Ex,factorialMean + Ex[[1]])
    names(res) <- paste("Moment",1:order)
    if(replace2ndwvar){
      res[[2]] <- res[[2]]-(res[[1]])^2
      names(res)[[2]] <- "Var"
      names(res)[[1]] <- "E"
    }
    res
  }
}

