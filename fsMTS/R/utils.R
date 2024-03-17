#' @title Ensemble feature selection for MTS
#'
#' @description
#' \code{fsEnsemble} implements methods for ensemble learning of features for multivariate time series
#'
#' @param feature.sets a list of matrixes that contains weights for features, estimated by several feature selection algorithms (base learners)
#' @param threshold the required sparsity of the resulting feature set
#' @param method a ensemble learning algorithm.
#' Implemented algorithms:
#' \itemize{
#'  \item{\strong{"ranking"}}{ - individual feature sets are ranked according to their weights
#'  and further the sum of ranks is used for feature selection (\emph{threshold} share of features is selected).
#'  The algorithm uses ranking of feature with a minor priority to earlier lags and even smaller priority to order of MTS components.
#'  So, if features of 1st  and 2nd lags have identical weights, the feature of the 1st lag will be preferred;
#'  if features of the same lag have identical weights, the order of features is used as a priority.
#'  }
#'  \item{\strong{"majority"}}{ - base feature sets are for feature selection (\emph{threshold} share of features is selected)
#'  and further the resulting feature set is estimated using majority voting (50 or more percent of base learners)
#'  }
#' }
#' @return returns a binary feature matrix.
#' Columns correpond to components of the time series; rows correspond to lags.
#'
#' @export
#'
#' @references
#'
#' Pes, B., 2019. Ensemble feature selection for high-dimensional data: a stability analysis across multiple domains. Neural Computing and Applications. https://doi.org/10.1007/s00521-019-04082-3
#'
#' @examples
#'
#' # Load traffic data
#' data(traffic.mini)
#'
#' # Scaling is sometimes useful for feature selection
#' # Exclude the first column - it contains timestamps
#' data <- scale(traffic.mini$data[,-1])
#'
#' mIndep<-fsMTS(data, max.lag=3, method="ownlags")
#' mCCF<-fsMTS(data, max.lag=3, method="CCF")
#' mDistance<-fsMTS(data, max.lag=3, method="distance", shortest = traffic.mini$shortest, step = 5)
#' mGLASSO<-fsMTS(data, max.lag=3,method="GLASSO", rho = 0.05)
#' mLARS<-fsMTS(data, max.lag=3,method="LARS")
#' mRF<-fsMTS(data, max.lag=3,method="RF")
#' mMI<-fsMTS(data, max.lag=3,method="MI")
#' mlist <- list(Independent = mIndep,
#'               Distance = mDistance,
#'               CCF = mCCF,
#'               GLASSO = mGLASSO,
#'               LARS = mLARS,
#'               RF = mRF,
#'               MI = mMI)
#'
#'
#' th<-0.30
#' mlist[["EnsembleRank"]] <- fsEnsemble(mlist, threshold = th, method="ranking")
#' mlist[["EnsembleMajV"]] <- fsEnsemble(mlist, threshold = th, method="majority")
#' (msimilarity <- fsSimilarityMatrix(mlist,threshold = th, method="Kuncheva"))
#'
fsEnsemble <- function(feature.sets, threshold, method=c("ranking", "majority")){
  method <- match.arg(method)
  result<-switch(method,
    ranking={
      lranks<-list()
      for (m in feature.sets){
        lranks[[length(lranks)+1]]<-rankElements(m)
      }
      res<-apply(simplify2array(lranks),1:2, stats::median)
      res <- cutoff(max(res)-res, threshold)
      res
    },
    majority={
      res <- NULL
      k <- length(feature.sets)
      for (m in feature.sets){
        mtmp<-cutoff(m, threshold)
        if (is.null(res)){
          res <- mtmp
        } else{
          res <- res + mtmp
        }
      }
      res <- ifelse(res>=k/2,1,0)
      res
    }
  )
  return(result)
}

#' @title Choosing most important features
#'
#' @description
#' \code{cutoff} chooses features of highest importance to reach the required percent of sparsity
#'
#' @param feature.set a matrix that contains feature weights.
#' @param threshold the required sparsity of the resulting feature set
#' @return returns a binary feature matrix.
#' Columns correspond to components of the time series; rows correspond to lags.
#'
#' @export
#'
#' @examples
#'
#' # Load traffic data
#' data(traffic.mini)
#'
#' # Scaling is sometimes useful for feature selection
#' # Exclude the first column - it contains timestamps
#' data <- scale(traffic.mini$data[,-1])
#'
#' mCCF<-fsMTS(data, max.lag=3, method="CCF")
#' cutoff(mCCF, 0.3)
#' cutoff(mCCF, 0.1)
#'
#' mIndependent<-fsMTS(data, max.lag=3, method="ownlags")
#' cutoff(mIndependent, 0.3)
#' cutoff(mIndependent, 0.1)
cutoff <- function(feature.set, threshold){
  mtmp<-rankElements(feature.set)
  th <- stats::quantile(mtmp, threshold)
  res <- ifelse(mtmp>th | feature.set==0,0,1)
  return(res)
}




#' @title Calculating sparsity of a feature set
#'
#' @description
#' \code{fsSparsity} calculates the sparsity (share of non-zero components) of the feature set
#'
#'
#' @param feature.set a matrix that contains feature weights.
#'
#' @return returns a share of non-zero components in the feature set
#'
#' @export
#'
#' @examples
#'
#' # Load traffic data
#' data(traffic.mini)
#'
#' # Scaling is sometimes useful for feature selection
#' # Exclude the first column - it contains timestamps
#' data <- scale(traffic.mini$data[,-1])
#'
#' mCCF<-fsMTS(data, max.lag=3, method="CCF")
#' fsSparsity(cutoff(mCCF,0.3))
#'
fsSparsity <- function(feature.set){
  m<-feature.set
  return(length(m[m>0])/(ncol(m)*nrow(m)))
}


#' @title Calculating similarity of two feature sets
#'
#' @description
#' \code{fsSimilarity} implements different methods for calculation similarity of two feature sets.
#'
#'
#' @param feature.set1 a matrix that contains feature weights.
#' @param feature.set2 a matrix that contains feature weights.
#' @param cutoff logical. If true, ihe input features sets are cut-off using the \code{cutoff} function with a specified \code{threshold}.
#' By default is FALSE.
#' @param threshold the threshold for feature selection using the \code{cutoff} function. By default is 1 (no cut-off)
#' @param method a similarity metric.
#' Implemented metrics:
#' \itemize{
#'  \item{\strong{"Jaccard"}}{ - a share of matching features to maximal possible number of matching features (Jaccard similarity)}
#'  \item{\strong{"Kuncheva"}}{ - Kuncheva-like correction to the expected number of features matched by chance. See Kuncheva (2007)}
#'  \item{\strong{"Hamming"}}{ - Hamming distance, normalised to [0,1], where 1 is for identical matrices}
#' }
#'
#' @return returns a value from the [-1, 1] interval for Kuncheva and from the [0,1] interval for other algorithms,
#' where 1 is for absolutely identical feature sets.
#'
#' @export
#'
#' @references
#'
#' Kuncheva L., 2007, A stability index for feature selection. In: 25th IASTED international multi-conference: artificial intelligence and applications, pp. 390–395
#'
#' @examples
#'
#' # Load traffic data
#' data(traffic.mini)
#'
#' # Scaling is sometimes useful for feature selection
#' # Exclude the first column - it contains timestamps
#' data <- scale(traffic.mini$data[,-1])
#'
#' mCCF<-fsMTS(data, max.lag=3, method="CCF")
#' mLARS<-fsMTS(data, max.lag=3, method="LARS")
#' fsSimilarity(mCCF, mLARS, cutoff=TRUE, threshold=0.2, method="Kuncheva")
#' fsSimilarity(mCCF, mLARS, cutoff=TRUE, threshold=0.2, method="Jaccard")
#' fsSimilarity(mCCF, mLARS, cutoff=TRUE, threshold=0.2, method="Hamming")
#'
fsSimilarity <- function(feature.set1, feature.set2, cutoff=FALSE, threshold=1, method = c("Kuncheva", "Jaccard", "Hamming")){
  method <- match.arg(method)
  if (cutoff){
    m1 <- cutoff(feature.set1,threshold)
    m2 <- cutoff(feature.set2,threshold)
  }else{
    m1 <- feature.set1
    m2 <- feature.set2
    if (!isBinary(m1) || !isBinary(m2))
      stop("Feature set matrices should be binary or cutoff should be set to TRUE")
    if (threshold<0|| threshold>1)
      stop('threshold is out of [0,1] interval')
  }
  res <- 0
  if (nrow(m1)==nrow(m2) && ncol(m1)==ncol(m2) ){
    res <- switch(method,
                  Jaccard={
                    max.intersection <- min(sum(m1==1),sum(m2==1))
                    obs.intersection <- sum(m1+m2==2)
                    obs.intersection/max.intersection
                  },
                  Kuncheva={
                    n <- nrow(m1)*ncol(m1)
                    max.intersection <- min(sum(m1==1),sum(m2==1))
                    k1 <- sum(m1==1)
                    k2 <- sum(m2==1)
                    mean.intersection <- k1*k2/n
                    obs.intersection <- sum(m1+m2==2)
                    sim<-(obs.intersection - mean.intersection)/(max.intersection - mean.intersection)
                    # Scale to [0, 1]
                    # (sim - (-1))/(1 - (-1))
                    sim
                  },
                  Hamming={
                    sum(m1==m2)/(nrow(m1)*ncol(m1))
                  }
    )
  }else{
    stop('Input feature matrices should have identical dimensions')
  }
  return(res)
}


#' @title Constructing the similarity matrix
#'
#' @description
#' \code{fsSimilarityMatrix} constructs a square matrix of similarity metric values between MTS feature sets.
#' Metrics are calculated using \code{\link{fsSimilarity}} function with cutting-off feature sets
#'
#' @param feature.sets a list of matrixes that contains weights for features, estimated by several feature selection algorithms.
#' @param threshold the required sparsity of the resulting feature set
#' @param method a similarity metric. Directly passed to \code{\link{fsSimilarity}} function
#' @return returns a real-valued square matrix with pairwise similarity metric values of feature sets
#'
#' @export
#' @seealso \code{\link{fsSimilarity}}
#' @examples
#'
#' # Load traffic data
#' data(traffic.mini)
#'
#' # Scaling is sometimes useful for feature selection
#' # Exclude the first column - it contains timestamps
#' data <- scale(traffic.mini$data[,-1])
#'
#' mIndep<-fsMTS(data, max.lag=3, method="ownlags")
#' mCCF<-fsMTS(data, max.lag=3, method="CCF")
#' mDistance<-fsMTS(data, max.lag=3, method="distance", shortest = traffic.mini$shortest, step = 5)
#' mGLASSO<-fsMTS(data, max.lag=3,method="GLASSO", rho = 0.05)
#' mLARS<-fsMTS(data, max.lag=3,method="LARS")
#' mRF<-fsMTS(data, max.lag=3,method="RF")
#' mMI<-fsMTS(data, max.lag=3,method="MI")
#' mlist <- list(Independent = mIndep,
#'               Distance = mDistance,
#'               CCF = mCCF,
#'               GLASSO = mGLASSO,
#'               LARS = mLARS,
#'               RF = mRF,
#'               MI = mMI)
#'
#' (msimilarity <- fsSimilarityMatrix(mlist,threshold = 0.3, method="Kuncheva"))
#'
fsSimilarityMatrix <- function(feature.sets, threshold, method){
  msimilarity <- matrix(0, ncol=length(feature.sets),nrow=length(feature.sets))
  ns <- names(feature.sets)
  colnames(msimilarity) <- ns
  rownames(msimilarity) <- ns
  for (i in 1:length(ns)){
    for (j in i:length(ns)){
        val <- fsSimilarity(feature.sets[[ns[i]]], feature.sets[[ns[j]]],
                                                   cutoff=TRUE,
                                                   threshold=threshold,
                                                   method=method)
        msimilarity[ns[i],ns[j]] <-val
        msimilarity[ns[j],ns[i]] <-val
    }
  }
  return(msimilarity)
}







# Internal functions
isBinary <-function(m) {
  identical(as.vector(m),as.numeric(as.logical(m)))
}

fsNames <- function(res, mts, max.lag){
  n <- ncol(mts)
  vars<-colnames(mts)
  if (is.null(vars)) vars <- paste0("V",1:n)
  colnames(res)<-vars
  rnames<-c()
  for (k in 1:max.lag){
    rnames <- append(rnames, paste0(vars,".l",k))
  }
  rownames(res)<-rnames
  return (res)
}

composeYX <- function(mts, i, max.lag){
  y <- mts[,i]
  x <- mts
  cnames<-colnames(mts)[i]
  dat<-NULL
  for (l in 1:max.lag){
    if(is.null(dat)){
      dat <- x[-c(1:l),]
    }else{
      dat <- cbind(dat[-nrow(dat),],x[-c(1:l),])
    }
    cnames <- c(cnames,paste0(colnames(x),".l",l))
  }
  dat <- cbind(y[-((length(y)-max.lag+1):length(y))],dat)
  colnames(dat)<-cnames
  return(dat)
}

rankElements <-function(m){
  # Add small priority pieces to earlier lags and (after that) for first time series components
  m<-matrix(rank(m),ncol=ncol(m))
  # for (i in 1:nrow(m)){
  #   for (j in 1:ncol(m)){
  #     if (m[i,j]>0) m[i,j]<-m[i,j]-(i/1e+5)-(j/1e+7)
  #   }
  # }
  m<-ifelse(m>0,m-(row(m)/1e+5)-(col(m)/1e+7),0)
  max <- ncol(m)*nrow(m)
  res<-matrix(max-rank(m)+1, ncol=ncol(m))
  colnames(res) <- colnames(m)
  rownames(res) <- rownames(m)
  return(res)
}
