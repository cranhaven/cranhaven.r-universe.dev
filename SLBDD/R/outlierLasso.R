#' Outliers LASSO
#'
#' Use LASSO estimation to identify outliers in a set of time series by creating dummy
#' variables for every time point.
#'
#' @param zt T by 1 vector of an observed scalar time series without missing values.
#' @param p Seasonal period. Default value is 12.
#' @param crit Criterion. Default is 3.5.
#' @param family Response type. See the glmnet command in R. Possible types are "gaussian", "binomial", "poisson",
#'  "multinomial", "cox", "mgaussian". Default is "gaussian".
#' @param standardize Logical flag for zt variable standardization. See the glmnet command in R.
#' Default is TRUE.
#' @param alpha Elasticnet mixing parameter, with \eqn{0 \leq \alpha \leq 1}. See the glmnet command in R. Default value is 1.
#' @param jend Number of first and last observations assumed to not be level shift outliers.
#' Default value is 3.
#'
#' @return A list containing:
#' \itemize{
#'    \item nAO - Number of additive outliers.
#'    \item nLS - Number of level shifts.
#' }
#'
#' @examples
#' data(TaiwanAirBox032017)
#' output <- outlierLasso(TaiwanAirBox032017[1:100,1])
#'
#' @import stats
#' @importFrom glmnet glmnet
#' @importFrom glmnet cv.glmnet
#'
#' @export
"outlierLasso" <- function(zt, p = 12, crit = 3.5, family = "gaussian", standardize = TRUE, alpha = 1, jend = 3){

  ti <- Sys.time()
  if(!is.matrix(zt))zt <- as.matrix(zt)
  k <- ncol(zt)
  nT <- nrow(zt)

  mm <- lsLasso(zt,p=p,crit=crit,family=family,standardize=standardize,alpha=alpha,output=FALSE,jend=jend)
  nLS <- mm$nLS
  xadj <- mm$zadj

  m1 <- aoLasso(xadj,p=p,crit=crit,family=family,standardize=standardize,alpha=alpha,output=FALSE)
  nAO <- m1$nAO

  tf <- Sys.time()
  tt <- tf-ti

  outlierLasso <- list(nAO=nAO,nLS=nLS)
}

"aoLasso" <- function(z,p=12,crit=3.5,alpha=1,family="gaussian",standardize=FALSE,output=TRUE){

  if(!is.matrix(z))z <- as.matrix(z)
  nT <- nrow(z)
  k <- ncol(z)
  noutliers <- rep(0,k)

  Y <- NULL
  for (i in 1:k){

    X <- NULL
    if(p > 0){
      ist <- p+1
      for (j in 1:p){
        X <- cbind(X,z[(ist-j):(nT-j),i])
      }
    }
    n1 <- lm(z[ist:nT,i]~X)
    y <- n1$residuals
    phi <- c(n1$coefficients[2:(p+1)])
    pi <- c(1,-phi)

    D <- NULL
    for (ii in 1:nT){
      dm <- rep(0,nT)
      iend <- ii+p
      if(iend > nT)iend <- nT
      dm[ii:iend] <- pi[1:(iend-ii+1)]
      D <- cbind(D,dm)
    }
    D <- as.matrix(D)

    m1 <- glmnet(D[ist:nT,],y,family=family,standardize=standardize,alpha=alpha)
    cv.m1 <- cv.glmnet(D[ist:nT,],y,family=family,standardize=standardize,alpha=alpha)
    b1 <- coef(m1,s=cv.m1$lambda.min)
    b1 <- c(b1[,1])[-1]
    kk <- length(b1)
    fr <- quantile(abs(b1),0.9)
    idx <- c(1:kk)[abs(b1) > fr]
    if(length(idx) > 0){
      Z <- as.matrix(D[ist:nT,idx])
      cz <- ncol(Z)
      n2 <- lm(y~Z)
      n2a <- summary(n2)
      tratio <- c(n2a$coefficients[,3])[-1]
      beta <- c(n2a$coefficients[,1])[-1]
      jdx <- c(1:cz)[abs(tratio) >= crit]
      if(length(jdx) > 0){
        b1 <- beta[jdx]
        Z1 <- as.matrix(Z[,jdx])
        if(output){
          cat("series and number of large coefs: ",c(i,length(jdx)),"\n")
          cat("Locators: ",idx[jdx],"\n")
        }
      }
      noutliers[i] <- length(jdx)

      if(length(jdx) > 0){
        if(length(jdx)==1){
          y <- y -b1[1]*c(Z1)
        }else{
          for (j in 1:length(jdx)){
            y <- y-b1[j]*Z1[,j]
          }
        }
      }
    }
    y <- stats::filter(y,phi,method="r")
    Y <- cbind(Y,y)
  }

  if(k==1){
    z <- c(z[1:p,],Y)
  }else{
    z <- rbind(z[1:p,],Y)
  }
  aoLasso <- list(nAO=noutliers,zadj=z)
}

"lsLasso" <- function(z,p=12,crit=3.5,family="gaussian",standardize=TRUE,alpha=1,output=TRUE,jend=3){

  if(!is.matrix(z))z <- as.matrix(z)
  nT <- nrow(z)
  k <- ncol(z)

  noutliers <- rep(0,k)
  ist <- p+1
  Y <- NULL
  for (i in 1:k){

    X <- NULL
    if(p > 0){
      ist <- p+1
      for (j in 1:p){
        X <- cbind(X,z[(ist-j):(nT-j),i])
      }
    }
    n1 <- lm(z[ist:nT,i]~X)
    y <- n1$residuals
    phi <- c(n1$coefficients[2:(p+1)])
    pi <- c(1,-phi)
    pi <- cumsum(pi)
    D <- NULL
    for (j in 1:(nT-1)){
      dm <- c(rep(0,j),rep(pi[p+1],(nT-j)))
      iend <- min(j+p,nT)
      dm[(j+1):iend] <- pi[1:(iend-j)]
      D <- cbind(D,dm)
    }

    X <- D[ist:nT,-c(1:jend,(nT-jend):(nT-1))]
    m1 <- glmnet(X,y,family=family,standardize=standardize,alpha=alpha)
    cv.m1 <- cv.glmnet(X,y,family=family,standardize=standardize,alpha=alpha)
    b1 <- coef(m1,s=cv.m1$lambda.min)
    b1 <- c(b1[,1])[-1]
    kk <- length(b1)
    c1 <- quantile(abs(b1),0.9)
    idx <- c(1:kk)[abs(b1) > c1]
    if(length(idx) > 0){
      Z <- as.matrix(X[,idx])
      cz <- ncol(Z)

      n2 <- lm(y~Z)
      n2a <- summary(n2)
      tratio <- c(n2a$coefficients[,3])[-1]
      beta <- c(n2a$coefficients[,1])[-1]
      jdx <- c(1:cz)[abs(tratio) >= crit]
      if(length(jdx) > 0){
        b1 <- beta[jdx]
        Z1 <- as.matrix(Z[,jdx])

        if(output){
          cat("series and number of large coefs: ",c(i,length(jdx)),"\n")
          cat("Locators: ",idx[jdx]+jend,"\n")
        }

        noutliers[i] <- length(jdx)

        if(length(jdx) > 0){
          if(length(jdx)==1){
            y <- y - b1[1]*c(Z1[,1])
          }else{
            for (j in 1:length(jdx)){
              y <- y-b1[j]*Z1[,j]
            }
          }
        }
      }
    }
    y <- stats::filter(y,phi,method="r")
    Y <- cbind(Y,y)

  }
  if(k==1){
    z <- c(z[1:p,],Y)
  }else{
    z <- rbind(z[1:p,],Y)
  }

  lsLasso <- list(nLS=noutliers,zadj=z)
}
