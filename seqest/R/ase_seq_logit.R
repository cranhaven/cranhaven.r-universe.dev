#' @title variable selection and stopping criterion
#'
#' @description
#' \code{ase_seq_logit} determine the effective variables and whether to stop
#' selecting samples
#'
#' @details
#' ase_seq_logit estimates the logistic regression coefficient and determines
#' the effecrive variables and decides whether to stop selecting samples based
#' on the current sample and its corresponding label. The parameters 'upper',
#' 'lower' and 'divid.num' is used to get different epsilons. If different
#' epsilons get the same value, we choose the smallest epsilon.
#' @param X A dataframe that each row is a sample,each column represents an
#'   independent variable.
#' @param Y Numeric vector consists of 0 or 1. The length of Y must be the same
#'   as the X.
#' @param intercept A logical value indicating whether add intercept to model.
#'   The default value is FALSE.
#' @param criterion For the "chosfun" methods, a character string that
#'   determines the model selection criterion to be used, matching one of 'BIC'
#'   or 'AIC. The default value is 'BIC'.
#' @param d A numeric number specifying the length of the fixed size
#'   confidence set for our model. Note that the smaller the d, the larger
#'   the sample size and the longer the time costs. The default value is 0.5.
#' @param alpha A numeric number used in the chi-square distribution. The
#'   default value is 0.95.
#' @param gamma A numeric number to determine the effective variables with eta.
#'   The default value is 1.
#' @param eta A numeric number to determine the effective variables with gamma.
#'   The default value is 0.75.
#' @param upper A numeric number to choose the right epsilon with params lower
#'   and divide.num. The value of upper should be larger than lower. The default
#'   value is 2.
#' @param lower A numeric number to choose the right epsilon with params upper
#'   and divide.num. The default value is 0.1.
#' @param divid.num A numeric number to choose the right epsilon with params
#'   upper and lower. Note that it should be a integer. The default value is 10.
#' @return a list containing the following components
#' \item{N}{current sample size}
#' \item{is_stopped}{the label of sequential stop or not. When the value of
#' is_stopped is 1, it means the iteration stops}
#' \item{betahat}{the estimated coefficients based on current X and Y. Note that
#' some coefficient will be zero. These are the non-effectiva variables should
#' be ignored.}
#' \item{cov}{the covariance matrix between variables}
#' \item{phat}{the number of effective varriables.}
#' \item{ak}{1-alpha quantile of chisquare distribution with degree of freedom
#' phat}
#' \item{lamdmax}{the maximum eigenvalue based on the covariance of data}


ase_seq_logit <- function(X, Y, intercept=FALSE, criterion="BIC", d=0.5, alpha= 0.95, gamma=1, eta=0.75,
                          upper=2, lower=0.1, divid.num=10)
{
  n <- length(as.vector(Y))
  if(intercept == TRUE) {X <- cbind(rep(1,n),X)}
  pnum <- dim(X)[2]
  data <- data.frame(y=Y,x=X)
  tmp <- stats::glm(y~.-1,data=data,family=stats::binomial("logit"))
  bhat <- stats::coef(tmp)
  bhatglm <- bhat
  epsilon <- seq(lower,upper,length=divid.num)
  thresh <- sqrt(n)*n^(-eta)/abs(bhat)^gamma
  i <- 0
  chos.seq <- foreach(i=1:length(epsilon),.combine=rbind) %dopar%  {
    loca <- thresh<epsilon[i]
    betahat <- rep(0,length(thresh))
    betahat[loca] <- bhat[loca]
    df <- sum(abs(betahat)>1.0e-10)
    #tmp <- chosfun(X,Y,betahat,df,criterion)
    nsam <- length(Y)
    z <- c(X%*%betahat)
    tmp <- exp(z)/(1+exp(z))
    tmp <- sum(Y*log(tmp)+(1-Y)*log(1-tmp))
    if (criterion == 'BIC'){
      tmp <- -2*tmp+log(nsam)*df
    }else{
      tmp <- -2*tmp+2*df
    }
    tmp <- c(i,tmp)
  }
  chos.seq <- chos.seq[order(chos.seq[,1]),2]
  loca <- which(chos.seq==min(chos.seq))
  loca <- loca[length(loca)]
  thresh0 <- epsilon[loca]
  loca <- thresh<thresh0
  betahat <- rep(0,length(thresh))
  betahat[loca] <- bhat[loca]
  phat <- sum(abs(betahat)>1.0e-10)
  tmp <- rep(0,pnum)
  tmp[abs(betahat)>1.0e-10] <- 1
  In <- diag(tmp)
  z <- c(X%*%bhatglm)
  tmp <- exp(z)/((1+exp(z))^2)
  W <- diag(tmp)
  beta.cov <- In%*%solve(t(X)%*%W%*%X)%*%In
  maxeigen <- max(eigen(n*beta.cov)$values)
  ak <- sqrt(stats::qchisq(alpha,df=phat))
  sigseq <- 0.0
  if((ak^2*maxeigen)<=(d^2*n) & (phat>0)) {sigseq <- 1.0}
  result.seq <- list(N=n,
                     is_stopped=sigseq,
                     betahat=betahat,
                     cov = beta.cov,
                     phat=phat,
                     ak=ak,
                     lamdmax=maxeigen)
  return(result.seq)
}
