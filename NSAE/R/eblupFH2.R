#' EBLUP under stationary Fay-Herriot model for sample and  non-sample area
#'
#' @description This function gives the EBLUP and the estimate of mean squared error (mse)
#'     based on a stationary Fay-Herriot model for both sample and non-sample area.
#'
#' @param formula an object of class list of formula, describe the model to be fitted
#' @param vardir a vector of sampling variances of direct estimators for each small area
#' @param indicator a vector indicating the sample and non-sample area
#' @param method type of fitting method, default is "REML" methods
#' @param MAXITER number of iterations allowed in the algorithm. Default is 100 iterations
#' @param PRECISION convergence tolerance limit for the Fisher-scoring algorithm. Default value is 1e-04
#' @param data a data frame comprising the variables named in formula and vardir
#'
#' @return The function returns a list with the following objects:
#' \describe{
#'   \item{eblup}{a vector with the values of the estimators for each sample area}
#'   \item{eblup.out}{a vector with the values of the estimators for each non-sample area}
#'   \item{mse}{a vector of the mean squared error estimates for each sample area}
#'   \item{mse.out}{a vector of the mean squared error estimates for each non-sample area}
#'   \item{sample}{a matrix consist of area code, eblup, mse, SE and CV for sample area}
#'   \item{nonsample}{a matrix consist of area code, eblup, mse, SE and CV for non-sample area}
#'   \item{fit}{a list containing the following objects:}
#'   \itemize{
#'     \item estcoef : a data frame with the estimated model coefficients in the first column (beta),their asymptotic standard errors in the second column (std.error), the t statistics in the third column (tvalue) and the p-values of the significance of each coefficient in last column (pvalue)
#'     \item refvar : estimated random effects variance
#'     \item goodness : goodness of fit statistics
#'     \item randomeffect : a data frame with the values of the random effect estimators
#'   }
#'  }
#' @export eblupFH2
#'
#'
#' @examples
#' # Load data set
#' data(paddy)
#' # Fit Fay-Herriot model using sample and non-sample part of paddy data
#' result <- eblupFH2(y ~ x1+x2, var, indicator ,"REML", 100, 1e-04,paddy)
#' result
eblupFH2 <- function (formula, vardir, indicator, method = "REML", MAXITER, PRECISION, data) {
  namevar <- deparse(substitute(vardir))
  nameindic <- deparse(substitute(indicator))
  if (!missing(data)) {
    formuladata <- model.frame(formula, na.action = na.omit,data)
    X <- model.matrix(formula, data)
    vardir <- data[, namevar]
    indicator <- data[, nameindic]
  }
  else {
    formuladata <- model.frame(formula, na.action = na.omit)
    X <- model.matrix(formula)
  }
  if (attr(attributes(formuladata)$terms, "response") == 1)
    textformula <- paste(formula[2], formula[1], formula[3])
  else textformula <- paste(formula[1], formula[2])
  if (length(na.action(formuladata)) > 0)
    stop("Argument formula=", textformula, " contains NA values.")
  if (any(is.na(vardir)))
    stop("Argument vardir=", namevar, " contains NA values.")

  y <- formuladata[, 1]
  m <- length(y)
  y.sam <- y[indicator==1]
  m.sam <- length(y.sam)
  x.sam <- X[1:m.sam,]
  direct <- y.sam
  I<-diag(1,m.sam)
  Xpop <- x.sam
  p<-dim(Xpop)[2]
  Aest.REML <- 0
  Aest.REML[1] <- median(vardir[1:m.sam])
  k <- 0
  diff <- PRECISION + 1
  while ((diff > PRECISION) & (k < MAXITER)) {
    k <- k + 1
    Vi <- 1/(Aest.REML[k] + vardir[1:m.sam])
    XtVi <- t(Vi * x.sam)
    Q <- solve(XtVi %*% x.sam)
    P <- diag(Vi) - t(XtVi) %*% Q %*% XtVi
    Py <- P %*% y.sam
    s <- (-0.5) * sum(diag(P)) + 0.5 * (t(Py) %*% Py)
    F <- 0.5 * sum(diag(P %*% P))
    Aest.REML[k + 1] <- Aest.REML[k] + s/F
    diff <- abs((Aest.REML[k + 1] - Aest.REML[k])/Aest.REML[k])
  }
  A.REML <- max(Aest.REML[k + 1], 0)
  if (k >= MAXITER && diff >= PRECISION)
    warning(paste("REML failed to converge in", MAXITER, "steps"))
  estsigma2u<-A.REML
  Xpopt <-t(Xpop)
  D=diag(1,m.sam)
  V<-estsigma2u*D%*%t(D)+I*vardir[1:m.sam]
  Vi<-solve(V)
  Q<-solve(Xpopt%*%Vi%*%Xpop)
  Beta.hat<-Q%*%Xpopt%*%Vi%*%direct
  res<-direct-Xpop%*%Beta.hat
  Sigma.u=estsigma2u*I
  u.hat=Sigma.u%*%t(D)%*%Vi%*%res
  EBLUP.Mean<-Xpop%*%Beta.hat+D%*%u.hat
  zvalue <- Beta.hat/sqrt(diag(Q))
  pvalue <- 2 * pnorm(abs(zvalue), lower.tail = FALSE)
  loglike <- (-0.5) * (sum(log(2 * pi * (A.REML + vardir[1:m.sam])) + (res^2)/(A.REML + vardir[1:m.sam])))
  AIC <- (-2) * loglike + 2 * (p + 1)
  BIC <- (-2) * loglike + (p + 1) * log(m)
  goodness <- c(loglike = loglike, AIC = AIC, BIC = BIC)
  coef <- data.frame(beta = Beta.hat, std.error = sqrt(diag(Q)),tvalue = zvalue, pvalue)
  g1<-NULL
  Ga<-Sigma.u-Sigma.u%*%Vi%*%Sigma.u
  for (i in 1:m.sam)  {
      g1[i]<-Ga[i,i]
  }
  g2<-NULL
  Gb<-Sigma.u%*%Vi%*%Xpop
  Xpopa<-matrix(0,1,p)
  for (i in 1:m.sam)  {
      Xpopa[1,]<-Xpop[i,]-Gb[i,]
      g2[i]<-Xpopa%*%Q%*%t(Xpopa)
  }
  g3<-NULL
  Deriv1=solve((estsigma2u*I)+ diag(c(vardir),m.sam))
  II<-((1/2)*sum(diag(Deriv1%*%Deriv1)))^(-1)
  for (i in 1:m.sam)  {
      g3[i]=(vardir[i]^2)*(vardir[i]+estsigma2u)^(-3)*II
  }
  EBLUP.MSE.PR<-c(g1+g2+2*g3)
  areacode=1:m.sam
  FH.SE=round(sqrt(EBLUP.MSE.PR),2)
  FH.CV=round(100*(sqrt(EBLUP.MSE.PR)/EBLUP.Mean),2)
  result1= cbind(areacode,EBLUP.Mean, EBLUP.MSE.PR,FH.SE,FH.CV)
  colnames(result1)=c("area","EBLUP","EBLUP.MSE","EBLUP.SE","EBLUP.CV")
  m.out<-m-m.sam
  x.out <- X[-(1:m.sam),]
  FH.out=numeric(m.out)
  for (i in 1:m.out)  {
      FH.out[i]=t(x.out[i,])%*%Beta.hat
  }
  MSE.out<-rep(0,m.out)
  for(i in 1:m.out) {
      MSE.out[i] <- (t(x.out[i,])%*% Q%*%x.out[i,])+estsigma2u
  }
  FH.SE.out=round(sqrt(MSE.out),2)
  FH.CV.out=round(100*(sqrt(MSE.out)/FH.out),2)
  areacode.out=1:m.out
  result2= cbind(areacode.out,FH.out,MSE.out,FH.SE.out,FH.CV.out)
  colnames(result2)=c("area.out","EBLUP.out","EBLUP.mse.out","EBLUP.SE.out","EBLUP.CV.out")
  result <- list(eblup = NA, eblup.out = NA,mse = NA, mse.out = NA,sample =NA,nonsample =NA,
                 fit = list(estcoef = NA, refvar = NA, randomeffect = NA, goodness = NA))
  result$fit$estcoef <- coef
  result$fit$refvar <- estsigma2u
  result$fit$goodness <- goodness
  result$fit$randomeffect <- u.hat
  result$eblup <- EBLUP.Mean
  result$eblup.out <- FH.out
  result$mse <- EBLUP.MSE.PR
  result$mse.out <- MSE.out
  result$sample <- result1
  result$nonsample <- result2
  return(result)
}

