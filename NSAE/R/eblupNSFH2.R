#' EBLUP under nonstationary Fay-Herriot model for sample and  non-sample area
#'
#' @description This function gives the EBLUP and the estimate of mean squared error (mse)
#'     based on a nonstationary Fay-Herriot model for both sample and non-sample area.
#'
#' @param formula an object of class list of formula, describe the model to be fitted
#' @param vardir a vector of sampling variances of direct estimators for each small area
#' @param lat a vector of latitude for each small area
#' @param long a vector of longitude for each small area
#' @param indicator a vector indicating the sample and non-sample area
#' @param method type of fitting method, default is "REML" methods
#' @param MAXITER number of iterations allowed in the algorithm. Default is 100 iterations
#' @param PRECISION convergence tolerance limit for the Fisher-scoring algorithm. Default value is 1e-04
#' @param data a data frame comprising the variables named in formula, vardir, lat and long
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
#'     \item spatialcorr : estimated spatial correlation parameter
#'     \item randomeffect : a data frame with the values of the random effect estimators
#'     \item goodness : goodness of fit statistics
#'   }
#'  }
#' @export eblupNSFH2
#'
#' @examples
#' # Load data set
#' data(paddy)
#' # Fit nonstationary Fay-Herriot model using sample and non-sample part of paddy data
#' result <- eblupNSFH2(y ~ x1+x2, var, latitude, longitude, indicator , "REML", 100, 1e-04,paddy)
#' result
eblupNSFH2 <- function (formula, vardir, lat, long, indicator, method = "REML", MAXITER,PRECISION, data) {
  namevar <- deparse(substitute(vardir))
  nameindic <- deparse(substitute(indicator))
  namelat <- deparse(substitute(lat))
  namelong <- deparse(substitute(long))
  if (!missing(data)) {
    formuladata <- model.frame(formula, na.action = na.omit,data)
    X <- model.matrix(formula, data)
    vardir <- data[, namevar]
    lat <- data[, namelat]
    long <- data[, namelong]
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
  x.out <- X[-(1:m.sam),]
  direct <- y.sam
  I<-diag(1,m.sam)
  p<-dim(x.sam)[2]
  distance<-matrix(0,m,m)
  distance<-as.matrix(dist(cbind(as.vector(lat),as.vector(long))))
  W <- 1/(1+distance)
  W.sam <- W[1:m.sam,1:m.sam]
  par.stim <- matrix(0, 2, 1)
  stime.fin <- matrix(0, 2, 1)
  s <- matrix(0, 2, 1)
  Idev <- matrix(0, 2, 2)
  sigma2.u.stim.S <- 0
  lamda.stim.S <- 0
  sigma2.u.stim.S[1] <- median(vardir[1:m.sam])
  lamda.stim.S[1] <- 0.2
  k <- 0
  diff.S <- PRECISION + 1
  while ((diff.S > PRECISION) & (k < MAXITER)) {
    k <- k + 1
    Z.area=diag(1,m.sam)
    C<-lamda.stim.S[k]*diag(1,p)
    Cov<-(x.sam%*%C%*%t(x.sam))*W.sam+sigma2.u.stim.S[k]*Z.area%*%t(Z.area)
    V <- Cov + I * vardir[1:m.sam]
    Vi <- solve(V)
    Xt=t(x.sam)
    yt=t(y.sam)
    XtVi <- Xt %*% Vi
    Q <- solve(XtVi %*% x.sam)
    P <- Vi - t(XtVi) %*% Q %*% XtVi
    b.s <- Q %*% XtVi %*% y.sam
    derVlamda<-(x.sam%*%t(x.sam))*W.sam
    derSigma<-Z.area%*%t(Z.area)
    PD <- P %*% derSigma
    PR <- P %*% derVlamda
    Pdir <- P %*% y.sam
    s[1, 1] <- (-0.5) * sum(diag(PD)) + (0.5) * (yt %*%PD %*% Pdir)
    s[2, 1] <- (-0.5) * sum(diag(PR)) + (0.5) * (yt %*%PR %*% Pdir)
    Idev[1, 1] <- (0.5) * sum(diag(PD %*% PD))
    Idev[1, 2] <- (0.5) * sum(diag(PD %*% PR))
    Idev[2, 1] <- Idev[1, 2]
    Idev[2, 2] <- (0.5) * sum(diag(PR %*% PR))
    par.stim[1, 1] <- sigma2.u.stim.S[k]
    par.stim[2, 1] <- lamda.stim.S[k]
    stime.fin <- par.stim + solve(Idev) %*% s
    sigma2.u.stim.S[k + 1] <- stime.fin[1, 1]
    lamda.stim.S[k + 1] <- stime.fin[2, 1]
    diff.S <- max(abs(stime.fin - par.stim)/par.stim)
  }
  lambda.stim.S <- lamda.stim.S[k + 1]
  sigma2.u.stim.S[k + 1] <- max(sigma2.u.stim.S[k + 1], 0)
  sigma2.u.stim.S <- sigma2.u.stim.S[k + 1]
  C.est<-lambda.stim.S*diag(1,p)
  Sigma.l<-kronecker(C.est,W.sam)
  z.mat = list()
  for (i in 1:p) {
      z.mat[[i]] <- diag(x.sam[,i])
  }
  Z <- rlist::list.cbind(z.mat)
  Cov.est<-Z%*%Sigma.l%*%t(Z)+sigma2.u.stim.S*I%*%t(I)
  V<-Cov.est+I*vardir[1:m.sam]
  Vi<-solve(V)
  Q<-solve(t(x.sam)%*%Vi%*%x.sam)
  Beta.hat<-Q%*%t(x.sam)%*%Vi%*%direct
  P<-Vi-Vi%*%x.sam%*%solve(t(x.sam)%*%Vi%*%x.sam)%*%t(x.sam)%*%Vi
  res<-direct-c(x.sam%*%Beta.hat)
  Sigma.u=sigma2.u.stim.S*I
  spatial.hat=Sigma.l%*%t(Z)%*%Vi%*%res
  u.hat=Sigma.u%*%t(I)%*%Vi%*%res
  EBLUP.Mean<-x.sam%*%Beta.hat+Z%*%spatial.hat+I%*%u.hat
  zvalue <- Beta.hat/sqrt(diag(Q))
  pvalue <- 2 * pnorm(abs(zvalue), lower.tail = FALSE)
  loglike <- (-0.5) * (m * log(2 * pi) + determinant(V, logarithm = TRUE)$modulus +
                         t(res) %*% Vi %*% res)
  AIC <- (-2) * loglike + 2 * (p + 2)
  BIC <- (-2) * loglike + (p + 2) * log(m)
  goodness <- c(loglike = loglike, AIC = AIC, BIC = BIC)
  coef <- data.frame(beta = Beta.hat, std.error = sqrt(diag(Q)),tvalue = zvalue, pvalue)
  Sigma.w<-matrix(0,((p+1)*m.sam),((p+1)*m.sam))
  Sigma.w[1:(p*m.sam),1:(p*m.sam)]<-Sigma.l
  Sigma.w[(p*m.sam+1):((p+1)*m.sam),(p*m.sam+1):((p+1)*m.sam)]<-Sigma.u
  w.i<-cbind(Z,I)
  c.i<-x.sam-w.i%*%Sigma.w%*%t(w.i)%*%Vi%*%x.sam
  g1<-matrix(0,m.sam,1)
  for (i in 1:m.sam)  {
      g1[i,1]<-c.i[i,]%*%solve(t(x.sam)%*%Vi%*%x.sam)%*%cbind(c.i[i,])
  }
  g2<-matrix(0,m.sam,1)
  for (i in 1:m.sam) {
      g2[i,1]<-w.i[i,]%*%Sigma.w%*%(diag((p+1)*m.sam)-t(w.i)%*%Vi%*%w.i%*%Sigma.w)%*%cbind(w.i[i,])
  }
  g3<-matrix(0,m.sam,1)
  Ds.1<-matrix(0,((p+1)*m.sam),((p+1)*m.sam))
  Ds.1[1:(p*m.sam),1:(p*m.sam)]<-kronecker(diag(1,p),W.sam)
  Ds.1[(p*m.sam+1):((p+1)*m.sam),(p*m.sam+1):((p+1)*m.sam)]<-0
  Ds.2<-diag(c(rep(0,p*m.sam),rep(1,m.sam)))
  B.1<-Z%*%(kronecker(diag(1,p),W.sam))%*%t(Z)
  B.2<-I%*%t(I)
  B<-list(B.1,B.2)
  Dv.1<--Vi%*%B.1%*%Vi
  Dv.2<--Vi%*%B.2%*%Vi
  II<-matrix(0,2,2)
  P<-Vi-Vi%*%x.sam%*%solve(t(x.sam)%*%Vi%*%x.sam)%*%t(x.sam)%*%Vi
  for(i in 1:2) {
    for(j in 1:2) {
        II[i,j]<--0.5*sum(diag(P%*%B[[i]]%*%P%*%B[[j]]))
    }
  }
  II<--II
  ESS<-matrix(0,2,m.sam)
  for (i in 1:m.sam)  {
      ESS[1,]<-w.i[i,]%*%(Ds.1%*%t(w.i)%*%Vi+Sigma.w%*%t(w.i)%*%Dv.1)
      ESS[2,]<-w.i[i,]%*%(Ds.2%*%t(w.i)%*%Vi+Sigma.w%*%t(w.i)%*%Dv.2)
      g3[i,1]<-2*t(direct-x.sam%*%Beta.hat)%*%t(ESS)%*%solve(II)%*%ESS%*%(direct-x.sam%*%Beta.hat)
  }
  EBLUP.MSE.PR<-c(g1+g2+g3)
  areacode=1:m.sam
  FH.SE=round(sqrt(EBLUP.MSE.PR),2)
  FH.CV=round(100*(sqrt(EBLUP.MSE.PR)/EBLUP.Mean),2)
  result1= cbind(areacode,EBLUP.Mean, EBLUP.MSE.PR,FH.SE,FH.CV)
  colnames(result1)=c("area","EBLUP","EBLUP.MSE","EBLUP.SE","EBLUP.CV")
  y.out <- y[indicator==0]
  m.out <- length(y.out)
  W.out <-W[-(1:m.sam),-(1:m.sam)]
  area.out<-m-m.sam
  dist.r=matrix(0,m.out,m.sam)
  lon_r<-long[(m.sam+1):m]
  lat_r<-lat[(m.sam+1):m]
  for (i in 1:m.out)  {
      dbase=as.matrix(rbind(cbind(lon_r[i],lat_r[i]),cbind(as.vector(long[1:m.sam]),as.vector(lat[1:m.sam]))))
      dim(dbase)
      dist.r[i,]=c(as.matrix(dist(dbase))[-1,1])
  }
  distance.out.out<-matrix(0,m.out,m.out)
  distance.out.out<-as.matrix(dist(cbind(as.vector(lon_r),as.vector(lat_r))))
  W.out<-1/(1+dist.r)
  W.out.out<-1/(1+distance.out.out)
  Sigma.l.out<-kronecker(C.est,W.out)
  Sigma.l.out.out<-kronecker(C.est,W.out.out)
  z.mat.out = list()
  for (i in 1:p) {
      z.mat.out[[i]] <- diag(x.out[,i])
  }
  Z.out <- rlist::list.cbind(z.mat.out)
  spatial.hat=Sigma.l.out%*%t(Z)%*%Vi%*%res
  EBLUP.Mean.out<-x.out%*%Beta.hat+Z.out%*%spatial.hat
  AA<-matrix(0,m.out,1)
  for (i in 1:m.out){
    AA[i,1]<-x.out[i,]%*%solve(t(x.sam)%*%Vi%*%x.sam)%*%cbind(x.out[i,])
  }
  BB<-matrix(0,m.out,1)
  for(i in 1:m.out){
    ZZ<-matrix(Z.out[i,],1,p*m.out)
    BB[i,1]<-ZZ%*%(Sigma.l.out.out-Sigma.l.out%*%t(Z)%*%Vi%*%Z%*%t(Sigma.l.out))%*%t(ZZ)
  }
  EBLUP.MSE.PR.out<-c(AA+sigma2.u.stim.S+BB)
  areacode.out=1:area.out
  FH.SE.out=round(sqrt(EBLUP.MSE.PR.out),2)
  FH.CV.out=round(100*(sqrt(EBLUP.MSE.PR.out)/EBLUP.Mean.out),2)
  result2= cbind(areacode.out,EBLUP.Mean.out, EBLUP.MSE.PR.out,FH.SE.out,FH.CV.out)
  colnames(result2)=c("area.out","EBLUP.out","EBLUP.MSE_out","EBLUP.SE.out","EBLUP.CV.out")
  result <- list(eblup = NA, eblup.out = NA,mse = NA, mse.out = NA, sample = NA, nonsample = NA,
                 fit = list(estcoef = NA, refvar = NA, spatialcorr = NA, randomeffect = NA, goodness = NA))
  result$fit$estcoef <- coef
  result$fit$refvar <- sigma2.u.stim.S
  result$fit$spatialcorr <- lambda.stim.S
  result$fit$goodness <- goodness
  result$fit$randomeffect <- u.hat
  result$eblup <- EBLUP.Mean
  result$eblup.out <- EBLUP.Mean.out
  result$mse <- EBLUP.MSE.PR
  result$mse.out <- EBLUP.MSE.PR.out
  result$sample <- result1
  result$nonsample<-result2
  return(result)
}

