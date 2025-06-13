#' Variability Estimation and Testing
#'
#' This function estimates the parameters in Model (1) of Gijbels etal (2017)
#' and tests the variability function.
#'
#' @param times The vector of time variable.
#' @param subj The vector of subjects/individuals.
#' @param X The covariate containing 1 as its first component
#' (including intercept in the model)
#' @param y The response vector.
#' @param d The order of differencing operator for each covariate.
#' @param kn The number of internal knots for each covariate.
#' @param degree The degree of B-spline basis for each covariate.
#' @param lambda The grid of smoothing parameter to control the trade of
#' between fidelity and penalty term (use a fine grid of lambda).
#' @param gam The power used in estimating the smooting parameter for each
#' covariate (e.g. gam=1 or gam=0.5).
#' @param tau The quantiles of interest.
#' @param nr.bootstrap.samples The number of bootstrap samples used
#' @param seed The seed for the random generator in the bootstrap resampling
#' @param test To request for testing the variability function ("Y" for test
#' and "N" for only estimation of the parameters, the default is "Y")
#' @param model The variability model used to estimate the quantile of errors
#' (the default is 4, model 4)
#'
#' @return
#' \describe{
#' \item{est_median}{the median estimator.}
#' \item{hat_bt50}{The median coefficients estimators.}
#' \item{qhat5_s2_m0}{The variability (model 0) estimator.}
#' \item{qhat5_s2_m1}{The variability (model 1) estimator.}
#' \item{qhat5_s2_m2}{The variability (model 2) estimator.}
#' \item{qhat5_s2_m3}{The variability (model 3) estimator.}
#' \item{qhat5_s2_m4}{The variability (model 4) estimator.}
#' \item{qhat5_s2_m5}{The variability (model 5) estimator.}
#' \item{hat_btV_0}{The variability coefficients (model 0) estimators.}
#' \item{hat_btV_1}{The variability coefficients (model 1) estimators.}
#' \item{hat_btV_2}{The variability coefficients (model 2) estimators.}
#' \item{hat_btV_3}{The variability coefficients (model 3) estimators.}
#' \item{hat_btV_4}{The variability coefficients (model 4) estimators.}
#' \item{hat_btV_5}{The variability coefficients (model 5) estimators.}
#' \item{C}{The estimators of the tau-th quantile of the estimated residuals.}
#' \item{comp}{The pairwise comparisons for testing the variabilty function.}
#' \item{P}{The p-values.}
#' \item{GR}{The test statistics for the given data.}
#' \item{Gb}{The bootstrap test statistics.}
#' }
#'
#' @section Note:
#' Some warning messages are related to the function \code{\link{rq.fit.sfn}}
#' (See http://www.inside-r.org/packages/cran/quantreg/docs/sfnMessage).
#'
#' @section Author(s):
#' Mohammed Abdulkerim Ibrahim
#'
#' @section Refrences:
#' Gijbels, I., Ibrahim, M. A., and Verhasselt, A. (2017b). Testing the
#' heteroscedastic error structure in quantile varying coefficient models.
#' {\it Submitted}.
#'
#' Andriyana, Y. (2015). P-splines quantile regression in varying coefficient
#' models. {\it PhD Dissertation}. KU Leuven, Belgium. ISBN 978-90-8649-791-1.
#'
#' Andriyana, Y. and Gijbels, I. & Verhasselt, A. (2014). P-splines quantile
#' regression estimation in varying coefficient models. {\it Test}, 23, 153-194.
#'
#' Andriyana, Y., Gijbels, I. and Verhasselt, A. (2017). Quantile regression
#' in varying-coefficient models: non-crossing quantile curves and
#' heteroscedasticity. {\it Statistical Papers}, to appear.
#' DOI:10.1007/s00362-016-0847-7
#'
#' He, X. (1997). Quantile curves without crossing. {\it The American Statistician},
#'  51, 186-192.
#'
#' @seealso \code{\link{rq.fit.sfn}} \code{\link{as.matrix.csr}}
#'
#' @examples
#' library(truncSP)
#' data(PM10)
#' PM10 = PM10[order(PM10$day,PM10$hour,decreasing=FALSE),]

#' y = PM10$PM10 ## the logarithm of the concentration of PM10
#' times = PM10$hour  ## the time in hours
#' subj = PM10$day  ## subject indicator (day)
#' dim=length(y) ## number of rows in the data = 500
#' x0 = rep(1,dim) ## for intercept
## the covariates
#' x1 = PM10$cars ## logarithm of number of cars per hour
#' x2 = PM10$wind.speed ## the wind speed (in meters/second)
#' x3 = PM10$temp ## the temperature (in degree Celsius)
#' X = cbind(x0, x1, x2, x3) ## the covariate matrix
#' px=ncol(X)
#'
#'##########################
#'### Input parameters ####
#'#########################
#' lambda = 10^seq(-2, 1, 0.1)
#' kn = rep(10,px)
#' degree = rep(3,px)
#' d = rep(1,px)
#' gam=0.25
#' nr.bootstrap.samples=100
#' seed=110
#' taus = seq(0.1,0.9,0.2)
#' #########################
#' test1=test_variability(times=times, subj=subj, X=X, y=y, d=d, kn=kn,
#'                       degree=degree, lambda=lambda, gam=gam, tau=taus,
#'                       nr.bootstrap.samples=nr.bootstrap.samples,seed=seed,
#'                       test="Y",model=4)
#'#### Testing results
#'test1$comp  #the comparisons
#' test1$P  ## p-values
#' test1$GR ## test statistics
#'
#' ### estimation results
#' qhat5_s2_m4=test1$qhat5_s2_m4
#' qhat5_s2_m5=test1$qhat5_s2_m5
#' qhat5_s2_m0=test1$qhat5_s2_m0*rep(1,dim)
#' gamma0=test1$hat_btV_4[1:dim]
#' gamma1=test1$hat_btV_4[(dim+1):(dim*2)]
#' gamma2=test1$hat_btV_4[(dim*2+1):(dim*3)]
#' gamma3=test1$hat_btV_4[(dim*3+1):(dim*4)]
#'
#' i = order(times, qhat5_s2_m4, qhat5_s2_m5, qhat5_s2_m0,gamma0,gamma1,
#' gamma2,gamma3);
#' times_o = times[i];  qhat5_s2_m4_o=qhat5_s2_m4[i];
#' qhat5_s2_m5_o=qhat5_s2_m5[i]; qhat5_s2_m0_o=qhat5_s2_m0[i];
#' gamma0_o=gamma0[i]; gamma1_o=gamma1[i]; gamma2_o=gamma2[i];
#' gamma3_o=gamma3[i]
#'
#'#####  variability functions plots
#' win.graph()
#' par(las=1)
#' plot(qhat5_s2_m4_o~times_o, col="magenta", cex=0.2,
#' xlab="hour", ylab="estimated variability function")
#' lines(qhat5_s2_m5_o~times_o, col="red", cex=0.2, lty=1, lwd=2);
#' lines(qhat5_s2_m0_o~times_o, col="black", cex=0.2, lty=5, lwd=2);
#' legend("topleft", c("Model 4", "Model 5", "Model 0"),
#'        ncol=1, col=c("magenta","red","black"),
#'        lwd=c(1,2,2), lty=c(3,1,5))
#'
#'### Plot of coefficients for variability function
#' plot(gamma0_o~times_o, lwd=2, type="l", xlab="hour", ylab=expression(hat(gamma)(T)));
#' plot(gamma1_o~times_o, lwd=2, type="l", xlab="hour",
#' ylab="coefficient of logarithm of number of cars per hour");
#' plot(gamma2_o~times_o, lwd=2, type="l", xlab="hour",ylab="coefficient of wind speed");
#' plot(gamma3_o~times_o, lwd=2, type="l", xlab="hour",ylab="coefficient of temperature")
#'
#' @export
test_variability<- function(times, subj, X, y, d, kn, degree, lambda, gam, tau,
                            nr.bootstrap.samples,seed,test="Y",model=4){

  #### the testing procedure
  GR=rep(0,5)
  P=rep(0,5)
  Gb=matrix(0,nr.bootstrap.samples,5)


  # calculating weights (W) for the repeated measurements
  W = Weight_Ni(y, subj)$W

  # standardizing the covariates
  XX=X
  px = ncol(X)
  if (all(X[, 1] == 1))
  {X[, 1] = X[, 1]} else {X[, 1] = (X[, 1] - min(X[, 1]))/(3*(max(X[, 1]) -
                                                                min(X[,1])))}
  for (k in 2:px) {
    X[, k] = (X[, k] - min(X[, k]))/(3*(max(X[, k]) - min(X[,k])))
  }

  norm1=rowSums(X[, 2:px])
  dim = length(y)  ## number of raws in the data

  ##Step1 in Section 3.1
  lambda50all = lambdak_gl(times, subj, X, y, d, tau= 0.5, kn, degree, lambda, gam)
  lambdasicr50=lambda50all$lambdasic
  ranges=lambda50all$range
  qvcsicr50=qrvcp_gl(times, subj, y, X, tau= 0.5, kn, degree, lambda=lambdasicr50,
                     d,ranges)
  hat_btsic = qvcsicr50$hat_bt ## the estimated coefficients of the median for
  # the standardized covariates

  yhatsic_k = matrix(NA, dim, px)
  for (k in 1:px) {
    yhatsic_k[, k] = hat_btsic[seq((k - 1) * dim + 1, k * dim)] * X[, k]
  }
  qhat5_s1 = rowSums(yhatsic_k) # the estimated median

  ######## Step2 in Section 3.2 ######################
  res50 = y - qhat5_s1
  abs_res=abs(y-qhat5_s1)+1e-05
  log_abs_res=log(abs(y-qhat5_s1)+1e-05)


  ##Step2_model0
  p=0
  sX <- as.matrix.csr(W*matrix(X[, 1], nrow=dim))
  tmpmax <- floor(1e5 + exp(-12.1)*(sX@ia[p+1]-1)^2.35)
  rhs = (1-0.5)*(t(sX))%*%rep(1,dim)
  hat_bt50_s2 <- rq.fit.sfn(sX, W*log_abs_res,rhs=rhs, control = list(tmpmax=
                                                                       tmpmax))$coef

  qhat5_s2_m0 =hat_btV_0= exp(hat_bt50_s2) ## the estimated V_0
  res0=abs_res-qhat5_s2_m0


  #Step2_model1
  p=1
  x4=1+norm1
  lx4=log(1+norm1)
  sX <- as.matrix.csr(W*matrix(cbind(X[, 1], lx4), nrow=dim))
  tmpmax <- floor(1e5 + exp(-12.1)*(sX@ia[p+1]-1)^2.35)
  rhs = (1-0.5)*(t(sX))%*%rep(1,dim)

  hat_bt50_s2 <- rq.fit.sfn(sX, W*log_abs_res,rhs=rhs, control = list(tmpmax=
                                                                          tmpmax))$coef ## the estimated coefficients for V_1

  hat_b0t50_s2 = hat_bt50_s2[1]
  hat_b1t50_s2 = hat_bt50_s2[2]
  qhat5_s2_m1 = exp(hat_b0t50_s2*X[, 1])*x4^hat_b1t50_s2 # the estimated V_1
  hat_btV_1=c(exp(hat_b0t50_s2),hat_b1t50_s2)
  res1=abs_res-qhat5_s2_m1


  ##Step2_model2
  x4=1+norm1
  lx4=log(1+norm1)
  lambda50all = lambdak_grl(times, subj, X=cbind(X[, 1], lx4), y=log_abs_res,
                            d=d[1], tau= 0.5, kn=kn[1], degree=degree[1], lambda, gam=0)
  lambdasicr50=lambda50all$lambdasic
  ranges=lambda50all$range

  hat_bt50_s2 = qrvcp_grl(times, subj, y=log_abs_res, X=cbind(X[, 1], lx4), tau= 0.5,
                          kn[1], degree[1], lambda=lambdasicr50, d[1],ranges)$hat_bt## the estimated coefficients for V_2

  hat_b0t50_s2 = hat_bt50_s2[seq(1,dim)]
  hat_b1t50_s2 = hat_bt50_s2[dim+1]
  qhat5_s2_m2 = exp(hat_b0t50_s2*X[, 1])*x4^hat_b1t50_s2 # the estimated V_2
  hat_btV_2=c(exp(hat_b0t50_s2),hat_b1t50_s2)
  res2=abs_res-qhat5_s2_m2

  ##Step2_model3
  lambda50all = lambdak_grl(times, subj, X=cbind(X[, 1], norm1), y=log_abs_res,
                            d[1], tau= 0.5, kn[1], degree[1], lambda, gam=0)## str(X)
  lambdasicr50=lambda50all$lambdasic
  ranges=lambda50all$range

  hat_bt50_s2 = qrvcp_grl(times, subj, y=log_abs_res, X=cbind(X[, 1], norm1), tau= 0.5,
                          kn[1], degree[1], lambda=lambdasicr50, d[1],ranges)$hat_bt ## the estimated coefficients for V_3

  hat_b0t50_s2 = hat_bt50_s2[seq(1,dim)]
  hat_b1t50_s2 = hat_bt50_s2[dim+1]
  qhat5_s2_m3 = exp(hat_b0t50_s2*X[, 1]+norm1*hat_b1t50_s2) # the estimated V_3
  hat_btV_3=c(exp(hat_b0t50_s2),hat_b1t50_s2)
  res3=abs_res-qhat5_s2_m3


  #Step2_model4
  lambda50all = lambdak_gl(times, subj, X, y=log_abs_res, d, tau= 0.5, kn, degree,
                           lambda, gam=1/4)
  lambdasicr50=lambda50all$lambdasic
  ranges=lambda50all$range
  hat_btsic_v = qrvcp_gl(times, subj, y=log_abs_res, X, tau= 0.5, kn, degree,
                         lambda=lambdasicr50, d,ranges)$hat_bt ## the estimated coefficients for V_4

  yhatsic_k = matrix(NA, dim, px)
  for (k in 1:px) {
    yhatsic_k[, k] = hat_btsic_v[seq((k - 1) * dim + 1, k * dim)] * X[, k]
  }
  qhat5_s2_m4 = exp(rowSums(yhatsic_k)) # the estimated V_4
  res4=abs_res-qhat5_s2_m4


  ##Step2_model5
  lambda50all = lambdak_gl(times, subj, X=X[, 1], y=log_abs_res, d[1], tau= 0.5, kn[1],
                           degree[1], lambda, gam=0)
  lambdasicr50=lambda50all$lambdasic
  ranges=lambda50all$range

  hat_bt50_s2 = qrvcp_gl(times, subj, y=log_abs_res, X=X[, 1], tau= 0.5, kn[1], degree[1],
                         lambda=lambdasicr50, d[1],ranges)$hat_bt
  qhat5_s2_m5 = hat_btV_5= exp(hat_bt50_s2) # the estimated V_5
  res5=abs_res-qhat5_s2_m5

  ## estimated coeffitients for median for the original covariates
  hat_bt50 = matrix(NA, dim, px)
  hat_bt50_0k = matrix(0, dim, px)
  for (k in 2:px) {
    hat_bt50[, k] = hat_btsic[seq((k - 1) * dim + 1, k *
                                    dim)]/(3*(max(XX[, k]) - min(XX[, k])))
    hat_bt50_0k[, k] = hat_btsic[seq((k - 1) * dim + 1, k *
                                       dim)] * min(XX[, k])/(3*(max(XX[, k]) - min(XX[, k])))
  }
  hat_bt50[, 1] = hat_btsic[seq(1, dim)] - rowSums(hat_bt50_0k)
  hat_bt50 = c(hat_bt50)


  ## estimated coeffitients for variability for V_4
  hat_btV_4 = matrix(NA, dim, px)
  hat_bt50_0k_s2 = matrix(0, dim, px)
  for (k in 2:px) {
    hat_btV_4[, k] = hat_btsic_v[seq((k - 1) * dim + 1, k *
                                       dim)]/(3*(max(XX[, k]) - min(XX[, k])))
    hat_bt50_0k_s2[, k] = hat_btsic_v[seq((k - 1) * dim + 1, k *
                                            dim)] * min(XX[, k])/(3*(max(XX[, k]) - min(XX[, k])))
  }
  hat_btV_4[, 1] = exp(hat_btsic_v[seq(1, dim)] - rowSums(hat_bt50_0k_s2))
  hat_btV_4 = c(hat_btV_4)

  ##step3
  if ( model==0) {
    hat_Vt=qhat5_s2_m0
  } else if ( model==1) {
    hat_Vt=qhat5_s2_m1
  } else if ( model==2) {
    hat_Vt=qhat5_s2_m2
  } else if ( model==3) {
    hat_Vt=qhat5_s2_m3
  } else if ( model==4) {
    hat_Vt=qhat5_s2_m4
  } else
    hat_Vt=qhat5_s2_m5

  H = length(tau)
  C = matrix(NA, dim, H)
  for (h in 1:H) {
    lambda50all = lambdak_gl(times, subj, X=hat_Vt, y=res50, d=d[1], tau= tau[h], kn=kn[1],
                             degree =degree[1], lambda, gam=0)
    lambdasicr50=lambda50all$lambdasic
    ranges=lambda50all$range
    C[, h] =qrvcp_gl(times, subj, y=res50, X=hat_Vt, tau= tau[h], kn=kn[1], degree =degree[1],
                     lambda=lambdasicr50, d=d[1],ranges)$hat_bt
  }

  if (test != "Y"){
    out = list(qhat5_s2_m0=qhat5_s2_m0,qhat5_s2_m1=qhat5_s2_m1, qhat5_s2_m2=qhat5_s2_m2,
               qhat5_s2_m3=qhat5_s2_m3,qhat5_s2_m4=qhat5_s2_m4,qhat5_s2_m5=qhat5_s2_m5,
               est_median=qhat5_s1,est_err_quantile=C,hat_bt50=hat_bt50,hat_btV_0=hat_btV_0,
               hat_btV_1=hat_btV_1,hat_btV_2=hat_btV_2,hat_btV_3=hat_btV_3,
               hat_btV_4=hat_btV_4,hat_btV_5=hat_btV_5)
    return(out)
  }

  ##### calculating G in Section 4.1 of the paper
  ### V_0 Vs V_4
  GR[1]=2*(sum(W*(res0*(0.5-1*(res0<0))-res4*(0.5-1*(res4<0)))))
  ### V_1 Vs V_4
  GR[2]=2*(sum(W*(res1*(0.5-1*(res1<0))-res4*(0.5-1*(res4<0)))))
  ### V_5 Vs V_4
  GR[3]=2*(sum(W*(res5*(0.5-1*(res5<0))-res4*(0.5-1*(res4<0)))))
  ### V_2 Vs V_4
  GR[4]=2*(sum(W*(res2*(0.5-1*(res2<0))-res4*(0.5-1*(res4<0)))))
  ### V_3 Vs V_4
  GR[5]=2*(sum(W*(res3*(0.5-1*(res3<0))-res4*(0.5-1*(res4<0)))))

  ### obtain psedo response under H_0
  Py0=qhat5_s1+qhat5_s2_m0*(y-qhat5_s1)/qhat5_s2_m4
  Py1=qhat5_s1+qhat5_s2_m1*(y-qhat5_s1)/qhat5_s2_m4
  Py2=qhat5_s1+qhat5_s2_m2*(y-qhat5_s1)/qhat5_s2_m4
  Py3=qhat5_s1+qhat5_s2_m3*(y-qhat5_s1)/qhat5_s2_m4
  Py5=qhat5_s1+qhat5_s2_m5*(y-qhat5_s1)/qhat5_s2_m4
  data1=data.frame(subj,times,norm1,W,Py0,Py1,Py5,Py2,Py3,X)

  ## Bootstrap
  data1_b=NULL
  Dim_b=NULL
  sample.size= n= length(unique(subj))
  subj1=unique(subj)

  for (b in 1:nr.bootstrap.samples){
    set.seed(seed+b)
    sel=sample(sample.size,sample.size,replace=TRUE)

    data=NULL
    for (i in 1:n){
      data =rbind(data,data1[subj==subj1[sel[i]],])
    }

    data1_b=rbind(data1_b,data)
    N=length(data$Py0)
    Dim_b=rbind(Dim_b,N)
  }

  Dim_b=Dim_b[,1]


  ################################################################################
  ########################## calculating Gb: the bootstrap G

  #### comp1:  V_0 vs V_4
  tau=0.5

  vi=0

  for (b in 1:nr.bootstrap.samples){

    dim = Dim_b[b]
    a=c((vi+1):(vi+dim))
    subj=data1_b[a,1]
    times = data1_b[a,2]
    norm1=data1_b[a,3]
    W=data1_b[a,4]
    y = data1_b[a,5]
    X=data1_b[a,10:(9+px)]
    X=as.matrix(X)

    ##Step1
    lambda50all = lambdak_gl(times, subj, X, y, d, tau, kn, degree, lambda, gam)
    lambdasicr50=lambda50all$lambdasic
    ranges=lambda50all$range
    hat_btsic = qrvcp_gl(times, subj, y, X, tau, kn, degree, lambda=lambdasicr50,
                         d,ranges)$hat_bt

    yhatsic_k = matrix(NA, dim, px)
    for (k in 1:px) {
      yhatsic_k[, k] = hat_btsic[seq((k - 1) * dim + 1,
                                     k * dim)] * X[, k]
    }
    qhat5_s1 = rowSums(yhatsic_k)

    ######## Step2 ######################
    abs_res=abs(y-qhat5_s1)+1e-05
    log_abs_res=log(abs(y-qhat5_s1)+1e-05)


    ##Step2_model0
    p=0
    sX <- as.matrix.csr(W*matrix(X[, 1], nrow=dim))
    tmpmax <- floor(1e5 + exp(-12.1)*(sX@ia[p+1]-1)^2.35)
    rhs = (1-0.5)*(t(sX))%*%rep(1,dim)
    hat_bt50_s2 <- rq.fit.sfn(sX, W*log_abs_res,rhs=rhs, control = list(tmpmax=
                                                                            tmpmax))$coef

    res0=abs_res-exp(hat_bt50_s2)

    #Step2_model4
    lambda50all = lambdak_gl(times, subj, X, y=log_abs_res, d, tau, kn, degree,
                             lambda, gam)
    lambdasicr50=lambda50all$lambdasic
    ranges=lambda50all$range
    hat_btsic = qrvcp_gl(times, subj, y=log_abs_res, X, tau, kn, degree,
                         lambda=lambdasicr50, d,ranges)$hat_bt

    yhatsic_k = matrix(NA, dim, px)
    for (k in 1:px) {
      yhatsic_k[, k] = hat_btsic[seq((k - 1) * dim + 1, k * dim)] * X[, k]
    }
    res4=abs_res-exp(rowSums(yhatsic_k))

    Gb[b,1]=2*(sum(W*(res0*(0.5-1*(res0<0))-res4*(0.5-1*(res4<0)))))
    vi=sum(Dim_b[1:b])
  }

  P[1]=sum(1*(Gb[,1]>=GR[1]))/nr.bootstrap.samples



  ## Comp2: V_1 vs V_4
  vi=0
  for (b in 1:nr.bootstrap.samples){

    dim = Dim_b[b]
    a=c((vi+1):(vi+dim))
    subj=data1_b[a,1]
    times = data1_b[a,2]
    norm1=data1_b[a,3]
    W=data1_b[a,4]
    y = data1_b[a,6]
    X=data1_b[a,10:(9+px)]
    X=as.matrix(X)

    ##Step1
    lambda50all = lambdak_gl(times, subj, X, y, d, tau, kn, degree, lambda, gam)
    lambdasicr50=lambda50all$lambdasic
    ranges=lambda50all$range
    hat_btsic = qrvcp_gl(times, subj, y, X, tau, kn, degree, lambda=lambdasicr50,
                         d,ranges)$hat_bt

    yhatsic_k = matrix(NA, dim, px)
    for (k in 1:px) {
      yhatsic_k[, k] = hat_btsic[seq((k - 1) * dim + 1, k * dim)] * X[, k]
    }
    qhat5_s1 = rowSums(yhatsic_k)

    ######## Step2 ######################
    abs_res=abs(y-qhat5_s1)+1e-05
    log_abs_res=log(abs(y-qhat5_s1)+1e-05)


    #Step2_model1
    p=1
    x4=1+norm1
    lx4=log(1+norm1)
    sX <- as.matrix.csr(W*matrix(cbind(X[, 1], lx4), nrow=dim))
    tmpmax <- floor(1e5 + exp(-12.1)*(sX@ia[p+1]-1)^2.35)
    rhs = (1-0.5)*(t(sX))%*%rep(1,dim)
    hat_bt50_s2 = rq.fit.sfn(sX, W*log_abs_res,rhs=rhs, control = list(tmpmax=
                                                                         tmpmax))$coef

    res1=abs_res-exp(hat_bt50_s2[1]*X[, 1])*x4^ hat_bt50_s2[2]

    #Step2_model4
    lambda50all = lambdak_gl(times, subj, X, y=log_abs_res, d, tau, kn, degree,
                             lambda, gam)
    lambdasicr50=lambda50all$lambdasic
    ranges=lambda50all$range
    hat_btsic = qrvcp_gl(times, subj, y=log_abs_res, X, tau, kn, degree,
                         lambda=lambdasicr50, d,ranges)$hat_bt

    yhatsic_k = matrix(NA, dim, px)
    for (k in 1:px) {
      yhatsic_k[, k] = hat_btsic[seq((k - 1) * dim + 1,
                                     k * dim)] * X[, k]
    }
    res4=abs_res-exp(rowSums(yhatsic_k))

    Gb[b,2]=2*(sum(W*(res1*(0.5-1*(res1<0))-res4*(0.5-1*(res4<0)))))
    vi=sum(Dim_b[1:b])
  }

  P[2]=sum(1*(Gb[,2]>=GR[2]))/nr.bootstrap.samples



  ## Comp3: V_5 vs V_4
  vi=0
  for (b in 1:nr.bootstrap.samples){

    dim = Dim_b[b]
    a=c((vi+1):(vi+dim))
    subj=data1_b[a,1]
    times = data1_b[a,2]
    norm1=data1_b[a,3]
    W=data1_b[a,4]
    y = data1_b[a,7]
    X=data1_b[a,10:(9+px)]
    X=as.matrix(X)

    ##Step1
    lambda50all = lambdak_gl(times, subj, X, y, d, tau, kn, degree, lambda, gam)
    lambdasicr50=lambda50all$lambdasic
    ranges=lambda50all$range
    hat_btsic = qrvcp_gl(times, subj, y, X, tau, kn, degree, lambda=lambdasicr50,
                         d,ranges)$hat_bt

    yhatsic_k = matrix(NA, dim, px)
    for (k in 1:px) {
      yhatsic_k[, k] = hat_btsic[seq((k - 1) * dim + 1,
                                     k * dim)] * X[, k]
    }
    qhat5_s1 = rowSums(yhatsic_k)

    ######## Step2 ######################
    abs_res=abs(y-qhat5_s1)+1e-05
    log_abs_res=log(abs(y-qhat5_s1)+1e-05)


    #Step2_model4
    lambda50all = lambdak_gl(times, subj, X, y=log_abs_res, d, tau, kn, degree,
                             lambda, gam=1/4)
    lambdasicr50=lambda50all$lambdasic
    ranges=lambda50all$range
    hat_btsic = qrvcp_gl(times, subj, y=log_abs_res, X, tau, kn, degree,
                         lambda=lambdasicr50, d,ranges)$hat_bt

    yhatsic_k = matrix(NA, dim, px)
    for (k in 1:px) {
      yhatsic_k[, k] = hat_btsic[seq((k - 1) * dim + 1, k * dim)] * X[, k]
    }
    res4=abs_res-exp(rowSums(yhatsic_k))


    ##Step2_model5
    lambda50all = lambdak_gl(times, subj, X=X[,1], y=log_abs_res, d[1], tau, kn[1],
                             degree[1], lambda, gam=0)
    lambdasicr50=lambda50all$lambdasic
    ranges=lambda50all$range

    hat_bt50_s2 = qrvcp_gl(times, subj, y=log_abs_res, X[1], tau, kn[1], degree[1],
                           lambda=lambdasicr50, d[1],ranges)$hat_bt
    res5=abs_res-exp(hat_bt50_s2)

    Gb[b,3]=2*(sum(W*(res5*(0.5-1*(res5<0))-res4*(0.5-1*(res4<0)))))
    vi=sum(Dim_b[1:b])
  }

  P[3]=sum(1*(Gb[,3]>=GR[3]))/nr.bootstrap.samples



  ## Comp4: V_2 vs V_4
  vi=0
  for (b in 1:nr.bootstrap.samples){

    dim = Dim_b[b]
    a=c((vi+1):(vi+dim))
    subj=data1_b[a,1]
    times = data1_b[a,2]
    norm1=data1_b[a,3]
    W=data1_b[a,4]
    y = data1_b[a,8]
    X=data1_b[a,10:(9+px)]
    X=as.matrix(X)

    ##Step1
    lambda50all = lambdak_gl(times, subj, X, y, d, tau, kn, degree, lambda, gam)
    lambdasicr50=lambda50all$lambdasic
    ranges=lambda50all$range
    hat_btsic = qrvcp_gl(times, subj, y, X, tau, kn, degree, lambda=lambdasicr50,
                         d,ranges)$hat_bt

    yhatsic_k = matrix(NA, dim, px)
    for (k in 1:px) {
      yhatsic_k[, k] = hat_btsic[seq((k - 1) * dim + 1,
                                     k * dim)] * X[, k]
    }
    qhat5_s1 = rowSums(yhatsic_k)

    ######## Step2 ######################
    abs_res=abs(y-qhat5_s1)+1e-05
    log_abs_res=log(abs(y-qhat5_s1)+1e-05)


    ##Step2_model2
    x4=1+norm1
    lx4=log(1+norm1)
    lambda50all = lambdak_grl(times, subj, X=cbind(X[, 1], lx4), y=log_abs_res,
                              d[1], tau, kn[1], degree[1], lambda, gam=0)
    lambdasicr50=lambda50all$lambdasic
    ranges=lambda50all$range
    hat_bt50_s2 = qrvcp_grl(times, subj, y=log_abs_res, X=cbind(X[, 1], lx4), tau,
                            kn[1], degree[1], lambda=lambdasicr50, d[1],ranges)$hat_bt

    res2=abs_res-exp(hat_bt50_s2[seq(1,dim)]*X[, 1])*x4^hat_bt50_s2[dim+1]


    #Step2_model4
    lambda50all = lambdak_gl(times, subj, X, y=log_abs_res, d, tau, kn, degree,
                             lambda, gam)
    lambdasicr50=lambda50all$lambdasic
    ranges=lambda50all$range
    hat_btsic = qrvcp_gl(times, subj, y=log_abs_res, X, tau, kn, degree,
                         lambda=lambdasicr50, d,ranges)$hat_bt

    yhatsic_k = matrix(NA, dim, px)
    for (k in 1:px) {
      yhatsic_k[, k] = hat_btsic[seq((k - 1) * dim + 1,
                                     k * dim)] * X[, k]
    }
    res4=abs_res-exp(rowSums(yhatsic_k))

    Gb[b,4]=2*(sum(W*(res2*(0.5-1*(res2<0))-res4*(0.5-1*(res4<0)))))
    vi=sum(Dim_b[1:b])
  }

  P[4]=sum(1*(Gb[,4]>=GR[4]))/nr.bootstrap.samples


  ## Comp5: V_3 vs V_4
  vi=0
  for (b in 1:nr.bootstrap.samples){

    dim = Dim_b[b]
    a=c((vi+1):(vi+dim))
    subj=data1_b[a,1]
    times = data1_b[a,2]
    norm1=data1_b[a,3]
    W=data1_b[a,4]
    y = data1_b[a,9]
    X=data1_b[a,10:(9+px)]
    X=as.matrix(X)

    ##Step1
    lambda50all = lambdak_gl(times, subj, X, y, d, tau, kn, degree, lambda, gam)
    lambdasicr50=lambda50all$lambdasic
    ranges=lambda50all$range
    hat_btsic = qrvcp_gl(times, subj, y, X, tau, kn, degree, lambda=lambdasicr50,
                         d,ranges)$hat_bt

    yhatsic_k = matrix(NA, dim, px)
    for (k in 1:px) {
      yhatsic_k[, k] = hat_btsic[seq((k - 1) * dim + 1, k * dim)] * X[, k]
    }
    qhat5_s1 = rowSums(yhatsic_k)

    ######## Step2 ######################
    abs_res=abs(y-qhat5_s1)+1e-05
    log_abs_res=log(abs(y-qhat5_s1)+1e-05)


    ##Step2_model3
    lambda50all = lambdak_grl(times, subj, X=cbind(X[, 1], norm1), y=log_abs_res,
                              d[1], tau, kn[1], degree[1], lambda, gam=0)
    lambdasicr50=lambda50all$lambdasic
    ranges=lambda50all$range
    hat_bt50_s2 = qrvcp_grl(times, subj, y=log_abs_res, X=cbind(X[, 1], norm1), tau,
                            kn[1], degree[1], lambda=lambdasicr50, d[1],ranges)$hat_bt

    res3=abs_res-exp(hat_bt50_s2[seq(1,dim)]*X[, 1]+norm1*hat_bt50_s2[dim+1])


    #Step2_model4
    lambda50all = lambdak_gl(times, subj, X, y=log_abs_res, d, tau, kn, degree,
                             lambda, gam)
    lambdasicr50=lambda50all$lambdasic
    ranges=lambda50all$range
    hat_btsic = qrvcp_gl(times, subj, y=log_abs_res, X, tau, kn, degree,
                         lambda=lambdasicr50, d,ranges)$hat_bt

    yhatsic_k = matrix(NA, dim, px)
    for (k in 1:px) {
      yhatsic_k[, k] = hat_btsic[seq((k - 1) * dim + 1, k * dim)] * X[, k]
    }
    res4=abs_res-exp(rowSums(yhatsic_k))

    Gb[b,5]=2*(sum(W*(res3*(0.5-1*(res3<0))-res4*(0.5-1*(res4<0)))))
    vi=sum(Dim_b[1:b])
  }

  P[5]=sum(1*(Gb[,5]>=GR[5]))/nr.bootstrap.samples

  out = list(qhat5_s2_m0=qhat5_s2_m0,qhat5_s2_m1=qhat5_s2_m1, qhat5_s2_m2=qhat5_s2_m2,
             qhat5_s2_m3=qhat5_s2_m3,qhat5_s2_m4=qhat5_s2_m4,qhat5_s2_m5=qhat5_s2_m5,
             est_median=qhat5_s1,est_err_quantile=C,hat_bt50=hat_bt50,hat_btV_0=hat_btV_0,
             hat_btV_1=hat_btV_1,hat_btV_2=hat_btV_2,hat_btV_3=hat_btV_3,
             hat_btV_4=hat_btV_4,hat_btV_5=hat_btV_5,
             comp=paste(c("M0 Vs M4","M1 Vs M4","M5 Vs M4","M2 Vs M4",
                          "M3 Vs M4")),P = P, GR = GR, Gb=Gb)
  return(out)

}
