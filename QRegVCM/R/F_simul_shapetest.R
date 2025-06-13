#' Testing the shape of a functional coefficient in the median and/or the variabilty function
#'
#' This function tests a constancy of a functional coefficient in Model (1) of Gijbels etal (2017a)
#'
#' @param times The vector of time variable.
#' @param subj The vector of subjects/individuals.
#' @param X The covariates, containing 1 as its first component
#' (including intercept in the model)
#' @param y The response vector.
#' @param d The order of differencing operator for each covariate.
#' @param kn The number of internal knots for each covariate.
#' @param degree The degree of B-spline basis for each covariate.
#' @param lambda The grid of smoothing parameter to control the trade of
#' between fidelity and penalty term (use a fine grid of lambda).
#' @param gam The power used in estimating the smooting parameter for each
#' covariate (e.g. gam=1 or gam=0.5).
#' @param v The covariate indicator for which the shape test is interested
#' @param nr.bootstrap.samples The number of bootstrap samples used
#' @param seed The seed for the random generator in the bootstrap resampling
#' @param test The requested type of testing, it consists two arguments: 
#' the first argument for median and the second for the variability function.
#' "c" stands for constancy, "m" stands for monotonicity, and "conv" stands for convexity.
#' insert NA to the other argument when only for median/ variability function is needed.
#' @param omega a user defined constraint parameter ( in Equation (7) of
#' Gijbels etal (2017a)) chosen as large as possible
#'
#' @return
#' \describe{
#' \item{result}{The testing procedures.}
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
#' #' Gijbels, I., Ibrahim, M. A., and Verhasselt, A. (2017a). Shape testing in 
#' quantile varying coefficient models with heteroscedastic error.
#' {\it Journal of Nonparametric Statistics, 29(2):391--406.} 
#' 
#' Gijbels, I., Ibrahim, M. A., and Verhasselt, A. (2017b). Testing the
#' heteroscedastic error structure in quantile varying coefficient models.
#' {\it The Canadian Journal of Statistics,} DOI:10.1002/cjs.11346.
#'
#' Andriyana, Y. (2015). P-splines quantile regression in varying coefficient
#' models. {\it PhD Dissertation}. KU Leuven, Belgium. ISBN 978-90-8649-791-1.
#'
#' Andriyana, Y. and Gijbels, I. & Verhasselt, A. (2014). P-splines quantile
#' regression estimation in varying coefficient models. {\it Test}, 23, 153-194.
#'
#' Andriyana, Y., Gijbels, I. and Verhasselt, A. (2017). Quantile regression
#' in varying-coefficient models: non-crossing quantile curves and
#' heteroscedasticity. {\it Statistical Papers}, DOI:10.1007/s00362-016-0847-7
#'
#' He, X. (1997). Quantile curves without crossing. {\it The American Statistician},
#'  51, 186-192.
#'
#' @seealso \code{\link{rq.fit.sfn}} \code{\link{as.matrix.csr}}
#'
#' @examples
#' library(QRegVCM)
#' data(Wages)
#' y = wages[,5] ## the hourly wage
#' times = wages[,2] ## the duration of work experience in years 
#' subj = wages[,1] ## subject indicator (day)
#' dim=length(y) ## number of rows in the data = 6402#' 
#' x0 = rep(1,dim) ## for intercept
#' ### the covariates
#' ## creating 2 dummy variables for the race covariate
#' wages$r1[wages$race=="black"]=1  ## View(wages)  str(y)
#' wages$r1[wages$race!="black"]=0
#' wages$r2[wages$race=="hisp"]=1  ## View(wages)  str(wages)
#' wages$r2[wages$race!="hisp"]=0
#' x1 = wages$r1 # stands for black 
#' x2 = wages$r2 # stands for hispanic
#' x3 = wages$hgc ## the highest grade completed by the indiviadual
#' X = cbind(x0, x1, x2, x3) ## the covariate matrix
#' px=ncol(X)
#'
#'##########################
#'### Input parameters ####
#'#########################
#' lambda = 10^seq(-2, 1, 0.1)
#' kn = c(5,5,5,5) # the number of knot intervals
#' degree = rep(2,4) # the degree of splines
#' d = c(1,1,1,1)
#' gam=0.25
#' nr.bootstrap.samples=200
#' seed=110
#' #########################
#' test1=simul_shapetest(times=times, subj=subj, X=X, y=y, d=d, kn=kn,
#'                       degree=degree, lambda=lambda, gam=gam, v=1,
#'                       nr.bootstrap.samples=nr.bootstrap.samples,seed=seed,
#'                       test=c("c",NA),omega=10^3)
#'#### Testing results
#'test1$result  #the testing procedures
#' test1$P  ## p-values
#' test1$R ## test statistics
#'
#' @export
simul_shapetest<- function(times, subj, X, y, d, kn, degree, lambda, gam,v, 
                           nr.bootstrap.samples,seed,test,omega){
  
  dim = length(y)  ## number of raws in the data
  
  # standardizing the covariates
  px = ncol(X)
  if (all(X[, 1] == 1))
  {X[, 1] = X[, 1]} else {X[, 1] = (X[, 1] - min(X[, 1]))/((max(X[, 1]) -
                                                              min(X[,1])))}
  for (k in 2:px) {
    X[, k] = (X[, k] - min(X[, k]))/(max(X[, k]) - min(X[,k]))
  }
  
  # calculating weights (W) for the repeated measurements
  W = Weight_Ni(y, subj)$W
    m=kn+degree 
  cum_mB = cumsum(m)
  cum_mA = c(1, c(cum_mB + 1))
  
  #####################################################################################
  ##### The Real Statistics
  #####################################################################################
  
  ########## The full model
  lambda50all = lambdak_gl(times, subj, X, y, d, tau=0.5, kn, degree, lambda=lambda,gam)
  lambdasicr50=lambda50all$lambdasic
  ranges=lambda50all$range
  qvcsicr50=qrvcp_gl(times, subj, y, X, tau=0.5, kn, degree, lambda=lambdasicr50, d,ranges)
  hat_alpha = qvcsicr50$alpha  
  hat_bt = qvcsicr50$hat_bt ## the estimated coefficients of the median for
  # the standardized covariates
  
  yhatsic_k = matrix(NA, dim, px)
  for (k in 1:px) {
    yhatsic_k[, k] = hat_bt[seq((k - 1) * dim + 1, k * dim)] * X[, k]
  }
  medf=rowSums(yhatsic_k) 
  resf=y-medf
  
  ###################################################################################
  #### Only constancy of a functional coefficient in median
  ###################################################################################   
  if ( test[1] == "c" & test[2] %in% NA) { 
    ## L
    dd=diff(diag(m[v]), diff = 1) 
    
    ### calculate the norm tests
    L1R=sum(abs(dd%*%hat_alpha[cum_mA[v]:cum_mB[v]]))
    L2R=sqrt(sum((dd%*%hat_alpha[cum_mA[v]:cum_mB[v]])^2))
    LmR=max(abs(dd%*%hat_alpha[cum_mA[v]:cum_mB[v]]))
    
    ## the model under H_0
    Xr=X[,-v]
    lambda50all = lambdak_grl(times, subj, X=cbind(Xr,X[,v]), y, d[-v], tau=0.5, 
                              kn[-v], degree[-v], lambda=lambda, gam)
    lambdasicr50=lambda50all$lambdasic
    ranges=lambda50all$range
    qvcsicr50=qrvcp_grl(times, subj, y, X=cbind(Xr,X[,v]), tau=0.5, kn[-v], degree[-v],
                        lambda=lambdasicr50, d[-v],ranges)
    hat_btr = qvcsicr50$hat_bt
    
    yhatsic_k = matrix(NA, dim, px-1)
    for (k in 1:(px-1)) {
      yhatsic_k[, k] = hat_btr[seq((k - 1) * dim + 1, k * dim)] * Xr[, k]
    }
    medr=rowSums(yhatsic_k)+ hat_btr[((px-1)*dim)+1]*X[,v]
    
    resr=y-medr
    
    ### calculate LRT
    GR=2*(sum(W*(resr*(0.5-1*(resr<0))-resf*(0.5-1*(resf<0)))))
    
    ### obtain psedo response under H_0
    Py0=medr+(y-medf)
    
  } else if (test[1] == "m" & test[2] %in% NA) {
    ###################################################################################
    #### Only monotoncity of a functional coefficient in median
    ################################################################################### 
    ## dmin
    dd=diff(diag(m[v]), diff = 1) 
    
    dminR=min(dd%*%hat_alpha[cum_mA[v]:cum_mB[v]])
    
    lambda50all = lambdak_gmonl(times, subj, X, y, d, tau=0.5, kn, degree, 
                                lambda=lambda, gam,omega,mv=v)
    lambdasicr50=lambda50all$lambdasic
    ranges=lambda50all$range
    qvcsicr50=qrvcp_gmonl(times, subj, y, X, tau=0.5, kn, degree, lambda=lambdasicr50, d,omega,
                          range=ranges,mv=v)
    hat_btr = qvcsicr50$hat_bt
    
    yhatsic_k = matrix(NA, dim, px)
    for (k in 1:(px)) {
      yhatsic_k[, k] = hat_btr[seq((k - 1) * dim + 1, k * dim)] * X[, k]
    }
    medr=rowSums(yhatsic_k)
    
    resr=y-medr
    GR=2*(sum(W*(resr*(0.5-1*(resr<0))-resf*(0.5-1*(resf<0)))))
    
    ### obtain psedo response under H_0
    Py0=medr+(y-medf)
    
  } else if (test[1] == "conv" & test[2] %in% NA) {
    ###################################################################################
    #### Only convexity of a functional coefficient in median
    ################################################################################### 
    ## L
    dd=diff(diag(m[v]), diff = 2)
    
    dminR=min(dd%*%hat_alpha[cum_mA[v]:cum_mB[v]])
    
    lambda50all = lambdak_gconl(times, subj, X, y, d, tau=0.5, kn, degree, lambda=lambda,
                                gam,omega,v)
    lambdasicr50=lambda50all$lambdasic
    ranges=lambda50all$range
    qvcsicr50=qrvcp_gconl(times, subj, y, X, tau=0.5, kn, degree, lambda=lambdasicr50, d,
                          omega,range=ranges,v)
    hat_btr = qvcsicr50$hat_bt
    
    yhatsic_k = matrix(NA, dim, px)
    for (k in 1:(px)) {
      yhatsic_k[, k] = hat_btr[seq((k - 1) * dim + 1, k * dim)] * X[, k]
    }
    medr=rowSums(yhatsic_k)
    
    resr=y-medr
    
    GR=2*(sum(W*(resr*(0.5-1*(resr<0))-resf*(0.5-1*(resf<0)))))
    
    ### obtain psedo response under H_0
    Py0=medr+(y-medf)
    
  } else if (test[1] %in% NA & test[2] == "c") {
    ###################################################################################
    #### Only constancy of a functional coefficient in variability
    ################################################################################### 
    ########### Step2
    log_abs_res=log(abs(y-medf)+1e-05)
    lambda50all = lambdak_gl(times, subj, X, y=log_abs_res, d, tau=0.5, kn, degree, 
                             lambda=lambda,gam)
    lambdasicr50=lambda50all$lambdasic
    ranges=lambda50all$range
    qvcsicr50=qrvcp_gl(times, subj, y=log_abs_res, X, tau=0.5, kn, degree, 
                       lambda=lambdasicr50, d,ranges)
    hat_alpha = qvcsicr50$alpha  
    hat_bt = qvcsicr50$hat_bt ## the estimated coefficients for the standardized covariates
    
    yhatsic_k = matrix(NA, dim, px)
    for (k in 1:px) {
      yhatsic_k[, k] = hat_bt[seq((k - 1) * dim + 1, k * dim)] * X[, k]
    }
    lvarf=rowSums(yhatsic_k) 
    resf=log_abs_res-lvarf
    varf = exp(lvarf)
    
    ## L
    dd=diff(diag(m[v]), diff = 1) 
    
    ### calculate the norm tests
    L1R=sum(abs(dd%*%hat_alpha[cum_mA[v]:cum_mB[v]]))
    L2R=sqrt(sum((dd%*%hat_alpha[cum_mA[v]:cum_mB[v]])^2))
    LmR=max(abs(dd%*%hat_alpha[cum_mA[v]:cum_mB[v]]))
    
    
    ## the model under H_0
    Xr=X[,-v]
    lambda50all = lambdak_grl(times, subj, X=cbind(Xr,X[,v]), y=log_abs_res, d[-v], tau=0.5, 
                              kn[-v], degree[-v], lambda=lambda, gam)
    lambdasicr50=lambda50all$lambdasic
    ranges=lambda50all$range
    qvcsicr50=qrvcp_grl(times, subj, y=log_abs_res, X=cbind(Xr,X[,v]), tau=0.5, kn[-v], 
                        degree[-v],lambda=lambdasicr50, d[-v],ranges)
    hat_btr = qvcsicr50$hat_bt
    
    yhatsic_k = matrix(NA, dim, px-1)
    for (k in 1:(px-1)) {
      yhatsic_k[, k] = hat_btr[seq((k - 1) * dim + 1, k * dim)] * Xr[, k]
    }
    lvarr=rowSums(yhatsic_k)+ hat_btr[((px-1)*dim)+1]*X[,v] 
    resr=log_abs_res-lvarr
    varr = exp(lvarr)
    
    ### calculate LRT
    GR=2*(sum(W*(resr*(0.5-1*(resr<0))-resf*(0.5-1*(resf<0)))))
    
    
    ### obtain psedo response under H_0
    Py0=medf+varr*(y-medf)/varf 
    
  } else if (test[1] %in% NA & test[2] == "m") {
    ###################################################################################
    #### Only monotoncity of a functional coefficient in variability
    ################################################################################### 
    log_abs_res=log(abs(y-medf)+1e-05)
    lambda50all = lambdak_gl(times, subj, X, y=log_abs_res, d, tau=0.5, kn, degree, 
                             lambda=lambda,gam)
    lambdasicr50=lambda50all$lambdasic
    ranges=lambda50all$range
    qvcsicr50=qrvcp_gl(times, subj, y=log_abs_res, X, tau=0.5, kn, degree, 
                       lambda=lambdasicr50, d,ranges)
    hat_alpha = qvcsicr50$alpha  
    hat_bt = qvcsicr50$hat_bt ## the estimated coefficients for the standardized covariates
    
    yhatsic_k = matrix(NA, dim, px)
    for (k in 1:px) {
      yhatsic_k[, k] = hat_bt[seq((k - 1) * dim + 1, k * dim)] * X[, k]
    }
    lvarf=rowSums(yhatsic_k) 
    resf=log_abs_res-lvarf
    varf = exp(lvarf)
    
    ## dmin
    dd=diff(diag(m[v]), diff = 1) 
    
    dminR=min(dd%*%hat_alpha[cum_mA[v]:cum_mB[v]])
    
    lambda50all = lambdak_gmonl(times, subj, X, log_abs_res, d, tau=0.5, kn, degree, 
                                lambda=lambda, gam,omega,mv=v)
    lambdasicr50=lambda50all$lambdasic
    ranges=lambda50all$range
    qvcsicr50=qrvcp_gmonl(times, subj, log_abs_res, X, tau=0.5, kn, degree, lambda=lambdasicr50, d,omega,
                          range=ranges,mv=v)
    hat_btr = qvcsicr50$hat_bt
    
    yhatsic_k = matrix(NA, dim, px)
    for (k in 1:(px)) {
      yhatsic_k[, k] = hat_btr[seq((k - 1) * dim + 1, k * dim)] * X[, k]
    }
    lvarr=rowSums(yhatsic_k) 
    resr=log_abs_res-lvarr
    varr = exp(lvarr)
    
    ### calculate LRT
    GR=2*(sum(W*(resr*(0.5-1*(resr<0))-resf*(0.5-1*(resf<0)))))
    
    
    ### obtain psedo response under H_0
    Py0=medf+varr*(y-medf)/varf
    
  } else if (test[1] %in% NA & test[2] == "conv") {
    ###################################################################################
    #### Only convexity of a functional coefficient in variabilty
    ################################################################################### 
    log_abs_res=log(abs(y-medf)+1e-05)
    lambda50all = lambdak_gl(times, subj, X, y=log_abs_res, d, tau=0.5, kn, degree, 
                             lambda=lambda,gam)
    lambdasicr50=lambda50all$lambdasic
    ranges=lambda50all$range
    qvcsicr50=qrvcp_gl(times, subj, y=log_abs_res, X, tau=0.5, kn, degree, 
                       lambda=lambdasicr50, d,ranges)
    hat_alpha = qvcsicr50$alpha  
    hat_bt = qvcsicr50$hat_bt ## the estimated coefficients for the standardized covariates
    
    yhatsic_k = matrix(NA, dim, px)
    for (k in 1:px) {
      yhatsic_k[, k] = hat_bt[seq((k - 1) * dim + 1, k * dim)] * X[, k]
    }
    lvarf=rowSums(yhatsic_k) 
    resf=log_abs_res-lvarf
    varf = exp(lvarf)
    
    ## L
    dd=diff(diag(m[v]), diff = 2)
    
    dminR=min(dd%*%hat_alpha[cum_mA[v]:cum_mB[v]])
    
    lambda50all = lambdak_gconl(times, subj, X, y=log_abs_res, d, tau=0.5, kn, degree, 
                                lambda=lambda, gam,omega,v)
    lambdasicr50=lambda50all$lambdasic
    ranges=lambda50all$range
    qvcsicr50=qrvcp_gconl(times, subj, y=log_abs_res, X, tau=0.5, kn, degree, 
                          lambda=lambdasicr50, d,	omega,range=ranges,v)
    hat_btr = qvcsicr50$hat_bt
    
    yhatsic_k = matrix(NA, dim, px)
    for (k in 1:(px)) {
      yhatsic_k[, k] = hat_btr[seq((k - 1) * dim + 1, k * dim)] * X[, k]
    }
    lvarr=rowSums(yhatsic_k) 
    resr=log_abs_res-lvarr
    varr = exp(lvarr)
    
    ### calculate LRT
    GR=2*(sum(W*(resr*(0.5-1*(resr<0))-resf*(0.5-1*(resf<0)))))
    
    
    ### obtain psedo response under H_0
    Py0=medf+varr*(y-medf)/varf
    
  } else {
    ###################################################################################
    #### simultaneous test of a functional coefficient in both median and variabilty
    ################################################################################### 
    ##### step1
    if (test[1] == "c"){
      ## the model under H_0
      Xr=X[,-v]
      lambda50all = lambdak_grl(times, subj, X=cbind(Xr,X[,v]), y, d[-v], tau=0.5, 
                                kn[-v], degree[-v], lambda=lambda, gam)
      lambdasicr50=lambda50all$lambdasic
      ranges=lambda50all$range
      qvcsicr50=qrvcp_grl(times, subj, y, X=cbind(Xr,X[,v]), tau=0.5, kn[-v], degree[-v],
                          lambda=lambdasicr50, d[-v],ranges)
      hat_btr = qvcsicr50$hat_bt
      
      yhatsic_k = matrix(NA, dim, px-1)
      for (k in 1:(px-1)) {
        yhatsic_k[, k] = hat_btr[seq((k - 1) * dim + 1, k * dim)] * Xr[, k]
      }
      medr=rowSums(yhatsic_k)+ hat_btr[((px-1)*dim)+1]*X[,v]
      
      resr=y-medr
    }  
    
    if (test[1] == "m"){
      lambda50all = lambdak_gmonl(times, subj, X, y, d, tau=0.5, kn, degree, 
                                  lambda=lambda, gam,omega,mv=v)
      lambdasicr50=lambda50all$lambdasic
      ranges=lambda50all$range
      qvcsicr50=qrvcp_gmonl(times, subj, y, X, tau=0.5, kn, degree, lambda=lambdasicr50, d,omega,
                            range=ranges,mv=v)
      hat_btr = qvcsicr50$hat_bt
      
      yhatsic_k = matrix(NA, dim, px)
      for (k in 1:(px)) {
        yhatsic_k[, k] = hat_btr[seq((k - 1) * dim + 1, k * dim)] * X[, k]
      }
      medr=rowSums(yhatsic_k)
      
      resr=y-medr
    } 
    
    if (test[1] == "conv"){                          
      lambda50all = lambdak_gconl(times, subj, X, y, d, tau=0.5, kn, degree, lambda=lambda,
                                  gam,omega,v)
      lambdasicr50=lambda50all$lambdasic
      ranges=lambda50all$range
      qvcsicr50=qrvcp_gconl(times, subj, y, X, tau=0.5, kn, degree, lambda=lambdasicr50, d,
                            omega,range=ranges,v)
      hat_btr = qvcsicr50$hat_bt
      
      yhatsic_k = matrix(NA, dim, px)
      for (k in 1:(px)) {
        yhatsic_k[, k] = hat_btr[seq((k - 1) * dim + 1, k * dim)] * X[, k]
      }
      medr=rowSums(yhatsic_k)
      
      resr=y-medr
    } 
    
    ###### step2   
    log_abs_res=log(abs(y-medf)+1e-05)
    lambda50all = lambdak_gl(times, subj, X, y=log_abs_res, d, tau=0.5, kn, degree, 
                             lambda=lambda,gam)
    lambdasicr50=lambda50all$lambdasic
    ranges=lambda50all$range
    qvcsicr50=qrvcp_gl(times, subj, y=log_abs_res, X, tau=0.5, kn, degree, 
                       lambda=lambdasicr50, d,ranges)
    hat_alpha = qvcsicr50$alpha  
    hat_bt = qvcsicr50$hat_bt ## the estimated coefficients for the standardized covariates
    
    yhatsic_k = matrix(NA, dim, px)
    for (k in 1:px) {
      yhatsic_k[, k] = hat_bt[seq((k - 1) * dim + 1, k * dim)] * X[, k]
    }
    lvarf=rowSums(yhatsic_k) 
    resf_v=log_abs_res-lvarf
    varf = exp(lvarf)
    
    if (test[2] == "c"){ 
      ## the model under H_0
      Xr=X[,-v]
      lambda50all = lambdak_grl(times, subj, X=cbind(Xr,X[,v]), y=log_abs_res, d[-v], tau=0.5, 
                                kn[-v], degree[-v], lambda=lambda, gam)
      lambdasicr50=lambda50all$lambdasic
      ranges=lambda50all$range
      qvcsicr50=qrvcp_grl(times, subj, y=log_abs_res, X=cbind(Xr,X[,v]), tau=0.5, kn[-v], 
                          degree[-v],lambda=lambdasicr50, d[-v],ranges)
      hat_btr = qvcsicr50$hat_bt
      
      yhatsic_k = matrix(NA, dim, px-1)
      for (k in 1:(px-1)) {
        yhatsic_k[, k] = hat_btr[seq((k - 1) * dim + 1, k * dim)] * Xr[, k]
      }
      lvarr=rowSums(yhatsic_k)+ hat_btr[((px-1)*dim)+1]*X[,v] 
      resr_v=log_abs_res-lvarr
      varr = exp(lvarr)
    }
    
    if (test[2] == "m"){
      lambda50all = lambdak_gmonl(times, subj, X, log_abs_res, d, tau=0.5, kn, degree, 
                                  lambda=lambda, gam,omega,mv=v)
      lambdasicr50=lambda50all$lambdasic
      ranges=lambda50all$range
      qvcsicr50=qrvcp_gmonl(times, subj, log_abs_res, X, tau=0.5, kn, degree, lambda=lambdasicr50, d,omega,
                            range=ranges,mv=v)
      hat_btr = qvcsicr50$hat_bt
      
      yhatsic_k = matrix(NA, dim, px)
      for (k in 1:(px)) {
        yhatsic_k[, k] = hat_btr[seq((k - 1) * dim + 1, k * dim)] * X[, k]
      }
      lvarr=rowSums(yhatsic_k) 
      resr_v=log_abs_res-lvarr
      varr = exp(lvarr)
    }
    
    if (test[2] == "conv"){
      lambda50all = lambdak_gconl(times, subj, X, y=log_abs_res, d, tau=0.5, kn, degree, 
                                  lambda=lambda, gam,omega,v)
      lambdasicr50=lambda50all$lambdasic
      ranges=lambda50all$range
      qvcsicr50=qrvcp_gconl(times, subj, y=log_abs_res, X, tau=0.5, kn, degree, 
                            lambda=lambdasicr50, d,	omega,range=ranges,v)
      hat_btr = qvcsicr50$hat_bt
      
      yhatsic_k = matrix(NA, dim, px)
      for (k in 1:(px)) {
        yhatsic_k[, k] = hat_btr[seq((k - 1) * dim + 1, k * dim)] * X[, k]
      }
      lvarr=rowSums(yhatsic_k) 
      resr_v=log_abs_res-lvarr
      varr = exp(lvarr)
    }
    
    GR=2*(sum(W*(resr*(0.5-1*(resr<0))-resf*(0.5-1*(resf<0))))+
            sum(W*(resr_v*(0.5-1*(resr_v<0))-resf_v*(0.5-1*(resf_v<0)))))#
    
    Py0=medr+varr*(y-medf)/varf #View(y)
    
  }
  
  #####################################################################################
  ##### The bootstrap Statistics
  #####################################################################################
  data1=data.frame(subj,times,Py0,X,W)
  
  ## Bootstrap
  data1_b=NULL
  Dim_b=NULL
  sample.size= n = length(unique(subj))
  
  for (b in 1:nr.bootstrap.samples){
    set.seed(seed+b)
    sel=sample(sample.size,sample.size,replace=TRUE)
    subj1=unique(subj)
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
  
  Gb=rep(0,nr.bootstrap.samples)
  L1b=rep(0,nr.bootstrap.samples)
  L2b=rep(0,nr.bootstrap.samples)
  Lmb=rep(0,nr.bootstrap.samples)
  dminb=rep(0,nr.bootstrap.samples)
  
  vi=0 
  
  for (b in 1:nr.bootstrap.samples){
    dim = Dim_b[b]        
    a=c((vi+1):(vi+dim))
    subj=data1_b[a,1]
    times = data1_b[a,2]
    y = data1_b[a,3]
    X=data1_b[a,5:(4+px)]
    X=as.matrix(X)
    W=data1_b[a,4]
    
    
    ########## The full model
    lambda50all = lambdak_gl(times, subj, X, y, d, tau=0.5, kn, degree, lambda=lambda,gam)
    lambdasicr50=lambda50all$lambdasic
    ranges=lambda50all$range
    qvcsicr50=qrvcp_gl(times, subj, y, X, tau=0.5, kn, degree, lambda=lambdasicr50, d,ranges)
    hat_alpha = qvcsicr50$alpha  
    hat_bt = qvcsicr50$hat_bt ## the estimated coefficients of the median for
    # the standardized covariates
    
    yhatsic_k = matrix(NA, dim, px)
    for (k in 1:px) {
      yhatsic_k[, k] = hat_bt[seq((k - 1) * dim + 1, k * dim)] * X[, k]
    }
    medf=rowSums(yhatsic_k) 
    resf=y-medf
    
    ###################################################################################
    #### Only constancy of a functional coefficient in median
    ###################################################################################   
    if ( test[1] == "c" & test[2] %in% NA) { 
      ## L
      dd=diff(diag(m[v]), diff = 1) 
      
      ### calculate the norm tests
      L1b[b]=sum(abs(dd%*%hat_alpha[cum_mA[v]:cum_mB[v]]))
      L2b[b]=sqrt(sum((dd%*%hat_alpha[cum_mA[v]:cum_mB[v]])^2))
      Lmb[b]=max(abs(dd%*%hat_alpha[cum_mA[v]:cum_mB[v]]))
      
      ## the model under H_0
      Xr=X[,-v]
      lambda50all = lambdak_grl(times, subj, X=cbind(Xr,X[,v]), y, d[-v], tau=0.5, 
                                kn[-v], degree[-v], lambda=lambda, gam)
      lambdasicr50=lambda50all$lambdasic
      ranges=lambda50all$range
      qvcsicr50=qrvcp_grl(times, subj, y, X=cbind(Xr,X[,v]), tau=0.5, kn[-v], degree[-v],
                          lambda=lambdasicr50, d[-v],ranges)
      hat_btr = qvcsicr50$hat_bt
      
      yhatsic_k = matrix(NA, dim, px-1)
      for (k in 1:(px-1)) {
        yhatsic_k[, k] = hat_btr[seq((k - 1) * dim + 1, k * dim)] * Xr[, k]
      }
      medr=rowSums(yhatsic_k)+ hat_btr[((px-1)*dim)+1]*X[,v]
      
      resr=y-medr
      
      ### calculate LRT
      Gb[b]=2*(sum(W*(resr*(0.5-1*(resr<0))-resf*(0.5-1*(resf<0)))))
      
    } else if (test[1] == "m" & test[2] %in% NA) {
      ###################################################################################
      #### Only monotoncity of a functional coefficient in median
      ################################################################################### 
      ## dmin
      dd=diff(diag(m[v]), diff = 1) 
      
      dminb[b]=min(dd%*%hat_alpha[cum_mA[v]:cum_mB[v]])
      
      lambda50all = lambdak_gmonl(times, subj, X, y, d, tau=0.5, kn, degree, 
                                  lambda=lambda, gam,omega,mv=v)
      lambdasicr50=lambda50all$lambdasic
      ranges=lambda50all$range
      qvcsicr50=qrvcp_gmonl(times, subj, y, X, tau=0.5, kn, degree, lambda=lambdasicr50, d,omega,
                            range=ranges,mv=v)
      hat_btr = qvcsicr50$hat_bt
      
      yhatsic_k = matrix(NA, dim, px)
      for (k in 1:(px)) {
        yhatsic_k[, k] = hat_btr[seq((k - 1) * dim + 1, k * dim)] * X[, k]
      }
      medr=rowSums(yhatsic_k)
      
      resr=y-medr
      Gb[b]=2*(sum(W*(resr*(0.5-1*(resr<0))-resf*(0.5-1*(resf<0)))))
      
    } else if (test[1] == "conv" & test[2] %in% NA) {
      ###################################################################################
      #### Only convexity of a functional coefficient in median
      ################################################################################### 
      ## L
      dd=diff(diag(m[v]), diff = 2)
      
      dminb[b]=min(dd%*%hat_alpha[cum_mA[v]:cum_mB[v]])
      
      lambda50all = lambdak_gconl(times, subj, X, y, d, tau=0.5, kn, degree, lambda=lambda,
                                  gam,omega,v)
      lambdasicr50=lambda50all$lambdasic
      ranges=lambda50all$range
      qvcsicr50=qrvcp_gconl(times, subj, y, X, tau=0.5, kn, degree, lambda=lambdasicr50, d,
                            omega,range=ranges,v)
      hat_btr = qvcsicr50$hat_bt
      
      yhatsic_k = matrix(NA, dim, px)
      for (k in 1:(px)) {
        yhatsic_k[, k] = hat_btr[seq((k - 1) * dim + 1, k * dim)] * X[, k]
      }
      medr=rowSums(yhatsic_k)
      
      resr=y-medr
      
      Gb[b]=2*(sum(W*(resr*(0.5-1*(resr<0))-resf*(0.5-1*(resf<0)))))
      
    } else if (test[1] %in% NA & test[2] == "c") {
      ###################################################################################
      #### Only constancy of a functional coefficient in variability
      ################################################################################### 
      ########### Step2
      log_abs_res=log(abs(y-medf)+1e-05)
      lambda50all = lambdak_gl(times, subj, X, y=log_abs_res, d, tau=0.5, kn, degree, 
                               lambda=lambda,gam)
      lambdasicr50=lambda50all$lambdasic
      ranges=lambda50all$range
      qvcsicr50=qrvcp_gl(times, subj, y=log_abs_res, X, tau=0.5, kn, degree, 
                         lambda=lambdasicr50, d,ranges)
      hat_alpha = qvcsicr50$alpha  
      hat_bt = qvcsicr50$hat_bt ## the estimated coefficients for the standardized covariates
      
      yhatsic_k = matrix(NA, dim, px)
      for (k in 1:px) {
        yhatsic_k[, k] = hat_bt[seq((k - 1) * dim + 1, k * dim)] * X[, k]
      }
      lvarf=rowSums(yhatsic_k) 
      resf=log_abs_res-lvarf
      varf = exp(lvarf)
      
      ## L
      dd=diff(diag(m[v]), diff = 1) 
      
      ### calculate the norm tests
      L1b[b]=sum(abs(dd%*%hat_alpha[cum_mA[v]:cum_mB[v]]))
      L2b[b]=sqrt(sum((dd%*%hat_alpha[cum_mA[v]:cum_mB[v]])^2))
      Lmb[b]=max(abs(dd%*%hat_alpha[cum_mA[v]:cum_mB[v]]))
      
      
      ## the model under H_0
      Xr=X[,-v]
      lambda50all = lambdak_grl(times, subj, X=cbind(Xr,X[,v]), y=log_abs_res, d[-v], tau=0.5, 
                                kn[-v], degree[-v], lambda=lambda, gam)
      lambdasicr50=lambda50all$lambdasic
      ranges=lambda50all$range
      qvcsicr50=qrvcp_grl(times, subj, y=log_abs_res, X=cbind(Xr,X[,v]), tau=0.5, kn[-v], 
                          degree[-v],lambda=lambdasicr50, d[-v],ranges)
      hat_btr = qvcsicr50$hat_bt
      
      yhatsic_k = matrix(NA, dim, px-1)
      for (k in 1:(px-1)) {
        yhatsic_k[, k] = hat_btr[seq((k - 1) * dim + 1, k * dim)] * Xr[, k]
      }
      lvarr=rowSums(yhatsic_k)+ hat_btr[((px-1)*dim)+1]*X[,v] 
      resr=log_abs_res-lvarr
      varr = exp(lvarr)
      
      ### calculate LRT
      Gb[b]=2*(sum(W*(resr*(0.5-1*(resr<0))-resf*(0.5-1*(resf<0)))))
      
    } else if (test[1] %in% NA & test[2] == "m") {
      ###################################################################################
      #### Only monotoncity of a functional coefficient in variability
      ################################################################################### 
      log_abs_res=log(abs(y-medf)+1e-05)
      lambda50all = lambdak_gl(times, subj, X, y=log_abs_res, d, tau=0.5, kn, degree, 
                               lambda=lambda,gam)
      lambdasicr50=lambda50all$lambdasic
      ranges=lambda50all$range
      qvcsicr50=qrvcp_gl(times, subj, y=log_abs_res, X, tau=0.5, kn, degree, 
                         lambda=lambdasicr50, d,ranges)
      hat_alpha = qvcsicr50$alpha  
      hat_bt = qvcsicr50$hat_bt ## the estimated coefficients for the standardized covariates
      
      yhatsic_k = matrix(NA, dim, px)
      for (k in 1:px) {
        yhatsic_k[, k] = hat_bt[seq((k - 1) * dim + 1, k * dim)] * X[, k]
      }
      lvarf=rowSums(yhatsic_k) 
      resf=log_abs_res-lvarf
      varf = exp(lvarf)
      
      ## dmin
      dd=diff(diag(m[v]), diff = 1) 
      
      dminb[b]=min(dd%*%hat_alpha[cum_mA[v]:cum_mB[v]])
      
      lambda50all = lambdak_gmonl(times, subj, X, log_abs_res, d, tau=0.5, kn, degree, 
                                  lambda=lambda, gam,omega,mv=v)
      lambdasicr50=lambda50all$lambdasic
      ranges=lambda50all$range
      qvcsicr50=qrvcp_gmonl(times, subj, log_abs_res, X, tau=0.5, kn, degree, lambda=lambdasicr50, d,omega,
                            range=ranges,mv=v)
      hat_btr = qvcsicr50$hat_bt
      
      yhatsic_k = matrix(NA, dim, px)
      for (k in 1:(px)) {
        yhatsic_k[, k] = hat_btr[seq((k - 1) * dim + 1, k * dim)] * X[, k]
      }
      lvarr=rowSums(yhatsic_k) 
      resr=log_abs_res-lvarr
      varr = exp(lvarr)
      
      ### calculate LRT
      Gb[b]=2*(sum(W*(resr*(0.5-1*(resr<0))-resf*(0.5-1*(resf<0)))))
      
    } else if (test[1] %in% NA & test[2] == "conv") {
      ###################################################################################
      #### Only convexity of a functional coefficient in variabilty
      ################################################################################### 
      log_abs_res=log(abs(y-medf)+1e-05)
      lambda50all = lambdak_gl(times, subj, X, y=log_abs_res, d, tau=0.5, kn, degree, 
                               lambda=lambda,gam)
      lambdasicr50=lambda50all$lambdasic
      ranges=lambda50all$range
      qvcsicr50=qrvcp_gl(times, subj, y=log_abs_res, X, tau=0.5, kn, degree, 
                         lambda=lambdasicr50, d,ranges)
      hat_alpha = qvcsicr50$alpha  
      hat_bt = qvcsicr50$hat_bt ## the estimated coefficients for the standardized covariates
      
      yhatsic_k = matrix(NA, dim, px)
      for (k in 1:px) {
        yhatsic_k[, k] = hat_bt[seq((k - 1) * dim + 1, k * dim)] * X[, k]
      }
      lvarf=rowSums(yhatsic_k) 
      resf=log_abs_res-lvarf
      varf = exp(lvarf)
      
      ## L
      dd=diff(diag(m[v]), diff = 2)
      
      dminb[b]=min(dd%*%hat_alpha[cum_mA[v]:cum_mB[v]])
      
      lambda50all = lambdak_gconl(times, subj, X, y=log_abs_res, d, tau=0.5, kn, degree, 
                                  lambda=lambda, gam,omega,v)
      lambdasicr50=lambda50all$lambdasic
      ranges=lambda50all$range
      qvcsicr50=qrvcp_gconl(times, subj, y=log_abs_res, X, tau=0.5, kn, degree, 
                            lambda=lambdasicr50, d,	omega,range=ranges,v)
      hat_btr = qvcsicr50$hat_bt
      
      yhatsic_k = matrix(NA, dim, px)
      for (k in 1:(px)) {
        yhatsic_k[, k] = hat_btr[seq((k - 1) * dim + 1, k * dim)] * X[, k]
      }
      lvarr=rowSums(yhatsic_k) 
      resr=log_abs_res-lvarr
      varr = exp(lvarr)
      
      ### calculate LRT
      Gb[b]=2*(sum(W*(resr*(0.5-1*(resr<0))-resf*(0.5-1*(resf<0)))))
      
    } else {
      ###################################################################################
      #### simultaneous test of a functional coefficient in both median and variabilty
      ################################################################################### 
      ##### step1
      if (test[1] == "c"){
        ## the model under H_0
        Xr=X[,-v]
        lambda50all = lambdak_grl(times, subj, X=cbind(Xr,X[,v]), y, d[-v], tau=0.5, 
                                  kn[-v], degree[-v], lambda=lambda, gam)
        lambdasicr50=lambda50all$lambdasic
        ranges=lambda50all$range
        qvcsicr50=qrvcp_grl(times, subj, y, X=cbind(Xr,X[,v]), tau=0.5, kn[-v], degree[-v],
                            lambda=lambdasicr50, d[-v],ranges)
        hat_btr = qvcsicr50$hat_bt
        
        yhatsic_k = matrix(NA, dim, px-1)
        for (k in 1:(px-1)) {
          yhatsic_k[, k] = hat_btr[seq((k - 1) * dim + 1, k * dim)] * Xr[, k]
        }
        medr=rowSums(yhatsic_k)+ hat_btr[((px-1)*dim)+1]*X[,v]
        
        resr=y-medr
      }  
      
      if (test[1] == "m"){
        lambda50all = lambdak_gmonl(times, subj, X, y, d, tau=0.5, kn, degree, 
                                    lambda=lambda, gam,omega,mv=v)
        lambdasicr50=lambda50all$lambdasic
        ranges=lambda50all$range
        qvcsicr50=qrvcp_gmonl(times, subj, y, X, tau=0.5, kn, degree, lambda=lambdasicr50, d,omega,
                              range=ranges,mv=v)
        hat_btr = qvcsicr50$hat_bt
        
        yhatsic_k = matrix(NA, dim, px)
        for (k in 1:(px)) {
          yhatsic_k[, k] = hat_btr[seq((k - 1) * dim + 1, k * dim)] * X[, k]
        }
        medr=rowSums(yhatsic_k)
        
        resr=y-medr
      } 
      
      if (test[1] == "conv"){                          
        lambda50all = lambdak_gconl(times, subj, X, y, d, tau=0.5, kn, degree, lambda=lambda,
                                    gam,omega,v)
        lambdasicr50=lambda50all$lambdasic
        ranges=lambda50all$range
        qvcsicr50=qrvcp_gconl(times, subj, y, X, tau=0.5, kn, degree, lambda=lambdasicr50, d,
                              omega,range=ranges,v)
        hat_btr = qvcsicr50$hat_bt
        
        yhatsic_k = matrix(NA, dim, px)
        for (k in 1:(px)) {
          yhatsic_k[, k] = hat_btr[seq((k - 1) * dim + 1, k * dim)] * X[, k]
        }
        medr=rowSums(yhatsic_k)
        
        resr=y-medr
      } 
      
      ###### step2   
      log_abs_res=log(abs(y-medf)+1e-05)
      lambda50all = lambdak_gl(times, subj, X, y=log_abs_res, d, tau=0.5, kn, degree, 
                               lambda=lambda,gam)
      lambdasicr50=lambda50all$lambdasic
      ranges=lambda50all$range
      qvcsicr50=qrvcp_gl(times, subj, y=log_abs_res, X, tau=0.5, kn, degree, 
                         lambda=lambdasicr50, d,ranges)
      hat_alpha = qvcsicr50$alpha  
      hat_bt = qvcsicr50$hat_bt ## the estimated coefficients for the standardized covariates
      
      yhatsic_k = matrix(NA, dim, px)
      for (k in 1:px) {
        yhatsic_k[, k] = hat_bt[seq((k - 1) * dim + 1, k * dim)] * X[, k]
      }
      lvarf=rowSums(yhatsic_k) 
      resf_v=log_abs_res-lvarf
      varf = exp(lvarf)
      
      if (test[2] == "c"){ 
        ## the model under H_0
        Xr=X[,-v]
        lambda50all = lambdak_grl(times, subj, X=cbind(Xr,X[,v]), y=log_abs_res, d[-v], tau=0.5, 
                                  kn[-v], degree[-v], lambda=lambda, gam)
        lambdasicr50=lambda50all$lambdasic
        ranges=lambda50all$range
        qvcsicr50=qrvcp_grl(times, subj, y=log_abs_res, X=cbind(Xr,X[,v]), tau=0.5, kn[-v], 
                            degree[-v],lambda=lambdasicr50, d[-v],ranges)
        hat_btr = qvcsicr50$hat_bt
        
        yhatsic_k = matrix(NA, dim, px-1)
        for (k in 1:(px-1)) {
          yhatsic_k[, k] = hat_btr[seq((k - 1) * dim + 1, k * dim)] * Xr[, k]
        }
        lvarr=rowSums(yhatsic_k)+ hat_btr[((px-1)*dim)+1]*X[,v] 
        resr_v=log_abs_res-lvarr
        varr = exp(lvarr)
      }
      
      if (test[2] == "m"){
        lambda50all = lambdak_gmonl(times, subj, X, log_abs_res, d, tau=0.5, kn, degree, 
                                    lambda=lambda, gam,omega,mv=v)
        lambdasicr50=lambda50all$lambdasic
        ranges=lambda50all$range
        qvcsicr50=qrvcp_gmonl(times, subj, log_abs_res, X, tau=0.5, kn, degree, lambda=lambdasicr50, d,omega,
                              range=ranges,mv=v)
        hat_btr = qvcsicr50$hat_bt
        
        yhatsic_k = matrix(NA, dim, px)
        for (k in 1:(px)) {
          yhatsic_k[, k] = hat_btr[seq((k - 1) * dim + 1, k * dim)] * X[, k]
        }
        lvarr=rowSums(yhatsic_k) 
        resr_v=log_abs_res-lvarr
        varr = exp(lvarr)
      }
      
      if (test[2] == "conv"){
        lambda50all = lambdak_gconl(times, subj, X, y=log_abs_res, d, tau=0.5, kn, degree, 
                                    lambda=lambda, gam,omega,v)
        lambdasicr50=lambda50all$lambdasic
        ranges=lambda50all$range
        qvcsicr50=qrvcp_gconl(times, subj, y=log_abs_res, X, tau=0.5, kn, degree, 
                              lambda=lambdasicr50, d,	omega,range=ranges,v)
        hat_btr = qvcsicr50$hat_bt
        
        yhatsic_k = matrix(NA, dim, px)
        for (k in 1:(px)) {
          yhatsic_k[, k] = hat_btr[seq((k - 1) * dim + 1, k * dim)] * X[, k]
        }
        lvarr=rowSums(yhatsic_k) 
        resr_v=log_abs_res-lvarr
        varr = exp(lvarr)
      }
      
      Gb[b]=2*(sum(W*(resr*(0.5-1*(resr<0))-resf*(0.5-1*(resf<0))))+
                 sum(W*(resr_v*(0.5-1*(resr_v<0))-resf_v*(0.5-1*(resf_v<0)))))
      
    } 
    
    vi=sum(Dim_b[1:b])
  }
  
  
  #####################################################################################
  ##### The P-values
  #####################################################################################
  
  if ( (test[1] == "c" & test[2] %in% NA) | (test[1] %in% NA & test[2] == "c") ) {    
    Gp=sum(1*(Gb>=GR))/nr.bootstrap.samples  
    
    L1p=sum(1*(L1b>=L1R))/nr.bootstrap.samples  ## View(L2b) 
    L2p=sum(1*(L2b>=L2R))/nr.bootstrap.samples 
    Lmp=sum(1*(Lmb>=LmR))/nr.bootstrap.samples 
    
    out = list(result=paste(c("LRT","L1","L2","Lmax")),P = c(Gp,L1p,L2p,Lmp), 
               R = c(GR,L1R,L2R,LmR), B=cbind(Gb,L1b,L2b,Lmb))
    
  } else if ( (test[1] == "m" & test[2] %in% NA) | (test[1] %in% NA & test[2] == "m") |
              (test[1] == "conv" & test[2] %in% NA) | (test[1] %in% NA & test[2] == "conv") ) {
    if (dminR>=0) {
      dminp=1
    } else {
      dminp=sum(1*(dminb<=dminR))/nr.bootstrap.samples  
    }
    Gp=sum(1*(Gb>=GR))/nr.bootstrap.samples 
    
    out = list(result=paste(c("LRT","dmin")),P = c(Gp,dminp), 
               R = c(GR,dminR), B=cbind(Gb,dminb))
    
  } else {
    Gp=sum(1*(Gb>=GR))/nr.bootstrap.samples 
    
    out = list(result=paste("LRT"),P = Gp, 
               R = GR, B=Gb)
    
  }
  
  
  return(out)
}