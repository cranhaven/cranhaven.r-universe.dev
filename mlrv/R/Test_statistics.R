#' @useDynLib mlrv,.registration = TRUE
#' @import Rcpp RcppArmadillo mathjaxr stats doParallel foreach magrittr numDeriv xtable
#' @section mlrv functions:
#' Heter_LRV, heter_covariate, heter_gradient, gcv_cov, MV_critical

#' @title Long memory tests for non-stationary time series regression
#' @description  \loadmathjax Test for long memory of \mjseqn{e_i} in the time series regression \deqn{y_i = x_i \beta_i + e_i, 1\le i \le n} where \mjseqn{x_i} is the multivariate covariate process with first component 1, \mjseqn{\beta_i} is the functional coefficient, \mjseqn{e_i} is the error term which can be long memory. In particular,covariates and the error term are allowed to be dependent.
#' @param data a list with the vector y and the matrix x, for example, list(x=...,y=...).
#' @param param a list of parameters, list(B =..., lrvmethod =...,gcv = ..., neighbour =..., lb = ..., ub = ..., tau_n = ..., type = ..., ind = ...)
#' @param mvselect the value of moving window parameter \mjseqn{m}. In addition, mvselect=-1 provides data-driven smoothing parameters via Minimum Volatility of the long-run covariance estimator as proposed in Chapter 9 of Politis et al.(1999), while mvselect = -2 provides data-driven smoothing parameters via Minimum Volatility of the bootstrap statistics, see Bai and Wu (2023a).
#' @param bw the bandwidth parameter in the local linear regression, default 0.2.
#' @param shift modify bw by a factor, default 1.
#' @param verbose_dist whether to print intermediate results, i.e., the bootstrap distribution and statistics, default FALSE.
#' @param hyper whether to only print the selected values of the smoothing parameters,\mjseqn{m} and \mjseqn{\tau_n}, default FALSE.
#' @details param
#'* B, the number of bootstrap simulation, say 2000
#'*lrvmethod,  the method of long-run variance estimation, lrvmethod = 0 uses the plug-in estimator in Zhou (2010), lrvmethod = 1 offers the debias difference-based estimator in Bai and Wu (2023b), lrvmethod = 2 provides the plug-in estimator using the \mjseqn{\breve{\beta}}, the pilot estimator proposed in Bai and Wu (2023b)
#'* gcv,  1 or 0, whether to use Generalized Cross Validation for the selection of \mjseqn{b}, the bandwidth parameter in the local linear regression
#'* neighbour, the number of neighbours in the extended minimum volatility, for example 1,2 or 3
#'* lb, the lower bound of the range of \mjseqn{m} in the extended minimum volatility Selection
#'* ub,  the upper bound of the range of \mjseqn{m} in the extended minimum volatility Selection
#'* bw_set, the proposed grid of the range of bandwidth selection. if not presented, a rule of thumb method will be used for the data-driven range
#'* tau_n,  the value of \mjseqn{\tau} when no data-driven selection is used. if \mjseqn{tau} is set to \mjseqn{0}, the rule of thumb \mjseqn{n^{-1/5}} will be used
#'* type, c( "KPSS","RS","VS","KS") type of tests, see  Bai and Wu (2023a).
#'* ind,  types of kernels
#'* 1 Triangular \mjseqn{1-|u|}, \mjseqn{u \le 1}
#'* 2 Epanechnikov kernel \mjseqn{3/4(1 - u^{2})}, \mjseqn{u \le 1}
#'* 3 Quartic \mjseqn{15/16(1 - u^{2})^{2}}, \mjseqn{u \le 1}
#'* 4 Triweight \mjseqn{35/32(1 - u^{2})^{3}}, \mjseqn{u \le 1}
#'* 5 Tricube  \mjseqn{70/81(1 - |u|^{3})^{3}}, \mjseqn{u \le 1}
#' @return p-value of the long memory test
#' @examples
#' param = list(d = -0.2, heter = 2, tvd = 0,
#'  tw = 0.8, rate = 0.1, cur = 1,
#'  center = 0.3, ma_rate =  0, cov_tw =  0.2,
#'  cov_rate = 0.1, cov_center = 0.1, all_tw  = 1, cov_trend = 0.7)
#' data = Qct_reg(1000, param)
#' ### KPSS test B
#' heter_covariate(data, list(B=20, lrvmethod = 1,
#' gcv = 1, neighbour = 1, lb = 3, ub = 11, type = "KPSS"), mvselect = -2, verbose_dist = TRUE)
#' @references
#' Bai, L., and Wu, W. (2023a). Detecting long-range dependence for time-varying linear models. To appear in Bernoulli
#'
#' Bai, L., and Wu, W. (2023b). Difference-based covariance matrix estimate in time series nonparametric regression with applications to specification tests.
#'
#' Zhou, Z. and Wu, W. B. (2010). Simultaneous inference of linear models with time varying coefficients.J. R. Stat. Soc. Ser. B. Stat. Methodol., 72(4):513–531.
#'
#' Politis, D. N., Romano, J. P., and Wolf, M. (1999). Subsampling. Springer Science & Business Media.
#'
#' @export
heter_covariate<- function(data, param=list(B=2000, lrvmethod = 1, gcv = 1, neighbour = 1, lb = 3, ub = 11, tau_n = 0.3, type = "KPSS"), mvselect = -1, bw= 0.2, shift = 1,
                           verbose_dist = FALSE, hyper = FALSE){
  if("B" %in% names(param)){
    B = param$B
  }else{
    B = 2000
  }
  if("lrvmethod" %in% names(param)){
    lrvmethod = param$lrvmethod
  }else{
    lrvmethod = 1
  }
  if("gcv" %in% names(param)){
    gcv = param$gcv
  }else{
    gcv = 1
  }
  if("neighbour" %in% names(param)){
    neighbour = param$neighbour
  }else{
    neighbour = 1
  }

  if("tau_n" %in% names(param)){
    tau_n = param$tau_n
  }else{
    tau_n = 0.3
  }

  if("type" %in% names(param)){
    type = param$type
  }else{
    type = "KPSS"
  }

  if("ind" %in% names(param)){
    ind = param$ind
  }else{
    ind = 2
  }
  if("res" %in% names(param)){
    rescale = param$res
  }else{
    rescale = 0
  }


  if(!is.list(data)) data = list(y = data, x = as.matrix(rep(1,length(data))))
  y = data$y
  n = length(y)
  p = ncol(data$x)
  sigma = array(rep(0, n * p * p),dim = c(p , p, n))

  if("lb" %in% names(param)){
    lb = param$lb
  }else{
    lb = max(floor(3/7*n^(4/15)) - neighbour,1)
  }
  if("ub" %in% names(param)){
    ub = param$ub
  }else{
    ub = max(floor(11/7*n^(4/15)) + neighbour,
             lb + neighbour+1)
  }
  # if(p>1) data$x[,-1] = foreach(i=2:p,.combine="cbind") %do% tv_center(0.2, n,data$x[, i])

  gridtau = seq(n^(-2/15)*12/15, n^(-2/15)*15/15, by = 0.05)  #RATE CHANGE
  m = 7             ##defalt
  e = rep(0, n)
  t = (1 : n) / n
  #print("initialization")
  ####################gcv##################
  if(gcv){
    f <- function(bw) gcv_cov(bw, t, y, data$x, verbose = 0)
    # time = seq(0.08, 0.25, by= 0.01)
    # plot(time,lapply(time, f),type="l")
    # eblow point is at 0.09 approximately
    if("bw_set" %in% names(param)){
      bw_set = param$bw_set
    }else{
      bw_set = rule_of_thumb(y, data$x)
    }
    result = optimize(f, bw_set)
    bw = result$minimum*shift
  }
  ###############  jackknife for the middle #############
  result1 = LocLinear(bw, t, data$y,data$x)
  result2 = LocLinear(bw/sqrt(2), t, data$y,data$x)
  e = y - (2*result2$mu - result1$mu)
  ##################  MV method #########################
  if(mvselect == -1) {#TO DO
    # traditional method
    gridm = floor(max(0,lb)) : floor(min(n,ub))
    if(lrvmethod == 1){
      lrv_cub = MV_cov_heter(y, data$x, gridm, gridtau, 1, ind = ind)
    }else if (lrvmethod == 0){
      lrv_cub = MV_cov_heter(e, data$x, gridm, gridtau, 0, ind = ind)
    }else if (lrvmethod == 2){
      lrv_cub = MV_cov_heter(y, data$x, gridm, gridtau, 2, ind = ind)
    }else if (lrvmethod == 3){
      lrv_cub = MV_cov_heter(y, data$x, gridm, gridtau, 3, ind = ind)
    }

    mv_result = MV_ise_heter(lrv_cub, p, n, neighbour)
    m = gridm[mv_result$minp + 1]
    tau_n = gridtau[mv_result$minq + 1]
  }else if(mvselect == -2){
    gridm = floor(max(0,lb)) : floor(min(n,ub))
    critical = matrix(nrow = length(gridm), ncol = length(gridtau))

    B_c = 100
    R_c = array(rnorm(n*p*B_c),dim = c(p,B_c,n))

    if(lrvmethod == 0) y0 = e else y0 = y

    critical = MV_critical(y0, result1, R_c, gridm, gridtau,
                           type = which(type == c("KPSS","RS","VS","KS")),
                           cvalue = 0.05, B = B_c, lrvmethod = lrvmethod, ind = ind, rescale = rescale)
    mv_result = MV_ise_heter_critical(critical,  neighbour)
    m = gridm[mv_result$minp + 1]
    tau_n = gridtau[mv_result$minq + 1]

  }else if(mvselect > 0){
    m = mvselect
    tau_n = tau_n
  }

  if(hyper == TRUE){
    # only returning hyperparameter
    return(c(m, tau_n))
  }else{
    # we have finised choosing m and tau_n
    ################  Long run variance  #########################
    if(lrvmethod == 0){
      #plug in
      sigma = Heter_LRV(e, data$x, m, tau_n, lrv_method = 0, ind = ind, rescale = rescale)
    }else if(lrvmethod == 1){
      sigma = Heter_LRV(y, data$x, m, tau_n, lrv_method = 1, ind = ind, rescale = rescale)
      #1 difference & correction
    }else if(lrvmethod == 2){
      #2 plug in second order beta
      sigma = Heter_LRV(y, data$x, m, tau_n, lrv_method = 2, ind = ind, rescale = rescale)
    }else if(lrvmethod == 3){
      #2 plug in second order beta
      sigma = Heter_LRV(y, data$x, m, tau_n, lrv_method = 3, ind = ind, rescale = rescale)
    }

    ################  Bootstrap B times ####################
    R_j = array(rnorm(n*p*B),dim = c(p,B,n))

    if(type == "KPSS"){
      STAT_dist   = sim_Phi_heter(result1, B, sigma, R_j)
    }else if (type == "VS"){
      # print("VS")
      STAT_dist   = sim_Phi_heter_VS(result1, B, sigma, R_j)
    }else if(type == "RS"){
      STAT_dist = sim_Phi_heter_RS(result1, B, sigma, R_j)
    }else if(type == "KS"){
      STAT_dist = sim_Phi_heter_KS(result1, B, sigma, R_j)
    }

    STAT_dist = as.vector(STAT_dist)
    ##################  Test statistic   ###################
    # whether to include boundary points
    sc = 0
    sc = n-2*floor(n*bw) #sample cut
    s <- cumsum(e[(floor(n*bw) + 1):(n-floor(n*bw))])
    if(type == "KPSS"){
      eta <- sum(s^2)/(n*sc)
    }else if (type == "VS"){
      eta <- (sum(s^2) - (sum(s))^2/sc)/(n*sc)
    }else if(type == "RS"){
      eta <- max(s) - min(s)
    }else if(type == "KS"){
      eta <- max(abs(s))
    }
    # print(sc)

    if(verbose_dist){
      print(paste("gcv",bw))
      print(paste("m",m,"tau_n",tau_n))
      print(paste("test statistic:", eta))
      #print(summary(KPSS_dist_new))
      print("Bootstrap distribution")
      print(summary(STAT_dist))
    }
    pvalue = sum(STAT_dist >= eta)/B
    return(pvalue)
  }
}


#' @title rule of thumb interval for the selection of smoothing parameter b
#' @description The function will compute a data-driven interval for the Generalized Cross Validation performed later, see also Bai and Wu (2023) . \loadmathjax
#' @param y a vector, the response variable.
#' @param x a matrix of covariates. If the intercept should be includes, the elements of the first column should be 1.
#' @return c(left, right), the vector with the left and right points of the interval
#' @examples
#' param = list(d = -0.2, heter = 2, tvd = 0,
#' tw = 0.8, rate = 0.1, cur = 1, center = 0.3,
#'  ma_rate =  0, cov_tw =  0.2, cov_rate = 0.1,
#'  cov_center = 0.1, all_tw  = 1, cov_trend = 0.7)
#' data = Qct_reg(1000, param)
#' rule_of_thumb(data$y, data$x)
#' @references
#'
#' Bai, L., and Wu, W. (2023). Detecting long-range dependence for time-varying linear models. To appear in Bernoulli
#' @export
  rule_of_thumb<-function(y, x){
    # esimation of lrv: lrv_method =1 similar to rb_fast
    p = ncol(x)
    n = nrow(x)
    t = (1:n)/n

    m = floor(n^(4/15))
    tau_n= n^(-2/15)
    bw = n^(-1/5)

    result1 = LocLinear(bw, t, y, x, deriv2 = 1)
    result2 = LocLinear(bw/sqrt(2), t, y, x)
    i = NULL

    sigma = Diff1(y, x, m, tau_n)
    if(p > 1){
      Ajmc = DiffX(x, m , tau_n^(3.0/2))
      Ajmb = DiffA(y, x, m, tau_n^(3.0/2))
      beta  = foreach(i=1:n, .combine = 'rbind') %do% solve(Ajmc[,,i],Ajmb[,,i])
      xb = foreach(i=1:n,.combine='c') %do% (t(x[i,])%*%beta[i,])
      Ajmhat = Diff1(xb, x, m, tau_n)
      lrv = sigma - Ajmhat
    }else{
      lrv = sigma
    }
    lrv[lrv<0] = 0 # thresholding
    trace_int = apply(lrv, c(1,2), mean)%>%diag()%>%sum()

    b2_int = result1$b2
    phi0 = 3/5  # Epanechnikov
    mu2 = 1/5   # Epanechnikov

    bopt.lb = (phi0*trace_int/(mu2^2*b2_int))^(1/5)*n^(-1/4)
    bopt.ub = (phi0*trace_int/(mu2^2*b2_int))^(1/5)*n^(-1/6)
    if(bopt.ub > 0.3){
      bopt.ub = 2/3 * n^(-1/6)
      bopt.lb = 1/2 * n^(-1/4)
    }

    return(c(bopt.lb, bopt.ub))
}








#' @title Structural stability tests for non-stationary time series regression
#' @description  \loadmathjax Test for long memory of \mjseqn{e_i} in the time series regression \mjsdeqn{y_i = x_i \beta_i + e_i, 1\leq i \leq n} where \mjseqn{x_i} is the multivariate covariate process with first component 1, \mjseqn{\beta_i} is the coefficient, \mjseqn{e_i} is the error term which can be long memory. The goal is to test whether the null hypothesis  \mjsdeqn{\beta_1 = \ldots = \beta_n = \beta} holds. The alternative hypothesis is that the coefficient function \mjseqn{\beta_i} is time-varying.  Covariates and the error term are allowed to be dependent.
#' @param data a list with the vector y (response) and the matrix x (covariates), for example, list(x=...,y=...).
#' @param param a list of parameters, list(B =..., lrvmethod =...,gcv = ..., neighbour =..., lb = ..., ub = ..., tau_n = ..., type = ..., ind = ...)
#' @param mvselect the value of moving window parameter \mjseqn{m}. In addition, mvselect=-1 provides data-driven smoothing parameters via Minimum Volatility of the long-run covariance estimator, while mvselect = -2 provides data-driven smoothing parameters via Minimum Volatility of the bootstrap statistics.
#' @param verbose_dist whether to print intermediate results, i.e., the bootstrap distribution and statistics, default FALSE.
#' @param hyper whether to only print the selected values of the smoothing parameters,\mjseqn{m} and \mjseqn{\tau_n}, default FALSE.
#' @details param
#'* B, the number of bootstrap simulation, say 2000
#'* lrvmethod  the method of long-run variance estimation, lrvmethod = -1 uses the ols plug-in estimator as in Wu and Zhou (2018), lrvmethod = 0 uses the plug-in estimator in Zhou (2010), lrvmethod = 1 offers the debias difference-based estimator in Bai and Wu (2023), lrvmethod = 2 provides the plug-in estimator using the \mjseqn{\breve{\beta}}, the pilot estimator proposed in Bai and Wu (2023)
#'* gcv,  1 or 0, whether to use Generalized Cross Validation for the selection of \mjseqn{b}, the bandwidth parameter in the local linear regression, which will not be used when lrvmethod is -1, 1 or 2.
#'* neighbour, the number of neighbours in the extended minimum volatility, for example 1,2 or 3
#'* lb, the lower bound of the range of \mjseqn{m} in the extended minimum volatility Selection
#'* ub,  the upper bound of the range of \mjseqn{m} in the extended minimum volatility Selection
#'* bw_set, the proposed grid of the range of bandwidth selection, which is only useful when lrvmethod = 1. if not presented, a rule of thumb method will be used for the data-driven range.
#'* tau_n,  the value of \mjseqn{\tau} when no data-driven selection is used. if \mjseqn{tau} is set to \mjseqn{0}, the rule of thumb \mjseqn{n^{-1/5}} will be used
#'* type, default 0, uses the residual-based statistic proposed in Wu and Zhou (2018). ``type'' can also be set to -1, using the coefficient-based statistic in Wu and Zhou (2018).
#'* ind,  types of kernels
#'* 1 Triangular \mjseqn{1-|u|}, \mjseqn{u \le 1}
#'* 2 Epanechnikov kernel \mjseqn{3/4(1 - u^{2})}, \mjseqn{u \le 1}
#'* 3 Quartic \mjseqn{15/16(1 - u^{2})^{2}}, \mjseqn{u \le 1}
#'* 4 Triweight \mjseqn{35/32(1 - u^{2})^{3}}, \mjseqn{u \le 1}
#'* 5 Tricube  \mjseqn{70/81(1 - |u|^{3})^{3}}, \mjseqn{u \le 1}
#' @return p-value of the structural stability test
#' @examples
#' # choose a small B for tests
#' param = list(B = 50, bw_set = c(0.15, 0.25), gcv =1, neighbour = 1, lb = 10, ub = 20, type = 0)
#' n = 300
#' data = bregress2(n, 2, 1) # time series regression model with 2 changes points
#' param$lrvmethod = 0 # plug-in
#' heter_gradient(data, param, 4, 1)
#' param$lrvmethod = 1 # difference based
#' heter_gradient(data, param, 4, 1)
#' @references
#' Bai, L., and Wu, W. (2023). Difference-based covariance matrix estimate in time series nonparametric regression with applications to specification tests.
#'
#' Wu, W., and Zhou, Z. (2018). Gradient-based structural change detection for nonstationary time series M-estimation. The Annals of Statistics, 46(3), 1197-1224.
#'
#' Politis, D. N., Romano, J. P., and Wolf, M. (1999). Subsampling. Springer Science & Business Media.
#' @export
heter_gradient <- function(data, param, mvselect = -1, verbose_dist = FALSE, hyper = FALSE){
  B = param$B
  lrvmethod = param$lrvmethod
  neighbour = param$neighbour
  lb = param$lb
  ub = param$ub
  tau_n = param$tau_n
  type = param$type
  gcv = param$gcv
  type = param$type
  if("ind" %in% names(param)){
    ind = param$ind
  }else{
    ind = 2
  }
  if("res" %in% names(param)){
    rescale = param$res
  }else{
    rescale = 0
  }
  #type 0 res type 1 coef

  if(!is.list(data)) data = list(y = data, x = as.matrix(rep(1,length(data))))
  y = data$y
  p = ncol(data$x)
  n = length(y)
  t = (1:n)/n


  ##### Test Statistics ######
  reg <- lm(y~x-1, data = data)
  # residual based
  if(type == 0){
    STAT_res = sweep(data$x, 1, reg$residuals, `*`)/sqrt(n)
    # print(summary(STAT_res))
    STAT_res = STAT_res %>% apply(2,cumsum) %>% apply(1, function(x){sqrt(sum(x^2))}) %>% max()
  }else{
    # coefficient based
    aa = 0.3
    an=round(aa*n)
    beta1=matrix(0,nrow=p ,ncol = n-an+1)
    for (i in an:n){
      beta1[,(i-an+1)]= coef(lm(data$y[1:i]~data$x[1:i,]-1))
    }
    beta1 = beta1-beta1[,(n-an+1)]
    STAT_coef = sqrt(n) *  apply(beta1, 2, function(x){sqrt(sum(x^2))}) %>% max()
  }



  ##### LRV estimation ########

  ##################  MV method #########################
  if(lrvmethod %in% c(-1, 1, 2)){
    gridtau = seq(n^(-2/15)*12/15, n^(-2/15)*15/15, by = 0.05)
  }else{
    gridtau = seq(n^(-1/7)*4/7, n^(-1/7)*6/7, by = 0.05)

    ##### compute nonparametric residuals e
    if(gcv){
      f <- function(bw) gcv_cov(bw, t, y, data$x, verbose = 0)
      if("bw_set" %in% names(param)){
        bw_set = param$bw_set
      }else{
        bw_set = rule_of_thumb(y, data$x)
      }
      result = optimize(f, bw_set)
      bw = result$minimum
    }else{bw = 0.2}
    result1 = LocLinear(bw, t, data$y,data$x)
    result2 = LocLinear(bw/sqrt(2), t, data$y,data$x)
    e = y - (2*result2$mu - result1$mu)

  }
  #RATE CHANGE

  if(mvselect == -1){# TO DO
    # traditional method
    gridm = floor(max(0,lb)) : floor(min(n,ub))
    if(lrvmethod == 1){
      lrv_cub = MV_cov_heter(y, data$x, gridm, gridtau, 1, ind)
    }else if (lrvmethod == 0){
      lrv_cub = MV_cov_heter(e, data$x, gridm, gridtau, 0, ind)
    }else if (lrvmethod == -1){
      lrv_cub = MV_cov_heter(reg$residuals, data$x, gridm, gridtau, 0, ind)
    }else if (lrvmethod == -2){
      lrv_cub = MV_cov_heter(data$e, data$x, gridm, gridtau, 0, ind)
    }
    mv_result = MV_ise_heter(lrv_cub, p, n, neighbour)
    m = gridm[mv_result$minp + 1]
    tau_n = gridtau[mv_result$minq + 1]
  }else if(mvselect == -2){
    gridm = floor(max(0,lb)) : floor(min(n,ub))
    critical = matrix(nrow = length(gridm), ncol = length(gridtau))

    B_c = 100

    if(lrvmethod == 1){
      critical = MV_critical_cp(y, data$x,t,  gridm, gridtau,
                                cvalue = 0.05, B = B_c, lrvmethod = 1, ind, rescale = rescale)
    }else if(lrvmethod == 2){
      critical = MV_critical_cp(y, data$x, t,  gridm, gridtau,
                                cvalue = 0.05, B = B_c, lrvmethod = 2, ind, rescale = rescale)
    }else if (lrvmethod == 0){
      critical = MV_critical_cp(e, data$x,t,  gridm, gridtau,
                                cvalue = 0.05, B = B_c, lrvmethod = 0, ind, rescale = rescale)
    }else if (lrvmethod == -1){
      critical = MV_critical_cp(reg$residuals, data$x, t,  gridm, gridtau,
                                cvalue = 0.05, B = B_c, lrvmethod = 0, ind, rescale = rescale)
    }else if (lrvmethod == -2){
      critical = MV_critical_cp(data$e, data$x, t,  gridm, gridtau,
                                cvalue = 0.05, B = B_c, lrvmethod = 0, ind, rescale = rescale)
    }
    mv_result = MV_ise_heter_critical(critical,  neighbour)
    m = gridm[mv_result$minp + 1]
    tau_n = gridtau[mv_result$minq + 1]
  }else if(mvselect > 0){
    m = mvselect
    tau_n = max(tau_n,0)
  }else{
    warning("invalid m and taun!")
  }


  if(hyper == TRUE){
    # only returning hyperparameter
    return(c(m, tau_n))
  }else{
    if(lrvmethod == 0){
      #0 plug in  nonparametric
      sigma = Heter_LRV(e, data$x, m, tau_n,  0, ind, 0,   rescale)
    }else if(lrvmethod == 1){   #1 difference & correction
      sigma = Heter_LRV(y, data$x, m, tau_n,  1, ind, 0,  rescale)
    }else if (lrvmethod == -1){  # plug in ols
      sigma = Heter_LRV(reg$residuals - mean(reg$residuals), data$x,m, tau_n,  0, ind, 0,  rescale)
    }else if(lrvmethod == -2){  # plug in true error
      sigma = Heter_LRV(data$e, data$x,m, tau_n,   0, ind, 0, rescale)
    }else if(lrvmethod == 2){
      #2 plug in second order beta
      sigma = Heter_LRV(y, data$x, m, tau_n, lrv_method = 2, ind = ind, rescale = rescale)
    }else{
      warning("lrvmethod: choose -2, -1, 0 or 1 2")
    }

    ######## Bootstrap  ########
    # sup_t|U(t) - M(t)M^{-1}(1)U(1)| type = res
    # sup_t|M^{-1}(t)U(t) - M^{-1}(1)U(1)| type = coef
    STAT_dist = sim_T(data$x, t, sigma, m, B, type)

    if(verbose_dist){
      if(lrvmethod <= 0)
        print(paste("gcv", bw))
      print(paste("m", m, "tau_n", tau_n))
      if(type == 0){
        print(STAT_res)
        print(summary((STAT_dist)))
      }else{
        print(STAT_coef)
        print(summary((STAT_dist)))
      }

    }
    if(type == 0)
      pvalue = sum(STAT_dist >= STAT_res)/B
    else
      pvalue = sum(STAT_dist >= STAT_coef)/B

    return(pvalue)
  }
}


#' @title Comparing bias or mse of lrv estimators based on numerical methods
#' @param data a list of data
#' @param param a list of parameters
#' @param lrvmethod int, method of long-run variance estimation
#' @param mvselect int, method of MV selection
#' @param tau double, value of tau. If tau is 0, a rule-of-thunk value will be applied
#' @param verbose_dist bool, whether to output distributional information
#' @param mode default "mse", It can be set as "bias".
#' @return empirical MSE of the estimator.
#' @examples
#' n = 300
#' param = list(gcv = 1, neighbour = 1,lb = 6, ub = 13, ind = 2)    # covariates heterskadecity
#' data = bregress2(n, 2, 1) # with 2 change pointa
#' lrv_measure(data, param, lrvmethod = -1, mvselect = -2) #ols plug-in
#' #debiased difference-based
#' lrv_measure(data, param, lrvmethod = 1, mvselect = -2)
#' @export
lrv_measure <- function(data, param, lrvmethod, mvselect = -1, tau = 0, verbose_dist = FALSE, mode = "mse"){
  B = param$B
  neighbour = param$neighbour
  lb = param$lb
  ub = param$ub
  tau_n = param$tau_n
  type = param$type
  gcv = param$gcv
  eta = param$eta
  if("ind" %in% names(param)){
    ind = param$ind
  }else{
    ind = 2
  }

  #type 0 res type 1 coef

  if(!is.list(data)) data = list(y = data, x = as.matrix(rep(1,length(data))))
  y = data$y
  p = ncol(data$x)
  n = length(y)
  t = (1:n)/n
  reg <- lm(y~x-1, data = data)


  ##### LRV estimation ########

  ##################  MV method #########################
  ####ground truth#####
  gridtau = seq(n^(-1/7)*4/7, n^(-1/7)*6/7, by = 0.05)
  gridm = floor(max(0,lb)) : floor(min(n,ub))
  lrv_cub = MV_cov_heter(data$e, data$x, gridm, gridtau, 0, ind)
  mv_result = MV_ise_heter(lrv_cub, p, n, neighbour)
  m = gridm[mv_result$minp + 1]
  tau_n = gridtau[mv_result$minq + 1]
  sigmatruth = Heter_LRV(data$e, data$x,m, tau_n, lrv_method = 0, ind = ind)
  ############
  mse <-c(0,0,0,0,0,0)

  #  for(lrvmethod in c(-1, 0, 1)){
  if(lrvmethod == 1){
    gridtau = seq(n^(-2/15)*9/15, n^(-2/15)*13/15, by = 0.05)
  }else{
    gridtau = seq(n^(-1/7)*4/7, n^(-1/7)*6/7, by = 0.05)

    ##### compute nonparametric residuals e
    if(gcv){
      f <- function(bw) gcv_cov(bw, t, y, data$x, verbose = 0)
      if("bw_set" %in% names(param)){
        bw_set = param$bw_set
      }else{
        bw_set = rule_of_thumb(y, data$x)
      }
      result = optimize(f, bw_set)
      bw = result$minimum
    }else{bw = 0.2}
    result1 = LocLinear(bw, t, data$y,data$x)
    result2 = LocLinear(bw/sqrt(2), t, data$y,data$x)
    e = y - (2*result2$mu - result1$mu)

  }
  #RATE CHANGE

  if(mvselect == -1){#TO DO
    # traditional method
    gridm = floor(max(0,lb)) : floor(min(n,ub))
    if(lrvmethod == 1){
      lrv_cub = MV_cov_heter(y, data$x, gridm, gridtau, 1, ind = ind)
    }else if (lrvmethod == 0){
      lrv_cub = MV_cov_heter(e, data$x, gridm, gridtau, 0, ind = ind)
    }else if (lrvmethod == -1){
      lrv_cub = MV_cov_heter(reg$residuals, data$x, gridm, gridtau, 0, ind = ind)
    }else if (lrvmethod == -2){
      lrv_cub = MV_cov_heter(data$e, data$x, gridm, gridtau, 0, ind = ind)
    }
    mv_result = MV_ise_heter(lrv_cub, p, n, neighbour)
    m = gridm[mv_result$minp + 1]
    tau_n = gridtau[mv_result$minq + 1]

  }else if(mvselect == -2){
    gridm = floor(max(0,lb)) : floor(min(n,ub))
    critical = matrix(nrow = length(gridm), ncol = length(gridtau))

    B_c = 100

    if(lrvmethod == 1){
      critical = MV_critical_cp(y, data$x,t,  gridm, gridtau,
                                cvalue = 0.05, B = B_c, lrvmethod = 1, ind)
    }else if(lrvmethod == 2){
      critical = MV_critical_cp(y, data$x, t,  gridm, gridtau,
                                cvalue = 0.05, B = B_c, lrvmethod = 2, ind)
    }else if (lrvmethod == 0){
      critical = MV_critical_cp(e, data$x,t,  gridm, gridtau,
                                cvalue = 0.05, B = B_c, lrvmethod = 0, ind)
    }else if (lrvmethod == -1){
      critical = MV_critical_cp(reg$residuals, data$x, t,  gridm, gridtau,
                                cvalue = 0.05, B = B_c, lrvmethod = 0, ind)
    }else if (lrvmethod == -2){
      critical = MV_critical_cp(data$e, data$x, t,  gridm, gridtau,
                                cvalue = 0.05, B = B_c, lrvmethod = 0, ind)
    }
    mv_result = MV_ise_heter_critical(critical,  neighbour)
    m = gridm[mv_result$minp + 1]
    tau_n = gridtau[mv_result$minq + 1]
  }else if(mvselect > 0){
    m = mvselect
    tau_n = tau
  }else{
    warning("invalid m and taun!")
  }


  if(lrvmethod == 0){
    #0 plug in  nonparametric
    sigma = Heter_LRV(e, data$x, m, tau_n, lrv_method = 0, ind  = ind)
  }else if(lrvmethod == 1){   #1 difference & correction
    sigma = Heter_LRV(y, data$x, m, tau_n, lrv_method = 1, ind = ind)
  }else if (lrvmethod == -1){  # plug in ols
    sigma = Heter_LRV(reg$residuals - mean(reg$residuals), data$x,m, tau_n, lrv_method = 0, ind = ind)
  }else{
    warning("lrvmethod: choose  -1, 0 or 1")
  }
  # mse[1] = (sum((sigma-sigmatruth)^2))# mse
  # mse[2] = sum((sigma-sigmatruth))  # bias  integrated
  # }
  if(mode == "mse"){
    return(sum((sigma-sigmatruth)^2))
  }else{
    return(sum((sigma-sigmatruth)))
  }
}


#' @name mlrv
#' mlrv: A package for long-run covariance matrix for multivariate time series
#' The mlrv package provides four categories of important functions:  estimators for long-run covariance function, tests for non-stationary time series (structural stability and long memory detection), functions for the selection of smoothing parameters (Generalized Cross validation and Minimum Volatility), simulated non-staitonary time series regression
