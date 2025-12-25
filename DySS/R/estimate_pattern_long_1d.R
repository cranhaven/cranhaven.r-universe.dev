##' Estimate the Regular Longitudinal Pattern of Univariate Data
##'
##' @title Estimate the Regular Longitudinal Pattern of Univariate Data
##' @description Function \code{estimate_pattern_long_1d} estimate the regular longitudinal pattern 
##' of a univariate variable from a dataset of n subjects. This is usually the first step of dynamic screening.
##' The pattern can be described by mean, variance, covariance, and distribution depending on the estimation method. 
##' When the estimated pattern is used for monitoring new subjects, the collected data from new subjects are 
##' compared to the estimated pattern for monitoring abnormality.
##' @param data_matrix observed data arranged in a numeric matrix format. \cr
##' \code{data_matrix[i,j]} is the jth observation of the kth dimension of the ith subject.
##' @param time_matrix observation times arranged in a numeric matrix format. \cr
##' \code{time_matrix[i,j]} is the jth observation time of the ith subject. \cr
##' \code{data_matrix[i,j]} is observed at \code{time_matrix[i,j]}.
##' @param nobs number of observations arranged as an integer vector. \cr
##' \code{nobs[i]} is the number of observations for the ith subject.
##' @param design_interval a numeric vector of length two that 
##' gives the left- and right- limits of the design interval. 
##' By default, \code{design_interval=range(time_matrix,na.rm=TRUE)}.
##' @param n_time_units an integer value that gives the number of basic time units
##' in the design time interval. \cr
##' The design interval will be discretized to \cr
##' \code{seq(design_interval[1],design_interval[2],length.out=n_time_units)}
##' @param time_unit an optional numeric value of basic time unit. Only used when \code{n_time_units} is missing.\cr
##' The design interval will be discretized to \cr
##' \code{seq(design_interval[1],design_interval[2],by=time_unit)}
##' @param estimation_method a character specifying the estimation method. \cr
##' If \code{estimation_method="meanvar"}, the function will estimate 
##' the mean and variance functions using local smoothing
##' (c.f., Qiu and Xiang, 2014). 
##' Parameters \code{bw_mean} and \code{bw_var} are required. \cr
##' If \code{estimation_method="meanvarcov"}, the function will estimate 
##' the mean, variance and covariance functions using local smoothing 
##' (c.f., Li and Qiu, 2016). 
##' Parameters \code{bw_mean}, \code{bw_var} and \code{bw_cov} are required. \cr
##' If \code{estimation_method="meanvarcovmean"}, the function will estimate
##' the mean, variance and covariance functions (c.f., Li and Qiu, 2016).
##' In the last step, the mean function will be updated using the
##' covariance function. 
##' Parameters \code{bw_mean}, \code{bw_var} and \code{bw_cov} are required. \cr
##' If \code{estimation_method="distribution"}, the function will estimate the 
##' distribution function (c.f., You and Qiu, 2020).
##' Parameters \code{bw_t} and \code{bw_y} are required. \cr
##' If \code{estimation_method="distributionvarcov"}, the function will estimate the 
##' distribution function and the covariance function of standardized values
##' (c.f., You and Qiu 2020).
##' Parameters \code{bw_cov}, \code{bw_t} and \code{bw_y} are required.
##' @param smoothing_method a character value specifying the smoothing method. \cr
##' If \code{smoothing_method="local constant"}, apply local constant approximation. \cr
##' If \code{smoothing_method="local linear"}, apply local linear approximation.
##' @param bw_mean a numeric value. \cr
##' The bandwidth parameter for estimating mean function.
##' @param bw_var a numeric value. \cr
##' The bandwidth parameter for estimating variance function.
##' @param bw_cov a numeric value. \cr
##' The bandwidth parameter for estimating covariance function.
##' @param bw_t a numeric value. \cr
##' The bandwidth parameter in time axis for estimating distribution function.
##' @param bw_y a numeric value. \cr
##' The bandwidth parameter in y-axis for estimating distribution function.
##' @return a list that stores the estimated longitudinal pattern and model parameters. \cr
##' If \code{estimation_method="meanvar"}, returns a list of class \code{pattern_long_1d_meanvar} \cr
##' If \code{estimation_method="meanvarcov"} or \code{"meanvarcovmean"}, returns a list of class \code{pattern_long_1d_meanvarcov} \cr
##' If \code{estimation_method="distribution"}, returns a list of class \code{pattern_long_1d_distribution} \cr
##' If \code{estimation_method="distributionvarcov"}, returns a list of class \code{pattern_long_1d_distributionvarcov} \cr
##' \item{$grid}{Discretized design interval.}
##' \item{$mean_est}{Estimated mean function.}
##' \item{$var_est}{Estimated variance function.}
##' \item{$cov_est}{Estimated covariance function.}
##' @references 
##' Qiu, P. and Xiang, D. (2014). Univariate dynamic screening system: an approach for identifying individuals with irregular longitudinal behavior. Technometrics, 56:248-260. \cr
##' Li, J. and Qiu, P. (2016). Nonparametric dynamic screening system for monitoring correlated longitudinal data. IIE Transactions, 48(8):772-786. \cr
##' You, L. and Qiu, P. (2019). Fast computing for dynamic screening systems when analyzing correlated data. Journal of Statistical Computation and Simulation, 89(3):379-394. \cr
##' You, L., Qiu, A., Huang, B., and Qiu, P. (2020). Early detection of severe juvenile idiopathic arthritis by sequential monitoring of patients' health-related quality of life scores. Biometrical Journal, 62(5). \cr
##' You, L. and Qiu, P. (2021). A robust dynamic screening system by estimation of the longitudinal data distribution. Journal of Quality Technology, 53(4). 
##' @export
##' @examples 
##' data("data_example_long_1d")
##' 
##' result_pattern<-estimate_pattern_long_1d(
##'   data_matrix=data_example_long_1d$data_matrix_IC,
##'   time_matrix=data_example_long_1d$time_matrix_IC,
##'   nobs=data_example_long_1d$nobs_IC,
##'   design_interval=data_example_long_1d$design_interval,
##'   n_time_units=data_example_long_1d$n_time_units,
##'   estimation_method="meanvar",
##'   smoothing_method="local linear",
##'   bw_mean=0.1,
##'   bw_var=0.1)
estimate_pattern_long_1d<-function(
  data_matrix,time_matrix,nobs,
  design_interval,n_time_units,time_unit,
  estimation_method,smoothing_method="local linear",
  bw_mean,bw_var,bw_cov,bw_t,bw_y)
{
  if(any(dim(data_matrix)!=dim(time_matrix)))stop("Dimensions of 'data_matrix' and 'time_matrix' don't match.")
  if(dim(data_matrix)[1]!=length(nobs))stop("Dimensions of 'data_matrix' and 'nobs' don't match.")
  
  nind<-dim(data_matrix)[1]
  nmaxobs<-dim(data_matrix)[2]
  data_matrix<-clean_matij_by_nobs(data_matrix,nobs,"data_matrix")
  time_matrix<-clean_matij_by_nobs(time_matrix,nobs,"time_matrix")
  
  list_time_unit<-clean_time_unit(
    design_interval,n_time_units,time_unit,range(time_matrix,na.rm=TRUE))
  design_interval<-list_time_unit$design_interval
  time_unit<-list_time_unit$time_unit
  n_time_units<-list_time_unit$n_time_units
  ttmin<-list_time_unit$ttmin
  ttmax<-list_time_unit$ttmax
  
  time_matrix_int<-pmin(pmax(round((time_matrix-ttmin)/time_unit)+1,1),n_time_units)
  
  if(!missing(bw_mean))bw_mean_int<-ceiling(bw_mean/time_unit)
  if(!missing(bw_var))bw_var_int<-ceiling(bw_var/time_unit)
  if(!missing(bw_cov))bw_cov_int<-ceiling(bw_cov/time_unit)
  if(!missing(bw_t))bw_t_int<-ceiling(bw_t/time_unit)
  
  if(estimation_method=="meanvar"){
    if(missing(bw_mean_int)|missing(bw_var_int))
      stop("Method 'meanvar' requires arguments bw_mean_int, bw_var_int.")
  }else if(estimation_method=="meanvarcov"){
    if(missing(bw_mean_int)|missing(bw_var_int)|missing(bw_cov_int))
      stop("Method 'meanvarcov' requires arguments bw_mean_int, bw_var_int, bw_cov_int.")
  }else if(estimation_method=="meanvarcovmean"){
    if(missing(bw_mean_int)|missing(bw_var_int)|missing(bw_cov_int))
      stop("Method 'meanvarcovmean' requires arguments bw_mean_int, bw_var_int, bw_cov_int.")
  }else if(estimation_method=="distribution"){
    if(missing(bw_t_int)|missing(bw_y))
      stop("Method 'distribution' requires arguments bw_t_int, bw_y.")
  }else if(estimation_method=="disttributionvarcov"){
    if(missing(bw_t_int)|missing(bw_y)|missing(bw_cov_int))
      stop("Method 'disttributionvarcov' requires arguments bw_t_int, bw_y, bw_cov_int.")
  }else{
    stop("Error in the argument 'estimation_method'.")
  }
  
  if(!smoothing_method%in%c("local constant","local linear"))
    stop("Error in the argument 'smoothing_method'.")
  
  if(estimation_method%in%c("meanvar","meanvarcov","meanvarcovmean")){
    
    if(smoothing_method=="local linear"){
      mean_est<-local_linear_mean_est_faster(data_matrix,time_matrix_int,nobs,1:n_time_units,bw_mean_int)
    }else if(smoothing_method=="local constant"){
      mean_est<-local_const_mean_est_faster(data_matrix,time_matrix_int,nobs,1:n_time_units,bw_mean_int)
    }
    
    epsij<-matrix(NA,nind,nmaxobs)
    eps2ij<-matrix(NA,nind,nmaxobs)
    for(ii in 1:nind){
      epsij[ii,1:nobs[ii]]<-data_matrix[ii,1:nobs[ii]]-mean_est[time_matrix_int[ii,1:nobs[ii]]]
      eps2ij[ii,1:nobs[ii]]<-epsij[ii,1:nobs[ii]]^2
    }
    
    if(smoothing_method=="local linear"){
      var_est<-local_linear_mean_est_faster(eps2ij,time_matrix_int,nobs,1:n_time_units,bw_var_int)
    }else if(smoothing_method=="local constant"){
      var_est<-local_const_mean_est_faster(eps2ij,time_matrix_int,nobs,1:n_time_units,bw_var_int)
    }
    
    if(estimation_method=="meanvar"){
      pattern<-list(
        grid=ttmin+time_unit*(0:(n_time_units-1)),
        mean_est=mean_est,
        var_est=var_est,
        data_matrix=data_matrix,
        time_matrix_int=time_matrix_int,
        nobs=nobs,
        design_interval=design_interval,
        ttmin=ttmin,
        ttmax=ttmax,
        time_unit=time_unit,
        n_time_units=n_time_units,
        smoothing_method=smoothing_method,
        estimation_method=estimation_method,
        bw_mean_int=bw_mean_int,
        bw_var_int=bw_var_int)
      class(pattern)<-c(class(pattern),"pattern_long_1d_meanvar")
      return(pattern)
    }
    
    if(smoothing_method=="local linear"){
      cov_est<-local_linear_cov_est_faster(epsij,time_matrix_int,nobs,1:n_time_units,bw_cov_int)
    }else if(smoothing_method=="local constant"){
      cov_est<-local_const_cov_est_faster(epsij,time_matrix_int,nobs,1:n_time_units,bw_cov_int)
    }
    diag(cov_est)<-var_est
    
    if(estimation_method=="meanvarcov"){
      pattern<-list(
        grid=ttmin+time_unit*(0:(n_time_units-1)),
        mean_est=mean_est,
        var_est=var_est,
        cov_est=cov_est,
        data_matrix=data_matrix,
        time_matrix_int=time_matrix_int,
        nobs=nobs,
        design_interval=design_interval,
        ttmin=ttmin,
        ttmax=ttmax,
        time_unit=time_unit,
        n_time_units=n_time_units,
        smoothing_method=smoothing_method,
        estimation_method=estimation_method,
        bw_mean_int=bw_mean_int,
        bw_var_int=bw_var_int,
        bw_cov_int=bw_cov_int)
      class(pattern)<-c(class(pattern),"pattern_long_1d_meanvarcov")
      return(pattern)
    }
    
    if(smoothing_method=="local constant"){
      mean2_est<-local_const_mean_est_update_faster(data_matrix,time_matrix_int,nobs,1:n_time_units,bw_mean_int,cov_est)
      mean2_est<-c(mean2_est)
    }else if(smoothing_method=="local linear"){
      mean2_est<-local_linear_mean_est_update_faster(data_matrix,time_matrix_int,nobs,1:n_time_units,bw_mean_int,cov_est)
      mean2_est<-c(mean2_est)
    }
    
    if(estimation_method=="meanvarcovmean"){
      pattern<-list(
        grid=ttmin+time_unit*(0:(n_time_units-1)),
        mean_est=mean2_est,
        var_est=var_est,
        cov_est=cov_est,
        mean0_est=mean_est,
        data_matrix=data_matrix,
        time_matrix_int=time_matrix_int,
        nobs=nobs,
        design_interval=design_interval,
        ttmin=ttmin,
        ttmax=ttmax,
        time_unit=time_unit,
        n_time_units=n_time_units,
        smoothing_method=smoothing_method,
        estimation_method=estimation_method,
        bw_mean_int=bw_mean_int,
        bw_var_int=bw_var_int,
        bw_cov_int=bw_cov_int)
      class(pattern)<-c(class(pattern),"pattern_long_1d_meanvarcov")
      return(pattern)
    }
  }
  
  if(estimation_method%in%c("distribution","distributionvarcov")){
    yy_raw<-c()
    tt_raw<-c()
    for(ii in 1:nind){
      yy_raw<-c(yy_raw,data_matrix[ii,1:nobs[ii]])
      tt_raw<-c(tt_raw,time_matrix_int[ii,1:nobs[ii]])
    }
    tt_order<-order(tt_raw)
    tt_ref<-tt_raw[tt_order]
    yy_ref<-yy_raw[tt_order]
    rm(yy_raw)
    rm(tt_raw)
    
    starting_idx<-rep(NA,n_time_units)
    ending_idx<-rep(NA,n_time_units)
    upper_line<-rep(NA,n_time_units)
    for(tt in 1:n_time_units){
      starting_idx[tt]<-min(which(abs(tt_ref-tt)<bw_t_int))
      ending_idx[tt]<-max(which(abs(tt_ref-tt)<bw_t_int))
      upper_line[tt]<-quantile(yy_ref[starting_idx[tt]:ending_idx[tt]],probs=0.8,na.rm=TRUE)
    }
    
    zzij<-local_const_percentile_est_faster(
      data_matrix,time_matrix_int,nobs,yy_ref,tt_ref,
      starting_idx,ending_idx,upper_line,n_time_units,bw_t_int,bw_y)
    
    if(estimation_method=="distribution"){
      pattern<-list(
        grid=ttmin+time_unit*(0:(n_time_units-1)),
        yy_ref=yy_ref,
        tt_ref=tt_ref,
        starting_idx=starting_idx,
        ending_idx=ending_idx,
        upper_line=upper_line,
        data_matrix=data_matrix,
        zzij=zzij,
        time_matrix_int=time_matrix_int,
        nobs=nobs,
        design_interval=design_interval,
        ttmin=ttmin,
        ttmax=ttmax,
        time_unit=time_unit,
        n_time_units=n_time_units,
        smoothing_method=smoothing_method,
        estimation_method=estimation_method,
        bw_t_int=bw_t_int,
        bw_y=bw_y)
      class(pattern)<-c(class(pattern),"pattern_long_1d_distribution")
      return(pattern)
    }
    
    cov_est<-local_const_cov_est_faster(zzij,time_matrix_int,nobs,1:n_time_units,bw_cov_int)
    diag(cov_est)<-1.0
    
    if(estimation_method=="distributionvarcov"){
      pattern<-list(
        grid=ttmin+time_unit*(0:(n_time_units-1)),
        yy_ref=yy_ref,
        tt_ref=tt_ref,
        starting_idx=starting_idx,
        ending_idx=ending_idx,
        upper_line=upper_line,
        cov_est=cov_est,
        data_matrix=data_matrix,
        zzij=zzij,
        time_matrix_int=time_matrix_int,
        nobs=nobs,
        design_interval=design_interval,
        ttmin=ttmin,
        ttmax=ttmax,
        time_unit=time_unit,
        n_time_units=n_time_units,
        smoothing_method=smoothing_method,
        estimation_method=estimation_method,
        bw_t_int=bw_t_int,
        bw_y=bw_y,
        bw_cov_int=bw_cov_int)
      class(pattern)<-c(class(pattern),"pattern_long_1d_distributionvarcov")
      return(pattern)
    }
  }
}
