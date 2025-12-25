##' Estimate the Regular Longitudinal Pattern of Multivariate Data
##'
##' @title Estimate the Regular Longitudinal Pattern of Multivariate Data
##' @description Function \code{estimate_pattern_long_md} estimate the regular longitudinal pattern 
##' of multivariate processes from a dataset of n subjects. This is usually the first step of dynamic screening.
##' The pattern can be described by mean, variance, covariance, and distribution depending on the estimation method. 
##' When the estimated pattern is used for monitoring new subjects, the collected data from new subjects are 
##' compared to the estimated pattern for monitoring abnormality.
##' @param data_array observed data arranged in a 3d array format. \cr
##' \code{data_array[i,j,k]} is the jth observation of the kth dimension of the ith subject.
##' @param time_matrix observation times arranged in a numeric matrix format. \cr
##' \code{time_matrix[i,j]} is the jth observation time of the ith subject. \cr
##' \code{data_array[i,j,]} is observed at \code{time_matrix[i,j]}.
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
##' @param estimation_method a string. \cr
##' If \code{estimation_method="meanvar"}, the function will estimate the 
##' mean function (\eqn{\mathrm{E}[\mathbf{y}(t)]}), and 
##' variance function (\eqn{\mathrm{Var}(\mathbf{y}(t))}). 
##' Parameters \code{bw_mean_int} and \code{bw_var_int} are needed. \cr
##' If \code{estimation_method="meanvarcov"}, the function will estimate the 
##' mean function (\eqn{\mathrm{E}[\mathbf{y}(t)]}), 
##' variance function (\eqn{\mathrm{Var}(\mathbf{y}(t))}), and 
##' covariance function (\eqn{\mathrm{Cov}(\mathbf{y}(s),\mathbf{y}(t))}). 
##' Parameters \code{bw_mean_int}, \code{bw_var_int} and \code{bw_cov_int}.
##' @param bw_mean a numeric value. \cr
##' The bandwidth parameter for estimating mean function.
##' @param bw_var a numeric value. \cr
##' The bandwidth parameter for estimating variance function.
##' @param bw_cov a numeric value. \cr
##' The bandwidth parameter for estimating covariance function.
##' @return an object that stores the estimated longitudinal pattern and model parameters. \cr
##' If \code{estimation_method="meanvar"}, returns an object of class \code{pattern_long_md_meanvar}. \cr
##' If \code{estimation_method="meanvarcov"}, returns an object of class \code{pattern_long_md_meanvarcov}. \cr
##' \item{$grid}{Discretized design interval.}
##' \item{$mean_est}{Estimated mean function.}
##' \item{$var_est}{Estimated variance function.}
##' \item{$cov_est}{Estimated covariance function.}
##' @references 
##' Qiu, P. and Xiang, D. (2015). Surveillance of cardiovascular diseases using a multivariate dynamic screening system. Statistics in Medicine, 34:2204-2221. \cr
##' Li, J. and Qiu, P. (2017). Construction of an efficient multivariate dynamic screening system. Quality and Reliability Engineering International, 33(8):1969-1981. \cr
##' You, L., Qiu, A., Huang, B., and Qiu, P. (2020). Early detection of severe juvenile idiopathic arthritis by sequential monitoring of patients' health-related quality of life scores. Biometrical Journal, 62(5).
##' @export
##' @examples 
##' data("data_example_long_md")
##' 
##' result_pattern<-estimate_pattern_long_md(
##'   data_array=data_example_long_md$data_array_IC,
##'   time_matrix=data_example_long_md$time_matrix_IC,
##'   nobs=data_example_long_md$nobs_IC,
##'   design_interval=data_example_long_md$design_interval,
##'   n_time_units=data_example_long_md$n_time_units,
##'   estimation_method="meanvar",
##'   bw_mean=0.1,
##'   bw_var=0.1)
estimate_pattern_long_md<-
  function(data_array,time_matrix,nobs,
           design_interval,n_time_units,time_unit,
           estimation_method,bw_mean,bw_var,bw_cov){
    
    if(any(dim(data_array)[1:2]!=dim(time_matrix)))stop("Dimensions of 'data_array' and 'time_matrix' don't match.")
    if(dim(data_array)[1]!=length(nobs))stop("Dimensions of 'data_array' and 'nobs' don't match.")
    
    nind<-dim(data_array)[1]
    nmaxobs<-dim(data_array)[2]
    ndim<-dim(data_array)[3]
    
    time_matrix<-clean_matij_by_nobs(time_matrix,nobs,"time_matrix")
    for(kk in 1:ndim){
      data_array[,,kk]<-clean_matij_by_nobs(data_array[,,kk],nobs,"data_array")
    }
    
    list_time_unit<-clean_time_unit(
      design_interval,n_time_units,time_unit,range(time_matrix,na.rm=TRUE))
    design_interval<-list_time_unit$design_interval
    n_time_units<-list_time_unit$n_time_units
    time_unit<-list_time_unit$time_unit
    ttmin<-list_time_unit$ttmin
    ttmax<-list_time_unit$ttmax
    
    time_matrix_int<-pmin(pmax(round((time_matrix-ttmin)/time_unit)+1,1),n_time_units)
    
    if(!missing(bw_mean))bw_mean_int<-ceiling(bw_mean/time_unit)
    if(!missing(bw_var))bw_var_int<-ceiling(bw_var/time_unit)
    if(!missing(bw_cov))bw_cov_int<-ceiling(bw_cov/time_unit)
    
    if(estimation_method=="meanvar"){
      if(missing(bw_mean_int)|missing(bw_var_int))
        stop("Method 'meanvar' requires arguments bw_mean, bw_var.")
    }else if(estimation_method=="meanvarcov"){
      if(missing(bw_mean_int)|missing(bw_var_int)|missing(bw_cov_int))
        stop("Method 'meanvarcov' requires arguments bw_mean, bw_var, bw_cov.")
    }else{
      stop("Error in the argument 'estimation_method'.")
    }
    
    mean_est<-matrix(0,n_time_units,ndim)
    mean_est<-f90_local_const_mean_est_mult_wrap(
      data_array,time_matrix_int,nobs,nind,nmaxobs,ndim,n_time_units,bw_mean_int,mean_est)
    
    epsijk<-array(NA,c(nind,nmaxobs,ndim))
    for(ii in 1:nind){
      epsijk[ii,1:nobs[ii],]<-data_array[ii,1:nobs[ii],]-mean_est[time_matrix_int[ii,1:nobs[ii]],]
    }
    
    var_est<-array(0,c(n_time_units,ndim,ndim))
    var_est<-f90_local_const_var_est_mult_wrap(
      epsijk,time_matrix_int,nobs,nind,nmaxobs,ndim,n_time_units,bw_var_int,var_est)
    
    if(estimation_method=="meanvar"){
      pattern<-list(
        grid=ttmin+time_unit*(0:(n_time_units-1)),
        mean_est=mean_est,
        var_est=var_est,
        data_array=data_array,
        time_matrix_int=time_matrix_int,
        nobs=nobs,
        design_interval=design_interval,
        ttmin=ttmin,
        ttmax=ttmax,
        time_unit=time_unit,
        n_time_units=n_time_units,
        estimation_method=estimation_method)
      class(pattern)<-c(class(pattern),"pattern_long_md_meanvar")
      return(pattern)
    }
    
    cov_est<-array(0,c(n_time_units,n_time_units,ndim,ndim))
    cov_est<-f90_local_const_cov_est_mult_wrap(
      epsijk,time_matrix_int,nobs,nind,nmaxobs,ndim,n_time_units,bw_cov_int,cov_est)
    for(kk in 1:ndim){
      diag(cov_est[,,kk,kk])<-var_est[,kk,kk]
    }
    
    if(estimation_method=="meanvarcov"){
      pattern<-list(
        grid=ttmin+time_unit*(0:(n_time_units-1)),
        mean_est=mean_est,
        var_est=var_est,
        cov_est=cov_est,
        data_array=data_array,
        time_matrix_int=time_matrix_int,
        nobs=nobs,
        design_interval=design_interval,
        ttmin=ttmin,
        ttmax=ttmax,
        time_unit=time_unit,
        n_time_units=n_time_units,
        estimation_method=estimation_method)
      class(pattern)<-c(class(pattern),"pattern_long_md_meanvarcov")
      return(pattern)
    }
  }
