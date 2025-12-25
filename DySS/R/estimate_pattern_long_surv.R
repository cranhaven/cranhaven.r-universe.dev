##' Estimate the Pattern of Longitudinal and Survival Data
##'
##' @title Estimate the Pattern of Longitudinal and Survival Data
##' @description Function \code{estimate_pattern_long_surv} estimate the pattern of longitudinal and survival 
##' data from a dataset of n subjects. This is usually the first step of dynamic screening.
##' The risk of a subject to event is quantified by a linear combination of longitudinal data by a Cox model.
##' The risk pattern can be described by mean and variance depending on the estimation method. 
##' When the estimated pattern is used for monitoring new subjects, the collected data from new subjects are 
##' compared to the estimated pattern for monitoring abnormality.
##' @param data_array observed data arranged in a 3d array format. \cr
##' \code{data_array[i,j,k]} is the jth observation of the kth dimension of the ith subject.
##' @param time_matrix observation times arranged in a numeric matrix format. \cr
##' \code{time_matrix[i,j]} is the jth observation time of the ith subject. \cr
##' \code{data_array[i,j,]} is observed at \code{time_matrix[i,j]}.
##' @param nobs number of observations arranged as an integer vector. \cr
##' \code{nobs[i]} is the number of observations for the ith subject.
##' @param starttime a vector of entry times \cr
##' \code{starttime[i]} is the entry time of the ith subject.
##' @param survtime a vector of survival times \cr
##' \code{survtime[i]} is the survival time of the ith subject.
##' @param survevent a logical vector of survival events \cr
##' If \code{survevents[i]==TRUE}, then a survival event is observed at \code{survtime[i]}. \cr
##' If \code{survevents[i]==FALSE}, then no survival event is observed at \code{survtime[i]}.
##' @param design_interval a numeric vector of length two that 
##' gives the left- and right- limits of the design interval. 
##' By default, \code{design_interval=range(time_matrix,na.rm=TRUE)}.
##' @param n_time_units an integer value that gives the number of basic time units
##' in the design time interval. \cr
##' The design interval will be discretized to \code{seq(design_interval[1],design_interval[2],length.out=n_time_units)}
##' @param time_unit an optional numeric value of basic time unit. Only used when \code{n_time_units} is missing.\cr
##' The design interval will be discretized to \code{seq(design_interval[1],design_interval[2],by=time_unit)}
##' @param estimation_method a string. \cr
##' If \code{estimation_method="risk"}, apply the risk monitoring method (c.f., You and Qiu 2020). \cr
##' (Currently only the method "risk" is available.)
##' @param smoothing_method a string. \cr
##' If \code{smoothing_method="local constant"}, apply local constant smoothing \cr
##' If \code{smoothing_method="local linear"}, apply local linear smoothing \cr
##' @param bw_beta an integer value. \cr
##' The bandwidth parameter for estimating the regression coefficients beta in the Cox model.
##' @param bw_mean an integer value. \cr
##' The bandwidth parameter for estimating mean function.
##' @param bw_var an integer value. \cr
##' The bandwidth parameter for estimating variance function.
##' @return an object that stores the estimated longitudinal pattern and model parameters. \cr
##' If \code{estimation_method="risk"}, returns an object of class \code{pattern_long_surv_risk}. \cr
##' \item{$grid}{discretized design interval.}
##' \item{$beta_est}{Estimated regression coefficients.}
##' \item{$mean_risk_est}{Estimated mean function.}
##' \item{$var_risk_est}{Estimated variance function.}
##' @references 
##' You, L. and Qiu, P. (2020). An effective method for online disease risk monitoring. Technometrics, 62(2):249-264.
##' @export
##' @examples 
##' data("data_example_long_surv")
##' 
##' result_pattern<-estimate_pattern_long_surv(
##'   data_array=data_example_long_surv$data_array_IC,
##'   time_matrix=data_example_long_surv$time_matrix_IC,
##'   nobs=data_example_long_surv$nobs_IC,
##'   starttime=data_example_long_surv$starttime_IC,
##'   survtime=data_example_long_surv$survtime_IC,
##'   survevent=data_example_long_surv$survevent_IC,
##'   design_interval=data_example_long_surv$design_interval,
##'   n_time_units=data_example_long_surv$n_time_units,
##'   estimation_method="risk",
##'   smoothing_method="local linear",
##'   bw_beta=0.05,
##'   bw_mean=0.1,
##'   bw_var=0.1)
estimate_pattern_long_surv<-function(
  data_array,time_matrix,nobs,
  starttime,survtime,survevent,
  design_interval,n_time_units,time_unit,
  estimation_method="risk",smoothing_method="local linear",
  bw_beta,bw_mean,bw_var)
{
  if(any(dim(data_array)[1:2]!=dim(time_matrix)))stop("Dimensions of 'data_array' and 'time_matrix' don't match.")
  if(dim(data_array)[1]!=length(nobs))stop("Dimensions of 'data_array' and 'nobs' don't match.")
  
  nind<-dim(data_array)[1]
  nmaxobs<-dim(data_array)[2]
  ndim<-dim(data_array)[3]
  
  if(length(starttime)!=nind)stop("Lengths of 'starttime' and 'nobs' don't match.")
  if(length(survtime)!=nind)stop("Lengths of 'survtime' and 'nobs' don't match.")
  if(length(survevent)!=nind)stop("Lengths of 'survevent' and 'nobs' don't match.")
  
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
  starttime_int<-pmin(pmax(round((starttime-ttmin)/time_unit)+1,1),n_time_units)
  survtime_int<-pmin(pmax(round((survtime-ttmin)/time_unit)+1,1),n_time_units)
  
  if(!missing(bw_beta))bw_beta_int<-ceiling(bw_beta/time_unit)
  if(!missing(bw_mean))bw_mean_int<-ceiling(bw_mean/time_unit)
  if(!missing(bw_var))bw_var_int<-ceiling(bw_var/time_unit)
  
  if(is.numeric(survevent))survevent<-ifelse(survevent==1,TRUE,FALSE)
  if(!is.logical(survevent))stop("Values of 'survevent' should be logical or 0/1.")
  
  sumt<-0
  sumn<-0
  for(ii in 1:nind){
    sumt<-sumt+(time_matrix_int[ii,nobs[ii]]-time_matrix_int[ii,1])
    sumn<-sumn+(nobs[ii]-1)
  }
  delta_bar<-sumt/sumn
  
  if(estimation_method=="risk"){
    if(missing(bw_beta_int)|missing(bw_mean_int)|missing(bw_var_int))
      stop("Method 'risk' requires arguments 'bw_beta_int', 'bw_mean_int', 'bw_var_int'.")
  }else{
    stop("Error in the argument 'estimation_method'.")
  }
  
  if(estimation_method=="risk"){
    yfij<-matrix(NA,nind,ndim)
    for(ii in 1:nind){
      tt<-survtime_int[ii]
      idx<-which(time_matrix_int[ii,]<=tt)
      if(length(idx)>0){
        yfij[ii,]<-data_array[ii,length(idx),]
      }else{
        yfij[ii,]<-data_array[ii,1,]
      }
    }
    
    beta_est<-risk_estimate_beta(
      data_array,time_matrix_int,nobs,
      starttime_int,survtime_int,survevent,
      yfij,bw_beta_int)
    rrij<-matrix(NA,nind,nmaxobs)
    for(ii in 1:nind){
      rrij[ii,1:nobs[ii]]<-data_array[ii,1:nobs[ii],]%*%beta_est
    }
    if(smoothing_method=="local linear"){
      mean_risk_est<-local_linear_mean_est_faster(rrij,time_matrix_int,nobs,1:n_time_units,bw_mean_int)
    }else if(smoothing_method=="local constant"){
      mean_risk_est<-local_const_mean_est_faster(rrij,time_matrix_int,nobs,1:n_time_units,bw_mean_int)
    }
    
    epsij<-matrix(NA,nind,nmaxobs)
    eps2ij<-matrix(NA,nind,nmaxobs)
    for(ii in 1:nind){
      epsij[ii,1:nobs[ii]]<-rrij[ii,1:nobs[ii]]-mean_risk_est[time_matrix_int[ii,1:nobs[ii]]]
      eps2ij[ii,1:nobs[ii]]<-epsij[ii,1:nobs[ii]]^2
    }
    if(smoothing_method=="local linear"){
      var_risk_est<-local_linear_mean_est_faster(eps2ij,time_matrix_int,nobs,1:n_time_units,bw_var_int)
    }else if(smoothing_method=="local constant"){
      var_risk_est<-local_const_mean_est_faster(eps2ij,time_matrix_int,nobs,1:n_time_units,bw_var_int)
    }
    
    pattern<-list(
      grid=ttmin+time_unit*(0:(n_time_units-1)),
      beta_est=beta_est,
      mean_risk_est=mean_risk_est,
      var_risk_est=var_risk_est,
      data_array=data_array,
      time_matrix_int=time_matrix_int,
      starttime_int=starttime_int,
      survtime_int=survtime_int,
      survevent=survevent,
      nobs=nobs,
      delta_bar=delta_bar,
      design_interval=design_interval,
      ttmin=ttmin,
      ttmax=ttmax,
      time_unit=time_unit,
      n_time_units=n_time_units,
      smoothing_method=smoothing_method,
      estimation_method=estimation_method,
      bw_beta_int=bw_beta_int,
      bw_mean_int=bw_mean_int,
      bw_var_int=bw_var_int)
    class(pattern)<-c(class(pattern),"pattern_long_surv_risk")
    return(pattern)
  }
}

