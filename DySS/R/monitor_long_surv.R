##' Monitor Longitudinal Data for Survival Outcomes
##'
##' @title Monitor Longitudinal Data for Survival Outcomes
##' @param data_array_new observed data arranged in a numeric array format. \cr
##' \code{data_array_new[i,j,k]} is the jth observation of the kth dimension of the ith subject.
##' @param time_matrix_new observation times arranged in a numeric matrix format. \cr
##' \code{time_matrix_new[i,j]} is the jth observation time of the ith subject. \cr
##' \code{data_array_new[i,j,]} is observed at \code{time_matrix[i,j]}.
##' @param nobs_new number of observations arranged as an integer vector. \cr
##' \code{nobs_new[i]} is the number of observations for the ith subject.
##' @param pattern the estimated longitudinal and survival pattern from \code{estimate_pattern_long_surv}.
##' @param method a character value specifying the smoothing method\cr
##' If \code{method="risk"}, apply the risk monitoring method by You and Qiu (2020).
##' @param parameter a numeric value. \cr
##' The weighting parameter in the modified EWMA charts.
##' @param CL a numeric value specifying the control limit
##' @return a list that stores the result.\cr
##' \item{$chart}{charting statistics arranged in a matrix.}
##' \item{$standardized_values}{standardized values arranged in a matrix.}
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
##' 
##' result_monitoring<-monitor_long_surv(
##'   data_array_new=data_example_long_surv$data_array_OC,
##'   time_matrix_new=data_example_long_surv$time_matrix_OC,
##'   nobs_new=data_example_long_surv$nobs_OC,
##'   pattern=result_pattern,
##'   method="risk",
##'   parameter=0.5)
monitor_long_surv<-function(
  data_array_new,time_matrix_new,nobs_new,
  pattern,method,
  parameter=0.5,
  CL=Inf)
{
  if(any(dim(data_array_new)[1:2]!=dim(time_matrix_new)))stop("Dimensions of 'data_array_new' and 'time_matrix_new' don't match.")
  if(dim(data_array_new)[1]!=length(nobs_new))stop("Dimensions of 'data_array_new' and 'nobs_new' don't match.")
  
  nind<-dim(data_array_new)[1]
  nmaxobs<-dim(data_array_new)[2]
  ndim<-dim(data_array_new)[3]
  
  time_matrix_new<-clean_matij_by_nobs(time_matrix_new,nobs_new,"time_matrix_new")
  for(kk in 1:ndim){
    data_array_new[,,kk]<-clean_matij_by_nobs(data_array_new[,,kk],nobs_new,"data_array_new")
  }
  
  ttmax<-pattern$ttmax
  ttmin<-pattern$ttmin
  n_time_units<-pattern$n_time_units
  if(ttmin>min(time_matrix_new,na.rm=TRUE)|ttmax<min(time_matrix_new,na.rm=TRUE))
    warning(
      "'design_interval' does not cover all elements of 'time_matrix_new'.\n",
      "Please consider re-estimating the pattern with a wider 'design_interval'. ")
  time_unit<-(ttmax-ttmin)/(n_time_units-1)
  time_matrix_int_new<-round((time_matrix_new-ttmin)/time_unit)+1
  
  if(method=="risk"){
    if(parameter>1.0|parameter<0.0)
      stop("For method 'risk' parameter should be in the interval [0,1].")
  }else{
    stop("Error in the argument 'method'.")
  }
  
  if(method=="risk"){
    
    beta_est<-pattern$beta_est
    mean_risk_est<-pattern$mean_risk_est
    var_risk_est<-pattern$var_risk_est
    delta_bar<-pattern$delta_bar
    sd_rr_est<-sqrt(var_risk_est)
    
    rrij_new<-matrix(NA,nind,nmaxobs)
    eeij_new<-matrix(NA,nind,nmaxobs)
    for(ii in 1:nind){
      rrij_new[ii,1:nobs_new[ii]]<-data_array_new[ii,1:nobs_new[ii],]%*%beta_est
      eeij_new[ii,1:nobs_new[ii]]<-
        (rrij_new[ii,1:nobs_new[ii]]-mean_risk_est[time_matrix_int_new[ii,1:nobs_new[ii]]])/
        sd_rr_est[time_matrix_int_new[ii,1:nobs_new[ii]]]
    }
    
    chartij_new<-chart_risk(eeij_new,time_matrix_int_new,nobs_new,parameter,delta_bar,CL)
    
    result<-list(chart=chartij_new,
                 time_matrix_int=time_matrix_int_new,
                 standardized_values=eeij_new)
    
  }
}







