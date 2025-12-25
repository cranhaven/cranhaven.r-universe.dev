##' Calculate ATS
##'
##' @title Calculate ATS
##' @description The function \code{calculate_ATS} calculates the average time to signals (ATS) given 
##' a control chart matrix and a specified control limit (CL). ATS is defined as the average time from
##' the start of process monitoring to signal times.
##' @param chart_matrix charting statistic values arranged as a numeric matrix. \cr
##' \code{chart_matrix[i,j]} is the jth charting statistic of the ith subject.
##' @param time_matrix observation times arranged as a numeric matrix. \cr
##' \code{time_matrix[i,j]} is the jth observation time of the ith subject,
##' corresponding to the time the charting statistic \code{chart_matrix[i,j]} is computed.
##' @param nobs number of observations arranged as an integer vector. \cr
##' \code{nobs[i]} is the number of observations for the ith subject.
##' @param starttime a numeric vector that gives the start times. \cr
##' \code{starttime[i]} is the time that the ith subject starts to be monitored.
##' @param endtime a numeric vector that gives the end times. \cr
##' \code{endtime[i]} is the time that the ith subject is lost to be monitored.
##' @param design_interval a numeric vector of length two that 
##' gives the left- and right- limits of the design interval. 
##' By default, \code{design_interval=range(time_matrix,na.rm=TRUE)}.
##' @param n_time_units an integer value that gives the number of basic time units
##' in the design time interval. \cr
##' The design interval will be discretized to \cr
##' \code{seq(design_interval[1],design_interval[2],length.out=n_time_units)}
##' @param time_unit an optional numeric value of basic time unit. Only used when \code{n_time_units} is missing. \cr
##' The design interval will be discretized to \cr
##' \code{seq(design_interval[1],design_interval[2],by=time_unit)}
##' @param CL a numeric value specifying the control limit. \cr
##' \code{CL} is the control limit, signals will be given if charting statistics are greater than the control limit. 
##' @param no_signal_action a character specifying the method to use when a signal is not given to a process.
##' If \code{no_signal_action="omit"} take averages by omitting the processes with no signals, namely, average only the processes with signals. \cr
##' If \code{no_signal_action="maxtime"} impute the signal times by the maximum time, which is the right limit of design time interval. \cr
##' If \code{no_signal_action="endtime"} impute the signal times by the end times.
##' @return a numeric value, the ATS given the charting statistics and the control limit.
##' @references 
##' Qiu, P. and Xiang, D. (2014). Univariate dynamic screening system: an approach for identifying individuals with irregular longitudinal behavior. Technometrics, 56:248-260. \cr
##' Qiu, P., Xia, Z., and You, L. (2020). Process monitoring roc curve for evaluating dynamic screening methods. Technometrics, 62(2).
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
##' 
##' result_monitoring<-monitor_long_1d(
##'   data_matrix_new=data_example_long_1d$data_matrix_OC,
##'   time_matrix_new=data_example_long_1d$time_matrix_OC,
##'   nobs_new=data_example_long_1d$nobs_OC,
##'   pattern=result_pattern,
##'   side="upward",
##'   chart="CUSUM",
##'   method="standard",
##'   parameter=0.5)
##' 
##' result_ATS<-calculate_ATS(
##'   chart_matrix=result_monitoring$chart,
##'   time_matrix=data_example_long_1d$time_matrix_OC,
##'   nobs=data_example_long_1d$nobs_OC,
##'   starttime=rep(0,nrow(data_example_long_1d$time_matrix_OC)),
##'   endtime=rep(1,nrow(data_example_long_1d$time_matrix_OC)),
##'   design_interval=data_example_long_1d$design_interval,
##'   n_time_units=data_example_long_1d$n_time_units,
##'   CL=2.0)
##'   
calculate_ATS<-function(
  chart_matrix,time_matrix,nobs,starttime,endtime,
  design_interval,n_time_units,time_unit,
  CL,no_signal_action="omit")
{
  if(any(dim(chart_matrix)!=dim(time_matrix)))stop("Dimensions of 'chart_matrix' and 'time_matrix' don't match.")
  if(dim(chart_matrix)[1]!=length(nobs))stop("Dimensions of 'chart_matrix' and 'nobs' don't match.")
  
  nind<-dim(chart_matrix)[1]
  nmaxobs<-dim(chart_matrix)[2]
  chart_matrix<-clean_matij_by_nobs(chart_matrix,nobs,"chart_matrix")
  time_matrix<-clean_matij_by_nobs(time_matrix,nobs,"time_matrix")
  
  list_time_unit<-clean_time_unit(
    design_interval,n_time_units,time_unit,range(time_matrix,na.rm=TRUE))
  design_interval<-list_time_unit$design_interval
  time_unit<-list_time_unit$time_unit
  n_time_units<-list_time_unit$n_time_units
  ttmin<-list_time_unit$ttmin
  ttmax<-list_time_unit$ttmax
  
  if(missing(starttime))time_matrix<-time_matrix[,1]
  if(missing(endtime))endtime<-time_matrix[cbind(1:nind,nobs)]
  
  if(missing(CL))stop("Missing argument 'CL'.")
  
  if(!no_signal_action%in%c("omit","maxtime","endtime"))
    stop("Error in argument 'no_signal_action'.")
  
  time_matrix_int<-round((time_matrix-ttmin)/time_unit)+1
  starttime_int<-round((starttime-ttmin)/time_unit)+1
  endtime_int<-round((endtime-ttmin)/time_unit)+1
  
  time_matrix_int_shifted<-sweep(time_matrix_int,1,starttime_int)
  
  if(no_signal_action=="omit"){
    ATS<-eva_calculate_ATS_omit(
      chart_matrix,time_matrix_int_shifted,nobs,endtime_int,CL)
    return(ATS)
  }
  
  if(no_signal_action=="maxtime"){
    imputetime<-n_time_units-starttime_int
    ATS<-eva_calculate_ATS_impute(
      chart_matrix,time_matrix_int_shifted,nobs,endtime_int,CL)
    return(ATS)
  }
  
  if(no_signal_action=="endtime"){
    imputetime<-endtime_int-starttime_int
    ATS<-eva_calculate_ATS_impute(
      chart_matrix,time_matrix_int_shifted,nobs,endtime_int,CL)
    return(ATS)
  }
}
