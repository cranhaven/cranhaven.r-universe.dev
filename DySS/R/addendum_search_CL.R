##' Search Control Limit
##'
##' @title Search Control Limit
##' @description Given a chart matrix, the function \code{search_CL} searches the control limit (CL)
##' so that the specified average time to signals (ATS) can be attained.
##' @param chart_matrix charting statistics arranged as a numeric matrix. \cr
##' \code{chart_matrix[i,j]} is the jth charting statistic of the ith subject.
##' @param time_matrix observation times arranged as a numeric matrix. \cr
##' \code{time_matrix[i,j]} is the jth observation time of the ith subject,
##' corresponding to the time the charting statistic \code{chart_matrix[i,j]} is computed.
##' @param nobs number of observations arranged as an integer vector. \cr
##' \code{nobs[i]} is the number of observations for the ith subject.
##' @param starttime a vector of times from the start of monitoring. \cr
##' \code{starttime[i]} is the time that the ith subject starts to be monitored.
##' @param endtime a vector of times from the start of monitoring. \cr
##' \code{endtime[i]} is the time that the ith subject is lost to be monitored.
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
##' @param ATS_nominal a numeric value. \cr
##' \code{ATS_nominal} is the nominal (or say targeted) ATS that is intended to achieve. 
##' @param CL_lower,CL_step,CL_upper three numeric values. \cr
##' The control limit will be searched within the interval [\code{CL_lower},\code{CL_upper}]. \cr
##' When applying grid search, the algorithm will use a step size of \code{CL_step}. \cr
##' (Namely, the algorithm will start with \code{CL_lower}, 
##' and search through the sequences \code{CL_lower}, \code{CL_lower+CL_step}, \code{CL_lower+2*CL_step}, ... until \code{CL_upper}.)
##' @param no_signal_action a character specifying the method to use when a signal is not given to a process.
##' If \code{no_signal_action="omit"} take averages by omitting the processes with no signals, namely, average only the processes with signals. \cr
##' If \code{no_signal_action="maxtime"} impute the signal times by the maximum time, which is the right limit of design time interval. \cr
##' If \code{no_signal_action="endtime"} impute the signal times by the end times.
##' @param ATS_tol a numeric value. \cr
##' Error tolerance for ATS.
##' @param CL_tol a numeric value. \cr
##' Error tolerance for control limit.
##' @return a numeric value, the control limit that gives the desired ATS.
##' @export
##' @examples 
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
##'   data_matrix_new=data_example_long_1d$data_matrix_IC,
##'   time_matrix_new=data_example_long_1d$time_matrix_IC,
##'   nobs_new=data_example_long_1d$nobs_IC,
##'   pattern=result_pattern,
##'   side="upward",
##'   chart="CUSUM",
##'   method="standard",
##'   parameter=0.5)
##' 
##' CL<-search_CL(
##'   chart_matrix=result_monitoring$chart,
##'   time_matrix=data_example_long_1d$time_matrix_IC,
##'   nobs=data_example_long_1d$nobs_IC,
##'   starttime=rep(0,nrow(data_example_long_1d$time_matrix_IC)),
##'   endtime=rep(1,nrow(data_example_long_1d$time_matrix_IC)),
##'   design_interval=data_example_long_1d$design_interval,
##'   n_time_units=data_example_long_1d$n_time_units,
##'   ATS_nominal=200,CL_lower=0,CL_upper=5)
##' 
search_CL<-function(
  chart_matrix,time_matrix,nobs,starttime,endtime,
  design_interval,n_time_units,time_unit,
  ATS_nominal,CL_lower,CL_step,CL_upper,
  no_signal_action="omit",ATS_tol,CL_tol)
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
  if(missing(ATS_nominal))stop("Error in argument 'ATS_nominal'.")
  if(missing(CL_lower))CL_lower<-min(chart_matrix,na.rm=TRUE)
  if(missing(CL_upper))CL_upper<-max(chart_matrix,na.rm=TRUE)
  if(missing(CL_step))CL_step<-(CL_upper-CL_lower)/20
  if(missing(ATS_tol))ATS_tol<-ATS_nominal*1e-7
  if(missing(CL_tol))CL_tol<-(CL_upper-CL_lower)*1e-7
  
  if(!no_signal_action%in%c("omit","maxtime","centime"))stop("Error in argument 'no_signal_action'.")
  
  time_matrix_int<-round((time_matrix-ttmin)/time_unit)+1
  starttime_int<-round((starttime-ttmin)/time_unit)+1
  endtime_int<-round((endtime-ttmin)/time_unit)+1
  time_matrix_int_shifted<-sweep(time_matrix_int,1,starttime_int)
  
  if(no_signal_action=="omit"){
    CL<-add_search_control_limit_omit(
      chart_matrix,time_matrix_int_shifted,nobs,endtime,ATS_nominal,ATS_tol,
      CL_lower,CL_step,CL_upper,CL_tol)
    if(CL-CL_lower<CL_tol|CL_upper-CL<CL_tol)
      stop("Control limit not found in the range: ","[",CL_lower,",",CL_upper,"]")
    return(CL)
  }
  
  if(no_signal_action=="maxtime"){
    imputetime<-n_time_units-starttime_int
    CL<-add_search_control_limit_impute(
      chart_matrix,time_matrix_int_shifted,nobs,imputetime,ATS_nominal,ATS_tol,
      CL_lower,CL_step,CL_upper,CL_tol)
    if(CL-CL_lower<CL_tol|CL_upper-CL<CL_tol)
      stop("Control limit not found in the range","[",CL_lower,",",CL_upper,"]")
    return(CL)
  }
  
  if(no_signal_action=="endtime"){
    imputetime<-endtime_int-starttime_int
    CL<-add_search_control_limit_impute(
      chart_matrix,time_matrix_int_shifted,nobs,imputetime,ATS_nominal,ATS_tol,
      CL_lower,CL_step,CL_upper,CL_tol)
    if(CL-CL_lower<CL_tol|CL_upper-CL<CL_tol)
      stop("Control limit not found in the range","[",CL_lower,",",CL_upper,"]")
    return(CL)
  }
}
