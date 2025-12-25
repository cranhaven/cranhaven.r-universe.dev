##' Evaluate Control Charts
##'
##' @title Evaluate Control Charts (in a single dataset)
##' @description The function \code{evaluate_control_chart_one_group} evaluates a control chart when
##' the in-control (IC) and out-of-control (OC) charting statistics are supplied together in one matrix \code{chart_matrix}.
##' The logical vector \code{status} indicates if the ith subject is IC or OC.
##' @param chart_matrix charting statistics arranged as a numeric matrix. \cr
##' \code{chart_matrix[i,j]} is the jth charting statistic of the ith subject.
##' @param time_matrix observation times arranged as a numeric matrix. \cr
##' \code{time_matrix[i,j]} is the jth observation time of the ith subject. \cr
##' \code{chart_matrix[i,j]} is the charting statistic of the ith subject at \code{time_matrix[i,j]}.
##' @param nobs number of observations arranged as an integer vector. \cr
##' \code{nobs[i]} is the number of observations for the ith subject.
##' @param starttime a numeric vector.
##' \code{starttime[i]} is the time when monitoring starts for ith subject.
##' @param endtime a numeric vector, times when monitoring end.
##' \code{endtime[i]} is the time when monitoring ends for ith subject.
##' @param status a logical vector.
##' \code{status[i]=FALSE} if the ith subject is IC, while \code{status[i]=TRUE} indicates the the ith subject is OC.
##' @param design_interval a numeric vector of length two that 
##' gives the left- and right- limits of the design interval. 
##' By default, \code{design_interval=range(time_matrix,na.rm=TRUE)}.
##' @param n_time_units an integer value that gives the number of basic time units
##' in the design time interval. \cr
##' The design interval will be discretized to \code{seq(design_interval[1],design_interval[2],length.out=n_time_units)}
##' @param time_unit an optional numeric value of basic time unit. Only used when \code{n_time_units} is missing.\cr
##' The design interval will be discretized to \code{seq(design_interval[1],design_interval[2],by=time_unit)}
##' @param no_signal_action a character value specifying how to set signal times when processes with no signals.\cr
##' If \code{no_signal_action=="omit"}, the signal time is set to be missing.\cr
##' If \code{no_signal_action=="maxtime"}, the signal time is set to be the time from start time to the end of the design interval.\cr
##' If \code{no_signal_action=="endtime"}, the signal time is set to be the time from start time to the end time.\cr
##' @return an list that stores the evaluation measures. \cr
##' \item{$thres}{A numeric vector. Threshold values for control limits.}
##' \item{$FPR}{A numeric vector. False positive rates.}
##' \item{$TPR}{A numeric vector. True positive rates.}
##' \item{$ATS0}{A numeric vector. In-control ATS.}
##' \item{$ATS1}{A numeric vector. Out-of-control ATS.}
##' @references 
##' Qiu, P. and Xiang, D. (2014). Univariate dynamic screening system: an approach for identifying individuals with irregular longitudinal behavior. Technometrics, 56:248-260. \cr
##' Qiu, P., Xia, Z., and You, L. (2020). Process monitoring ROC curve for evaluating dynamic screening methods. Technometrics, 62(2).
##' @export
##' @examples 
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
##'   data_array_new=data_example_long_surv$data_array_IC,
##'   time_matrix_new=data_example_long_surv$time_matrix_IC,
##'   nobs_new=data_example_long_surv$nobs_IC,
##'   pattern=result_pattern,
##'   method="risk",
##'   parameter=0.5)
##' 
##' output_evaluate<-evaluate_control_chart_one_group(
##'   chart_matrix=result_monitoring$chart[1:200,],
##'   time_matrix=data_example_long_surv$time_matrix_IC[1:200,],
##'   nobs=data_example_long_surv$nobs_IC[1:200],
##'   starttime=rep(0,200),
##'   endtime=rep(1,200),
##'   status=data_example_long_surv$survevent_IC[1:200],
##'   design_interval=data_example_long_surv$design_interval,
##'   n_time_units=data_example_long_surv$n_time_units,
##'   no_signal_action="maxtime")
##' 
evaluate_control_chart_one_group<-function(
  chart_matrix,time_matrix,nobs,
  starttime,endtime,status,
  design_interval,n_time_units,time_unit,
  no_signal_action="omit"){
  
  if(any(dim(chart_matrix)!=dim(time_matrix)))stop("Dimensions of 'chart_matrix' and 'time_matrix' don't match.")
  if(!all.equal(dim(chart_matrix)[1],length(starttime),length(endtime),length(status)))
    stop("Dimensions of 'starttime', 'endtime' and 'status' don't match.")
  
  chart_matrix<-clean_matij_by_nobs(chart_matrix,nobs,"chart_matrix")
  time_matrix<-clean_matij_by_nobs(time_matrix,nobs,"time_matrix")
  
  list_time_unit<-clean_time_unit(
    design_interval,n_time_units,time_unit,range(time_matrix,na.rm=TRUE))
  design_interval<-list_time_unit$design_interval
  n_time_units<-list_time_unit$n_time_units
  time_unit<-list_time_unit$time_unit
  ttmin<-list_time_unit$ttmin
  ttmax<-list_time_unit$ttmax
  
  add_thres<-min(chart_matrix,na.rm=TRUE)-0.1
  
  time_unit<-(ttmax-ttmin)/(n_time_units-1)
  time_matrix_int<-round((time_matrix-ttmin)/time_unit)+1
  starttime_int<-round((starttime-ttmin)/time_unit)+1
  endtime_int<-round((endtime-ttmin)/time_unit)+1
  time_matrix_int_shifted<-sweep(time_matrix_int,1,starttime_int)
  
  if(no_signal_action=="omit"){
    four_elements_output<-eva_four_elements_omit(
      chart_matrix[!status,],time_matrix_int_shifted[!status,],nobs[!status],
      chart_matrix[status,],time_matrix_int_shifted[status,],nobs[status],
      add_thres=add_thres)
    
    result<-list(
      thres=four_elements_output$thres,
      nFP=four_elements_output$nFP,
      nTP=four_elements_output$nTP,
      FPR=four_elements_output$FPR,
      TPR=four_elements_output$TPR,
      ATS0=four_elements_output$sumtime_ctrl/four_elements_output$nFP,
      ATS1=four_elements_output$sumtime_case/four_elements_output$nTP,
      sumtime_ctrl=four_elements_output$sumtime_ctrl,
      sumtime_case=four_elements_output$sumtime_case,
      ttmin=ttmin,
      ttmax=ttmax,
      n_time_units=n_time_units,
      no_signal_action=no_signal_action)
    
    class(result)<-"evaluate_control_chart"
    return(result)
  }
  
  if(no_signal_action=="maxtime"){
    
    imputetime<-n_time_units-starttime_int
    
    four_elements_output<-eva_four_elements_impute(
      chart_matrix[!status,],time_matrix_int_shifted[!status,],nobs[!status],imputetime[!status],
      chart_matrix[status,],time_matrix_int_shifted[status,],nobs[status],imputetime[status],
      add_thres=add_thres)
    
    result<-list(
      thres=four_elements_output$thres,
      nFP=four_elements_output$nFP,
      nTP=four_elements_output$nTP,
      FPR=four_elements_output$FPR,
      TPR=four_elements_output$TPR,
      ATS0=four_elements_output$sumtime_ctrl/sum(!status),
      ATS1=four_elements_output$sumtime_case/sum(status),
      sumtime_ctrl=four_elements_output$sumtime_ctrl,
      sumtime_case=four_elements_output$sumtime_case,
      ttmin=ttmin,
      ttmax=ttmax,
      n_time_units=n_time_units,
      no_signal_action=no_signal_action)
    
    class(result)<-"evaluate_control_chart"
    return(result)
  }
  
  if(no_signal_action=="endtime"){
    
    imputetime<-endtime_int-starttime_int
    
    four_elements_output<-eva_four_elements_impute(
      chart_matrix[!status,],time_matrix_int_shifted[!status,],nobs[!status],imputetime[!status],
      chart_matrix[status,],time_matrix_int_shifted[status,],nobs[status],imputetime[status],
      add_thres=add_thres)
    
    result<-list(
      thres=four_elements_output$thres,
      nFP=four_elements_output$nFP,
      nTP=four_elements_output$nTP,
      FPR=four_elements_output$FPR,
      TPR=four_elements_output$TPR,
      ATS0=four_elements_output$sumtime_ctrl/sum(!status),
      ATS1=four_elements_output$sumtime_case/sum(status),
      sumtime_ctrl=four_elements_output$sumtime_ctrl,
      sumtime_case=four_elements_output$sumtime_case,
      ttmin=ttmin,
      ttmax=ttmax,
      n_time_units=n_time_units,
      no_signal_action=no_signal_action)
    
    class(result)<-"evaluate_control_chart"
    return(result)
  }
}

##' Evaluate Control Charts
##'
##' @title Evaluate Control Charts
##' @description The function \code{evaluate_control_chart_two_groups} evaluates control charts when
##' the in-control (IC) and out-of-control (OC) charting statistics are supplied separately in two matrices 
##' \code{chart_matrix_IC} and \code{chart_matrix_OC}.
##' @param chart_matrix_IC,chart_matrix_OC charting statistics arranged as a numeric matrix. \cr
##' \code{chart_matrix_IC[i,j]} is the jth charting statistic of the ith IC subject. \cr
##' \code{chart_matrix_OC[i,j]} is the jth charting statistic of the ith OC subject.
##' @param time_matrix_IC,time_matrix_OC observation times arranged as a numeric matrix. \cr
##' \code{time_matrix_IC[i,j]} is the jth observation time of the ith IC subject. \cr
##' \code{time_matrix_OC[i,j]} is the jth observation time of the ith OC subject. \cr
##' \code{chart_matrix_IC[i,j]} is the charting statistic of the ith IC subject at \code{time_matrix[i,j]}. \cr
##' \code{chart_matrix_OC[i,j]} is the charting statistic of the ith OC subject at \code{time_matrix[i,j]}.
##' @param nobs_IC,nobs_OC number of observations arranged as an integer vector. \cr
##' \code{nobs_IC[i]} is the number of observations for the ith subject. \cr
##' \code{nobs_OC[i]} is the number of observations for the ith subject.
##' @param starttime_IC,starttime_OC a numeric vector that gives the start times. \cr
##' \code{starttime_IC[i]} is the time that the ith IC subject starts to be monitored. \cr
##' \code{starttime_OC[i]} is the time that the ith OC subject starts to be monitored.
##' @param endtime_IC,endtime_OC a numeric vector that gives the end times. \cr
##' \code{endtime_IC[i]} is the time that the ith IC subject is lost to be monitored. \cr
##' \code{endtime_OC[i]} is the time that the ith OC subject is lost to be monitored.
##' @param design_interval a numeric vector of length two that 
##' gives the left- and right- limits of the design interval. 
##' By default, \code{design_interval=range(time_matrix,na.rm=TRUE)}.
##' @param n_time_units an integer value that gives the number of basic time units
##' in the design time interval. \cr
##' The design interval will be discretized to \code{seq(design_interval[1],design_interval[2],length.out=n_time_units)}
##' @param time_unit an optional numeric value of basic time unit. Only used when \code{n_time_units} is missing.\cr
##' The design interval will be discretized to \code{seq(design_interval[1],design_interval[2],by=time_unit)}
##' @param no_signal_action a character value specifying how to set signal times when processes with no signals.\cr
##' If \code{no_signal_action=="omit"}, the signal time is set to be missing.\cr
##' If \code{no_signal_action=="maxtime"}, the signal time is set to be the time from start time to the end of the design interval.\cr
##' If \code{no_signal_action=="endtime"}, the signal time is set to be the time from start time to the end time.\cr
##' @return an list that stores the evaluation measures. \cr
##' \item{$thres}{A numeric vector. Threshold values for control limits.}
##' \item{$FPR}{A numeric vector. False positive rates.}
##' \item{$TPR}{A numeric vector. True positive rates.}
##' \item{$ATS0}{A numeric vector. In-control ATS.}
##' \item{$ATS1}{A numeric vector. Out-of-control ATS.}
##' @references 
##' Qiu, P. and Xiang, D. (2014). Univariate dynamic screening system: an approach for identifying individuals with irregular longitudinal behavior. Technometrics, 56:248-260. \cr
##' Qiu, P., Xia, Z., and You, L. (2020). Process monitoring ROC curve for evaluating dynamic screening methods. Technometrics, 62(2).
##' @examples 
##' pattern<-estimate_pattern_long_1d(
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
##' chart_IC_output<-monitor_long_1d(
##'   data_example_long_1d$data_matrix_IC,
##'   data_example_long_1d$time_matrix_IC,
##'   data_example_long_1d$nobs_IC,
##'   pattern=pattern,side="upward",chart="CUSUM",
##'   method="standard",parameter=0.2)
##' 
##' chart_OC_output<-monitor_long_1d(
##'   data_example_long_1d$data_matrix_OC,
##'   data_example_long_1d$time_matrix_OC,
##'   data_example_long_1d$nobs_OC,
##'   pattern=pattern,side="upward",chart="CUSUM",
##'   method="standard",parameter=0.2)
##' 
##' output_evaluate<-evaluate_control_chart_two_groups(
##'   chart_matrix_IC=chart_IC_output$chart[1:50,],
##'   time_matrix_IC=data_example_long_1d$time_matrix_IC[1:50,],
##'   nobs_IC=data_example_long_1d$nobs_IC[1:50],
##'   starttime_IC=rep(0,50),
##'   endtime_IC=rep(1,50),
##'   chart_matrix_OC=chart_OC_output$chart[1:50,],
##'   time_matrix_OC=data_example_long_1d$time_matrix_OC[1:50,],
##'   nobs_OC=data_example_long_1d$nobs_OC[1:50],
##'   starttime_OC=rep(0,50),
##'   endtime_OC=rep(1,50),
##'   design_interval=data_example_long_1d$design_interval,
##'   n_time_units=data_example_long_1d$n_time_units,
##'   no_signal_action="maxtime")
##' 
evaluate_control_chart_two_groups<-function(
  chart_matrix_IC,time_matrix_IC,nobs_IC,
  starttime_IC,endtime_IC,
  chart_matrix_OC,time_matrix_OC,nobs_OC,
  starttime_OC,endtime_OC,
  design_interval,n_time_units,time_unit,
  no_signal_action="omit"){
  
  if(any(dim(chart_matrix_IC)!=dim(time_matrix_IC)))stop("Dimensions of 'chart_matrix_IC' and 'time_matrix_IC' don't match.")
  if(any(dim(chart_matrix_OC)!=dim(time_matrix_OC)))stop("Dimensions of 'chart_matrix_OC' and 'time_matrix_OC' don't match.")
  if(!all.equal(dim(chart_matrix_IC)[1],length(starttime_IC),length(endtime_IC)))
    stop("Dimensions of 'chart_matrix_IC', 'starttime_IC' and 'endtime_IC' don't match.")
  if(!all.equal(dim(chart_matrix_OC)[1],length(starttime_OC),length(endtime_OC)))
    stop("Dimensions of 'chart_matrix_OC', 'starttime_OC' and 'endtime_OC' don't match.")
  
  chart_matrix_IC<-clean_matij_by_nobs(chart_matrix_IC,nobs_IC,"chart_matrix_IC")
  chart_matrix_OC<-clean_matij_by_nobs(chart_matrix_OC,nobs_OC,"chart_matrix_OC")
  time_matrix_IC<-clean_matij_by_nobs(time_matrix_IC,nobs_IC,"time_matrix_IC")
  time_matrix_OC<-clean_matij_by_nobs(time_matrix_OC,nobs_OC,"time_matrix_OC")
  nind_IC<-nrow(time_matrix_IC)
  nind_OC<-nrow(time_matrix_OC)
  
  list_time_unit<-clean_time_unit(
    design_interval,n_time_units,time_unit,range(c(time_matrix_IC,time_matrix_OC),na.rm=TRUE))
  design_interval<-list_time_unit$design_interval
  n_time_units<-list_time_unit$n_time_units
  time_unit<-list_time_unit$time_unit
  ttmin<-list_time_unit$ttmin
  ttmax<-list_time_unit$ttmax
  
  add_thres<-min(c(chart_matrix_IC,chart_matrix_OC),na.rm=TRUE)-0.1
  
  time_unit<-(ttmax-ttmin)/(n_time_units-1)
  time_matrix_IC_int<-round((time_matrix_IC-ttmin)/time_unit)+1
  time_matrix_OC_int<-round((time_matrix_OC-ttmin)/time_unit)+1
  starttime_IC_int<-round((starttime_IC-ttmin)/time_unit)+1
  starttime_OC_int<-round((starttime_OC-ttmin)/time_unit)+1
  endtime_IC_int<-round((endtime_IC-ttmin)/time_unit)+1
  endtime_OC_int<-round((endtime_OC-ttmin)/time_unit)+1
  time_matrix_IC_int_shifted<-sweep(time_matrix_IC_int,1,starttime_IC_int)
  time_matrix_OC_int_shifted<-sweep(time_matrix_OC_int,1,starttime_OC_int)
  
  if(no_signal_action=="omit"){
    four_elements_output<-eva_four_elements_omit(
      chart_matrix_IC,time_matrix_IC_int,nobs_IC,
      chart_matrix_OC,time_matrix_OC_int,nobs_OC,
      add_thres=add_thres)
    
    result<-list(
      thres=four_elements_output$thres,
      nFP=four_elements_output$nFP,
      nTP=four_elements_output$nTP,
      FPR=four_elements_output$FPR,
      TPR=four_elements_output$TPR,
      ATS0=four_elements_output$sumtime_ctrl/four_elements_output$nFP,
      ATS1=four_elements_output$sumtime_case/four_elements_output$nTP,
      sumtime_ctrl=four_elements_output$sumtime_ctrl,
      sumtime_case=four_elements_output$sumtime_case,
      ttmin=ttmin,
      ttmax=ttmax,
      n_time_units=n_time_units,
      no_signal_action=no_signal_action)
    
    class(result)<-"evaluate_control_chart"
    return(result)
  }
  
  if(no_signal_action=="maxtime"){
    
    imputetime_IC<-n_time_units-starttime_IC_int
    imputetime_OC<-n_time_units-starttime_OC_int
    
    four_elements_output<-eva_four_elements_impute(
      chart_matrix_IC,time_matrix_IC_int_shifted,nobs_IC,imputetime_IC,
      chart_matrix_OC,time_matrix_OC_int_shifted,nobs_OC,imputetime_OC,
      add_thres=add_thres)
    
    result<-list(
      thres=four_elements_output$thres,
      nFP=four_elements_output$nFP,
      nTP=four_elements_output$nTP,
      FPR=four_elements_output$FPR,
      TPR=four_elements_output$TPR,
      ATS0=four_elements_output$sumtime_ctrl/nind_IC,
      ATS1=four_elements_output$sumtime_case/nind_OC,
      sumtime_ctrl=four_elements_output$sumtime_ctrl,
      sumtime_case=four_elements_output$sumtime_case,
      ttmin=ttmin,
      ttmax=ttmax,
      n_time_units=n_time_units,
      no_signal_action=no_signal_action)
    
    class(result)<-"evaluate_control_chart"
    return(result)
  }
  
  if(no_signal_action=="endtime"){
    
    imputetime_IC<-endtime_IC_int-starttime_IC_int
    imputetime_OC<-endtime_OC_int-starttime_OC_int
    
    four_elements_output<-eva_four_elements_impute(
      chart_matrix_IC,time_matrix_IC_int_shifted,nobs_IC,imputetime_IC,
      chart_matrix_OC,time_matrix_OC_int_shifted,nobs_OC,imputetime_OC,
      add_thres=add_thres)
    
    result<-list(
      thres=four_elements_output$thres,
      nFP=four_elements_output$nFP,
      nTP=four_elements_output$nTP,
      FPR=four_elements_output$FPR,
      TPR=four_elements_output$TPR,
      ATS0=four_elements_output$sumtime_ctrl/nind_IC,
      ATS1=four_elements_output$sumtime_case/nind_OC,
      sumtime_ctrl=four_elements_output$sumtime_ctrl,
      sumtime_case=four_elements_output$sumtime_case,
      ttmin=ttmin,
      ttmax=ttmax,
      n_time_units=n_time_units,
      no_signal_action=no_signal_action)
    
    class(result)<-"evaluate_control_chart"
    return(result)
  }
}

##' Evaluate and Visualize Control Charts by ROC curves
##'
##' @title Evaluate and Visualize Control Charts by ROC curves
##' @param evaluate_control_chart an object of class evaluate_control_chart. \cr
##' \code{evaluate_control_chart} is an output from 
##' \code{evaluate_control_chart_one_group} or 
##' \code{evaluate_control_chart_two}.
##' @return No return value, called for drawing two ROC plots.
##' @examples 
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
##'   data_array_new=data_example_long_surv$data_array_IC,
##'   time_matrix_new=data_example_long_surv$time_matrix_IC,
##'   nobs_new=data_example_long_surv$nobs_IC,
##'   pattern=result_pattern,
##'   method="risk",
##'   parameter=0.5)
##' 
##' output_evaluate<-evaluate_control_chart_one_group(
##'   chart_matrix=result_monitoring$chart,
##'   time_matrix=data_example_long_surv$time_matrix_IC,
##'   nobs=data_example_long_surv$nobs_IC,
##'   starttime=rep(0,nrow(data_example_long_surv$time_matrix_IC)),
##'   endtime=rep(1,nrow(data_example_long_surv$time_matrix_IC)),
##'   status=data_example_long_surv$survevent_IC,
##'   design_interval=data_example_long_surv$design_interval,
##'   n_time_units=data_example_long_surv$n_time_units,
##'   no_signal_action="maxtime")
##' 
##' plot_evaluation(output_evaluate)
##' plot_PMROC(output_evaluate)
##' 
plot_evaluation<-function(evaluate_control_chart){
  if(evaluate_control_chart$no_signal_action=="omit"){
    ggplot2::qplot(
      evaluate_control_chart$FPR,
      evaluate_control_chart$TPR,
      geom="path",
      xlim=c(0,1),ylim=c(0,1),
      xlab="FPR",ylab="TPR",
      main="Positive Rates")+ggplot2::theme_bw()
  }else{
    qplot1<-ggplot2::qplot(
      evaluate_control_chart$FPR,
      evaluate_control_chart$TPR,
      geom="path",
      xlim=c(0,1),ylim=c(0,1),
      xlab="FPR",ylab="TPR",
      main="Positive Rates")+ggplot2::theme_bw()
    qplot2<-ggplot2::qplot(
      evaluate_control_chart$ATS0,
      evaluate_control_chart$ATS1,
      geom="path",
      xlim=c(0,evaluate_control_chart$n_time_units),
      ylim=c(0,evaluate_control_chart$n_time_units),
      xlab="ATS0",ylab="ATS1",
      main="Average Time to Signal")+ggplot2::theme_bw()
    gridExtra::grid.arrange(qplot1,qplot2,ncol=2)
  }
}

##' Evaluate and Visualize Control Charts by PM-ROC curves
##'
##' @title Evaluate and Visualize Control Charts by PM-ROC curves
##' @param evaluate_control_chart an object of class evaluate_control_chart. \cr
##' \code{evaluate_control_chart} is an output from 
##' \code{evaluate_control_chart_one_group} or 
##' \code{evaluate_control_chart_two_group}.
##' @return No return value, called for drawing one PM-ROC plot.
##' @examples 
##' pattern<-estimate_pattern_long_1d(
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
##' chart_IC_output<-monitor_long_1d(
##'   data_example_long_1d$data_matrix_IC,
##'   data_example_long_1d$time_matrix_IC,
##'   data_example_long_1d$nobs_IC,
##'   pattern=pattern,side="upward",chart="CUSUM",
##'   method="standard",parameter=0.2)
##' 
##' chart_OC_output<-monitor_long_1d(
##'   data_example_long_1d$data_matrix_OC,
##'   data_example_long_1d$time_matrix_OC,
##'   data_example_long_1d$nobs_OC,
##'   pattern=pattern,side="upward",chart="CUSUM",
##'   method="standard",parameter=0.2)
##' 
##' output_evaluate<-evaluate_control_chart_two_groups(
##'   chart_matrix_IC=chart_IC_output$chart[1:50,],
##'   time_matrix_IC=data_example_long_1d$time_matrix_IC[1:50,],
##'   nobs_IC=data_example_long_1d$nobs_IC[1:50],
##'   starttime_IC=rep(0,50),
##'   endtime_IC=rep(1,50),
##'   chart_matrix_OC=chart_OC_output$chart[1:50,],
##'   time_matrix_OC=data_example_long_1d$time_matrix_OC[1:50,],
##'   nobs_OC=data_example_long_1d$nobs_OC[1:50],
##'   starttime_OC=rep(0,50),
##'   endtime_OC=rep(1,50),
##'   design_interval=data_example_long_1d$design_interval,
##'   n_time_units=data_example_long_1d$n_time_units,
##'   no_signal_action="maxtime")
##' 
##' plot_evaluation(output_evaluate)
##' plot_PMROC(output_evaluate)
##' 
plot_PMROC<-function(evaluate_control_chart){
  if(evaluate_control_chart$no_signal_action=="omit"){
    message('plot_PMROC is not applicable when no_signal_action=="omit"')
  }else if(evaluate_control_chart$no_signal_action=="maxtime"){
    VATS0<-with(evaluate_control_chart,(ATS0-min(ATS0))/(max(ATS0)-min(ATS0)))
    VATS1<-with(evaluate_control_chart,(ATS1-min(ATS1))/(max(ATS1)-min(ATS1)))
    DFPR<-(1-VATS0)*evaluate_control_chart$FPR
    DTPR<-(1-VATS1)*evaluate_control_chart$TPR
    ggplot2::qplot(
      DFPR,DTPR,geom="path",
      xlim=c(0,1),ylim=c(0,1),
      xlab="DFPR",ylab="DTPR",
      main="PM-ROC")+ggplot2::theme_bw()
  }else if(evaluate_control_chart$no_signal_action=="endtime"){
    VATS0<-with(evaluate_control_chart,(ATS0-min(ATS0))/(max(ATS0)-min(ATS0)))
    VATS1<-with(evaluate_control_chart,(ATS1-min(ATS1))/(max(ATS1)-min(ATS1)))
    DFPR<-(1-VATS0)*evaluate_control_chart$FPR
    DTPR<-(1-VATS1)*evaluate_control_chart$TPR
    ggplot2::qplot(
      DFPR,DTPR,geom="path",
      xlim=c(0,1),ylim=c(0,1),
      xlab="DFPR",ylab="DTPR",
      main="PM-ROC")+ggplot2::theme_bw()
  }
}
