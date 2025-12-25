##' Monitor Univariate Longitudinal Data
##'
##' @title Monitor Univariate Longitudinal Data
##' @param data_matrix_new observed data arranged in a numeric matrix format. \cr
##' \code{data_matrix_new[i,j]} is the jth observation of the ith subject.
##' @param time_matrix_new observation times arranged in a numeric matrix format. \cr
##' \code{time_matrix_new[i,j]} is the jth observation time of the ith subject. \cr
##' \code{data_matrix_new[i,j]} is observed at \code{time_matrix_new[i,j]}.
##' @param nobs_new number of observations arranged as an integer vector. \cr
##' \code{nobs_new[i]} is the number of observations for the ith subject.
##' @param pattern the estimated regular longitudinal pattern
##' @param side a character value specifying the sideness/direction of process monitoring\cr
##' If \code{side="upward"}{apply control charts that aim to detect upward shifts.} \cr
##' If \code{side="downward"}{apply control charts that aim to detect downward shifts.} \cr
##' If \code{side="both"}{apply control charts that aim to detect shifts in both sides}
##' @param chart a string specifying the control charts to use.
##' If \code{chart="CUSUM"}{apply CUSUM charts.} \cr
##' If \code{chart="EWMA"}{apply EWMA charts.}
##' @param method a string \cr
##' If \code{method="standard"}, standardize observations by mean and variance (cf., Qiu and Xiang, 2014).\cr
##' If \code{method="decorrelation"}, standardize and decorrelate observations by mean and covariance (cf., Li and Qiu, 2016).\cr
##' If \code{method="sprint"}, standardize and decorrelate observations within sprint length by mean and covariance (cf., You and Qiu 2018).\cr
##' If \code{method="distribution and standard"}, standardize observations by distribution (cf., You and Qiu, 2020).\cr
##' If \code{method="distribution and decorrelation"}, standardize observations by distribution and covariance (cf., You and Qiu, 2020).\cr
##' If \code{method="distribution and sprint"},standardize and decorrelate observations within sprint length by distribution and covariance (cf., You and Qiu, 2020).\cr
##' \code{method="nonparametric and standard"} currently not supported.\cr
##' \code{method="nonparametric and decorrelation"} currently not supported
##' @param parameter a numeric value \cr
##' If \code{chart="CUSUM"}, \code{parameter} is the allowance constant in the control chart.\cr
##' If \code{chart="EWMA"}, \code{parameter} is the weighting in the control chart.
##' @param CL a numeric value speficying the control limit. \cr
##' A signal will be given if charting statistics are larger than the control limit. 
##' (Note: in this package, signs of charting statistics may be reversed such that 
##' larger values of charting statistics indicate worse performance of processes.)
##' After the signal is given, the algorithm stops calculating the charting statistics for the remaining observation times.
##' The default value of control limit is infinity, which means we will calculate the charting statistics for all observation times.
##' @return a list that stores the result.\cr
##' \item{$chart}{a numeric matrix, \code{$chart[i,j]} is the jth charting statistic of the ith subject.}
##' \item{$standardized_values}{a numeric matrix, \code{$standardized_values[i,j]} is the standardized value of the jth observation of the ith subject.}
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
monitor_long_1d<-function(
  data_matrix_new,time_matrix_new,nobs_new,pattern,
  side="upward",chart="CUSUM",
  method="standard",
  parameter=0.5,
  CL=Inf)
{
  if(any(dim(data_matrix_new)!=dim(time_matrix_new)))stop("Dimensions of 'data_matrix_new' and 'time_matrix_new' don't match.")
  if(dim(data_matrix_new)[1]!=length(nobs_new))stop("Dimensions of 'data_matrix_new' and 'nobs_new' don't match.")
  
  nind<-dim(data_matrix_new)[1]
  nmaxobs<-dim(data_matrix_new)[2]
  data_matrix_new<-clean_matij_by_nobs(data_matrix_new,nobs_new,"data_matrix")
  time_matrix_new<-clean_matij_by_nobs(time_matrix_new,nobs_new,"time_matrix")
  
  ttmax<-pattern$ttmax
  ttmin<-pattern$ttmin
  n_time_units<-pattern$n_time_units
  if(ttmin>min(time_matrix_new,na.rm=TRUE)|ttmax<min(time_matrix_new,na.rm=TRUE))
    warning(
      "'design_interval' does not cover all elements of 'time_matrix_new'.\n",
      "Please consider re-estimating the pattern with a wider 'design_interval'. ")

  time_unit<-(ttmax-ttmin)/(n_time_units-1)
  time_matrix_int_new<-pmin(pmax(round((time_matrix_new-ttmin)/time_unit)+1,1),n_time_units)
  
  if(!side%in%c("upward","downward","both"))stop("Error in argument 'side'.")
  if(chart=="CUSUM"){
    if(parameter<0.0)
      stop("For chart 'CUSUM', parameter should be greater than 0.")
  }else if(chart=="EWMA"){
    if(parameter>1.0|parameter<0.0)
      stop("For chart 'EWMA', parameter should be in the interval [0,1].")
  }else{
    stop("Error in argument 'chart'.")
  }
  
  all_methods<-c("standard",
                 "decorrelation",
                 "sprint",
                 "distribution and standard",
                 "distribution and decorrelation",
                 "distribution and sprint",
                 "nonparametric and standard",
                 "nonparametric and decorrelation")
  if(!method%in%all_methods)stop("Error in argument 'method'.")
  
  ############
  # standard #
  ############
  
  if(method=="standard"){
    if(!inherits(pattern,c("pattern_long_1d_meanvar","pattern_long_1d_meanvarcov")))
      stop("Pattern does not match method.")
    mean_est<-pattern$mean_est
    var_est<-pattern$var_est
    sd_est<-sqrt(var_est)
    eeij_new<-matrix(NA,nind,nmaxobs)
    for(ii in 1:nind){
      eeij_new[ii,1:nobs_new[ii]]<-
        (data_matrix_new[ii,1:nobs_new[ii]]-mean_est[time_matrix_int_new[ii,1:nobs_new[ii]]])/
        sd_est[time_matrix_int_new[ii,1:nobs_new[ii]]]
    }
    if(side=="upward"){
      if(chart=="CUSUM"){
        chartij_new<-chart_CUSUM_univariate_std(eeij_new,time_matrix_int_new,nobs_new,parameter,CL)
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     standardized_values=eeij_new)
        return(result)
      }else if(chart=="EWMA"){
        chartij_new<-chart_EWMA_univariate_std(eeij_new,time_matrix_int_new,nobs_new,parameter,sqrt(parameter/(2-parameter))*CL)
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     standardized_values=eeij_new)
        return(result)
      }
    }else if(side=="downward"){
      if(chart=="CUSUM"){
        chartij_new<-chart_CUSUM_univariate_std(-eeij_new,time_matrix_int_new,nobs_new,parameter,CL)
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     standardized_values=eeij_new)
        return(result)
      }else if(chart=="EWMA"){
        chartij_new<-chart_EWMA_univariate_std(-eeij_new,time_matrix_int_new,nobs_new,parameter,sqrt(parameter/(2-parameter))*CL)
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     standardized_values=eeij_new)
        return(result)
      }
    }else if(side=="both"){
      if(chart=="CUSUM"){
        chartij_upward_new<-chart_CUSUM_univariate_std(eeij_new,time_matrix_int_new,nobs_new,parameter,CL)
        chartij_downward_new<-chart_CUSUM_univariate_std(-eeij_new,time_matrix_int_new,nobs_new,parameter,CL)
        chartij_new<-pmax(chartij_upward_new,chartij_downward_new)
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     chart_upward=chartij_upward_new,
                     chart_downward=chartij_downward_new,
                     standardized_values=eeij_new)
        return(result)
      }else if(chart=="EWMA"){
        chartij_upward_new<-chart_EWMA_univariate_std(eeij_new,time_matrix_int_new,nobs_new,parameter,sqrt(parameter/(2-parameter))*CL)
        chartij_downward_new<-chart_EWMA_univariate_std(-eeij_new,time_matrix_int_new,nobs_new,parameter,sqrt(parameter/(2-parameter))*CL)
        chartij_new<-pmax(chartij_upward_new,chartij_downward_new)
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     chart_upward=chartij_upward_new,
                     chart_downward=chartij_downward_new,
                     standardized_values=eeij_new)
        return(result)
      }
    }
  }
  
  #################
  # decorrelation #
  #################
  
  if(method=="decorrelation"){
    if(!inherits(pattern,"pattern_long_1d_meanvarcov"))
      stop("Pattern does not match method.")
    mean_est<-pattern$mean_est
    var_est<-pattern$var_est
    cov_est<-pattern$cov_est
    epsij_new<-matrix(NA,nind,nmaxobs)
    for(ii in 1:nind){
      epsij_new[ii,1:nobs_new[ii]]<-data_matrix_new[ii,1:nobs_new[ii]]-mean_est[time_matrix_int_new[ii,1:nobs_new[ii]]]
    }
    if(side=="upward"){
      if(chart=="CUSUM"){
        chart_output<-chart_CUSUM_univariate_dec(epsij_new,time_matrix_int_new,nobs_new,cov_est,parameter,CL)
        chartij_new<-chart_output[[1]]
        eeij_new<-chart_output[[2]]
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     standardized_values=eeij_new)
        return(result)
      }else if(chart=="EWMA"){
        chart_output<-chart_EWMA_univariate_dec(epsij_new,time_matrix_int_new,nobs_new,cov_est,parameter,sqrt(parameter/(2-parameter))*CL)
        chartij_new<-chart_output[[1]]
        eeij_new<-chart_output[[2]]
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     standardized_values=eeij_new)
        return(result)
      }
    }else if(side=="downward"){
      if(chart=="CUSUM"){
        chart_output<-chart_CUSUM_univariate_dec(-epsij_new,time_matrix_int_new,nobs_new,cov_est,parameter,CL)
        chartij_new<-chart_output[[1]]
        eeij_new<- -chart_output[[2]]
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     standardized_values=eeij_new)
        return(result)
      }else if(chart=="EWMA"){
        chart_output<-chart_EWMA_univariate_dec(-epsij_new,time_matrix_int_new,nobs_new,cov_est,parameter,sqrt(parameter/(2-parameter))*CL)
        chartij_new<-chart_output[[1]]
        eeij_new<- -chart_output[[2]]
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     standardized_values=eeij_new)
        return(result)
      }
    }else if(side=="both"){
      if(chart=="CUSUM"){
        chart_upward_output<-chart_CUSUM_univariate_dec(epsij_new,time_matrix_int_new,nobs_new,cov_est,parameter,CL)
        chart_downward_output<-chart_CUSUM_univariate_dec(-epsij_new,time_matrix_int_new,nobs_new,cov_est,parameter,CL)
        chartij_upward_new<-chart_upward_output[[1]]
        chartij_downward_new<- -chart_downward_output[[1]]
        eeij_upward_new<-chart_upward_output[[2]]
        eeij_downward_new<- -chart_downward_output[[2]]
        chartij_new<-pmax(chartij_upward_new,-chartij_downward_new)
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     chart_upward=chartij_upward_new,
                     chart_downward=chartij_downward_new,
                     standardized_values_upward=eeij_upward_new,
                     standardized_values_downward=eeij_downward_new)
        return(result)
      }else if(chart=="EWMA"){
        chart_upward_output<-chart_EWMA_univariate_dec(epsij_new,time_matrix_int_new,nobs_new,cov_est,parameter,sqrt(parameter/(2-parameter))*CL)
        chart_downward_output<-chart_EWMA_univariate_dec(-epsij_new,time_matrix_int_new,nobs_new,cov_est,parameter,sqrt(parameter/(2-parameter))*CL)
        chartij_upward_new<-chart_upward_output[[1]]
        chartij_downward_new<- -chart_downward_output[[1]]
        eeij_upward_new<-chart_upward_output[[2]]
        eeij_downward_new<- -chart_downward_output[[2]]
        chartij_new<-pmax(chartij_upward_new,-chartij_downward_new)
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     chart_upward=chartij_upward_new,
                     chart_downward=chartij_downward_new,
                     standardized_values_upward=eeij_upward_new,
                     standardized_values_downward=eeij_downward_new)
        return(result)
      }
    }
  }
  
  ##########
  # sprint #
  ##########
  
  if(method=="sprint"){
    if(!inherits(pattern,"pattern_long_1d_meanvarcov"))
      stop("Pattern does not match method.")
    mean_est<-pattern$mean_est
    var_est<-pattern$var_est
    cov_est<-pattern$cov_est
    epsij_new<-matrix(NA,nind,nmaxobs)
    for(ii in 1:nind){
      epsij_new[ii,1:nobs_new[ii]]<-data_matrix_new[ii,1:nobs_new[ii]]-mean_est[time_matrix_int_new[ii,1:nobs_new[ii]]]
    }
    if(side=="upward"){
      if(chart=="CUSUM"){
        chart_output<-chart_CUSUM_univariate_sprint(epsij_new,time_matrix_int_new,nobs_new,cov_est,parameter,CL)
        chartij_new<-chart_output[[1]]
        eeij_new<-chart_output[[2]]
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     standardized_values=eeij_new)
        return(result)
      }else if(chart=="EWMA"){
        chart_output<-chart_EWMA_univariate_sprint(epsij_new,time_matrix_int_new,nobs_new,cov_est,parameter,sqrt(parameter/(2-parameter))*CL)
        chartij_new<-chart_output[[1]]
        eeij_new<-chart_output[[2]]
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     standardized_values=eeij_new)
        return(result)
      }
    }else if(side=="downward"){
      if(chart=="CUSUM"){
        chart_output<-chart_CUSUM_univariate_sprint(-epsij_new,time_matrix_int_new,nobs_new,cov_est,parameter,CL)
        chartij_new<-chart_output[[1]]
        eeij_new<- -chart_output[[2]]
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     standardized_values=eeij_new)
        return(result)
      }else if(chart=="EWMA"){
        chart_output<-chart_EWMA_univariate_sprint(-epsij_new,time_matrix_int_new,nobs_new,cov_est,parameter,sqrt(parameter/(2-parameter))*CL)
        chartij_new<-chart_output[[1]]
        eeij_new<- -chart_output[[2]]
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     standardized_values=eeij_new)
        return(result)
      }
    }else if(side=="both"){
      if(chart=="CUSUM"){
        chart_upward_output<-chart_CUSUM_univariate_sprint(epsij_new,time_matrix_int_new,nobs_new,cov_est,parameter,CL)
        chart_downward_output<-chart_CUSUM_univariate_sprint(-epsij_new,time_matrix_int_new,nobs_new,cov_est,parameter,CL)
        chartij_upward_new<-chart_upward_output[[1]]
        chartij_downward_new<- -chart_downward_output[[1]]
        eeij_upward_new<-chart_upward_output[[2]]
        eeij_downward_new<- -chart_downward_output[[2]]
        chartij_new<-pmax(chartij_upward_new,-chartij_downward_new)
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     chart_upward=chartij_upward_new,
                     chart_downward=chartij_downward_new,
                     standardized_values_upward=eeij_upward_new,
                     standardized_values_downward=eeij_downward_new)
        return(result)
      }else if(chart=="EWMA"){
        chart_upward_output<-chart_EWMA_univariate_sprint(epsij_new,time_matrix_int_new,nobs_new,cov_est,parameter,sqrt(parameter/(2-parameter))*CL)
        chart_downward_output<-chart_EWMA_univariate_sprint(-epsij_new,time_matrix_int_new,nobs_new,cov_est,parameter,sqrt(parameter/(2-parameter))*CL)
        chartij_upward_new<-chart_upward_output[[1]]
        chartij_downward_new<- -chart_downward_output[[1]]
        eeij_upward_new<-chart_upward_output[[2]]
        eeij_downward_new<- -chart_downward_output[[2]]
        chartij_new<-pmax(chartij_upward_new,-chartij_downward_new)
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     chart_upward=chartij_upward_new,
                     chart_downward=chartij_downward_new,
                     standardized_values_upward=eeij_upward_new,
                     standardized_values_downward=eeij_downward_new)
        return(result)
      }
    }
  }
  
  #############################
  # distribution and standard #
  #############################
  
  if(method=="distribution and standard"){
    if(!inherits(pattern,c("pattern_long_1d_dist","pattern_long_1d_distvarcov")))
      stop("Pattern does not match method.")
    yy_ref<-pattern$yy_ref
    tt_ref<-pattern$tt_ref
    starting_idx<-pattern$starting_idx
    ending_idx<-pattern$ending_idx
    upper_line<-pattern$upper_line
    hh_t<-pattern$hh_t
    hh_y<-pattern$hh_y
    zzij_new<-local_const_percentile_est_faster(
      data_matrix_new,time_matrix_int_new,nobs_new,yy_ref,tt_ref,
      starting_idx,ending_idx,upper_line,n_time_units,hh_t,hh_y)
    if(side=="upward"){
      if(chart=="CUSUM"){
        chartij_new<-chart_CUSUM_univariate_std(zzij_new,time_matrix_int_new,nobs_new,parameter,CL)
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     standardized_values=zzij_new)
        return(result)
      }else if(chart=="EWMA"){
        chartij_new<-chart_EWMA_univariate_std(zzij_new,time_matrix_int_new,nobs_new,parameter,sqrt(parameter/(2-parameter))*CL)
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     standardized_values=zzij_new)
        return(result)
      }
    }else if(side=="downward"){
      if(chart=="CUSUM"){
        chartij_new<-chart_CUSUM_univariate_std(-zzij_new,time_matrix_int_new,nobs_new,parameter,CL)
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     standardized_values=zzij_new)
        return(result)
      }else if(chart=="EWMA"){
        chartij_new<-chart_EWMA_univariate_std(-zzij_new,time_matrix_int_new,nobs_new,parameter,sqrt(parameter/(2-parameter))*CL)
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     standardized_values=zzij_new)
        return(result)
      }
    }else if(side=="both"){
      if(chart=="CUSUM"){
        chart_upward_output<-chart_CUSUM_univariate_std(zzij_new,time_matrix_int_new,nobs_new,parameter,CL)
        chart_downward_output<-chart_CUSUM_univariate_std(zzij_new,time_matrix_int_new,nobs_new,parameter,CL)
        chartij_upward_new<-chart_upward_output[[1]]
        chartij_downward_new<- -chart_downward_output[[1]]
        eeij_upward_new<-chart_upward_output[[2]]
        eeij_downward_new<- -chart_downward_output[[2]]
        chartij_new<-pmax(chartij_upward_new,-chartij_downward_new)
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     chart_upward=chartij_upward_new,
                     chart_downward=chartij_downward_new,
                     standardized_values_upward=eeij_upward_new,
                     standardized_values_downward=eeij_downward_new)
        return(result)
      }else if(chart=="EWMA"){
        chart_upward_output<-chart_EWMA_univariate_std(zzij_new,time_matrix_int_new,nobs_new,parameter,sqrt(parameter/(2-parameter))*CL)
        chart_downward_output<-chart_EWMA_univariate_std(zzij_new,time_matrix_int_new,nobs_new,parameter,sqrt(parameter/(2-parameter))*CL)
        chartij_upward_new<-chart_upward_output[[1]]
        chartij_downward_new<- -chart_downward_output[[1]]
        eeij_upward_new<-chart_upward_output[[2]]
        eeij_downward_new<- -chart_downward_output[[2]]
        chartij_new<-pmax(chartij_upward_new,-chartij_downward_new)
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     chart_upward=chartij_upward_new,
                     chart_downward=chartij_downward_new,
                     standardized_values_upward=eeij_upward_new,
                     standardized_values_downward=eeij_downward_new)
        return(chartij_new)
      }
    }
  }
  
  ##################################
  # distribution and decorrelation #
  ##################################
  
  if(method=="distribution and decorrelation"){
    if(!inherits(pattern,"pattern_long_1d_distvarcov"))
      stop("Pattern does not match method.")
    yy_ref<-pattern$yy_ref
    tt_ref<-pattern$tt_ref
    starting_idx<-pattern$starting_idx
    ending_idx<-pattern$ending_idx
    upper_line<-pattern$upper_line
    cov_est<-pattern$cov_est
    hh_t<-pattern$hh_t
    hh_y<-pattern$hh_y
    hh_cov<-pattern$hh_cov
    zzij_new<-local_const_percentile_est_faster(
      data_matrix_new,time_matrix_int_new,nobs_new,yy_ref,tt_ref,
      starting_idx,ending_idx,upper_line,n_time_units,hh_t,hh_y)
    if(side=="upward"){
      if(chart=="CUSUM"){
        chart_output<-chart_CUSUM_univariate_dec(zzij_new,time_matrix_int_new,nobs_new,cov_est,parameter,CL)
        chartij_new<-chart_output[[1]]
        eeij_new<-chart_output[[2]]
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     standardized_values=eeij_new)
        return(result)
      }else if(chart=="EWMA"){
        chart_output<-chart_EWMA_univariate_dec(zzij_new,time_matrix_int_new,nobs_new,cov_est,parameter,sqrt(parameter/(2-parameter))*CL)
        chartij_new<-chart_output[[1]]
        eeij_new<-chart_output[[2]]
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     standardized_values=eeij_new)
        return(result)
      }
    }else if(side=="downward"){
      if(chart=="CUSUM"){
        chart_output<-chart_CUSUM_univariate_dec(-zzij_new,time_matrix_int_new,nobs_new,cov_est,parameter,CL)
        chartij_new<-chart_output[[1]]
        eeij_new<- -chart_output[[2]]
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     standardized_values=eeij_new)
        return(result)
      }else if(chart=="EWMA"){
        chart_output<-chart_EWMA_univariate_dec(-zzij_new,time_matrix_int_new,nobs_new,cov_est,parameter,sqrt(parameter/(2-parameter))*CL)
        chartij_new<-chart_output[[1]]
        eeij_new<- -chart_output[[2]]
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     standardized_values=eeij_new)
        return(result)
      }
    }else if(side=="both"){
      if(chart=="CUSUM"){
        chart_upward_output<-chart_CUSUM_univariate_dec(zzij_new,time_matrix_int_new,nobs_new,cov_est,parameter,CL)
        chart_downward_output<-chart_CUSUM_univariate_dec(-zzij_new,time_matrix_int_new,nobs_new,cov_est,parameter,CL)
        chartij_upward_new<-chart_upward_output[[1]]
        chartij_downward_new<- -chart_downward_output[[1]]
        eeij_upward_new<-chart_upward_output[[2]]
        eeij_downward_new<- -chart_downward_output[[2]]
        chartij_new<-pmax(chartij_upward_new,-chartij_downward_new)
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     chart_upward=chartij_upward_new,
                     chart_downward=chartij_downward_new,
                     standardized_values_upward=eeij_upward_new,
                     standardized_values_downward=eeij_downward_new)
        return(result)
      }else if(chart=="EWMA"){
        chart_upward_output<-chart_EWMA_univariate_dec(zzij_new,time_matrix_int_new,nobs_new,cov_est,parameter,sqrt(parameter/(2-parameter))*CL)
        chart_downward_output<-chart_EWMA_univariate_dec(-zzij_new,time_matrix_int_new,nobs_new,cov_est,parameter,sqrt(parameter/(2-parameter))*CL)
        chartij_upward_new<-chart_upward_output[[1]]
        chartij_downward_new<- -chart_downward_output[[1]]
        eeij_upward_new<-chart_upward_output[[2]]
        eeij_downward_new<- -chart_downward_output[[2]]
        chartij_new<-pmax(chartij_upward_new,-chartij_downward_new)
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     chart_upward=chartij_upward_new,
                     chart_downward=chartij_downward_new,
                     standardized_values_upward=eeij_upward_new,
                     standardized_values_downward=eeij_downward_new)
        return(result)
      }
    }
  }
  
  ###########################
  # distribution and sprint #
  ###########################
  
  if(method=="distribution and sprint"){
    if(!inherits(pattern,"pattern_long_1d_distvarcov"))
      stop("Pattern does not match method.")
    yy_ref<-pattern$yy_ref
    tt_ref<-pattern$tt_ref
    starting_idx<-pattern$starting_idx
    ending_idx<-pattern$ending_idx
    upper_line<-pattern$upper_line
    cov_est<-pattern$cov_est
    hh_t<-pattern$hh_t
    hh_y<-pattern$hh_y
    hh_cov<-pattern$hh_cov
    zzij_new<-local_const_percentile_est_faster(
      data_matrix_new,time_matrix_int_new,nobs_new,yy_ref,tt_ref,
      starting_idx,ending_idx,upper_line,n_time_units,hh_t,hh_y)
    if(side=="upward"){
      if(chart=="CUSUM"){
        chart_output<-chart_CUSUM_univariate_sprint(zzij_new,time_matrix_int_new,nobs_new,cov_est,parameter,CL)
        chartij_new<-chart_output[[1]]
        eeij_new<-chart_output[[2]]
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     standardized_values=eeij_new)
        return(result)
      }else if(chart=="EWMA"){
        chart_output<-chart_EWMA_univariate_sprint(zzij_new,time_matrix_int_new,nobs_new,cov_est,parameter,sqrt(parameter/(2-parameter))*CL)
        chartij_new<-chart_output[[1]]
        eeij_new<-chart_output[[2]]
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     standardized_values=eeij_new)
        return(result)
      }
    }else if(side=="downward"){
      if(chart=="CUSUM"){
        chart_output<-chart_CUSUM_univariate_sprint(-zzij_new,time_matrix_int_new,nobs_new,cov_est,parameter,CL)
        chartij_new<-chart_output[[1]]
        eeij_new<- -chart_output[[2]]
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     standardized_values=eeij_new)
        return(result)
      }else if(chart=="EWMA"){
        chart_output<-chart_EWMA_univariate_sprint(-zzij_new,time_matrix_int_new,nobs_new,cov_est,parameter,sqrt(parameter/(2-parameter))*CL)
        chartij_new<-chart_output[[1]]
        eeij_new<- -chart_output[[2]]
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     standardized_values=eeij_new)
        return(result)
      }
    }else if(side=="both"){
      if(chart=="CUSUM"){
        chart_upward_output<-chart_CUSUM_univariate_sprint(zzij_new,time_matrix_int_new,nobs_new,cov_est,parameter,CL)
        chart_downward_output<-chart_CUSUM_univariate_sprint(-zzij_new,time_matrix_int_new,nobs_new,cov_est,parameter,CL)
        chartij_upward_new<-chart_upward_output[[1]]
        chartij_downward_new<- -chart_downward_output[[1]]
        eeij_upward_new<-chart_upward_output[[2]]
        eeij_downward_new<- -chart_downward_output[[2]]
        chartij_new<-pmax(chartij_upward_new,-chartij_downward_new)
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     chart_upward=chartij_upward_new,
                     chart_downward=chartij_downward_new,
                     standardized_values_upward=eeij_upward_new,
                     standardized_values_downward=eeij_downward_new)
        return(result)
      }else if(chart=="EWMA"){
        chart_upward_output<-chart_EWMA_univariate_sprint(zzij_new,time_matrix_int_new,nobs_new,cov_est,parameter,sqrt(parameter/(2-parameter))*CL)
        chart_downward_output<-chart_EWMA_univariate_sprint(-zzij_new,time_matrix_int_new,nobs_new,cov_est,parameter,sqrt(parameter/(2-parameter))*CL)
        chartij_upward_new<-chart_upward_output[[1]]
        chartij_downward_new<- -chart_downward_output[[1]]
        eeij_upward_new<-chart_upward_output[[2]]
        eeij_downward_new<- -chart_downward_output[[2]]
        chartij_new<-pmax(chartij_upward_new,-chartij_downward_new)
        result<-list(chart=chartij_new,
                     time_matrix_int=time_matrix_int_new,
                     chart_upward=chartij_upward_new,
                     chart_downward=chartij_downward_new,
                     standardized_values_upward=eeij_upward_new,
                     standardized_values_downward=eeij_downward_new)
        return(result)
      }
    }
  }
  
  ##############################
  # nonparametric and standard #
  ##############################
  
  if(method=="nonparametric and standard"){
    stop("Method 'nonparametric and standard' currently not supported")
  }
  
  ###################################
  # nonparametric and decorrelation #
  ###################################
  
  if(method=="nonparametric and decorrelation"){
    stop("Method 'nonparametric and decorrelation' currently not supported")
  }
}
