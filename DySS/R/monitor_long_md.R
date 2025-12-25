##' Monitor Multivariate Longitudinal Data
##'
##' @title Monitor Multivariate Longitudinal Data
##' @param data_array_new an array of longitudinal observations. \cr
##' \code{data_array_new[i,j,k]} is the jth observation of the kth dimension of the ith subject.
##' @param time_matrix_new a matrix of observation times. \cr
##' \code{time_matrix_new[i,j]} is the jth observation time of the ith subject. \cr
##' \code{data_array_new[i,j,]} is observed at \code{time_matrix[i,j]}.
##' @param nobs_new an integer vector for number of observations. \cr
##' \code{nobs_new[i]} is the number of observations for the ith subject.
##' @param pattern the estimated regular longitudinal pattern
##' @param side a string \cr
##' If \code{side="upward"}, control charts aim to detect upward shifts. \cr
##' If \code{side="downward"}, control charts aim to detect downward shifts. \cr
##' If \code{side="both"}, control charts aim to detect shifts in both sides.
##' @param method a string \cr
##' If \code{method="simultaneous CUSUM"}, 
##' apply simultaneous CUSUM charts. (See SIMUL in You et al, 2020.) \cr
##' If \code{method="simultaneous EWMA"},
##' apply simultaneous EWMA charts. (See SIMUL in You et al, 2020.) \cr
##' If \code{method="multivariate CUSUM"},
##' apply multivariate CUSUM charts. \cr
##' If \code{method="multivariate EWMA"},
##' apply multivariate EWMA charts. (See Qiu and Xiang, 2015 or QX-1S/QS-2S in You et al, 2020.) \cr
##' If \code{method="decorrelation CUSUM"},
##' apply decorrelation CUSUM charts. (See Li and Qiu, 2017 or LQ-1S/LQ-2S in You et al, 2020) \cr
##' If \code{method="decorrelation EWMA"},
##' apply decorrelation EWMA charts. (See Li and Qiu, 2017 or LQ-1S/LQ-2S in You et al, 2020) \cr
##' If \code{method="nonparametric CUSUM"} \cr
##' If \code{method="nonparametric EWMA"} \cr
##' @param parameter a numeric value. \cr
##' \code{parameter} is the allowance constant if \code{method} is a CUSUM chart. \cr
##' \code{parameter} is the weighting parameter if \code{method} is an EWMA chart.
##' @param CL a numeric value \cr
##' \code{CL} is the control limit. 
##' A signal will be given if charting statistics are larger than the control limit. 
##' (Note: in this package, signs of charting statistics may be reversed such that 
##' larger values of charting statistics indicate worse performance of processes.)
##' After the signal is given, the algorithm stops calculating the charting statistics for the remaining observation times.
##' The default value of control limit is infinity, which means we will calculate the charting statistics for all observation times.
##' @return a list that stores the result.\cr
##' \item{$chart}{a numeric matrix, \code{$chart[i,j]} is the jth charting statistic of the ith subject calculated at time \code{time_matrix_new[i,j]}.}\cr
##' \item{$SSijk}{a numeric array, the multivariate statistics used in the calculation of control charts.
##' \code{$SSijk[i,j,]} is the jth multivariate statistic for the ith subject.}
##' \item{$standardized_values}{a numeric array.
##' \code{$standardized_values[i,j,]} is the jth standardized vector for the ith subject.}
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
##' 
##' result_monitoring<-monitor_long_md(
##' data_array_new=data_example_long_md$data_array_OC,
##' time_matrix_new=data_example_long_md$time_matrix_OC,
##' nobs_new=data_example_long_md$nobs_OC,
##' pattern=result_pattern,
##' side="both",
##' method="multivariate EWMA",
##' parameter=0.5)
##' 
##' result_ATS<-calculate_ATS(
##'   chart_matrix=result_monitoring$chart_matrix,
##'   time_matrix=data_example_long_md$time_matrix_OC,
##'   nobs=data_example_long_md$nobs_OC,
##'   starttime=rep(0,nrow(data_example_long_md$time_matrix_OC)),
##'   endtime=rep(1,nrow(data_example_long_md$time_matrix_OC)),
##'   design_interval=data_example_long_md$design_interval,
##'   n_time_units=data_example_long_md$n_time_units,
##'   CL=16.0)
monitor_long_md<-function(
  data_array_new,time_matrix_new,nobs_new,
  pattern,side="both",method="multivariate EWMA",
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
  
  if(!side%in%c("upward","downward","both"))stop("Error in argument 'side'.")
  
  all_methods<-c("simultaneous CUSUM",
                 "simultaneous EWMA",
                 "multivariate CUSUM",
                 "multivariate EWMA",
                 "decorrelation CUSUM",
                 "decorrelation EWMA",
                 "nonparametric CUSUM",
                 "nonparametric EWMA")
  if(!method%in%all_methods)stop("Error in argument 'method'.")
  
  ######################
  # simultaneous CUSUM #
  ######################
  
  if(method=="simultaneous CUSUM"){
    if(!inherits(pattern,c("pattern_long_md_meanvar","pattern_long_md_meanvarcov")))
      stop("Pattern does not match method.")
    mean_est<-pattern$mean_est
    var_est<-pattern$var_est
    sd_est<-matrix(NA,n_time_units,ndim)
    for(kk in 1:ndim)sd_est[,kk]<-sqrt(var_est[,kk,kk])
    eeijk_new<-array(NA,c(nind,nmaxobs,ndim))
    for(ii in 1:nind){
      eeijk_new[ii,,]<-(data_array_new[ii,,]-mean_est[time_matrix_int_new[ii,],])/
        sd_est[time_matrix_int_new[ii,],]
    }
    if(side=="both"){
      CCij_new<-matrix(0.0,nind,nmaxobs)
      SSijk_upward_new<-array(0.0,c(nind,nmaxobs,ndim))
      SSijk_downward_new<-array(0.0,c(nind,nmaxobs,ndim))
      chart_output<-f90_mchart_simultaneous_CUSUM_both_wrap(
        eeijk_new,nobs_new,nind,nmaxobs,ndim,parameter,CL,
        CCij_new,SSijk_upward_new,SSijk_downward_new)
      chartij_new<-chart_output[[1]]
      SSijk_upward_new<-chart_output[[2]]
      SSijk_downward_new<-chart_output[[3]]
      result<-list(chart_matrix=chartij_new,
                   time_matrix_int=time_matrix_int_new,
                   SSijk_upward=SSijk_upward_new,
                   SSijk_downward=SSijk_downward_new,
                   standardized_values=eeijk_new)
      return(result)
    }else if(side=="upward"){
      CCij_new<-matrix(0.0,nind,nmaxobs)
      SSijk_upward_new<-array(0.0,c(nind,nmaxobs,ndim))
      chart_output<-f90_mchart_simultaneous_CUSUM_upward_wrap(
        eeijk_new,nobs_new,nind,nmaxobs,ndim,parameter,CL,
        CCij_new,SSijk_upward_new)
      chartij_new<-chart_output[[1]]
      SSijk_upward_new<-chart_output[[2]]
      result<-list(chart_matrix=chartij_new,
                   time_matrix_int=time_matrix_int_new,
                   SSijk_upward=SSijk_upward_new,
                   standardized_values=eeijk_new)
      return(result)
    }else if(side=="downward"){
      CCij_new<-matrix(0.0,nind,nmaxobs)
      SSijk_downward_new<-array(0.0,c(nind,nmaxobs,ndim))
      chart_output<-f90_mchart_simultaneous_CUSUM_upward_wrap(
        -eeijk_new,nobs_new,nind,nmaxobs,ndim,parameter,CL,
        CCij_new,SSijk_downward_new)
      chartij_new<-chart_output[[1]]
      SSijk_downward_new<-chart_output[[2]]
      result<-list(chart_matrix=chartij_new,
                   time_matrix_int=time_matrix_int_new,
                   SSijk_downward=SSijk_downward_new,
                   standardized_values=eeijk_new)
      return(result)
    }
  }
  
  #####################
  # simultaneous EWMA #
  #####################
  
  if(method=="simultaneous EWMA"){
    if(!inherits(pattern,c("pattern_long_md_meanvar","pattern_long_md_meanvarcov")))
      stop("Pattern does not match method.")
    mean_est<-pattern$mean_est
    var_est<-pattern$var_est
    sd_est<-matrix(NA,n_time_units,ndim)
    for(kk in 1:ndim)sd_est[,kk]<-sqrt(var_est[,kk,kk])
    eeijk_new<-array(NA,c(nind,nmaxobs,ndim))
    for(ii in 1:nind){
      eeijk_new[ii,,]<-(data_array_new[ii,,]-mean_est[time_matrix_int_new[ii,],])/
        sd_est[time_matrix_int_new[ii,],]
    }
    if(side=="both"){
      CCij_new<-matrix(0.0,nind,nmaxobs)
      SSijk_new<-array(0.0,c(nind,nmaxobs,ndim))
      chart_output<-f90_mchart_simultaneous_EWMA_both_wrap(
        eeijk_new,nobs_new,nind,nmaxobs,ndim,parameter,CL,
        CCij_new,SSijk_new)
      chartij_new<-chart_output[[1]]
      SSijk<-chart_output[[2]]
      result<-list(chart_matrix=chartij_new,
                   time_matrix_int=time_matrix_int_new,
                   SSijk=SSijk_new,
                   standardized_values=eeijk_new)
      return(result)
    }else if(side=="upward"){
      CCij_new<-matrix(0.0,nind,nmaxobs)
      SSijk_new<-array(0.0,c(nind,nmaxobs,ndim))
      chart_output<-f90_mchart_simultaneous_EWMA_upward_wrap(
        eeijk_new,nobs_new,nind,nmaxobs,ndim,parameter,CL,
        CCij_new,SSijk_new)
      chartij_new<-chart_output[[1]]
      SSijk<-chart_output[[2]]
      result<-list(chart_matrix=chartij_new,
                   time_matrix_int=time_matrix_int_new,
                   SSijk=SSijk_new,
                   standardized_values=eeijk_new)
      return(result)
    }else if(side=="downward"){
      CCij_new<-matrix(0.0,nind,nmaxobs)
      SSijk_new<-array(0.0,c(nind,nmaxobs,ndim))
      chart_output<-f90_mchart_simultaneous_EWMA_upward_wrap(
        -eeijk_new,nobs_new,nind,nmaxobs,ndim,parameter,CL,
        CCij_new,SSijk_new)
      chartij_new<-chart_output[[1]]
      SSijk<-chart_output[[2]]
      result<-list(chart_matrix=chartij_new,
                   time_matrix_int=time_matrix_int_new,
                   SSijk=SSijk_new,
                   standardized_values=eeijk_new)
      return(result)
    }
  }
  
  ######################
  # multivariate CUSUM #
  ######################
  
  if(method=="multivariate CUSUM"){
    if(!inherits(pattern,c("pattern_long_md_meanvar","pattern_long_md_meanvarcov")))
      stop("Pattern does not match method.")
    mean_est<-pattern$mean_est
    var_est<-pattern$var_est
    epsijk_new<-array(NA,c(nind,nmaxobs,ndim))
    for(ii in 1:nind){
      epsijk_new[ii,,]<-data_array_new[ii,,]-mean_est[time_matrix_int_new[ii,],]
    }
    if(side=="both"){
      CCij_new<-matrix(NA,nind,nmaxobs)
      SSijk_new<-array(NA,c(nind,nmaxobs,ndim))
      for(ii in 1:nind){
        eps_ii<-t(epsijk_new[ii,,])
        var_cube_ii<-var_est[time_matrix_int_new[ii,],,]
        chart_output<-mchart1_multivariate_CUSUM_multivariate_both(
          eps_ii,nobs_new[ii],var_cube_ii,ndim,parameter,CL)
        CCij_new[ii,1:nobs_new[ii]]<-chart_output[[1]]
        SSijk_new[ii,1:nobs_new[ii],]<-t(chart_output[[2]])
      }
      result<-list(chart_matrix=CCij_new,
                   time_matrix_int=time_matrix_int_new,
                   SSijk=SSijk_new)
      return(result)
    }else if(side=="upward"){
      CCij_new<-matrix(NA,nind,nmaxobs)
      SSijk_new<-array(NA,c(nind,nmaxobs,ndim))
      for(ii in 1:nind){
        eps_ii<-t(epsijk_new[ii,,])
        var_cube_ii<-var_est[time_matrix_int_new[ii,],,]
        chart_output<-mchart1_multivariate_CUSUM_multivariate_upward(
          eps_ii,nobs_new[ii],var_cube_ii,ndim,parameter,CL)
        CCij_new[ii,1:nobs_new[ii]]<-chart_output[[1]]
        SSijk_new[ii,1:nobs_new[ii],]<-t(chart_output[[2]])
      }
      result<-list(chart_matrix=CCij_new,
                   time_matrix_int=time_matrix_int_new,
                   SSijk=SSijk_new)
      return(result)
    }else if(side=="downward"){
      CCij_new<-matrix(NA,nind,nmaxobs)
      SSijk_new<-array(NA,c(nind,nmaxobs,ndim))
      for(ii in 1:nind){
        eps_ii<-t(epsijk_new[ii,,])
        var_cube_ii<-var_est[time_matrix_int_new[ii,],,]
        chart_output<-mchart1_multivariate_CUSUM_multivariate_upward(
          -eps_ii,nobs_new[ii],var_cube_ii,ndim,parameter,CL)
        CCij_new[ii,1:nobs_new[ii]]<-chart_output[[1]]
        SSijk_new[ii,1:nobs_new[ii],]<-t(chart_output[[2]])
      }
      result<-list(chart_matrix=CCij_new,
                   time_matrix_int=time_matrix_int_new,
                   SSijk=SSijk_new)
      return(result)
    }
  }
  
  #####################
  # multivariate EWMA #
  #####################
  
  if(method=="multivariate EWMA"){
    if(!inherits(pattern,c("pattern_long_md_meanvar","pattern_long_md_meanvarcov")))
      stop("Pattern does not match method.")
    mean_est<-pattern$mean_est
    var_est<-pattern$var_est
    epsijk_new<-array(NA,c(nind,nmaxobs,ndim))
    for(ii in 1:nind){
      epsijk_new[ii,,]<-data_array_new[ii,,]-mean_est[time_matrix_int_new[ii,],]
    }
    if(side=="both"){
      CCij_new<-matrix(NA,nind,nmaxobs)
      SSijk_new<-array(NA,c(nind,nmaxobs,ndim))
      eeijk_new<-array(NA,c(nind,nmaxobs,ndim))
      for(ii in 1:nind){
        eps_ii<-t(epsijk_new[ii,,])
        var_cube_ii<-var_est[time_matrix_int_new[ii,],,]
        chart_output<-mchart1_multivariate_EWMA_multivariate_both(
          eps_ii,nobs_new[ii],var_cube_ii,ndim,parameter,CL)
        CCij_new[ii,1:nobs_new[ii]]<-chart_output[[1]]
        SSijk_new[ii,1:nobs_new[ii],]<-t(chart_output[[2]])
        eeijk_new[ii,1:nobs_new[ii],]<-t(chart_output[[3]])
      }
      result<-list(chart_matrix=CCij_new,
                   time_matrix_int=time_matrix_int_new,
                   SSijk=SSijk_new,
                   standardized_values=eeijk_new)
      return(result)
    }else if(side=="upward"){
      CCij_new<-matrix(NA,nind,nmaxobs)
      SSijk_new<-array(NA,c(nind,nmaxobs,ndim))
      eeijk_new<-array(NA,c(nind,nmaxobs,ndim))
      for(ii in 1:nind){
        eps_ii<-t(epsijk_new[ii,,])
        var_cube_ii<-var_est[time_matrix_int_new[ii,],,]
        chart_output<-mchart1_multivariate_EWMA_multivariate_upward(
          eps_ii,nobs_new[ii],var_cube_ii,ndim,parameter,CL)
        CCij_new[ii,1:nobs_new[ii]]<-chart_output[[1]]
        SSijk_new[ii,1:nobs_new[ii],]<-t(chart_output[[2]])
        eeijk_new[ii,1:nobs_new[ii],]<-t(chart_output[[3]])
      }
      result<-list(chart_matrix=CCij_new,
                   time_matrix_int=time_matrix_int_new,
                   SSijk=SSijk_new,
                   standardized_values=eeijk_new)
      return(result)
    }else if(side=="downward"){
      CCij_new<-matrix(NA,nind,nmaxobs)
      SSijk_new<-array(NA,c(nind,nmaxobs,ndim))
      eeijk_new<-array(NA,c(nind,nmaxobs,ndim))
      for(ii in 1:nind){
        eps_ii<-t(epsijk_new[ii,,])
        var_cube_ii<-var_est[time_matrix_int_new[ii,],,]
        chart_output<-mchart1_multivariate_CUSUM_multivariate_upward(
          -eps_ii,nobs_new[ii],var_cube_ii,ndim,parameter,CL)
        CCij_new[ii,1:nobs_new[ii]]<-chart_output[[1]]
        SSijk_new[ii,1:nobs_new[ii],]<-t(chart_output[[2]])
        eeijk_new[ii,1:nobs_new[ii],]<-t(chart_output[[3]])
      }
      result<-list(chart_matrix=CCij_new,
                   time_matrix_int=time_matrix_int_new,
                   SSijk=SSijk_new,
                   standardized_values=eeijk_new)
      return(result)
    }
  }
  
  #######################
  # decorrelation CUSUM #
  #######################
  
  if(method=="decorrelation CUSUM"){
    if(!inherits(pattern,c("pattern_long_md_meanvarcov")))
      stop("Pattern does not match method.")
    mean_est<-pattern$mean_est
    var_est<-pattern$var_est
    cov_est<-pattern$cov_est
    cov_mat_est<-matrix(aperm(cov_est,c(3,1,4,2)),n_time_units*ndim,n_time_units*ndim)
    epsijk_new<-array(NA,c(nind,nmaxobs,ndim))
    for(ii in 1:nind){
      epsijk_new[ii,,]<-data_array_new[ii,,]-mean_est[time_matrix_int_new[ii,],]
    }
    if(side=="both"){
      CCij_new<-matrix(NA,nind,nmaxobs)
      SSijk_new<-array(NA,c(nind,nmaxobs,ndim))
      eeijk_new<-array(NA,c(nind,nmaxobs,ndim))
      for(ii in 1:nind){
        idx_ii<-rep(time_matrix_int_new[ii,]-1,each=ndim)*ndim+1:ndim
        cov_ii<-cov_mat_est[idx_ii,idx_ii]
        eps_ii<-c(t(epsijk_new[ii,1:nobs_new[ii],]))
        chart_output<-mchart1_decorrelation_CUSUM_multivariate_both(
          eps_ii,cov_ii,nobs_new[ii],ndim,parameter,CL)
        CCij_new[ii,1:nobs_new[ii]]<-chart_output[[1]]
        SSijk_new[ii,1:nobs_new[ii],]<-t(chart_output[[2]])
        eeijk_new[ii,1:nobs_new[ii],]<-t(chart_output[[3]])
      }
      result<-list(chart_matrix=CCij_new,
                   time_matrix_int=time_matrix_int_new,
                   SSijk=SSijk_new,
                   standardized_values=eeijk_new)
      return(result)
    }else if(side=="upward"){
      CCij_new<-matrix(NA,nind,nmaxobs)
      SSijk_new<-array(NA,c(nind,nmaxobs,ndim))
      eeijk_new<-array(NA,c(nind,nmaxobs,ndim))
      for(ii in 1:nind){
        idx_ii<-rep(time_matrix_int_new[ii,]-1,each=ndim)*ndim+1:ndim
        cov_ii<-cov_mat_est[idx_ii,idx_ii]
        eps_ii<-c(t(epsijk_new[ii,1:nobs_new[ii],]))
        chart_output<-mchart1_decorrelation_CUSUM_multivariate_upward(
          eps_ii,cov_ii,nobs_new[ii],ndim,parameter,CL)
        CCij_new[ii,1:nobs_new[ii]]<-chart_output[[1]]
        SSijk_new[ii,1:nobs_new[ii],]<-t(chart_output[[2]])
        eeijk_new[ii,1:nobs_new[ii],]<-t(chart_output[[3]])
      }
      result<-list(chart_matrix=CCij_new,
                   time_matrix_int=time_matrix_int_new,
                   SSijk=SSijk_new,
                   standardized_values=eeijk_new)
      return(result)
    }else if(side=="downward"){
      CCij_new<-matrix(NA,nind,nmaxobs)
      SSijk_new<-array(NA,c(nind,nmaxobs,ndim))
      eeijk_new<-array(NA,c(nind,nmaxobs,ndim))
      for(ii in 1:nind){
        idx_ii<-rep(time_matrix_int_new[ii,]-1,each=ndim)*ndim+1:ndim
        cov_ii<-cov_mat_est[idx_ii,idx_ii]
        eps_ii<-c(t(epsijk_new[ii,1:nobs_new[ii],]))
        chart_output<-mchart1_decorrelation_CUSUM_multivariate_upward(
          -eps_ii,cov_ii,nobs_new[ii],ndim,parameter,CL)
        CCij_new[ii,1:nobs_new[ii]]<-chart_output[[1]]
        SSijk_new[ii,1:nobs_new[ii],]<-t(chart_output[[2]])
        eeijk_new[ii,1:nobs_new[ii],]<-t(chart_output[[3]])
      }
      result<-list(chart_matrix=CCij_new,
                   time_matrix_int=time_matrix_int_new,
                   SSijk=SSijk_new,
                   standardized_values=-eeijk_new)
      return(result)
    }
  }
  
  ######################
  # decorrelation EWMA #
  ######################
  
  if(method=="decorrelation EWMA"){
    if(!inherits(pattern,c("pattern_long_md_meanvarcov")))
      stop("Pattern does not match method.")
    mean_est<-pattern$mean_est
    var_est<-pattern$var_est
    cov_est<-pattern$cov_est
    cov_mat_est<-matrix(aperm(cov_est,c(3,1,4,2)),n_time_units*ndim,n_time_units*ndim)
    epsijk_new<-array(NA,c(nind,nmaxobs,ndim))
    for(ii in 1:nind){
      epsijk_new[ii,,]<-data_array_new[ii,,]-mean_est[time_matrix_int_new[ii,],]
    }
    if(side=="both"){
      CCij_new<-matrix(NA,nind,nmaxobs)
      SSijk_new<-array(NA,c(nind,nmaxobs,ndim))
      eeijk_new<-array(NA,c(nind,nmaxobs,ndim))
      for(ii in 1:nind){
        idx_ii<-rep(time_matrix_int_new[ii,]-1,each=ndim)*ndim+1:ndim
        cov_ii<-cov_mat_est[idx_ii,idx_ii]
        eps_ii<-c(t(epsijk_new[ii,1:nobs_new[ii],]))
        chart_output<-mchart1_decorrelation_EWMA_multivariate_both(
          eps_ii,cov_ii,nobs_new[ii],ndim,parameter,CL)
        CCij_new[ii,1:nobs_new[ii]]<-chart_output[[1]]
        SSijk_new[ii,1:nobs_new[ii],]<-t(chart_output[[2]])
        eeijk_new[ii,1:nobs_new[ii],]<-t(chart_output[[3]])
      }
      result<-list(chart_matrix=CCij_new,
                   time_matrix_int=time_matrix_int_new,
                   SSijk=SSijk_new,
                   standardized_values=eeijk_new)
      return(result)
    }else if(side=="upward"){
      CCij_new<-matrix(NA,nind,nmaxobs)
      SSijk_new<-array(NA,c(nind,nmaxobs,ndim))
      eeijk_new<-array(NA,c(nind,nmaxobs,ndim))
      for(ii in 1:nind){
        idx_ii<-rep(time_matrix_int_new[ii,]-1,each=ndim)*ndim+1:ndim
        cov_ii<-cov_mat_est[idx_ii,idx_ii]
        eps_ii<-c(t(epsijk_new[ii,1:nobs_new[ii],]))
        chart_output<-mchart1_decorrelation_EWMA_multivariate_upward(
          eps_ii,cov_ii,nobs_new[ii],ndim,parameter,CL)
        CCij_new[ii,1:nobs_new[ii]]<-chart_output[[1]]
        SSijk_new[ii,1:nobs_new[ii],]<-t(chart_output[[2]])
        eeijk_new[ii,1:nobs_new[ii],]<-t(chart_output[[3]])
      }
      result<-list(chart_matrix=CCij_new,
                   time_matrix_int=time_matrix_int_new,
                   SSijk=SSijk_new,
                   standardized_values=eeijk_new)
      return(result)
    }else if(side=="downward"){
      CCij_new<-matrix(NA,nind,nmaxobs)
      SSijk_new<-array(NA,c(nind,nmaxobs,ndim))
      eeijk_new<-array(NA,c(nind,nmaxobs,ndim))
      for(ii in 1:nind){
        idx_ii<-rep(time_matrix_int_new[ii,]-1,each=ndim)*ndim+1:ndim
        cov_ii<-cov_mat_est[idx_ii,idx_ii]
        eps_ii<-c(t(epsijk_new[ii,1:nobs_new[ii],]))
        chart_output<-mchart1_decorrelation_EWMA_multivariate_upward(
          -eps_ii,cov_ii,nobs_new[ii],ndim,parameter,CL)
        CCij_new[ii,1:nobs_new[ii]]<-chart_output[[1]]
        SSijk_new[ii,1:nobs_new[ii],]<-t(chart_output[[2]])
        eeijk_new[ii,1:nobs_new[ii],]<-t(chart_output[[3]])
      }
      result<-list(chart_matrix=CCij_new,
                   time_matrix_int=time_matrix_int_new,
                   SSijk=SSijk_new,
                   standardized_values=-eeijk_new)
      return(result)
    }
  }
  
  ##############################
  # nonparametric and standard #
  ##############################
  
  if(method=="nonparametric CUSUM"){
    stop("Method 'nonparametric CUSUM' currently not supported")
  }
  
  ##############################
  # nonparametric and standard #
  ##############################
  
  if(method=="nonparametric EWMA"){
    stop("Method 'nonparametric EWMA' currently not supported")
  }
}
