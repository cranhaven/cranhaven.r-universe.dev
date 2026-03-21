
#' Offline monitoring
#'
#'
#' Offline T2.var and W.var control charts for monitoring batch processes based on VAR model.
#' This approach is fully described in "Marcondes Filho, D., & Valk, M. (2020). Dynamic VAR Model-Based Control Charts
#'  for Batch Process Monitoring. European Journal of Operational Research."
#'
#'#' The maximum number of variables is five.
#'
#' All batches must have the same number of time-instants.
#'
#' The dataset of batches in "data" are considered to be in control
#'
#'
#' @param data  dataframe of reference dataset.
#' For each batch, variables are arranged in lines and columns are time-instants. The different batches are combined in a single dataset through
#' rbind
#' @param size  number of variables
#' @param newdata  dataframe of one or more new batches for monitoring (each with same number of variables and time instants of \code{data}).
#' Different batches are combined in a single dataset through rbind
#'
#' @param confidence.level H0 probability to be consider to define the quantile (default is 0.99)
#' @param type "T2.var" for Hotelling chart (default) and "W.var" for Generalized Variance chart
#' @param covvar "empirical" for sample covariance of coefficients (default) and "theoretical" for  estimated theoretical covariance
#' @param plot TRUE shows the charts plots (default TRUE)
#' @param var.estimates TRUE show informations about the VAR modeling phase (default FALSE)
#'
#' @return
#'
#' beyond.limits: returns the batches that the T2.var (or W.var) scores are above the control limit
#'
#'  LimT2 (or LimW): T2.var (or W.var) control limit
#'
#'  perc: perc_ref (and perc_new) returns the rate of reference batches (and new batches) above the control limit
#'        (perc_ref= g_ref/I and perc_new= g_new/Inew, where I (Inew) is the overall number of reference batches (new batches) and g_ref (g_new) is the number of reference batches (new batches) above the control limit)
#'
#'  arl: arl_ref (and arl_new) returns the mean number of reference batches (and new batches) before a signal is given by the charts
#'      (arl_ref=1/perc_ref and arl_new=1/perc_new)
#'
#'
#'  varest: If var.estimates=TRUE, it returns the matrices (vec.phis and vec.phis.new) in which each row contains the estimated VAR(1) phis for each reference batches and new batches, respectively;
#'  the matrices (vec.cov.theoretical and vec.cov.empirical) with  the  theoretical and empirical estimated phis covariances from the reference batches, respectively;
#'             the lists (cov.B1 and cov.B1new) of the theoretical estimated phis covariances of the reference and new batches, respectively;
#'             the number (I) of reference batches; the number (Inew) of new batches; and the number (n) of time-instants
#'
#' @examples
#'
#' # Example 1: Monitoring considering two variables and Inew= 10 in control batches
#'
#'    mydata=simoff()
#'    T2.var=offlinem(data=mydata$data,size=2)
#'    T2.var.new=offlinem(data=mydata$data,size=2,newdata=mydata$newdata)
#'    W.var=offlinem(data=mydata$data,size=2,type="W.var")
#'    W.var.new=offlinem(data=mydata$data,size=2,type="W.var",newdata=mydata$newdata)
#'
#'
#' # Example 2: Monitoring considering three variables and Inew=50 out of control batches
#'
#'    B1=matrix(c(-0.3,0,0.4,0,0.2,0,0,-0.1,0.5),3,byrow=TRUE)
#'    B1new=matrix(c(0.7,0,0.4,0,0.5,0,0,-0.1,0.5),3,byrow=TRUE)
#'    mydata2=simoff(n=100,I=100,size=3,Inew=50,B1,varcov=diag(3),B1new)
#'    T2.var=offlinem(data=mydata2$data,size=3)
#'    T2.var.new=offlinem(data=mydata2$data,size=3,newdata=mydata2$newdata)
#'    W.var=offlinem(data=mydata2$data,size=3,type="W.var")
#'    W.var.new=offlinem(data=mydata2$data,size=3,type="W.var",newdata=mydata2$newdata)
#'
#' @seealso simoff
#'
#' @export
offlinem=function(data,size,newdata=NULL, confidence.level=0.99, type="T2.var", covvar="empirical", plot=TRUE,var.estimates=FALSE){

  if(size<2 || size >5){stop("Error ... number of variables (size) must be between 2 and 5")}
  if(is.null(size)){stop("Error...size must be defined")}
  if(is.null(data)){stop("Error...data must be defined")}
  if(dim(data)[2]<50){stop("Error...Number of time-instants must be at least 50")}
  #if(dim(covvar)[1]!=size*size)stop(print("Error ... number of variables (size) must be according to the dimension of covvar matrix"))

   if (type=="T2.var"){

    #if ((dim(data)[1]/size)<30 & dim(data)[2]<100) {stop("Error...number of reference batches must be at least 30 for the number of time-instants less than 100")}
    if ((dim(data)[1]/size)<30 ) {stop("Error...number of reference batches must be at least 30")}

    }

  else {

    if((dim(data)[1]/size)<40){stop("Error... Number of batch samples must be at least 40")}
    #if((dim(data)[1]/size)>40 & (dim(data)[1]/size)<50 ){warning("Warning... Number of batch samples is not enough for a robust estimation")}

   }

    if(!is.null(newdata)){
       varest=varfitoff(data=data,size=size,newdata=newdata)
    mcharts=chartsoff(varest=varest,newdata=newdata,confidence.level=confidence.level,type=type,covvar=covvar,plot=plot)
    #mcharts$ARL=1/(sum((mcharts$violations$beyond.limits>=varest$I)*1)/dim(varest$vec.phis.new)[1])
    mcharts$LimT2
    mcharts$LimW

  }else{
    varest=varfitoff(data=data,size=size)
    mcharts=chartsoff(varest=varest,confidence.level=confidence.level,type=type,covvar=covvar,plot=plot)
    #mcharts$ARL=1/(length(mcharts$violations$beyond.limits)/dim(varest$vec.phis)[1])
    mcharts$LimT2
    mcharts$LimW
  }

  if(var.estimates==TRUE){
    mcharts$varest=varest

  }
  return(mcharts)
}




