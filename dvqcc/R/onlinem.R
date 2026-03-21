
#' Online monitoring
#'
#' Online T2.var and W.var control charts for monitoring batch processes based on VAR model.
#' This approach is fully described in "Marcondes Filho, D., & Valk, M. (2020). Dynamic VAR Model-Based Control Charts
#'  for Batch Process Monitoring. European Journal of Operational Research."
#'
#'
#' The maximum number of variables is five.
#'
#' All batches must have the same number of time-instants.
#'
#' This method is based on the use of "coupled vectors (cv)" (for more details, see Marcondes Filho, D., & Valk, M., 2020").
#'
#' The parameters Lc and Lr define the cv structure.
#'
#' Considering the new ongoing batch under monitoring:
#'
#' Lc is the number of elements of cv. (Lc is smaller than the number of time-instants);
#' Lr is the number of elements in cv randomly chosen from the in control batches in the reference dataset;
#' Lc-Lr is the number of elements in cv from the last (Lc-Lr) observations of the new ongoing batch.
#'
#' Default is  Lc= 50 e Lr=30.
#'
#' The batches in dataset "data" are considered to be in control
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
#' @param Lc length of the coupled vector
#' @param Lr length of random vector (Lr<Lc)
#' @param var.estimates TRUE show informations about the VAR modeling phase and the elements for setting T2.var / W.var control charts (default FALSE)
#'
#' @return  beyond.limits: returns the time-instants of each batch that the T2.var (or W.var) score are above the control limit
#'
#'  arl: returns for each new batch the mean number of time-instants before a signal is given by the control chart
#'      (arl=n/g, where n is the overall number of time-instants and g is the number of time-instants above the control limit)
#'
#'
#'   time.to.first.detection (TFD): returns for each new batch the first time-instant (the most recently instant) that can be considered
#'       as a possible signal of process change. TFD is the first point (t*) above the limit followed by two consecutive points above the control limit
#'
#'
#'  artl: returns the cumulative rate of the overall time-instants (n) until the first three consecutive points are above the control limit.
#'        The artl is computed using the first of these three points, that is, artl=t*/n
#'
#' varest: If var.estimates=TRUE, it returns the Lim_T2 vector (or Lim_W vector) of the T2.var (or W.var) control limits to each time-instant;
#' the T2.var (or W.var) scores for each new batch to each time-instant [(tnew) matrix for the T2.var and (wnew) matrix for the W.var scores];
#' the number (I) of reference batches; the number (C=size*size) of estimated phis coefficients; the number (Inew) of new batches;
#' the number (n) of time-instants and the (cov) list including the estimated mean
#' covariance from the reference batches to each time-instant
#'
#'
#'
#'
#'#' @examples
#'
#' # Example 1: Monitoring considering two variables and Inew=5 in control batches
#'
#'    mydata=simon()
#'
#'    T2.var.on=onlinem(data=mydata$data,size=2,newdata=mydata$newdata)
#'     W.var.on=onlinem(data=mydata$data,size=2,type="W.var",newdata=mydata$newdata)
#'
#'
#' # Example 2: Monitoring considering two variables and Inew=10 in control batches
#'
#'     B1=matrix(c(-0.3, 0.4, 0.4, 0.5), 2, byrow=TRUE)
#'     B1new=B1
#'     mydata2=simon(n=100,I=200,size=2,Inew=10,n1=50,B1=B1,varcov=diag(2),B1new=B1new)
#'    T2.var.on=onlinem(data=mydata2$data,size=2,newdata=mydata2$newdata, plot=F)
#'    W.var.on=onlinem(data=mydata2$data,size=2,type="W.var",newdata=mydata2$newdata, plot=F)
#'
#'
#'  # Example 3: Monitoring considering three variables and Inew=10 out of control batches
#'
#'    B1=matrix(c(-0.3,0,0.4,0,0.2,0,0,-0.1,0.5),3,byrow=TRUE)
#'    B1new=matrix(c(0.7,0,0.4,0,0.5,0,0,-0.1,0.5),3,byrow=TRUE)
#'    mydata3=simon(n=100,I=200,size=3,Inew=10,n1=50,B1=B1,varcov=diag(3),B1new=B1new)
#'    T2.var.on=onlinem(data=mydata3$data,size=3,newdata=mydata3$newdata, plot=F)
#'    W.var.on=onlinem(data=mydata3$data,size=3,type="W.var",newdata=mydata3$newdata, plot=F)
#'
#' @seealso simon
#'
#' @export
onlinem=function(data,size,newdata,Lc=50,Lr=30,confidence.level=0.99, type="T2.var", covvar="empirical", plot=TRUE,var.estimates=FALSE){

  if(size<2 || size >5){stop ("Error ... number of variables (size) must be between 2 and 5")}
  if(Lc<30){stop("Error...Lc must be at least 30")}
  #if(dim(data)[2]<50){stop("Error...Number of time-instants must be at least 50")}
  #if((dim(data)[1]/size)<50){stop("Error... Number of batch samples must be at least 50")}
  if(dim(data)[2]<Lc){stop("Error...Number of time-instants must be greater than Lc")}
  if(is.null(size)){stop("Error...size must be defined")}
  if(is.null(data)){stop("Error...data must be defined")}
  if(is.null(newdata)){stop("Error...newdata must be defined")}
  if(Lr>=Lc){stop("Error...Lc must be greater than Lr")}


  if (size==2){

  if(dim(data)[2]<50){stop("Error...Number of time-instants must be at least 50")}
  if((dim(data)[1]/size)<50){stop("Error... Number of batch samples must be at least 50")}

  if ( dim(data)[2]>=60 & dim(data)[2]<300 & (dim(data)[1]/size)<sqrt(dim(data)[2]/0.0135)) {stop ("Error...number of reference batches must be at least sqrt(n/0.0135) for the number of time-instants (n) between 60 and 299")}
  if ( dim(data)[2]>=300 & (dim(data)[1]/size)<dim(data)[2]/2) {stop ("Error...number of reference batches must be at least sqrt(n/2) for the number of time-instants (n) of at least 300")}

    } #size2

   if (type=="W.var") {


    if (size >2 & size<=5) {

      if(dim(data)[2]<50){stop("Error...Number of time-instants must be at least 50")}
      if((dim(data)[1]/size)<122){stop("Error... Number of batch samples must be at least 122")}
      #if ( dim(data)[2]>=50 & dim(data)[2]<200 & (dim(data)[1]/size)>=122) {stop ("Error...number of reference batches must be between 100 and 121 for the number of time-instants (n) between 50 and 199")}
      if ( dim(data)[2]>=200 & (dim(data)[1]/size)<sqrt(dim(data)[2]/0.0135)) {stop ("Error...number of reference batches must be at least sqrt(n/0.0135) for the number of time-instants (n) of at least 200")}


    } #size entre 2 e 5

  } # W.var


  if (type=="T2.var") {


    if (size==3){

      if(dim(data)[2]<50){stop("Error...Number of time-instants must be at least 50")}
      if((dim(data)[1]/size)<122){stop("Error... Number of batch samples must be at least 122")}
      #if ( dim(data)[2]>=50 & dim(data)[2]<200 & (dim(data)[1]/size)>=122) {stop ("Error...number of reference batches must be between 100 and 121 for the number of time-instants (n) between 50 and 199")}
      if ( dim(data)[2]>=200 & (dim(data)[1]/size)<sqrt(dim(data)[2]/0.0135)) {stop ("Error...number of reference batches must be at least sqrt(n/0.0135) for the number of time-instants (n) of at least 200")}


    } # size 3

    if (size==4){

      if(dim(data)[2]<50){stop("Error...Number of time-instants must be at least 50")}
      if((dim(data)[1]/size)<150){stop("Error... Number of batch samples must be at least 150")}
      #if ( dim(data)[2]>=50 & dim(data)[2]<100 & (dim(data)[1]/size)>=200) {stop ("Error...number of reference batches must be between 150 and 199 for the number of time-instants (n) between 50 and 99")}
      if ( dim(data)[2]>=100 & (dim(data)[1]/size)< (100+dim(data)[2])) {stop ("Error...number of reference batches must be at least (100+n) for the number of time-instants (n) of at least 100")}


    } #size 4


    if (size==5){

      if(dim(data)[2]<50){stop("Error...Number of time-instants must be at least 50")}
      if((dim(data)[1]/size)<200){stop("Error... Number of batch samples must be at least 200")}
      #if ( dim(data)[2]>=50 & dim(data)[2]<100 & (dim(data)[1]/size)>=300) {stop ("Error...number of reference batches must be between 200 and 299 for the number of time-instants (n) between 50 and 99")}
      if ( dim(data)[2]>=100 & (dim(data)[1]/size)< (200+dim(data)[2])) {stop ("Error...number of reference batches must be at least (200+n) for the number of time-instants (n) of at least 100")}


    } # size5

  } #T2.var



    varest=varfiton(data=data,size=size,newdata=newdata,Lc=Lc,Lr=Lr,confidence.level=confidence.level,covvar=covvar)
    mcharts=chartson(varest=varest,type=type,plot=plot)


  if(var.estimates==TRUE){

    if (type=="T2.var"){

      mcharts$varest$Lim_T2=varest$Lim_T2
      mcharts$varest$tnew=varest$tnew
      mcharts$varest$cov=varest$cov
      mcharts$varest$I=varest$I
      mcharts$varest$C=varest$C
      mcharts$varest$Inew=varest$Inew
      mcharts$varest$n=varest$n
    } else {

      mcharts$varest$Lim_W=varest$Lim_W
      mcharts$varest$wnew=varest$wnew
      mcharts$varest$cov=varest$cov
      mcharts$varest$I=varest$I
      mcharts$varest$C=varest$C
      mcharts$varest$Inew=varest$Inew
      mcharts$varest$n=varest$n
    }
   #mcharts$varest=varest
  }
  return(mcharts)
}

