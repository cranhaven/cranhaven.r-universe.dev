##' Ancillary Function for Cleaning Matrices
##'
##' @title Ancillary Function for Cleaning Matrices
##' @param matij a matrix to be cleaned by \code{nobs}. \cr
##' \code{matij[i,j]} is the jth observation of the ith subject.
##' @param nobs an integer vector of observation times. \cr
##' \code{nobs[i]} is the number of observations for the ith subject.
##' @param object_name name of the matrix object.
##' @return a cleaned matrix of \code{matij}. \cr
##' \code{matij[i,j]} is set to NA if \code{j>nobs[i]}
##' @noRd
clean_matij_by_nobs<-function(matij,nobs,object_name="matij"){
  nind<-dim(matij)[1]
  nmaxobs<-dim(matij)[2]
  if(any(nobs%%1!=0))stop("Error in 'nobs': 'nobs' should be a positive integer vector.")
  if(any(nobs<=0))stop("Error in 'nobs': 'nobs' should be a positive integer vector.")
  if(nmaxobs<max(nobs))stop(paste0("Error in dimensions of '",object_name,"' and 'nobs'."))
  if(nrow(matij)!=length(nobs))stop(paste0("Error in dimensions of '",object_name,"' and 'nobs'."))
  for(ii in 1:nind){
    if(any(!is.numeric(matij[ii,1:nobs[ii]])))stop(paste0("'",object_name,"' contains non-numeric values."))
    matij[ii,1:nmaxobs>nobs[ii]]<-NA
  }
  return(matij)
}

##' Ancillary Function for Cleaning Matrices
##'
##' @title Ancillary Function for Cleaning Matrices
##' @param design_interval design interval
##' @param n_time_units number of time units
##' @param time_unit basic time unit
##' @param range_time_matrix range of time matrix
##' @return a list with all design components
##' @noRd
clean_time_unit<-function(design_interval,n_time_units,time_unit,range_time_matrix){
  
  if(missing(design_interval)){
    design_interval<-range_time_matrix
  }else if(length(design_interval)!=2){
    stop("The length of 'design_interval' should be 2.")
  }
  
  ttmin<-design_interval[1]
  ttmax<-design_interval[2]
  if(ttmin>range_time_matrix[1])
    stop("'design_interval' does not cover all elements of 'time_matrix'.")
  if(ttmax<range_time_matrix[2])
    stop("'design_interval' does not cover all elements of 'time_matrix'.")
  
  if(missing(n_time_units)&missing(time_unit))
    stop("One of 'n_time_units' and 'time_unit' should be supplied.")
  if(!missing(n_time_units))
    time_unit<-(ttmax-ttmin)/(n_time_units-1)
  if(!all.equal(tail(seq(design_interval[1],design_interval[2],time_unit),1),design_interval[2]))
    warning("'design_interval' is not divisible by 'time_unit', applying approximation to 'time_unit'.")
  n_time_units<-round((design_interval[2]-design_interval[1])/time_unit)+1
  time_unit<-(ttmax-ttmin)/(n_time_units-1)
  
  if(n_time_units<10)
    warning("'n_time_units' is probably too small.")
  
  return(list(
    design_interval=design_interval,
    time_unit=time_unit,
    n_time_units=n_time_units,
    ttmin=ttmin,
    ttmax=ttmax))
}
