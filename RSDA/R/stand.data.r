#' Standardized Intervals
#'
#' @param sym.data An Interval Matrix
#' @param data.mean A vector of means
#' @param data.stan A vector of standard deviation
#' @param nn Number of concepts
#' @param mm Number of variables
#'
#' @return Standardized intervals
#'
stand.data<-function(sym.data,data.mean,data.stan,nn,mm){
  data<-sym.data$data
  for(j in 1:mm)
  {
    data[,(2*j-1):(2*j)]<-(data[,(2*j-1):(2*j)] - data.mean[j])/data.stan[j]
  }
  return(data)
}
