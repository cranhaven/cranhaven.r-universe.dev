#' sym.scale.matrix.j
#' @keywords internal
#' @param data A symbolic data
#' @param data.mean A vector of means
#' @param data.sd A vector of deviation standard
#' @param nn Number of concepts
#' @param mm Number of variables
#'
#' @return Standardized Data
#'
sym.scale.matrix.j<-function(data,data.mean,data.sd,nn,mm){
  data.stan<-data
  for(i in 1:mm)
  {
    data.stan[,i]<-(data[,i] - data.mean[i])/data.sd[i]
  }
  return(data.stan)
}
