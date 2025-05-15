#' generate.columns.interval
#' @keywords internal
generate.columns.interval<-function(data){
  min.char<-as.character(round(data[,1],2))
  max.char<-as.character(round(data[,2],2))
  return(paste0(paste0("[",paste(min.char,max.char,sep = ",")),"]"))
}

