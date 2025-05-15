#' generate.columns.multivalued
#' @keywords internal
#' @author Jorge Arce Garro
generate.columns.multivalued<-function(data){
  data.colnames<-colnames(data)
  sal<-apply(data,1,function(x) {
    paste0(paste0("{",paste( paste(data.colnames[x>0], x[x>0] , sep = ";") , collapse = ",")),"}")
  })
  return(sal)
}


