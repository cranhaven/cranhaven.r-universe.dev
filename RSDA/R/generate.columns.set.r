#' generate.columns.set
#' @keywords internal
#' @author Jorge Arce Garro
generate.columns.set<-function(data){
  data.colnames<-colnames(data)
  sal<-apply(data,1,function(x) {
    paste0(paste0("{",paste(data.colnames[x==1], collapse = ",")),"}")
  })
  return(sal)
}
