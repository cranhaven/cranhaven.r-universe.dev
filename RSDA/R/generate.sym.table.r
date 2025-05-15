#' generate.sym.table
#' @keywords internal
#' @author Jorge Arce Garro
generate.sym.table<-function(sym.data){
  sym.var.names<-sym.data$sym.var.names
  sym.var.starts<-sym.data$sym.var.starts
  sym.var.length<-sym.data$sym.var.length
  sym.var.types<-sym.data$sym.var.types
  sym.obj.names<-sym.data$sym.obj.names
  sym.var.names.length<-length(sym.var.names)
  sym.obj.names.length<-length(sym.obj.names)
  sym.tbl.columns<-rep("X",sym.var.names.length)
  sym.tbl<-matrix(rep("X",sym.obj.names.length*sym.var.names.length), nrow=sym.obj.names.length,ncol=sym.var.names.length, byrow = TRUE)
  for (i in 1:sym.var.names.length) {
    data<-sym.data$meta[,sym.var.starts[i]:(sym.var.starts[i]+sym.var.length[i]-1)]
    switch(sym.var.types[i], '$C' = {
      sym.tbl[,i]<-as.character(data)
      sym.tbl.columns[i]<-paste0('C ', sym.var.names[i])
    }, '$I' = {
      sym.tbl[,i]<-generate.columns.interval(data)
      sym.tbl.columns[i]<-paste0('I ', sym.var.names[i])
    }, '$H' = {
      sym.tbl[,i]<-generate.columns.multivalued(data)
      sym.tbl.columns[i]<-paste0('H ', sym.var.names[i])
    }, '$S' = {
      sym.tbl[,i]<-generate.columns.set(data)
      sym.tbl.columns[i]<-paste0('S ', sym.var.names[i])
    }, stop("Invalid variable type"))
  }
  sym.tbl<-as.data.frame(sym.tbl)
  colnames(sym.tbl)<- sym.tbl.columns
  row.names(sym.tbl)<-sym.obj.names
  return(sym.tbl)
}
