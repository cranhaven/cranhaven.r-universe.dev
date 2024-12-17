datasets = function(){
  cat("=====================================\n")
  cat(" Available datasets in package desk. \n")
  cat("=====================================\n\n")
  a = data(package = "desk")$results[,3:4]
  a = as.table(cbind(Name = a[,1], Description = a[,2]))
  row.names(a) = 1:dim(a)[1]
  return(a)
}
