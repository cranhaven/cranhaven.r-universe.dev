x2.set<-function(set,data,data.methylation,y){
  set<- unlist(set)
  snps<-data[,set[1]]
  cpg<-data.methylation[,set[2]]

  O.table<-table.e2(snps,cpg,y)
  O.orginal<-array(O.table,dim=c(9,2))

  O<-O.orginal[rowSums(O.orginal)!=0,]
  df<-NROW(O)-1
  if(df == 0)
    stop("genotypes should have at least two levels!")
  if(0 %in% O)
    O<-O+0.5
  o<-O
  O<-t(t(O)/colSums(O))
  O.p<-O/(1-O)
  OR<-O.p[,2]/O.p[,1]
  o<-cbind(o,t(colSums(o)-t(o)))
  sd<-sqrt(rowSums(1/o))
  x2.value=sum((log(OR)/sd)^2)
  x2.result<-c(unlist(set),x2.value,df)
  return(x2.result)
}
