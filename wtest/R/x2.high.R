x2.high<-function(set,data,y,w.order){
  set<- unlist(set)
  snps<-data[,set]
  if(w.order==1){
    O.table<-table.e1(snps,y)
    O.orginal<-array(O.table,dim=c(3,2))
  }else if(w.order==2){
    O.table<-table.e2(snps[,1],snps[,2],y)
    O.orginal<-array(O.table,dim=c(9,2))
  }else{
    if(nrow(as.data.frame(y))!=nrow(data)){
      y=t(y)
    }
    snps.table <- table(cbind(as.data.frame(snps),y))
    O.orginal <- array(snps.table[1:length(snps.table)], dim=(c(length(snps.table)/2,2)))
  }
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
