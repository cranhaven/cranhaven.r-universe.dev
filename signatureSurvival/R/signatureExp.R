signatureExp <-
function(svdata,weight){
 
  svcolname<-colnames(svdata)
  wcolname<-colnames(weight)

  if(length(wcolname)==2){
   sgene<-weight[,1]
   ind<-is.element(svcolname,sgene)
   weight1<-weight
  }else if(length(wcolname)==3){
  sgene1<-weight[,1]
  sgene2<-weight[,2]    
  ind1<-is.element(svcolname,sgene1)
  ind2<-is.element(svcolname,sgene2)
  j1<-which(ind1==TRUE)
  j2<-which(ind2==TRUE)
  if(length(j1)!=0){
    ind<-ind1
    weight1<-weight[,c(1,3)]
  }else if(length(j2)!=0){
    ind<-ind2
    weight1<-weight[,c(2,3)]
  }else{
    stop("no matches of genes between weigth and survival data")
  }
  }else{
     stop("need a list of gene or gene id in weight")
  }
  sigdat<-svdata[,ind]   
 # print(head(sigdat))
  sigdat.sort<-sigdat[,order(colnames(sigdat))]  
  wght.sort<-weight1[order(weight[,1]),]  
  ng<-nrow(wght.sort)
  np<-nrow(sigdat.sort)
  nc<-ncol(sigdat.sort)
  sexp<-rep(0,np)
  gene_svd<-colnames(sigdat.sort)
  gene_wght<-wght.sort[,1]
  for(i in seq(np)){
    for(j in seq(nc)){
      for(k in seq(ng)){
        if(identical(gene_svd[j],gene_wght[k])){
          sexp[i]<-sexp[i]+wght.sort[k,2]*sigdat.sort[i,j]
          }
        }
      }
}
  svdata<-cbind(svdata,sexp)
  colnames(svdata)<-c(svcolname,"signature")
  
  return(svdata)
}
