reRVineMatrix<-function(help.env) {

RVM<-get("RVM",help.env)
p<-dim(RVM)[2]
diagV<-diag(RVM)
cops<-list()
for(i in 2:p) {
  cops[[p-i+1]]<-matrix(NA,p-i+1,i)
}

for(i in (p-1):1) {
  for(k in p:(i+1)) {
  if(k==p) {
    cops[[k-1]][i,]<-c(diagV[i],RVM[k,i])
  }
  if(k<p) {
    k.seq<-seq(k+1,p)
    cops[[k-1]][i,]<-c(diagV[i],RVM[k,i],sort(RVM[k.seq,i]))
  }
}
}
cops[[p]]<-matrix(c(1:p),nrow=p,ncol=1)
assign("cops",rev(cops),help.env)
}
