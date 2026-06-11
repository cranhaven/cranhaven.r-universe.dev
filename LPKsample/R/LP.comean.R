LP.comean <-
function(x,y,perm=0){
  n<-length(x)
  if(length(unique(y))<=1 |length(unique(x))<=1  ){
    LP.mat<-0
  }else{
    m=length(unique(x))-1
    out<-list()

    Tx<-LP.Poly(x,m)
    Ty<-LP.Poly(y,m)

    LP.mat<-cor(Tx,Ty)
  }
  matdim<-dim(LP.mat)
  
  if(perm==0){
     df0<-nrow(LP.mat)*ncol(LP.mat)
     pval<-pchisq(n*sum(LP.mat^2),df=df0,lower.tail=FALSE)
   }else if(perm>=1){
      ostat<-n*sum(LP.mat^2)
      pstat<-rep(0,perm)
      for(i in 1:perm){
         x0<-sample(x,replace=F)
         Tx0<-LP.Poly(x0,m)
         LP.mat0<-cor(Tx0,Ty)
         pstat[i]<-n*sum(LP.mat0^2)
      }
      edfun<-ecdf(pstat)
      pval<-1-edfun(ostat)
   }

   out$LPINFOR<-sum(LP.mat^2)
   out$p.val<-pval
   out$LP.matrix<-matrix(LP.mat,matdim[1],matdim[2])
   return(out)
}
