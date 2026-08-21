GetPExpectedRisk<-function(Y, Prior, LossM, ClassSizes){
#global Prior LossM ClassSizes;
#library(pracma)


# Prior<-globalenv()$Prior
# LossM<-globalenv()$LossM
# ClassSizes<-globalenv()$ClassSizes

nc<-NCOL(Y)
y<-matrix(0,nrow = nc, ncol = 1)

for(d in 1:nc){
  nCat <- max(NROW(ClassSizes[[d]]),NCOL(ClassSizes[[d]]))
  L<-LossM[[d]]
  f<-histc(Y,1:nCat)$cnt
  E<-(as.vector(Prior[[d]])*f)/ClassSizes[[d]]
  if(NCOL(E)==1){
    pa<-sum(E)
  }else{
    pa<-colSums(E)
  }

  piA<-E/pa
  LpiA<-t(L)%*%piA #matrix product
  ra <- min(LpiA)
  y[d]<-which.min(LpiA)
}

return(y)

}
