GetCVErrorAndSE<-function(Y,Ypred,aMeasure=NULL, measure, Prior, LossM, ClassSizes){
#global measure Prior LossM ClassSizes;

# measure<-globalenv()$measure
# Prior<-globalenv()$Prior
# LossM<-globalenv()$LossM
# ClassSizes<-globalenv()$ClassSizes

if(!is.null(aMeasure)){
  ActiveMeasure <- aMeasure
}else{
  ActiveMeasure <- measure
}

if(ActiveMeasure==0){
  GSSEASEout<-GetSSErrorAndSE(Y,Ypred)
  Error<-GSSEASEout$Error
  SE<-GSSEASEout$SE
  SE2<-GSSEASEout$SE2
}else{
  stop('Undefined Heterogeneity Measure for SE')
}

return(list(Error=Error,SE=SE,SE2=SE2))

}




GetSSErrorAndSE<-function(Y,Ypred){
#Give Sum of Squared error in prediction
#library(pracma)

n<-dim(Y)[1]
E<-Y-Ypred
Error<-sum(E*E)
tE<-Y-repmat(colMeans(Y),n,1)
ym<-dim(Y)[2]
SEd<-matrix(0,nrow=ym,ncol=2)
for(d in 1:ym){
  s2<-sum(tE[,d]^2)/n
  Rcv<-sum(E^2)/n #MISSING colsums or sum?
  REcv<-Rcv/s2
  s2_1<-sum(E[,d]^4)/n-Rcv^2#if Rcv is a vector, then problem. I assume it is a vector in line 37
  s2_2<-sum(tE[,d]^4)/n-s2^2
  s_12<-sum((E^2)*(tE^2))/n-Rcv*s2
  SEd[d,1]<-REcv*sqrt(( (s2_1/Rcv^2)-(2*s_12/(Rcv*s2))+(s2_2/s2^2) )/n)
  SEd[d,2]<-sqrt(sum(Center(E[,d]^2)^2))/sum(tE[,d]^2)
}
SE<-SEd[1,1]
SE2<-SEd[1,2]

return(list(Error=Error,SE=SE,SE2=SE2))
}




# GetMisclassificationErrorAndSE<-function(Y){#MISSING In matlab also doesnt exist Error and SE
#   h<-0
#   yn<-dim(Y)[1]
#   ym<-dim(Y)[2]
#   if(yn==1){
#     return()
#   }
#
#   for(d in 1:ym){
#     O<-sort(Y[,d])
#     hf<-0
#     s<-1
#     for(i in 2:yn){
#       if(O[i]!=O[i-1]){
#         hf<-max(hf,(i-s))
#         s<-i
#       }
#     }
#     if(O[yn]==O[yn-1]){
#       hf<-max(hf,(yn+1-s))
#     }
#     h<-h+(yn-hf)
#   }
#
# return(list(Error=Error,SE=SE))
# }



# GetExpectedRiskAndSE<-function(Y, Prior, LossM, ClassSizes){
# #global Prior LossM ClassSizes;
# #library(pracma)
#
# # Prior<-globalenv()$Prior
# # LossM<-globalenv()$LossM
# # ClassSizes<-globalenv()$ClassSizes
# h<-0
# for(d in 1:dim(Y)[2]){
#   nCat <- max(dim(ClassSizes[[d]]))
#   L<-LossM[[d]]
#   f<-histc(x=Y[,d],edges=1:nCat)
#   f<-as.vector(f) #f has different shape if Y has one row
#   E<-as.vector(Prior[[d]])*f/ClassSizes[[d]]
#   pa<-colSums(E)#colSums or sum?
#   piA<-E/pa
#   ra<-min(t(L)%*%piA)
#
#   h<-h+pa%*%ra
# }
#
# return(list(Error=Error,SE=SE))
# }


