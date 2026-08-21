GetHeterogeneity<-function(Y,aMeasure=NULL, measure, Prior, LossM, ClassSizes){
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
  h<-GetSSE(Y)
}else if(ActiveMeasure==1){
  #h=GetMisclassification(Y);
  h<-GetMisclassification2(Y) #faster in M6.5
}else if(ActiveMeasure==3){
  h<-GetExpectedRisk(Y, Prior, LossM, ClassSizes)
}else{
  stop('Undefined Heterogeneity Measure')
}

return(h)
}





GetMisclassification<-function(Y){
h<-0
Y<-matrix(Y, nrow = NROW(Y), ncol = NCOL(Y))
for(d in 1:(NCOL(Y))){
  hf<-0
  O<-sort(Y[,d])
  while(dim(O)[1]>0){
    c<-min(O)
    I<-which(O>c) #[I]=find(O>c);
    f<-dim(O)[1]-dim(I)[1]
    hf<-max(hf,f)
    O<-O[I]
  }
  h<-h+(dim(Y)[1]-hf)
}

return(h)
}




GetMisclassification2<-function(Y){
h<-0
yn<-NROW(Y)
ym<-NCOL(Y)
Y<-matrix(Y, nrow = yn, ncol = ym)
if(yn==1){
  return()
}

for(d in 1:ym){
  O<-sort(Y[,d])
  hf<-0
  s<-1
  for(i in 2:yn){
    if(O[i]!=O[i-1]){
      hf<-max(hf,(i-s))
      s<-i
    }
  }
  if(O[yn]==O[yn-1]){
    hf<-max(hf,(yn+1-s))
  }
  h<-h+(yn-hf)
}


return(h)
}



GetSSE<-function(Y){
#library(pracma)
#Give Sum of Squared error in prediction
E<-Y-repmat(a=mean(Y),n=NROW(Y),m=1)

h<-sum(E*E)

return(h)
}




GetExpectedRisk<-function(Y, Prior, LossM, ClassSizes){
#global Prior LossM ClassSizes;

# Prior<-globalenv()$Prior
# LossM<-globalenv()$LossM
# ClassSizes<-globalenv()$ClassSizes
Y<-matrix(Y, nrow = NROW(Y), ncol = NCOL(Y))
h<-0
for(d in 1:NCOL(Y)){
  dimCS<-c(NROW(ClassSizes[[d]]), NCOL(ClassSizes[[d]]))
  nCat <- max(dimCS)
  L<-LossM[[d]]
  f<-histc(x=Y[,d],edges=1:nCat)$cnt
  f<-as.vector(f) #f has different shape if Y has one row
  E<-as.vector(Prior[[d]])*f/ClassSizes[[d]]
  if(NCOL(E)==1){
    pa<-sum(E)
  }else{
    pa<-colSums(E)
  }
  piA<-E/pa
  ra<-min(t(L)%*%piA)

  h=h+pa%*%ra
}

return(h)
}

