GetHeterogeneityTest<-function(Yt,Prediction,measure){
#global measure

#globalenv()$measure

if(measure==0){
  h<-GetSSETest(Yt,Prediction)
}else if(measure==1){
  h<-GetMisclassificationTest(Yt,Prediction)
}else{
  stop('Undefined Heterogeneity Measure')
}

return(h)
}





GetMisclassificationTest<-function(Yt,Prediction){
h<-0
for(d in 1:(dim(Yt)[2])){
I<-which(Yt[,d]!=Prediction[d])
h<-h+dim(I)[1]
}

return(h)
}


GetSSETest<-function(Yt,Prediction){
# %Give Sum of Squared error in prediction
# %E=Y-repmat(mean(Y),size(Y,1),1);
#library(pracma)
E<-Yt-repmat(Prediction,NROW(Yt),1)
h<-sum(E*E)
}
