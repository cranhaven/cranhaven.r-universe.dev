GetPrediction<-function(Y, measure, Prior, LossM, ClassSizes){
#global measure;

#measure<-globalenv()$measure

if(measure==0){
  y<-GetPSSE(Y)
}else if(measure==1){
  y<-GetPMisclassification(Y)
}else if(measure==3){
y<-GetPExpectedRisk(Y, Prior, LossM, ClassSizes)
}else{
  stop('Undefined Heterogeneity Measure')
}

return(y)

}


