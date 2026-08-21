SetupRiskMeasure<-function(Y,measure,Prior,LossM){
#if undefined setup default Prior and LossM (Measure=3)

if(measure==3){
  if(is.null(Prior)||(length(Prior)==0)){
    for(y in 1:(NCOL(Y))){
      Prior[[y]]<-getDefPrior(matrix(Y[,y], nrow = NROW(Y), ncol = 1))
    }
  }

  if(is.null(LossM)||(length(LossM)==0)){
    for(y in 1:(NCOL(Y))){
      LossM[[y]]<-getDefLossM(max(Y[,y]))
    }
  }

  ClassSizes <- list()
  for(y in 1:(NCOL(Y))){
    ClassSizes[[y]] <- getFreqY(Y[,y])
  }

}else{
  ClassSizes <- list()
}


return(list(Prior=Prior, LossM=LossM, ClassSizes=ClassSizes))


}
