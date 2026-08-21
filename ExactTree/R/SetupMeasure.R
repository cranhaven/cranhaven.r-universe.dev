SetupMeasure<-function(defMeasure){ #defMeasure is supposed to be a cell which is like a 2-d list
#%passes measure and checks whether defMeasure includes a Prior and Loss (Measure=3)
Prior<-list()
LossM<-list()

if(is.list(defMeasure)){
  measureCB <- defMeasure[[1]] # measure + callback bit
  measure <- bitwAnd(measureCB,127) # strip callback bit
  if(measure!=3){
    stop('Measure parameter is a list, but measure type is not 3')
  }
  if(length(defMeasure)>1){#MISSING IN R MAKES NO SENSE A 2-D LIST
    if(!is.list(defMeasure[[2]])){ #%ensure it has the correct cell structure
      Prior[[1]]<-matrix(defMeasure[[2]],ncol=1)
    }else{
      Prior<-defMeasure[[2]]
    }

    if(length(defMeasure)>2){
      if (!is.list(defMeasure[[3]])){ #%ensure it has the correct cell structure
        LossM[[1]]<-defMeasure[[3]]
      }else{
        LossM<-defMeasure[[3]]
      }
    }
  }


}else{
  measureCB <- defMeasure # measure + callback bit
  measure <- bitwAnd(measureCB,127) # strip callback bit
}

return(list(measure=measure, measureCB=measureCB, Prior=Prior, LossM=LossM))
}
