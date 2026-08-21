SetupY<-function(defY,measure){
#init Y: autorecode for discrete data (measure>1)

if((measure==1) || (measure==3)){
  Y<-defY
  for(y in 1:(NCOL(defY))){
    Y[,y]<-AutoRecode(defY[,y])$O
  }
}else{
  Y<-defY
}

return(Y)
}
