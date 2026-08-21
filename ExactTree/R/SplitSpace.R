SplitSpace<-function(Y,X,p,i){
  #Splits the Measurement space and dependent Variables according to split i in predictor p
  I1<-which(X[,p]<=i)
  Y1<-Y[I1,]
  X1<-X[I1,]
  I2<-which(X[,p]>i)
  #%I=setdiff(1:size(X,1),I);
  Y2<-Y[I2,]
  X2<-X[I2,]


  return(list(Y1=Y1,X1=X1,Y2=Y2,X2=X2,I1=I1,I2=I2))

}
