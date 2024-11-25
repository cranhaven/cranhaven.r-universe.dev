predAccuracy=function(x,y){
  Bias=mean(y-x,na.rm=TRUE)
  RMSE=sqrt(sum((y-x)^2,na.rm=TRUE)/length((y-y)))
  Rsquared=cor(y,x,use = "na.or.complete",method = "pearson")^2
  NSE=1-sum((y-x)^2,na.rm=TRUE)/sum((y-mean(x,na.rm=TRUE))^2,na.rm=TRUE)
  statia=data.frame(Bias,RMSE,Rsquared,NSE)
  return(statia)
}
