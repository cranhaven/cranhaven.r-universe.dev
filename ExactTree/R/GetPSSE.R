GetPSSE<-function(Y){
#Sum of Squared: best prediction is mean
if(NCOL(Y)==1){
  y<-mean(Y)
}else{
  y<-colMeans(Y)
}
#y=median(Y);

return(y)

}





