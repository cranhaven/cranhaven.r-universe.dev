Predict.LP.poly <-
function(x,Tx,x0){
  Tx0 <- matrix(0, length(x0),ncol(Tx))
  for(k in 1:ncol(Tx)){
    A <- approxfun(x,Tx[,k],rule=2,method="constant",f=1)
    Tx0[,k] <- A(x0)
  }
  return(Tx0)
}
