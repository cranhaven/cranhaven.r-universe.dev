dummy <-
function(Z,K){
  f = factor(Z,levels=1:K)
  Z = model.matrix(~0+f)
  matrix(as.numeric(Z),ncol=K)
}
