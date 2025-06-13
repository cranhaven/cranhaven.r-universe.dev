# Convert a time series from the stacked stationary form
# back to periodically correlated
# Not exported
stat2pc = function(Y,period=2,n=NULL){
  nr = nrow(Y)
  d = ncol(Y)
  if (is.null(n))
    n = nr*period
  dnew = d / period

  X = c()
  for (i in 1:dnew){
    X = cbind(X,matrix(t(Y[,1:period + period*(i-1)]),ncol=1))
  }
  X[1:n,]
}
