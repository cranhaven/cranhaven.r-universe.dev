# Convert periodically correlated multivariate time series to
# stacked stationary form
pc2stat = function(X,period=2){
  n = nrow(X)
  d = ncol(X)
  # add zeros

  extra = ceiling(n/period)*period - n
  X = rbind(X,matrix(0,nrow = extra, ncol=d))
  # convert

  matrix(t(X), ncol=period*d, byrow = T)
}
