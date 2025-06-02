train.ind <- function(X, y, distribution){
  nc <- length(unique(y))
  est <- list()
  for(c in 0:(nc-1)){
    Xc <- X[y == c,]
    #Estimar marginales
    den_est <- density.estimation(Xc,distribution = distribution)
    est[[c + 1]] <- list(den = den_est,
                         distribution = distribution,
                         cop = "independent",nclass = nc)
  }
  
  return(model = est)
}
