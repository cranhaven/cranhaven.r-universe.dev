cv.classofit <-function(predmat,y,type.measure,weights,foldid){
  
  family <- attr(predmat,"family")
  nobs <- nrow(predmat)
  etastart <- 0
  mustart <- NULL
  start <- NULL
  eval(family$initialize)

  y <- drop(y)  # we don't like matrix responses
  N <- nobs - apply(is.na(predmat), 2, sum)
  cvraw <- switch(type.measure,
                 mse = abs(y - predmat))
  
  list(cvraw=cvraw,weights=weights,N=N,type.measure=type.measure)
}