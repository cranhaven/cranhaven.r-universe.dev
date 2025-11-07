mean.center <-
function(m){
  if (!('matrix' %in% class(m)))
    m <- as.matrix(m)
  for (i in 1:ncol(m))
    m[,i] <- m[,i] - mean(m[,i])
  return(m)
  
}
