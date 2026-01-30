look.up.mat = function(L, a){
  b <- matrix(nrow=length(a), ncol=2)
  for(i in seq_along(a)) b[i,] <- c(L[[i]][a[i]], L[[i]][a[i]+1])
  b
}
