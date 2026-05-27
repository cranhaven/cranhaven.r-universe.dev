eLP.univar <-
function(x,m){ # x: data; m: order of LP-poly
  u <- (rank(x,ties.method = c("average")) - .5)/length(x)
  m <- min(length(unique(u ))-1, m )
  S.mat <- as.matrix(poly(u ,df=m))
  return(as.matrix(scale(S.mat)))
}
