cumquant <-
function(x, p, type = 7){
  na.pos <- which(is.na(x))
  if(length(na.pos) > 0){
    if(min(na.pos) == 1) xx <- rep(NA, length(x))
    if(min(na.pos) > 1){
      y <- rep(NA, length(x) - min(na.pos) + 1)
      x <- x[0:(min(na.pos) - 1)]  
      xx <- c(sapply(seq_along(x), function(k, z) quantile(z[1:k], probs = p[1], names = FALSE, type = type), z = x), y)  
    }
  } else xx <- sapply(seq_along(x), function(k, z) quantile(z[1:k], probs = p[1], names = FALSE, type = type), z = x)
  return(xx)
}
