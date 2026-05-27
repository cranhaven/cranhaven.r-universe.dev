ci <- function(Beta, Beta_bootstrap, alpha = 0.05, type = c("basic", "quantile", "bca", "basic2"), a, Beta2){
  
	
  p <- dim(Beta_bootstrap)[2]
  B <- dim(Beta_bootstrap)[1]
  interval <- matrix(0, 2, p)
  if (type == "basic") {
    bound.percentile <- apply(Beta_bootstrap, 2, function(u){ quantile(u, prob = c(1 - alpha/2, alpha/2)) })
    interval[1,] <- 2 * Beta - bound.percentile[1,]
    interval[2,] <- 2 * Beta-bound.percentile[2,]	
  }
  if (type == "basic2") {
    bound.percentile <- apply(Beta_bootstrap, 2, function(u){ quantile(u, prob = c(1 - alpha/2, alpha/2)) })
    interval[1,] <- Beta + Beta2 - bound.percentile[1,]
    interval[2,] <- Beta + Beta2 - bound.percentile[2,]
  }
  if (type == "quantile") {
    bound.percentile <- apply(Beta_bootstrap, 2, function(u){ quantile(u, prob = c(alpha/2, 1 - alpha/2)) })
    interval[1,] <- bound.percentile[1,]
    interval[2,] <- bound.percentile[2,]		
  }
  if (type == "bca") {
    if (missing(a)) {
      a <- 0
    }
    fstar <- Beta_bootstrap < (matrix(rep(1, B), B, 1) %*% matrix(Beta, 1, B))
    pstar <- apply(fstar, 2, mean)
    xi <- qnorm(pstar)
    for (j in 1:p) {
      if (xi[j] == -Inf || xi[j] == Inf){
        alpha1 <- alpha/2
        alpha2 <- 1 - alpha/2
      } else {
        alpha1 <- pnorm(xi[j] + (xi[j] + qnorm(alpha/2)) / (1 - a * (xi[j] + qnorm(alpha/2))))
        alpha2 <- pnorm(xi[j] + (xi[j] + qnorm(1 - alpha/2)) / (1 - a * (xi[j] + qnorm(1 - alpha/2))))
      }	
      interval[,j] <- quantile(Beta_bootstrap[, j, drop = FALSE], prob = c(alpha1, alpha2))			
    }
  }
  return(interval)
}



