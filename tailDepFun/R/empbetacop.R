empbetacop <-
function(x = NULL, ranks = NULL, u)
  {
    if (is.null(ranks)) {
      ranks <- apply(x, 2, rank)
    }
   
    n <- nrow(ranks)
    d <- ncol(ranks)
    
    if (!is.matrix(u)) {
      u <- matrix(u, nrow = 1, ncol = length(u))
    }
    
    stopifnot(d == ncol(u))
    
    k <- nrow(u)
    res <- numeric(k)
    
    for (l in seq(k))
    {
      tmp <- rep(1, n)
      
      for (j in seq(d))
      {
        BetaBinomProb <- cumsum(dbinom(x = 0:n, size = n, prob = 1 - u[l,j]))
        tmp <- tmp * BetaBinomProb[n - ranks[,j] + 1]
      }
      
      res[l] <- mean(tmp)
    }
    
    return(res)
  }
