initialize_fuc = function(data, K, n.start = 100){

  ## -----------------------------------------------------------------------------------------------------------------
  ## The name of the function: initialize_fuc
  ## -----------------------------------------------------------------------------------------------------------------
  ## Description:
  ##            Generating the initial values using K-means.
  ## -----------------------------------------------------------------------------------------------------------------
  ## Required preceding functions or packages: No
  ## -----------------------------------------------------------------------------------------------------------------

  n <- dim(data)[1]
  p <- dim(data)[2]
  Mu <- matrix(0, K, p)
  kmeans.clust <- kmeans(data, K, nstart = n.start)
  memb <- kmeans.clust$cluster
  prob <- kmeans.clust$size/n
  Theta <- array(0, dim = c(p, p, K))
  S <- array(0, dim = c(p, p, K))
  for(k in 1:K)
  {
    data.k = data[memb == k, , drop = FALSE]
    Mu[k,] <- t(colMeans(data.k) )
    S[,,k]  <- cov(data.k)
    n.k <- dim(data.k)[1]
    p.k <- dim(data.k)[2]
    if(n.k > p.k + 5){
      Theta[,,k] <- solve(S[,,k])
    } else {
      Theta[,,k] <- huge::huge(data.k, lambda = 0.5*sqrt(log(max(n.k,p.k))/n.k), method = "glasso", verbose = FALSE)$icov[[1]]
    }
    
  }

  int.res <- list()
  int.res$prob <-  prob
  int.res$Mu <-  Mu
  int.res$Theta <- Theta
  int.res$S <- S
  int.res$memb <- memb
  return(int.res)
}
