initialize_fuc.dbscan = function(data, K){

  ## -----------------------------------------------------------------------------------------------------------------
  ## The name of the function: initialize_fuc.dbscan
  ## -----------------------------------------------------------------------------------------------------------------
  ## Description:
  ##            Generating the initial values using hierarchical clustering.
  ## -----------------------------------------------------------------------------------------------------------------
  ## Required preceding functions or packages:
  ##            R packages: NbClust
  ## -----------------------------------------------------------------------------------------------------------------

  n <- as.integer(dim(data)[1])
  p <- as.integer(dim(data)[2])

  # initialization via dbscan clustering##
  Mu <- matrix(0, K, p)
  hc <- hclust(dist(data,method = "euclidean"),method = "ward.D2")
  memb <- cutree(hc,k=K)

  prob <- rep(0,K)
  Theta <- array(0, dim = c(p, p, K))
  S <- array(0, dim = c(p, p, K))
  for(k in 1:K)
  {
    Mu[k,] <- t(colMeans(data[memb == k, , drop = FALSE]) )
    S[,,k]  <- cov(data[memb == k, , drop = FALSE])
    Theta[,,k] <- solve(S[,,k] + diag(1,p))
    prob[k] <- sum(memb == k)/n
  }

  P <- list()
  P$prob <-  prob
  P$Mu <-  Mu
  P$Theta <- Theta
  P$S <- S
  P$memb <- memb
  return(P)
}
