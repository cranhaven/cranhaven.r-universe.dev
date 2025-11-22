start_clust <- function (Y, g, init_clust, nkmeans, nrandom) 
  #Based on function from package EMMIXmfa with the same name.
  #See package EMMIXmfa for more details. 
{
  n <- nrow(Y)
  if (!is.null(init_clust)) {
    if (any(class(init_clust) %in% "factor") || any(class(init_clust) %in% 
                                                    "numeric")) {
      if (n != length(init_clust)) 
        stop("Length of init_clust must be equal to number of samples in the data")
      init_clust <- as.num.fac(init_clust)
      init_clust <- matrix(init_clust, nrow = n, ncol = 1)
    }
    if (class(init_clust) %in% "matrix") {
      if (n != dim(init_clust)[1]) 
        stop("Length of init_clust must be equal to number of samples in the data")
      init_clust <- apply(init_clust, 2, as.num.fac)
      init_clust <- init_clust
    }
  }
  k_starts <- NULL
  if (!is.null(nkmeans) && is.numeric(nkmeans) && (nkmeans > 
                                                   0)) {
    k_starts <- matrix(NA, nrow = n, ncol = nkmeans)
    for (i in 1:nkmeans) k_starts[, i] <- kmeans(Y, g)$cluster
  }
  r_starts <- NULL
  if (!is.null(nrandom) && is.numeric(nrandom) && (nrandom > 
                                                   0)) 
    r_starts <- matrix(sample(1:g, n * nrandom, replace = TRUE), 
                       nrow = n, ncol = nrandom)
  nc <- ifelse(is.null(init_clust), 0, dim(init_clust)[2]) + 
    ifelse(is.null(k_starts), 0, dim(k_starts)[2]) + ifelse(is.null(r_starts), 
                                                            0, dim(r_starts)[2])
  starts <- matrix(NA, nrow = n, ncol = nc)
  ind <- 1
  if (!is.null(dim(init_clust))) {
    starts[, ind:dim(init_clust)[2]] <- init_clust
    ind <- ind + dim(init_clust)[2]
  }
  if (!is.null(dim(k_starts))) {
    starts[, ind:(ind - 1 + dim(k_starts)[2])] <- k_starts
    ind <- ind + dim(k_starts)[2]
  }
  if (!is.null(dim(r_starts))) {
    starts[, ind:(ind - 1 + dim(r_starts)[2])] <- r_starts
    ind <- ind + dim(r_starts)[2]
  }
  starts <- as.matrix(starts)
  if ((is.null(starts)) || (dim(starts)[2] == 0)) 
    stop("Incorrect specification of initial grouping parameters")
  if (dim(starts)[1] != n) 
    stop("Length of init_clust must be equal to number of samples in the data")
  return(starts)
}


as.num.fac <- function (x) 
{
  as.numeric(as.factor(x))
}