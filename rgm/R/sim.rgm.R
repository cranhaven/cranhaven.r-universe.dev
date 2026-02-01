#' Simulate Random Graph Model (RGM)
#'
#' This function simulates a random graph model based on the given parameters.
#' It performs various computations and returns a list containing the simulated data,
#' parameters, and other relevant results.
#'
#' @param n An integer, the number of nodes in the graph (default: 346).
#' @param D An integer, representing the dimension (default: 2).
#' @param p An integer, specifying the number of parameters (default: 87).
#' @param B An integer, the number of conditions (default: 13).
#' @param seed An integer, the seed for random number generation (default: 123).
#' @param mcmc_iter An integer, the number of MCMC iterations (default: 100).
#' @param alpha Numeric vector or NULL, initial values for alpha (default: NULL).
#' @param theta Numeric or NULL, initial value for theta (default: NULL).
#' @param loc Matrix or NULL, initial location values (default: NULL).
#' @param X Matrix or NULL, covariate data (default: NULL).
#'
#' @return A list containing the simulated data, X, loc, alpha, theta, and G.
#' @export
#'
#' @examples
#' sim_result <- sim.rgm(n = 100, D = 2, p = 50, B = 10)
sim.rgm <- function(n=346,
                    D = 2,
                    p = 87,
                    B = 13,
                    seed = 123,
                    mcmc_iter=100,
                    alpha=NULL,
                    theta=NULL,
                    loc=NULL,
                    X=NULL) {

  set.seed(seed)

  # Simulating latent space model
  if(is.null(loc)){
    cond.loc1 <- stats::rnorm(B, 0, 0.3)
    cond.loc2 <- stats::rnorm(B, 0, 0.3)
    cloc.true <- cbind(cond.loc1, cond.loc2)
  }  else
    cloc.true<-loc

  # Simulating one covariate
  n.edge <- p*(p-1)/2
  if(is.null(X)){
    X <- stats::runif(n.edge, -0.5, 0.5)
    X <- as.matrix(X)
  } else
    X<-as.matrix(X)

  # True parameters
  if(is.null(alpha))
    alpha.true <- stats::rnorm(B, -2)
  else
    alpha.true<-alpha
  if(is.null(theta))
     beta.true<-2.5
   else
    beta.true <- theta

  # Simulating true graph
  G.true <- matrix(0, ncol = B, nrow = n.edge)
  dist.cond <- matrix(ncol=B, nrow=n.edge)
  m <- matrix(1:p, ncol=p, nrow=p)
  e1 <- t(m)[lower.tri(m)]
  e2 <- m[lower.tri(m)]
  Pi.true <- matrix(ncol = B, nrow = n.edge)
  sp<-NULL
  for(i in 1:mcmc_iter) {
    for (b in 1:B){
      # Updating condition-specific intercept
     dist.cond[,b] <- apply(G.true, 1, function(g, cloc, b) { crossprod(apply(cloc*g, 2, sum) - cloc[b,]*g[b], cloc[b,]) }, cloc = cloc.true, b = b)
     for (k in 2:p){
      for (j in 1:(k-1)){
        ind<-e1==j & e2==k
        Pi.true[ind,b]<-stats::pnorm(alpha.true[b]+dist.cond[ind,b]+X[ind,]%*%beta.true)
      }
    }
    G.true[,b] = stats::rbinom(n.edge,1,Pi.true[,b])
  }
  sp<-c(sp,sum(G.true))
}

  # Simulating data (Gaussian)
  data <- vector(mode = "list", length = B)
  for (j in 1:B) {
    A <- matrix(0, nrow = p, ncol = p)
    A[lower.tri(A)] <- G.true[,j]
    A <- A + t(A)
    data[[j]] <- custom_graph_sim( p = p, n = n, graph = A)$data
  }

  list(data = data, X=X, loc = cloc.true, alpha = alpha.true, theta = beta.true,G=G.true)

}



custom_graph_sim <- function(p, n, graph) {
  # Validate inputs
  if (p < 2) stop("'p' must be greater than 1")
  if (n < 1) stop("'n' must be greater than 0")
  if (!is.matrix(graph)) stop("'graph' must be a matrix")
  if (!isSymmetric(graph)) stop("'graph' must be symmetric")
  if (!all(graph %in% c(0, 1))) stop("Elements of matrix 'graph' must be 0 or 1")

  # Set default values for other parameters
  mean = 0
  sigma = NULL

  # Use the provided 'graph' matrix
  G <- graph

  # Data Simulation for Gaussian data
  if (is.null(sigma)) {
    # Adjust for zero variance if necessary
    if (any(apply(G, 1, stats::var) == 0)) {
      G <- G + matrix(stats::runif(length(G)) * 1e-10, nrow = nrow(G), ncol = ncol(G))
    }
    # Compute sigma and ensure it's symmetric
    cor_G = stats::cor(G) + diag(1e-10, p)
    sigma = (cor_G + t(cor_G)) / 2
  }
  d <- rmvnorm(n = n, mean = rep(mean, p), sigma = sigma)

  # Return the simulation output
  list(G = G, graph = graph, data = d, sigma = sigma)
}


  rmvnorm <- function(n, mean, sigma) {
    if (!isSymmetric(sigma, tol = sqrt(.Machine$double.eps))) {
      stop("'sigma' must be a symmetric matrix")
    }
    sigma = as.matrix(sigma)
    p = nrow(sigma)
    if (length(mean) == 1) {
      mean = rep(mean, p)
    }
    if (length(mean) != p) {
      stop("'mean' and 'sigma' have non-conforming size")
    }
    chol_sigma = chol(sigma)

    z = matrix(stats::rnorm(n * p), nrow = p, ncol = n)

    # The resulting matrix from the multiplication is p x n
    # We need to transpose it to get n x p
    data = t(t(chol_sigma) %*% z) + matrix(mean, nrow = n, ncol = p, byrow = TRUE)

    return(data)
  }




