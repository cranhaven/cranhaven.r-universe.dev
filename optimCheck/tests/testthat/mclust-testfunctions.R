
# simulate random vector(s) from dirichlet distribution
rDirichlet <- function(n, alpha) {
  p <- length(alpha)
  X <- matrix(rgamma(n*p, shape = alpha), p, n)
  drop(t(X)/colSums(X))
}

# convert back and forth between "flattened" parameter representation,
# i.e., theta = c(pro[-G], mean, upper.tri(variance))
par2theta <- function(parameters) {
  c(parameters$pro[-length(parameters$pro)],
    parameters$mean, { # lambda function
      function(cs) cs[upper.tri(cs, diag = TRUE)]
    } (parameters$variance$cholSigma))
}

theta2par <- function(theta, d, G) {
  pro <- theta[1:(G-1)]
  pro <- c(pro, 1 - sum(pro))
  nchol <- d*(d+1)/2
  CS <- matrix(0, d, d)
  CS[upper.tri(CS, diag = TRUE)] <- theta[(G-1)+d*G + 1:nchol]
  list(pro = pro,
       mean = matrix(theta[(G-1) + 1:(d*G)], d, G),
       variance = list(modelName = "EEE", d = d, G = G,
                       cholSigma = CS))
}

# parameter names
theta.names <- function(d, G) {
  rho <- paste0("rho[", 1:(G-1), "]")
  ind <- as.matrix(expand.grid(d = 1:d, G = 1:G))
  mu <- paste0("mu[", ind[,"d"], ind[,"G"], "]")
  idd <- as.matrix(expand.grid(d1 = 1:d, d2 = 1:d))
  ind2 <- which(upper.tri(diag(d), diag = TRUE))
  Sigma <- paste0("Sigma[", idd[ind2,"d1"], idd[ind2,"d2"], "]")
  c(rho, mu, Sigma)
}
