# MC update for parameters
# input: x - input matrix 1
#	 size.x - input matrix 2
#        param - latest parameters 
#        m - the number of empty cluster used in MC
#        max_iter - the maximum iteration allowed
#        debug - whether need to print the procedure
# output: param - updated parameters

# author: Radford M. Neal 
# title: Markov Chain Sampling Methods for Dirichlet Process Mixture Models
# published in: Journal of Computational and Graphical Statistics, vol. 9, No.2,pp.249-265
# Algorithm 8 adopted here.
# define m, as used on page 262.

dpbbm_mc_iterations <- function(x, size.x, m = 1, max_iter = 2000, a = 0.1, b = 1, tau = 1, sig_alpha = 25/9, sig_beta = 25/9, tau.method = "auto", debug = FALSE){
  
  mat <- list()
  
  # first check the dimensional compatibility
  if (dim(x)[[1]] != dim(size.x)[[1]] | dim(x)[[2]] != dim(size.x)[[2]]) {
    stop("Dimension of matrix 1 should be the same as that of matrix 2!")
  }
  
  if (dim(x)[[1]] < dim(x)[[2]]) {
    warning("Number of features should be larger than number of samples for clustering")
  }
  
  if (!is.na(pmatch(tau.method, "auto"))) 
    method <- "euclidean"
  TAU.METHODS <- c("auto", "stable")
  method <- pmatch(tau.method, TAU.METHODS)
  if (is.na(method)) 
    stop("invalid update method for tau")
  if (method == -1) 
    stop("ambiguous update method for tau")  
  
  mat$k <- x
  mat$n <- size.x
  
  S <- dim(x)[2]
  
  c <- c(1:dim(x)[1])
  c.cluster <- as.numeric(table(c))
  k <- length(c.cluster)
  
  u_alpha <- rep(0, S) 
  u_beta <- rep(0, S) 
  sigma_alpha <- rep(sig_alpha,S)
  sigma_beta <- rep(sig_beta,S)
  prior_dbeta <- list()
  prior_dbeta$u_alpha <- u_alpha
  prior_dbeta$u_beta <- u_beta
  prior_dbeta$sigma_alpha <- sigma_alpha
  prior_dbeta$sigma_beta <- sigma_beta
  prior_dbeta$a <- a
  prior_dbeta$b <- b
  
  Phi_alpha  <- .generate_u_param(u_alpha,sigma_alpha,k, debug)
  Phi_beta  <- .generate_u_param(u_beta,sigma_beta,k, debug)
  
  Sigma_alpha <- matrix(rep(sigma_alpha,times = k),byrow = TRUE, ncol = S)
  Sigma_beta <- matrix(rep(sigma_beta,times = k),byrow = TRUE, ncol = S)
  
  if (method == 1){
    tau <- .dpbbm_update_tau(c.cluster,prior_dbeta,1)
  }
  pi <- dirichletrnd(c.cluster + tau/k, 1)
  likelihood <- c()
  likelihood_total <- c()
  for (index in 1:k){
    likelihood[index] <- (.dpbbm_update_likelihood(x[index,],size.x[index,],Phi_alpha[index,],Phi_beta[index,]))
  }
  likelihood_total[1] <- sum(pi * exp(likelihood[1:length(pi)])) 
  #  likelihood_total[1] <- sum(likelihood[1:length(pi)]) 
  tmp <- list()
  tmp$c <- c
  tmp$k <- length(as.numeric(table(c)))
  tmp$Phi_alpha <- Phi_alpha
  tmp$Phi_beta <- Phi_beta
  tmp$Sigma_alpha <- Sigma_alpha
  tmp$Sigma_beta <- Sigma_beta
  tmp$tau <- tau
  tmp$likelihood <- likelihood
  tmp$likelihood_total <- likelihood_total[1]
  
  
  
  param <- list()
  param[[1]] <- tmp
  k[1] <- length(as.numeric(table(param[[1]]$c)))
  print("Gibbs sampling started. It will take a long time.")
  print("Shown only the clustering information in the first 20 iterations.")
  print(c(1, as.numeric(table(param[[1]]$c))))
  
  for (iter in 2:max_iter){
    param[[iter]] <- .dpbbm_mc_single_iter(mat, param[[iter-1]], m, prior_dbeta, tau.method = method, debug = debug)
    if (iter < 21) {
      print(c(iter, as.numeric(table(param[[iter]]$c))))
    }
    
    k[iter] <- length(as.numeric(table(param[[iter]]$c)))
    tmp <- param[[iter]]$k
  }
  #return(param)
  cluster_result <- .choose_cluster_id_by_mode(param)
  return(cluster_result)
}