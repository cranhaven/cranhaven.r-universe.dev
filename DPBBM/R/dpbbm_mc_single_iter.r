# MC update for a single iteration
# input: data - 
#        param - latest parameters 
#        m - the number of empty cluster used in MC
#        debug - whether need to print the procedure
# output: param - updated parameters

# author: Radford M. Neal 
# title: Markov Chain Sampling Methods for Dirichlet Process Mixture Models
# published in: Journal of Computational and Graphical Statistics, vol. 9, No.2,pp.249-265
# Algorithm 8 adopted here.

.dpbbm_mc_single_iter <- function(mat, param, m, prior_dbeta, tau.method, debug = FALSE){
  
  x <- mat$k
  size.x <- mat$n
  S <- dim(x)[2]
  c.length <- dim(x)[1]
  
  c <- param$c
  Phi_alpha <- param$Phi_alpha 
  Phi_beta <- param$Phi_beta
  Sigma_alpha <- param$Sigma_alpha 
  Sigma_beta <- param$Sigma_beta 
  tau <- param$tau 
  likelihood <- param$likelihood
   

  u_alpha <- prior_dbeta$u_alpha 
  u_beta <- prior_dbeta$u_beta 
  sigma_alpha <- prior_dbeta$sigma_alpha 
  sigma_beta <- prior_dbeta$sigma_beta
  sig_alpha <- prior_dbeta$sig_alpha
  sig_beta <- prior_dbeta$sig_beta
  a <- prior_dbeta$a 
  b <- prior_dbeta$b


  # step one in Algorithm 8
  for (i in 1:c.length){
    if(debug) print("step one in Algorithm 8")
    #if (debug) print(i)
    c.cluster <- as.numeric(table(c))
    k <- length(c.cluster)
    
 #   if(debug) print(c(i, k))
    if(c.cluster[c[i]] == 1){
      if(c[i] != k){
        # singleton for c[i] and c[i] is not the last cluster
        #  exchange c[i] with k, as well as Phi
        id <- which(c == k)
        c[id] <- c[i]
        Phi_alpha[c[i],] <- Phi_alpha[k,]
        Phi_beta[c[i],] <- Phi_beta[k,]
        Sigma_alpha[c[i],] <- Sigma_alpha[k,]
        Sigma_beta[c[i],] <- Sigma_beta[k,]
      }
      k <- k - 1
    }
    h <- k + m
    
    # generate auxiliary parameters for c[i]
    Phi_alpha <- rbind(Phi_alpha[1:k,], .generate_u_param(u_alpha, sigma_alpha, m, debug))  # G_0
    Phi_beta <- rbind(Phi_beta[1:k,], .generate_u_param(u_beta, sigma_beta, m, debug))    # G_0
    Sigma_alpha <- rbind(Sigma_alpha[1:k,], matrix(rep(sigma_alpha,times = m),byrow = TRUE, ncol = S))
    Sigma_beta <- rbind(Sigma_beta[1:k,], matrix(rep(sigma_beta,times = m),byrow = TRUE, ncol = S))

    # probabilities to resample c[i]
    if(debug) print("sampling the cluster label for current sample")
    prop <- c()
    for (index in 1:h){
      likelihood[index] <- exp(.dpbbm_update_likelihood(x[i,],size.x[i,],Phi_alpha[index,],Phi_beta[index,]))
    }
    c.cluster <- as.numeric(table(c[-i]))
    prop[1:k] <- c.cluster*(likelihood[1:k])
    prop[(k+1):h] <- tau/m*(likelihood[(k+1):h])
    
    c[i] <- sample(1:h,1,prob = prop)
    if(c[i] > k + 1){
      # exchange all the settings to k+1 with c[i]
      Phi_alpha[k+1,] <- .generate_u_param(u_alpha, sigma_alpha, 1, debug)#Phi_alpha[c[i],]
      Phi_beta[k+1,] <- .generate_u_param(u_beta, sigma_beta, 1, debug)#Phi_beta[c[i],]
      Sigma_alpha[k+1,] <- Sigma_alpha[c[i],]
      Sigma_beta[k+1,] <- Sigma_beta[c[i],]
      c[i] <- k + 1
    }
  }
  
  param$c <- c
  param$k <- length(as.numeric(table(c)))
  param$Phi_alpha <- Phi_alpha
  param$Phi_beta <- Phi_beta
  param$Sigma_alpha <- Sigma_alpha
  param$Sigma_beta <- Sigma_beta

  # update the c.cluster as well as k
  c.cluster <- as.numeric(table(c))
  k <- length(c.cluster)

  if (tau.method == 1){
	  # update \tau
	  tau <- .dpbbm_update_tau(c.cluster, prior_dbeta, tau)
  	  param$tau <- tau
  }
  # update \pi
  pi <- dirichletrnd(c.cluster + tau/k, 1)
  param$pi <- pi
  
  # step 2 in Algorithm 8
  likelihood <- c()
  for (i in 1:k){
    if(debug) print("step 2 in Algorithm 8")
    # use metropolis hasting algorithm to sample Phi
    # start point: Phi_xx[i,] 
    # expected end point: Phi_xx_new 
    idx <- which(c == i)
    
    # updating alpha 
    Phi_alpha_new <- .generate_u_param(Phi_alpha[i,], Sigma_alpha[i,], 1, debug)
    # updating beta
    Phi_beta_new <- .generate_u_param(Phi_beta[i,], Sigma_beta[i,], 1, debug)

    likelihood[i] <- .dpbbm_update_likelihood(x[idx,],size.x[idx,],Phi_alpha[i,],Phi_beta[i,])
    likelihood_new <- .dpbbm_update_likelihood(x[idx,],size.x[idx,], Phi_alpha_new, Phi_beta_new)
    Fx <- dtmvnorm(Phi_alpha_new,mean=rep(0,S), sigma = diag(sigma_alpha),lower=rep(0,S))*dtmvnorm(Phi_beta_new,mean=rep(0,S), sigma = diag(sigma_beta),lower=rep(0,S))
    Fx_old <- dtmvnorm(Phi_alpha[i,],mean=rep(0,S), sigma = diag(sigma_alpha),lower=rep(0,S))* dtmvnorm(Phi_beta[i,],mean=rep(0,S), sigma = diag(sigma_beta), lower=rep(0,S))
 

    # accept the new cluster parameter
    tmp <- runif(1)
    if (tmp < exp(likelihood_new-likelihood[i])*exp(sum(Phi_alpha[i,]) + sum(Phi_beta[i,]) - sum(Phi_alpha_new) - sum(Phi_beta_new))*Fx/Fx_old) {
      # accept the new value for Phi
      Phi_alpha[i,] <- Phi_alpha_new
      Phi_beta[i,] <- Phi_beta_new
      
      likelihood[i] <- likelihood_new
    }
  }
  likelihood_total <- sum(pi * exp(likelihood[1:k])) 
  #likelihood_total <- sum(likelihood[1:k]) 
  
  if(likelihood_total >= param$likelihood_total){
    # if likelihoold is larger this single iteration, accept the updated parameters
    param$Phi_alpha <- Phi_alpha
    param$Phi_beta <- Phi_beta
  }
  param$likelihood <- likelihood
  param$likelihood_total <- likelihood_total
  return(param)
}