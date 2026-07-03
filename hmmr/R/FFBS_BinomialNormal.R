rdirichlet <- function(n, alpha) {
  # taken from gtools
  l <- length(alpha)
  x <- matrix(rgamma(l * n, alpha), ncol = l, byrow = TRUE)
  sm <- x %*% rep(1, l)
  x/as.vector(sm)
}

FFBS_BinomialNormal <- function(bin,norm,nstates,hyperPars=list(),ntimes,niter=1000,nburnin=0) {
  
  if(is.numeric(bin)) {
    if(!all(bin %in% c(0,1))) stop("bin must be a binary vector, factor, or matrix")
    bin_succ <- bin
    bin_fail <- 1 - bin_succ
  }
  if(is.factor(bin)) {
    bin_succ <- as.numeric(bin != levels(bin)[1])
    bin_fail <- 1 - bin_succ
  }
  if(is.matrix(bin)) {
    if(NCOL(bin) != 2) stop("bin must be a matrix with successes and failures in columns")
    bin_succ <- bin[,1]
    bin_fail <- bin[,2]
  }
  
  if(missing(ntimes)) ntimes <- length(norm)
  
  # counters
  lt <- length(ntimes)
  et <- cumsum(ntimes)
  bt <- c(1,et[-length(ntimes)] + 1)
  ns <- nstates
  nt <- sum(ntimes)
  
  # hyper-parameters
  if(is.null(hyperPars$init_alpha)) init_alpha <- rep(1,ns) else init_alpha <- hyperPars$init_alpha
  if(is.null(hyperPars$trans_alpha)) trans_alpha <- matrix(1,ncol=ns,nrow=ns) else trans_alpha <- hyperPars$trans_alpha
  bin_alpha <- ifelse(is.null(hyperPars$bern_alpha), 1, hyperPars$bern_alpha)
  bin_beta <- ifelse(is.null(hyperPars$bern_beta), 1, hyperPars$bern_beta)
  
  norm_mu_mean <- ifelse(is.null(hyperPars$norm_mu_mean), 0, hyperPars$norm_mu_mean)
  norm_mu_sca <- ifelse(is.null(hyperPars$norm_mu_sca), 1, hyperPars$norm_mu_sca)
  norm_invsigma_shape <- ifelse(is.null(hyperPars$norm_invsigma_shape), 1e-5, hyperPars$norm_invsigma_shape)
  norm_invsigma_scale <- ifelse(is.null(hyperPars$norm_invsigma_scale), 1e-5, hyperPars$norm_invsigma_scale)
  
  # output storage
  mcmc_samples <- list(states=matrix(0,nrow=niter-nburnin,ncol=nt),
                       init=matrix(0.0,nrow=niter-nburnin,ncol=ns),
                       transition=array(0.0,dim=c(niter-nburnin,ns,ns)),
                       pcor=matrix(0.0,nrow=niter-nburnin,ncol=ns),
                       mu=matrix(0.0,nrow=niter-nburnin,ncol=ns),
                       sigma=matrix(0.0,nrow=niter-nburnin,ncol=ns))
  
  # temporary storage
  A <- matrix(0.0,nrow=ns,ncol=ns)
  B <- matrix(0.0,nrow=nt,ncol=ns)
  scaled_alpha <- matrix(0.0,nrow=nt,ncol=ns)
  ct <- rep(0.0,nt)
  
  # draw a random state allocation vector
  sampled_states <- sample(1:ns,nt,replace=TRUE)
  
  # start main loop
  for(iter in 1:niter) {
    pcor <- mu <- sigma <- rep(0.0,ns)
    
    # compute "observed" counts of states and transitions
    state_counts <- init_counts <- rep(0,ns)
    trans_counts <- matrix(0,nrow=ns,ncol=ns)
    for(i in 1:lt) {
      init_counts[sampled_states[bt[i]]] <- init_counts[sampled_states[bt[i]]] + 1
      for(t in (bt[i] + 1):et[i]) {
        trans_counts[sampled_states[t-1],sampled_states[t]] <- trans_counts[sampled_states[t-1],sampled_states[t]] + 1
      }
    }
    # sample initial state probability vector
    init <- rdirichlet(1,init_alpha + init_counts)
    
    for(s in 1:ns) {
      state_counts[s] <- sum(sampled_states == s)
      # sample row of transition matrix
      A[s,] <- rdirichlet(1, trans_alpha[s,] + trans_counts[s,])
      
      # sample p(corr) for binomial
      pcor[s] <- rbeta(1,bin_alpha + sum(bin_succ[sampled_states == s]),bin_beta + sum(bin_fail[sampled_states == s]))
      
      # posterior shape of inv sigma
      ck <- norm_invsigma_shape + .5*state_counts[s]
      
      # "sample mean" of norm at state = s (may be undefined) 
      ms <- mean(norm[sampled_states==s])
      #if(is.na(ms)) ms <- 0 # to avoid NA later; note that if state_counts[s] == 0, the computed values of ck, Ck, bk, and Bk should be the prior values 
      
      # posterior scale of inv sigma
      Ck <- ifelse(state_counts[s] > 0, norm_invsigma_scale + .5*(sum((norm[sampled_states==s] - ms)^2) + ((state_counts[s]*norm_mu_sca)/(2*(state_counts[s] + norm_mu_sca)))*(ms-norm_mu_mean)^2), norm_invsigma_scale)
      sigma[s] <- 1/rgamma(1,ck,Ck)
      while(is.infinite(sigma[s])) sigma[s] <- 1/rgamma(1,ck,Ck)
      # posterior mean of mu 
      bk <- ifelse(state_counts[s] > 0, {norm_mu_sca/(state_counts[s] + norm_mu_sca)*norm_mu_mean + state_counts[s]/(state_counts[s] + norm_mu_sca)*ms}, norm_mu_mean)
      # posterior variance of mu
      Bk <- 1/(state_counts[s] + norm_mu_sca)*sigma[s]
      
      # sample mean
      mu[s] <- rnorm(1,bk,sqrt(Bk))
      if(is.na(mu[s])) cat("ck = ", ck, "; Ck = ", Ck, "; bk = ", Bk, "; Bk = ", Bk, "; ms =", ms, "; state count = ", state_counts[s], "; sigma = ", sigma[s], "\n")
    }
    
    # compute state-dependent densities
    for(s in 1:ns) {
      B[,s] <- dbinom(bin_succ,bin_succ + bin_fail,pcor[s])*dnorm(norm,mean=mu[s],sd=sqrt(sigma[s]))
    }
    
    
    for(i in 1:lt) {
      # run the scaled forward backward algorithm
      scaled_alpha[bt[i],] <- init[s]*B[bt[i],]
      ct[bt[i]] <- 1/sum(scaled_alpha[bt[i],])
      scaled_alpha[bt[i],] <- scaled_alpha[bt[i],]*ct[bt[i]]
      for(t in bt[i]:(et[i]-1)) {
        scaled_alpha[t+1,] <- (t(A)%*%scaled_alpha[t,])*B[t+1,]
        ct[t+1] <- 1/sum(scaled_alpha[t+1,])
        scaled_alpha[t+1,] <- ct[t+1]*scaled_alpha[t+1,]
      }
      # sample state at T
      sampled_states[et[i]] <- sample(1:ns,1,prob=scaled_alpha[et[i],])
      for(t in (et[i]-1):bt[i]) {
        # sample state at t
        tp <- A[,sampled_states[t+1]]*scaled_alpha[t,]
        tp <- tp/sum(tp)
        sampled_states[t] <- sample(1:ns,1,prob=tp)
      }
    }
    
    if(niter > nburnin) {
      mcmc_samples$states[iter-nburnin,] <- sampled_states
      mcmc_samples$init[iter-nburnin,] <- init
      mcmc_samples$transition[iter-nburnin,,] <- A
      mcmc_samples$pcor[iter-nburnin,] <- pcor
      mcmc_samples$mu[iter-nburnin,] <- mu
      mcmc_samples$sigma[iter-nburnin,] <- sigma
    }
  }
  return(mcmc_samples)
}