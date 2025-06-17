##########################################################################
# generate data based on beta binomial mixture model
bbm_data_generate<- function(S=3, G=50, K=3, prob=rep(1,times=3),
                              alpha_band=c(2,6),
                              beta_band=c(2,6),
                              nb_mu=100,nb_size=0.2, plotf = FALSE, 
                              max_cor=0.5){
  
  prob = prob/sum(prob)
  ff <- "repeat"
  ct <- 1 # iteration limitation
  while (ff=="repeat") {
    # generate the starting point
    # generate cluster ID
    gamma <- sample.int(K, size = G, replace = TRUE, prob = prob)  
    # generate alpha
    alpha <- exp(matrix(runif(K*S, min = alpha_band[1], max = alpha_band[2]),nrow=K))
    # generate_u_param(runif(S, min = alpha_band[1], max = alpha_band[2]), rep(4, S), n = K)
    # alpha <- exp(abs(alpha))
    #   
    
    # generate beta
    beta <- exp(matrix(runif(K*S, min = beta_band[1], max = beta_band[2]),nrow=K)) 
    # generate_u_param(runif(S, min = beta_band[1], max = beta_band[2]),  rep(4, S), n = K)
    # beta <- exp(abs(beta))
    
    mu <- alpha/(alpha+beta) # parameter for betabinomial
    a <- abs(cor(t(mu)))
    ID <- upper.tri(a,diag=F)
    m <- a[ID]
    # min_c <- min(m)
    max_c = max(m) # keep the sampled data relatively correlated, not too high, not too low
    if (max_c < max_cor) {ff = "go"; 
    # if (min_c > max_cor) {ff = "go"; 
    print(paste("after tried",ct,"times"))
    print(paste("sample data generated with correlation smaller than:",max_cor))}
    # print(paste("sample data generated with correlation larger than:",max_cor))}
    if (ct > 1000) {ff = "go"; 
    print(paste("Fail to generate data with correlation smaller than:",max_cor))
    print("clusters can be highly correlated ...")
    }
    ct <- ct + 1
  }
  
  # generate the number of reads (total number of reads for samples)
  n <- matrix(rnbinom(G*S, mu = nb_mu, size=nb_size),nrow=G) # generate the number of reads
  alpha_g <- alpha[gamma,]
  beta_g <- beta[gamma,]
  
  # generate the random number (number of reads for IP sample)
  k <- matrix(rbetabinom.ab(n=G*S, size=n, shape1=alpha_g, shape2=beta_g, .dontuse.prob = NULL),nrow=G)
  
  if (plotf == TRUE) {
    p <- k/n
    par(mfrow= c(K,1))
    for (m in 1:K) {
      plot( rep(1:S,sum(gamma==m)),t(p[gamma==m,]), main=m)
    }
  }
  
  
  # save the data
  mat=list(S=S, G=G, K=K, alpha_band=alpha_band,
            beta_band=beta_band, nb_mu=nb_mu, nb_size=nb_size, 
            gamma=gamma, alpha=alpha, beta=beta,
            k=k, n=n, c=n-k)
  return(mat)
}