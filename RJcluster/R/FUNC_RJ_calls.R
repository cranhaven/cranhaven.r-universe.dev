# default 
# RJ_hockey_stick = function(X, kmax)
RJ_hockey_stick = function(X, C_max = 10, modelNames = "VVI", verbose = FALSE, seed = 1)
{

  N = nrow(X)
  p = ncol(X)
  GG =  tcrossprod(X, X)/p
  gg_wodiag =  GG - diag(diag(GG))
  GG_new = cbind(gg_wodiag + diag(colSums(gg_wodiag)/(N - 1)), diag(GG))
  Gclust  =  Mclust(GG_new, modelNames = "VVI", G = 1, verbose = verbose)
  M1 = Gclust$parameters$mean  #N by 1 matrix
  RJMean1 =  RJ_mean(1, Gclust$class, GG, t(M1))
  W1 = vector(length = C_max - 1)

  # find all possiblities 
  for (i in 2:C_max)
  {
    Gclust = Mclust(GG_new, G = i, modelNames = modelNames, verbose = verbose)
    RJMean = RJ_mean(i, Gclust$class, GG, RJMean1)
    W1[i - 1] = kmeans(RJMean, centers = RJMean1, iter.max = 1000, algorithm = "Lloyd")$tot.withinss
    RJMean1 =  RJMean
  }

  W = W1 + (2:C_max) * (N + 1)/(p)
  K_hat =   which.min(W)
  GG_W =   Mclust(GG_new, modelNames = "VVI", G = K_hat , verbose = verbose)
  
  to_return = list(K = K_hat, class = GG_W$classification, penalty = W, mclust_object = GG_W)
  
  return(to_return)
}

# updated bic penalty for July 2021
RJ_bic = function(X, C_max, modelNames = "VVI", verbose = FALSE, seed = 1)
{
  set.seed(1)
  N = nrow(X)
  p = ncol(X)
  GG = tcrossprod(X, X)/p
  gg_wodiag =  GG - diag(diag(GG))
  GG_new =  cbind(gg_wodiag + diag(colSums(gg_wodiag)/(N - 1)), diag(GG))
  Gclust =  Mclust(GG_new, modelNames = modelNames, verbose = verbose, G = 1:C_max)
  
  to_return = list(class = Gclust$classification, K = Gclust$G, penalty = Gclust$BIC, mclust_object = Gclust)
  return(to_return)
}

#  old default RJ clust from April 2021 package
#  if use_bic == FALSE then aic is the default
RJclust_aic_bic = function(X, C_max = 10, use_bic = TRUE, use_aic = FALSE, modelNames = "VVI", verbose = FALSE)
{
  # source("RJ_mean.R")  
  # C_max = 10; use_bic = TRUE; use_aic = FALSE; modelNames = "VVI"
  
  if (use_bic & use_aic)
  {
    stop("Just one penalty can be used, set either use_bic == TRUE or use_aic == TRUE, not both")
  }
  
  p = ncol(X)
  N = nrow(X)
  GG = tcrossprod(X, X)/p
  gg_wodiag =  GG - diag(diag(GG))
  GG_new = cbind(gg_wodiag + diag(colSums(gg_wodiag)/(N - 1)), diag(GG))
  
  # initial clustering 
  # Gclust =  Mclust(GG_new, modelNames = modelNames, G = 1, verbose = F)
  # M1 = Gclust$parameters$mean  #N by 1 matrix
  # RJMean1 = RJ_mean(1, Gclust$class, GG)
  # W1 = NULL
  
  aic = vector(length = C_max)
  bic = vector(length = C_max)
  
  ### To Rachael: make this loop in C++ to make it faster.
  for (C in 1:C_max)
  {
    Gclust  = Mclust(GG_new, modelNames = modelNames, G = C, verbose = F)
    loglik  = Gclust$loglik
    nparams = nMclustParams(modelName = modelNames, d = ncol(GG_new), G = C)
    # nparams = nparams_G(C, N)

    if (use_bic)
    {
      bic[C] = 2 * loglik - nparams * log(p)
    }
    
    if (use_aic)
    {
      aic[C] = loglik - 2 * nparams
    }
    if (verbose)
    {
      print(paste("Evaluated at number of clusters = ", C))
    }
  }
  
  K = NULL

  if (use_bic)
  {
    K = which.max(bic)
  }
  if (use_aic)
  {
    K = which.max(aic)
  }
  
  results = Mclust(GG_new, modelNames = modelNames, G = K, verbose = F)
  class = results$classification
  prob = results$parameters$pro
  z = results$z      
  mu = RJ_mean_c(K, class, GG, GG_new)
  # mu = RJ_mean_old(K, class, GG, GG_new) # non-rcpp version

  if (use_bic)
  {
    to_return = list(K = K, class = class, penalty = bic, mean = mu, prob = prob, z = z)
  } else {
    to_return = list(K = K, class = class, penalty = aic, mean = mu, prob = prob, z = z)
  }
  
  return(to_return)
}

# timing
# new = microbenchmark::microbenchmark(RJclust_aic_bic(X), times = 5)
# old = microbenchmark::microbenchmark(RJclust_aic_bic(X), times = 5)


# group - true cluster labels basicall the y
# C_max - total number of clusters you want to find (like MClust's G)
# inter.max - default of 100
RJclust_fullcovariance = function(Z, C_max = 10, iter_max = 1000, verbose = FALSE)
{
  # set up RJ matrix
  p = ncol(Z)
  n = nrow(Z)
  GG = tcrossprod(Z, Z)/p
  gg = GG
  gg_wodiag = gg - diag(diag(gg))
  GG_new = cbind(gg_wodiag + diag(colSums(gg_wodiag)/(n - 1)), diag(gg))
  
  Lat = list(); 
  bic_eval1 = NULL
  N = n
  
  iter_max = 1000
  bic_eval1 = NULL 
  bic_eval1 = c(bic_eval1, Mclust(GG_new, G = 1, verbose = F)$bic)
  
  for (C in 2:C_max)
  { 
    begin = Sys.time()
    # initialize the paramaters
    mu = array(0, dim = c(C, N + 1))
    Sigma = array(0, dim = c(N + 1, N + 1, C))
    init = Mclust(GG_new, verbose = F, G = C)
    # init        =  Mclust(GG_new, verbose = F, modelNames = "VVI", G = C)
    
    # update the new paramaters 
    mu = t(init$parameters$mean)
    Sigma = init$parameters$variance$sigma
    prob = init$parameters$pr
    
    # calculate the bic
    bic.val = bic_G(C, GG_new, p, mu, Sigma, N, prob, iter_max, GG)
    bic_eval1 = c(bic_eval1, bic.val$bic.value)
    Lat[[C]] = bic.val$z

    end = Sys.time()
    
    if (verbose)
    {
      print(paste("Evaluated at number of clusters = ", C, "in", end - begin))
    }
    # print(end - bgin)
  }
  # plot(bic_eval1[1:6])
  
  n_clust = which(bic_eval1 == max(bic_eval1))
  Clust.bic = max(bic_eval1)
  Class.Matrix = Lat[[n_clust]]
  # Class.Sigma = Gcov(GG_new, n_clust, z = Class.Matrix, N)
  Class.member = list();     
  Sigma = list() 
  Class.pro = list() 
  Class.mean = list() 
  groupG = c(1:n)
  
  for (k in 1:n_clust)
  {
    Class.member[[k]] = which(Class.Matrix[,k] == 1)
    groupG[Class.member[[k]]] = k
    Class.mean[[k]] = colMeans(GG_new[Class.member[[k]], ]) 
    Class.pro[[k]] = length(Class.member[[k]])/N  
  }
  
  # to_return = list(bic = Clust.bic, bic_eval = bic_eval1, n_clust = n_clust, groupG = groupG, Sigma = Class.Sigma, mean = Class.mean, pro = Class.pro)
  to_return = list(K = n_clust, class = groupG, penalty = Clust.bic, mean = Class.mean, prob = unlist(Class.pro), z = NULL)
  
  return(to_return)
}
