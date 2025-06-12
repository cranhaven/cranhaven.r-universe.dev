##EM Algorithm for G-Matrix Cluster Estimation 
# lm(list = ls())
# library(MASS)
# library(mvtnorm)
# library(Rcpp)
# library(mclust)

# source("R/FUNC_Gcov.R")
# source("RJ_mean.R")
# # Rcpp::sourceCpp('Gcov.cpp')

##EM function
weights_multi = function(x, prob, mu, Sigma, C)
{
  ww = NULL
  for (k in 1:nrow(mu))
  {
    # ww = c(ww, prob[k] * dmvnorm(t(x), mean = mu[k,], Sigma[,,k], log = FALSE))
    ww = c(ww, prob[k] * dmvnrm_arma_fast(t(x), mean = mu[k,], Sigma[,,k], logd = FALSE))
  }

  to_return = ww/sum(ww)
  return(to_return)
}


loglik_G = function(y, prob, mu, Sigma, C, N)
{
  ll = NULL
  for (i in 1:N)
  {
    ww = NULL
    for (kk in 1:nrow(mu))
    {
      ww = c(ww, prob[kk] * dmvnorm(t(y[i,]), mean = mu[kk,], Sigma[,,kk], log = FALSE))
      # ww = c(ww, prob[kk] * dmvnrm_arma_fast(t(y[i,]), mean = mu[kk,], Sigma[,,kk], log = FALSE))
    }
    ll = c(ll, log(sum(ww)))
  }

  to_return = sum(ll)
  return(to_return)
}


# em algorithm
# here y is GG_new from the RJ function
emstep = function(y, C, mu, Sigma, prob, max_iter, N, GG)
{
  # multivariate normal log likelihood with 1 cluster.
  # loglik_new = sum(dmvnorm(y, mu[1,], Sigma[,,1], log = TRUE))  
  loglik_new = sum(dmvnrm_arma_fast(y, mu[1,], Sigma[,,1], logd = TRUE))  
  
  for (iter in 1:max_iter)
  {
    # for reproducibility, set the seed, commented out in library
    # set.seed(iter)
    
    #Estep:
    weights = weights_multi_matrix_c(y, prob, mu, Sigma, nrow(mu))
    z = t(apply(weights, 1, function(x) t(rmultinom(1, 1, x))))
    class_labels = apply(z, 1, which.max)
    
    # z = get_z_class_c(N, y, prob, mu, Sigma)

    GCOV = Gcov_c(y, nrow(mu), z, N)

    mu = RJ_mean_c(nrow(mu), class_labels, GG, y)
    
    # trucnate probability vector if the RJ_mean found fewer clusters
    prob = prob[1:nrow(mu)]

    ## Mstep Estimation:
    for (j in 1:nrow(mu))
    {
      if (sum(z[,j] <= 1))
      {
        break
      } else {
        y1 = y[which(z[,j] == 1), ]
        Sigma[,,j] = GCOV  #Calculated by G_cov function. 
        prob[j] = nrow(y1)/nrow(y)
      }
    }
    
    # calculate the loglikelihood diffrence and end the function if the difference is small enough
    loglik.diff = abs(loglik_new - loglik_G_c(y, prob, mu, Sigma, C, N)) 
    if (loglik.diff < 1e-6) 
    {
      break
    }
    else  {
      loglik_new = loglik_G_c(y, prob, mu, Sigma, C, N)
    }
  } 
  
  return(list(mu = mu, Sigma = Sigma, prob = prob, z = z))
}


nparams_G = function(C, N)
{
  # mu   
  nparams = C * (N + 1)
  # prob
  nparams = nparams + (C - 1)
  # Sigma:  3 * C + 1 + (C + 1) * C * (C - 1)/3 
  nparams = nparams + 4*C + 3*C*(C - 1)/2 + C*(C - 1)*(C - 2)/3 + (C + C + C*(C - 1)/2) 

  return(nparams)
}



bic_G = function(C, y, p, mu, Sigma, N, prob, iter_max, GG)
{
  mu = emstep(y, C, mu, Sigma, prob, iter_max, N, GG)$mu
  Sigma = emstep(y, C, mu, Sigma, prob, iter_max, N, GG)$Sigma
  z = emstep(y, C, mu, Sigma, prob, iter_max, N, GG)$z

  # check what BIC value should be returned
  if (any(colSums(z) <= 1))
  {
    bic_temp = -1e6
  } else {
    bic_temp = 2 * loglik_G_c(y, prob, mu, Sigma, nrow(mu), N) - (nparams_G(C, N) * log(p))
  }

  to_return = list(bic.value = bic_temp, z = z, Sigma = Sigma, mu = mu)
  return(to_return)
}





