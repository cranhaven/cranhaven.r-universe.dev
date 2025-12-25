stdp = function(x) {
  mx = mean(x)
  return(sqrt(mean((x - mx)^2)))
}

colStdp = function(x) {
  return(apply(x, 2, stdp))
}

data.gen = function(sample_size=100, p=20, n_g_non_zero=15, n_gxe_non_zero=10, family="gaussian",
                    mode="strong_hierarchical", normalize=FALSE, normalize_response=FALSE, seed=1, pG=0.2, pE=0.3,
                    n_confounders=NULL){
  set.seed(seed)
  n_train = sample_size
  n_valid = round(sample_size / 5)
  n_test = n_train * 5
  
  if (family == "gaussian"){
    n = n_train + n_valid + n_test
    beta_0 = 0
    beta_c = 1
    if (mode == "strong_hierarchical" || mode == "anti_hierarchical"){
      beta_G = 3
      beta_E = 3
      beta_gxe = 1.5
    }
    if (mode == "hierarchical"){
      beta_G = 1.5 / 2
      beta_E = 1.5 / 2
      beta_gxe = 1.5
    }
  } else {
    if (family == "binomial"){
      n = n_train * 50
      beta_0 = -1
      beta_c = 0.8
      if (mode == "strong_hierarchical" || mode == "anti_hierarchical"){
        beta_G = 0.8
        beta_E = 0.8
        beta_gxe = 0.6
      }
      if (mode == "hierarchical"){
        beta_G = 0.4
        beta_E = 0.4
        beta_gxe = 0.6
      }
      
    } else {
      stop("unknown family")
    }
  }
  
  G = matrix(as.double(rbinom(n*p, 1, pG)), nrow=n, ncol=p)
  E = as.double(rbinom(n, 1, pE))
  GxE = G*E
  
  sign_G = rbinom(p, 1, 0.5)*2 - 1
  sign_GxE = rbinom(p, 1, 0.5)*2 - 1
  
  Beta_G = rep(beta_G, p) * sign_G
  Beta_GxE = rep(beta_gxe, p) * sign_GxE
  
  
  index = 1:p
  index_beta_non_zero = sample(index, n_g_non_zero)
  index_beta_zero = setdiff(index, index_beta_non_zero)
  Beta_G[index_beta_zero] = 0
  
  if (mode == "hierarchical" || mode == "strong_hierarchical"){
    index_beta_gxe_non_zero = sample(index_beta_non_zero, n_gxe_non_zero)
  }
  if (mode == "anti_hierarchical"){
    index_beta_gxe_non_zero = sample(index_beta_zero, n_gxe_non_zero)
  }
  index_beta_gxe_zero = setdiff(index, index_beta_gxe_non_zero)
  Beta_GxE[index_beta_gxe_zero] = 0
  
  if (!is.null(n_confounders)){
    C = matrix(rnorm(n * n_confounders), nrow=n, ncol=n_confounders)
    Beta_C = sign(rnorm(n_confounders))*beta_c
    lp = beta_0 + G %*% Beta_G + beta_E*E + GxE %*% Beta_GxE + C %*% Beta_C
  } else {
    Beta_C=NULL; C_train=NULL; C_valid=NULL; C_test=NULL
    lp = beta_0 + G %*% Beta_G + beta_E*E + GxE %*% Beta_GxE 
  }
  
  
  if (family == "binomial"){
    pr = 1/(1+exp(-lp))
    Y = rbinom(n, 1, pr)
    id = seq(1:n)
    cases = sample(id[Y == 1], length(id[Y == 1]))
    controls = sample(id[Y == 0], length(id[Y == 0]))
    
    n_train_2 = n_train/2; n_valid_2 = n_valid/2; n_test_2 = n_test/2
    stopifnot((length(cases) >= n_train_2 + n_valid_2 + n_test_2))
    stopifnot((length(controls) >= n_train_2 + n_valid_2 + n_test_2))
    
    index_train = c(cases[1:n_train_2], controls[1:n_train_2])
    index_valid = c(cases[(n_train_2 + 1): (n_train_2 + n_valid_2)],
                    controls[(n_train_2 + 1): (n_train_2 + n_valid_2)])
    index_test = c(cases[(n_train_2 + n_valid_2 + 1):(n_train_2 + n_valid_2 + n_test_2)],
                   controls[(n_train_2 + n_valid_2 + 1):(n_train_2 + n_valid_2 + n_test_2)])
  } else {
    error_var = 0.5
    Y = lp + rnorm(n, 0, error_var)
    index_train = 1:n_train
    index_valid = (n_train + 1):(n_train + n_valid)
    index_test = (n_train + n_valid + 1):(n_train + n_valid + n_test)
    #SNR_g = var(G %*% Beta_G)/(error_var^2)
    #SNR_gxe = var(GxE %*% Beta_GxE)/(error_var^2)
  }
  
  G_train = G[index_train,]
  G_valid = G[index_valid,]
  G_test = G[index_test,]
  
  if (!is.null(n_confounders)){
    C_train = C[index_train,]
    C_valid = C[index_valid,]
    C_test = C[index_test,]
  }
  
  E_train = E[index_train]
  E_valid = E[index_valid]
  E_test = E[index_test]
  
  if (family == "gaussian") {
    SNR_g = var(G_train %*% Beta_G)/(error_var^2)
    SNR_gxe = var((G_train * E_train) %*% Beta_GxE)/(error_var^2)
  } else {
    SNR_g=NULL; SNR_gxe=NULL
  }
  
  Y = as.numeric(Y)
  Y_train = Y[index_train]
  Y_valid = Y[index_valid]
  Y_test = Y[index_test]
  
  if (normalize) {
    #mean_G = colMeans(G_train); mean_E = mean(E_train)
    std_E = stdp(E_train)
    std_G = colStdp(G_train)
    
    if (family == "gaussian" && normalize_response){
      mean_Y = mean(Y_train)
      std_Y = stdp(Y_train)
      #Y_train = (Y_train - mean_Y) / std_Y; Y_valid = (Y_valid - mean_Y) / std_Y; Y_test = (Y_test - mean_Y) / std_Y
      
      Y_train = Y_train / std_Y
      Y_valid = Y_valid / std_Y
      Y_test = Y_test / std_Y
    }
    
    #G_train = (G_train - rep(mean_G, rep.int(nrow(G_train), ncol(G_train)))) / rep(std_G, rep.int(nrow(G_train), ncol(G_train)))
    #G_valid = (G_valid - rep(mean_G, rep.int(nrow(G_valid), ncol(G_valid)))) / rep(std_G, rep.int(nrow(G_valid), ncol(G_valid)))
    #G_test = (G_test - rep(mean_G, rep.int(nrow(G_test), ncol(G_test)))) / rep(std_G, rep.int(nrow(G_test), ncol(G_test)))
    #E_train = (E_train - mean_E) / std_E; E_valid = (E_valid - mean_E) / std_E, E_test = (E_test - mean_E) / std_E
    
    G_train = G_train / rep(std_G, rep.int(nrow(G_train), ncol(G_train)))
    G_valid = G_valid / rep(std_G, rep.int(nrow(G_valid), ncol(G_valid)))
    G_test = G_test / rep(std_G, rep.int(nrow(G_test), ncol(G_test)))
    
    if (!is.null(n_confounders)){
      std_C = colStdp(C_train)
      C_train = C_train / rep(std_C, rep.int(nrow(C_train), ncol(C_train)))
      C_valid = C_valid / rep(std_C, rep.int(nrow(C_valid), ncol(C_valid)))
      C_test = C_test / rep(std_C, rep.int(nrow(C_test), ncol(C_test)))
    }
    E_train = E_train / std_E
    E_valid = E_valid / std_E
    E_test = E_test / std_E
  }
  
  GxE_train = G_train * E_train
  GxE_valid = G_valid * E_valid
  GxE_test = G_test * E_test
  
  dataset = list(G_train=G_train, G_valid=G_valid, G_test=G_test,
                 E_train=E_train, E_valid=E_valid, E_test=E_test,
                 Y_train=Y_train, Y_valid=Y_valid, Y_test=Y_test,
                 C_train=C_train, C_valid=C_valid, C_test=C_test,
                 GxE_train=GxE_train, GxE_valid=GxE_valid, GxE_test=GxE_test,
                 Beta_G=Beta_G, Beta_GxE=Beta_GxE, beta_0=beta_0, beta_E=beta_E, Beta_C=Beta_C,
                 index_beta_non_zero=index_beta_non_zero, index_beta_gxe_non_zero=index_beta_gxe_non_zero,
                 index_beta_zero=index_beta_zero, index_beta_gxe_zero=index_beta_gxe_zero,
                 n_g_non_zero=n_g_non_zero,
                 n_gxe_non_zero=n_gxe_non_zero,
                 n_total_non_zero=n_g_non_zero + n_gxe_non_zero,
                 SNR_g=SNR_g,
                 SNR_gxe=SNR_gxe,
                 family=family,
                 p=p, sample_size=sample_size,
                 seed=seed,
                 mode=mode)
  return(dataset)
}
