# X = as.matrix(read.csv("/Users/rachaelshudde/Desktop/TEST.csv"))
# scale version for RJ hockey stick 
RJ_hockey_stick_scale = function(X, num_cut, C_max = 10, seed = 1)
{
  p = ncol(X)
  N = nrow(X)
  set.seed(seed)
  fold_ids = sample(rep(seq_len(num_cut), length.out = N))

  d = 0
  CC = list()

  # can be parallelized
  # foreach(ii=1:num.samp) %dopar%
  for (ii in 1:num_cut)
  { 
    temp_index = which( fold_ids == ii )
    Z1 = X[temp_index, ]
    rj_out = RJ_hockey_stick(Z1, C_max)
    
    # seperating out the clusters
    # for (jj in 1:rj.out$K)
    # {
    #   CC[[d + jj]] = temp_index[which(rj.out$class == jj)]
    # }
    CC_temp = lapply(getCCmatrix_c(rj_out$class, temp_index, rj_out$K), as.vector) # needs to be updated, as.vec
    CC = c(CC, CC_temp)
    
    d = d + rj_out$K # counting how many clusters there are
  }

  # Z = X # unnecessary copy
  ZZ = getMatrixMeans_c(CC, X, d) # old C.means
  
  rj_Cmeans = RJ_hockey_stick(ZZ, min(d, C_max))
  K0 = rj_Cmeans$K
  
  ## C++ implemntaiton
  Group =  assignGroups_c(nrow(X), K0, rj_Cmeans$class, CC)
  
  # C++ implementation
  Lmark = getFinalMeans_c(K0, Group, X)

  LL  =  tcrossprod(X, Lmark) / p
  
  if (K0 == 1)
  {
    RJ_out_final = Mclust(LL, G = K0, verbose = FALSE)
    
  } else {
    RJ_out_final = Mclust(LL, modelNames = "VVI", G = K0, verbose = FALSE)
  }
  
  to_return = list(K = K0, class = RJ_out_final$class, penalty = rj_Cmeans$penalty, mclust_object = RJ_out_final)
  
  return(to_return)
  
}

RJ_bic_scale = function(X, num_cut, C_max = 10, seed = 1)
{
  
  p = ncol(X)
  N = nrow(X)
  
  # sample data
  set.seed( seed )
  fold_ids = sample( rep( seq_len(num_cut), length.out = N ))
  
  d = 0
  CC = list()
  
  # can be parallelized
  # foreach(ii=1:num.samp) %dopar%
  for (ii in 1:num_cut)
  { 
    temp_index = which( fold_ids == ii )
    Z1  = X[temp_index, ]
    GG1 = tcrossprod(Z1) / p # getting ZZ'
    gg_wodiag = GG1 - diag(diag(GG1)) # removing the diagonal
    cut = length(temp_index)
    GG_new = cbind(gg_wodiag + diag(colSums(gg_wodiag)/(cut - 1)), diag(GG1)) # adding diagonals as a new column
    
    MclustGG1 = Mclust(GG_new, modelNames = "VVI", verbose = F, G = 1:C_max) # calling actual clustering
    
    # for (jj in 1:MclustGG1$G)
    # {
    #   CC[[d + jj]] = temp_index[which(MclustGG1$classification == jj)]
    # }
    CC_temp = lapply(getCCmatrix_c(MclustGG1$class, temp_index, MclustGG1$G), as.vector) # needs to be updated, as.vec
    CC = c(CC, CC_temp)
    
    d = d + MclustGG1$G # counting how many clusters there are
  }
  
  ## C++ implemntaiton
  ZZ = getMatrixMeans_c(CC, X, d) # old Cmeans to prevent more copying

  p = ncol(ZZ)
  n = nrow(ZZ)
  GG = tcrossprod(ZZ) / p # getting ZZ'
  
  gg = GG
  gg_wodiag = gg - diag(diag(gg))
  GG_new = cbind(gg_wodiag + diag(colSums(gg_wodiag)/(n - 1)), diag(gg))
  
  Clust_J = Mclust(GG_new, modelNames = "VVI", verbose = F, G = 1:C_max)
  
  ## C++ implemntaiton
  Group =  assignGroups_c(nrow(X), Clust_J$G, Clust_J$classification, CC)
  
  ## C++ implemntaiton
  Lmark = getFinalMeans_c(Clust_J$G, Group, X)
  
  LL =  tcrossprod(X, Lmark) / p
  
  if (Clust_J$G == 1)
  {
    newRJ = Mclust(LL, G = Clust_J$G, verbose = F)
    
  } else {
    newRJ = Mclust(LL, modelNames = "VVI", G = Clust_J$G, verbose = F)
  }
  
  newRJ = Mclust(LL, modelNames = "VVI", G = Clust_J$G, verbose = F)
  
  to_return = list(K = newRJ$G, class = newRJ$classification, penalty = Clust_J$bic, mclust_object = newRJ)
  return(to_return)
}






