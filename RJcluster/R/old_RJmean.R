RJ_mean_old = function(K, class, GG, GG_new)
{
  gamma = list()
  mean.diag = rep(1,K)
  nn = matrix(0, nrow = K, ncol = K)
  mean_off = matrix(0, nrow = K, ncol = K)
  N = length(class)
  
  for (i in 1:K)
  {
    temp = which(class == i) 
    # if no cluseter is found, then ignore the gamma and keep looping
    if (length(temp) > 0)
    {
      gamma[[i]] =  temp
    }
  }
  
  K = length(unique(class))
  for (i in 1:K)
  { 
    if (length(gamma) < i) break
    if (length(gamma[[i]]) <= 1) break
    
    for (j in i:K)
    {
      if (length(gamma) < j) break
      if (length(gamma[[j]]) <= 1) break
      
      if (i == j)
      { 
        #homogenous-off diagonals
        GG_1 = GG[gamma[[i]], gamma[[j]]]
        nn[i,j] = (length(GG_1) - nrow(GG_1))
        mean.diag[i] = mean(diag(GG_1))   #diagonal mean
        mean_off[i,j] = (sum(GG_1) - sum(diag(GG_1)))/nn[i,j]  #offdiagonals mean
      }
      else
      {
        #heterogeneous -off diagonals 
        GG_2 = GG[gamma[[i]], gamma[[j]]]
        mean_off[i,j] = mean(GG_2)
      }
    }
  }
  
  # mean.diag     # real diagonals 
  # mean_off      # diagonals are homogeneous off diagonals
  if (K > 1)
  {
    mean_off =  mean_off + t(mean_off) - diag(diag(mean_off))
  }
  
  
  label = class
  MU = matrix(0, nrow = K, ncol = N + 1)
  for (i in 1:K)
  {
    # if there is only one element in a cluster, then take the mean to be simply the row vector for that particular 
    # observation
    if (length(gamma[[i]]) <= 1)
    {
      MU[i,] = GG_new[gamma[[i]],]
    } else {
      object  =  gamma[[i]][1]
      for (j in 1:N)
      {
        if (label[j] == label[object])
        {
          MU[i, j] = diag(mean_off)[label[object]]
        }
        if (label[j] != label[object])
        {
          MU[i, j] = mean_off[label[object], label[j]]
        }
      }
      MU[i, N + 1] = mean.diag[label[object]]
    }
  }
  
  
  return(MU)
}



### function input: G, C, z, N
### parameters estimates: ss.diag, ss.off, sss, mean.diag, mean.off, ss.bound 
### function output : List of Cov Matrix 1,....,C.   
Gcov_old = function(GG, C, z, N)  
{
  
  ss.diag = rep(1,C) 
  mean.diag = rep(1,C)
  ss.off = ss.bound = nn = matrix(0, nrow = C, ncol = C)
  mean.off = matrix(0, nrow = C, ncol = C)
  sss = nnn = array(0,dim = c(C, C, C))
  gamma = list()
  Cov = list()
  
  labels = vector()
  for (i in 1:C)
  {
    temp = which(z[,i] == 1) 
    # if no cluseter is found, then ignore the gamma and keep looping
    if (length(temp) > 0)
    {
      gamma[[i]]      =  which(z[,i] == 1) 
      labels[gamma[[i]]] =  i
    }
  }
  
  ###Diagonal Covariance Estimates:
  ###Total number of Diagonal Parameters: (C+1)  
  # reset labels to all be 1....true max.c
  C = max(labels)
  for (i in 1:C)
  { 
    if (length(gamma[[i]]) <= 1) break
    for (j in i:C)
    {
      if (length(gamma[[j]]) <= 1) break
      
      if (i == j)
      { 
        #homo-off diagonals
        GG_1 = GG[gamma[[i]], gamma[[j]]]
        nn[i,j] = (length(GG_1) - nrow(GG_1))
        mean.diag[i] = mean(diag(GG_1))   #diagonal mean
        ss.diag[i] = var(diag(GG_1))                #diagonal variance
        M = mean.off[i,j] = (sum(GG_1) - sum(diag(GG_1)))/nn[i,j]  #offdiagonals mean
        ss.off[i,j] = (sum((GG_1 - M)^2) - sum(diag((GG_1 - M)^2)))/nn[i,j] #offdiagonals variance  
      }
      else
      {
        #hetero-off diagonals 
        GG_2                  = GG[gamma[[i]], gamma[[j]]]
        nn[i,j]             = length(GG_2)
        M = mean.off[i,j]   = mean(GG_2)
        ss.off[i,j]         = sum((GG_2 - M)^2)/nn[i,j]
      }
    }
  }
  
  ss.off =  ss.off + t(ss.off) - diag(diag(ss.off))
  
  ####### ss.diag and ss.off are completely cluster specific and symmetric 
  ###Boundary Covariance Estimates:  non-symmetric
  ###Total number of Boundary parameters: C  
  
  for (i in 1:C)
  {
    if (length(gamma[[i]]) <= 1) break
    
    GG_1 = GG[gamma[[i]], gamma[[i]]]
    for (j in 1:C)
    {
      if (length(gamma[[j]]) <= 1) break
      
      if (i == j)
      {
        nn[i,j] = (length(GG_1) - nrow(GG_1))
        s_homogenous = 0
        for (k in 1:nrow(GG_1))
        {
          s_homogenous =  s_homogenous + sum((GG_1[k,] - mean.off[i,j])*(GG_1[k,k] - mean.diag[i])) - 
            (GG_1[k,k] - mean.off[i,j])*(GG_1[k,k] - mean.diag[i])
        }
        ss.bound[i,j] = s_homogenous/nn[i,j]                                 
      }
      else
      {
        GG_2 = GG[gamma[[i]], gamma[[j]]]
        GG_2 = GG_2 - mean.off[i,j]   
        nn[i,j] = length(GG_2)
        s_heterogeneous = 0
        for (k in 1:nrow(GG_2))
        {
          s_heterogeneous = s_heterogeneous + sum(GG_2[k,] * (GG_1[k, k] - mean.diag[i])) 
        }
        ss.bound[i,j] = s_heterogeneous/nn[i,j]    
      }
    }
  }
  
  #Offdiagonals Estimates: symmetric 
  ## 1<=ii <=jj <= kk <= C 
  ## Total number of parameters = C choose 3 + 2*C choose 2 + C choose 1. 
  ## Hardest part. 
  
  for (i in 1:C)
  {
    if (length(gamma[[i]]) <= 1) break
    
    for (j in i:C)
    { 
      if (length(gamma[[j]]) <= 1) break
      
      if (j == i)
      {
        GG_1 = GG[gamma[[i]], gamma[[j]]]
        GG_1 = (GG_1 - mean.off[i,j]) + diag(diag(GG_1) - mean.diag[i]) - diag(diag(GG_1) - mean.off[i,j])  
      }
      else 
      {
        GG_1 = GG[gamma[[i]], gamma[[j]]]
        GG_1 = GG_1 - mean.off[i,j]
      }  
      for (k in j:C)
      { 
        if (length(gamma[[k]]) <= 1) break
        
        GG_2 = GG[gamma[[i]], gamma[[k]]]
        GG_2 = GG_2 - mean.off[i,k]
        if (i == j && j == k )
        {
          GG_2 = GG[gamma[[i]], gamma[[k]]]
          GG_2 = (GG_2 - mean.off[i,k]) + diag(diag(GG_2) - mean.diag[i]) - diag(diag(GG_2) - mean.off[i,k])  
          S = 0
          nnn[i,j,k] = nrow(GG_1)*ncol(GG_1)*ncol(GG_2) - length(GG_1) - length(GG_2) + nrow(GG_1)
          
          for (r1 in 1:nrow(GG_1))
          {
            S = S + sum(kronecker(GG_1[r1,],GG_2[r1,])) - sum(GG_1*GG_2) - sum(GG_1[r1,r1]*GG_2[r1,]) + sum((GG_1[r1,r1])^2) 
          }
          sss[i,j,k] = S/nnn[i,j,k]
        }
        
        if (i == j && j < k)
        {
          S = 0
          nnn[i,j,k] = nrow(GG_1)*ncol(GG_1)*ncol(GG_2) - length(GG_2)
          
          for (r1 in 1:nrow(GG_1))
          {
            S = S + sum(kronecker(GG_1[r1,],GG_2[r1,])) - sum(GG_1[r1,r1]*GG_2[r1,])
          }
          sss[i,j,k] = S/nnn[i,j,k]
        }
        
        if (i < j && j == k)
        {
          S = 0
          nnn[i,j,k] = nrow(GG_1)*ncol(GG_1)*ncol(GG_2) - length(GG_1)
          
          for (r1 in 1:nrow(GG_1))
          {
            S = S + sum(kronecker(GG_1[r1,],GG_2[r1,])) - sum(GG_1[r1,]*GG_2[r1,])
          }
          sss[i,j,k] = S/nnn[i,j,k]
        }
        
        if (i < j && j < k)
        {
          S = 0
          nnn[i,j,k] = nrow(GG_1)*ncol(GG_1)*ncol(GG_2)
          for (r1 in 1:nrow(GG_1))
          {
            S = S +  sum(kronecker(GG_1[r1,],GG_2[r1,])) 
          }
          sss[i,j,k] = S/nnn[i,j,k]
        }
        sss[i,k,j] = sss[j,k,i] = sss[k,j,i] = sss[k,i,j] = sss[j,i,k] = sss[i,j,k]
      }
    }
  }
  
  ### Estimating all the cluster Covariance Matrix: 
  for (c in 1:C)
  {
    Cov[[c]] = matrix(0, nrow = (N + 1), ncol = (N + 1))
    Cov[[c]][(N + 1),(N + 1)] = ss.diag[c]                 #boundary
    for (i in 1:N)
    {
      if (i != gamma[[c]][1]) 
      {
        for (jj in i:N)
        {
          if (jj != gamma[[c]][1])
          {
            if (i == jj)                              #diagonals 
            {
              Cov[[c]][i,jj] = ss.off[c,labels[i]]
            } 
            else
              Cov[[c]][i,jj] = sss[c,labels[i],labels[jj]]  #offdiagonals 
          }
          Cov[[c]][jj,i] = Cov[[c]][i,jj]
        }
        Cov[[c]][(N + 1), i] = Cov[[c]][i, (N + 1)] =  ss.bound[c, labels[i]]      #boundary          
      }
    }
    # Covariance on the Average Column 
    Cov[[c]][gamma[[c]][1], 1:N] = colSums(Cov[[c]][1:N, 1:N])/(N - 1)
    Cov[[c]][1:N, gamma[[c]][1]] = t(Cov[[c]][gamma[[c]][1], 1:N])
    Cov[[c]][gamma[[c]][1], gamma[[c]][1]] = sum(diag(Cov[[c]])[1:N])/(N - 1)
    Cov[[c]][(N + 1), gamma[[c]][1]] = Cov[[c]][gamma[[c]][1],(N + 1)] = sum(Cov[[c]][1:N,(N + 1)])/(N - 1)
  }
  
  
  return(list(Cov = Cov, gamma = gamma))
}
