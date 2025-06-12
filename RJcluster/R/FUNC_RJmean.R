RJ_mean_c = function(K, class, GG, GG_new)
{
  N = length(class)
  
  values = table(class)
  real_class = vector()
  for (i in 1:length(values))
  {
    real_class = c(real_class, rep(i, as.numeric(values[i])))
  }
  class = real_class

  # get the gamma values  
  gamma = get_gamma_c(class, K)
  gamma = gamma[unlist(lapply(gamma, length)) > 0]
  
  K = length(unique(class)) # redefine K 
  temp = get_mean_values_c(K, gamma, GG)
  mean_off = temp$mean_off
  mean_diag = temp$mean_diag
  
  # mean_diag     # real diagonals 
  # mean_off      # diagonals are homogeneous off diagonals
  if (K > 1)
  {
    mean_off = mean_off + t(mean_off) - diag(diag(mean_off))
  }
  
  # get mu values
  MU = get_mu_c(class, gamma, GG_new, N, K, mean_off, mean_diag)
  
  return(MU)
}


RJ_mean = function(K, class, GG, Mean1){
  
  gamma     = list()
  mean.diag = rep(1,K)
  nn        = matrix(0, nrow = K, ncol = K)
  mean.off  = matrix(0, nrow = K, ncol = K)
  N         = length(class)
  
  for (ii in 1:K)
  {
    gamma[[ii]]         =  which(class == ii) 
    if (length(gamma[[ii]]) == 0)
    {
      return(Mean1)
      break
    }
  }
  
  
  for (ii in 1:K)
  { 
    for (jj in ii:K)
    {
      if (ii == jj)
      { 
        #homo-off diagonals
        GG_1                = GG[gamma[[ii]], gamma[[jj]]]
        nn[ii,jj]           = (length(GG_1) - nrow(GG_1))
        mean.diag[ii]       = mean(diag(GG_1))   #diagonal mean
        mean.off[ii,jj]     = (sum(GG_1) - sum(diag(GG_1)))/nn[ii,jj]  #offdiagonals mean
      }
      else
      {
        #hetero-off diagonals 
        GG_2                   = GG[gamma[[ii]], gamma[[jj]]]
        mean.off[ii,jj]        = mean(GG_2)
      }
    }
  }
  
  # mean.diag     # real diagonals 
  # mean.off      # diagonals are homogeneous off diagonals
  if (K > 1)
  {
    mean.off      =  mean.off + t(mean.off) - diag(diag(mean.off))
  }
  
  
  label = class
  MU = matrix(0, nrow = K, ncol = N+1)
  for (ii in 1 : K)
  {
    if (length(gamma[[ii]]) == 0)
    {
      break
    }
    else{
      object  =  gamma[[ii]][1]
      for (jj in 1:N)
      {
        if (label[jj] == label[object])
        {
          MU[ii, jj] = diag(mean.off)[label[object]]
        }
        if (label[jj] != label[object])
        {
          MU[ii, jj] = mean.off[label[object], label[jj]]
        }
      }
      MU[ii, N + 1] = mean.diag[label[object]]
    }
  }
  
  return(MU)
  
}



