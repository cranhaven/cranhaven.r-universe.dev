v_to_vobs = function(V, X, kappa, C, baseline.test){

  kap = kappa
  l = numeric()
  n = length(V)
  for(i in 1:n) l[i] = max(cumsum(X[i] > V[[i]]))
  m = sapply(V, length)
  r = l + 1
  cens = (m-1) == l

  i=1
  P = list()
  k = numeric()
  # Unit not tested at baseline
  for(i in 1:n){
  if(baseline.test[i] == 0){
    if(C[i] == 0){
      if(!cens[i]){
        p = kap
        if((m-l-2)[i]>0){
          for(j in 1:(m-l-2)[i] ){
            p[j+1] = kap * (1-kap)^j
          }
        }
        p[length(p) + 1] = 1-sum(p)
        P[[i]] = p
        k[i] =sum(rmultinom(1,1,p) * 0:(length(p)-1))
      }
      if(cens[i]){
        P[[i]] = 1
        k[i] = 0
      }}
    # if(C[i] == 1){
    #   k[i] = 0
    # }
    if(C[i] == 1){
      p = numeric()
      for(j in 1:(m-2)[i] ){
        p[j] = kap * (1-kap)^(j-1)
      }
      p[length(p) + 1] = 1-sum(p)
      P[[i]] = p
      k[i]   = sum(rmultinom(1,1,p) * 0:(length(p)-1))
    }
  }
  # Unit tested at baseline
  if(baseline.test[i] == 1){
      if(C[i] == 0){
        if(!cens[i]){
          p = kap
          if((m-l-2)[i]>0){
            for(j in 1:(m-l-2)[i] ){
              p[j+1] = kap * (1-kap)^j
            }
          }
          p[length(p) + 1] = 1-sum(p)
          P[[i]] = p
          k[i] =sum(rmultinom(1,1,p) * 0:(length(p)-1))
        }
        if(cens[i]){
          P[[i]] = 1
          k[i] = 0
        }}
      if(C[i] == 1){
        p = numeric()
        for(j in 1:(m-1)[i] ){
          p[j] = kap * (1-kap)^(j-1)
        }
        p[length(p) + 1] = 1-sum(p)
        P[[i]] = p
        k[i]   = sum(rmultinom(1,1,p) * 0:(length(p)-1))
      }
    }
  }
  Vobs = list()
  for(i in 1:length(V)){
    if(baseline.test[i]==0){
      if(C[i] == 0) Vobs[[i]] = V[[i]][1: (l[i]+k[i]+1)]
      if(C[i] == 1) Vobs[[i]] = V[[i]][1: (k[i]+2)]
    }
    if(baseline.test[i]==1){
      if(C[i] == 0) Vobs[[i]] = V[[i]][1: (l[i]+k[i]+1)]
      if(C[i] == 1) Vobs[[i]] = V[[i]][1: (k[i]+1)]
    }
  }
  Vobs
}

