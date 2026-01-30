r.trdist =  function(par, a = 0, b=Inf, dist , tol = 1e-8){ 
    n = length(a)
    cdf.a = pdist(a, par, dist)
    cdf.b = pdist(b, par, dist)
    dif = cdf.b-cdf.a
    dif = dif < tol
    u   = ifelse(dif, a, qdist( cdf.a + runif(n) * (cdf.b-cdf.a), par, dist ))
    return(u)
  }
