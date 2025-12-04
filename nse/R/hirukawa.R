# Automatic Bandwidth selection for Bartlett and Parzen Kernel
# Describe in A Two-Stage Plug-In Bandwidth Selection And Its Implementation For Covariance Estimation

.f.hiruk.bandwidth.solve <- function(y, type = c("bartlett", "parzen"), lag.prewhite = 0){
  
  bandwidth.solve = function(y, alpha, q, kq, intk2, intx2k2, st0, f.kernel) {
    n   = length(y)
    bt  = ((alpha^2)*intk2*st0^(2*q+1)/((2*q+1)*intx2k2))^(1/(4*q+1)) # bandwidth
    rn  = f.kernel(y, bt, q) # estimate of spectral density at origin
    rd  = f.kernel(y, bt, 0) # estimate of q'th generalized derivative of spectral density at origin
    r2  = (rn/rd)^2 # squared normalized curvature
    st1 = (q*(kq^2)*r2*n/intk2)^(1/(2*q+1)) # calculate new bandwidth
    
    # if st1 = st0 optimal bandwidth is found
    out = abs(st0 - st1)
    return(out)
  }
  
  n = length(y)
  y = f.prewhite(y, ar.order = lag.prewhite)$ar.resid
  
  # ar coefficient to cacluclate Alpha(q)
  arCoefficient = stats::ar(y, aic = FALSE, order.max = 1)$ar[1]
  
  type = type[1]
  if (type == "bartlett") {
    q       = 1 #characteristic exponent of Bartlett kernel
    k1      = 1 #generalized derivative of Bartlett kernel at origin
    intk2   = 2/3 #squared integral of Bartlett kernel
    intx2k2 = 1/15 #2th-order moment of Bartlett kernel
    kernel  = f.bartlett
  } else if (type == "parzen") {
    q       = 2 #characteristic exponent of Parzen kernel
    k1      = 6 #generalized derivative of Parzen kernel at origin
    intk2   = 151/280 #squared integral of Parzen kernel
    intx2k2 = 929/295680 #4th-order moment of Parzen kernel
    kernel  = f.parzen
    
  } else {
    stop("Invalid type : must be one of c('bartlett','parzen')")
  }
  
  # proxy for alpha(q)
  if (q == 1) {
    alpha = (arCoefficient ^ 2  + 1) / (arCoefficient ^ 2 - 1)
    
  } else if (q == 2) {
    alpha = -(arCoefficient ^ 2 + 8 * arCoefficient + 1) / (arCoefficient - 1) ^ 2
  }
  
  # function to optimize
  FUN = function(x) bandwidth.solve(y, alpha ,q, k1, intk2, intx2k2, x, kernel)
  # optimization
  optimalBandwidth = stats::optimize(f = FUN, lower = 4, upper =  n)
  out = optimalBandwidth$minimum
  return(out)
}
f.hiruk.bandwidth.solve = compiler::cmpfun(.f.hiruk.bandwidth.solve)
