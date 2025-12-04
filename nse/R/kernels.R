# Estimation of the generalized derivative of spectral density
# at the origin with the Bartlett kernel
.f.bartlett = function(g, st, q) {

  size = length(g)
  if (q == 0) {
    sbt = (t(g) %*% g) / size 
  } else {
    sbt = 0
  }

  if (st <= size - 1) {
    maxlagbt = st
  } else {
    maxlagbt = size - 1
  }

  jbt = 1
  while (jbt < maxlagbt) {
    xbt  = jbt / maxlagbt
    s0bt = (t(g[1:(size - jbt)]) %*% g[(1 + jbt):size]) / size 

    if (xbt <= 1) {
      wbt = 1 - xbt
    } else {
      wbt = 0
    }

    sbt = sbt + wbt * (jbt ^ q) * (s0bt + s0bt)
    jbt = jbt + 1
  }

  return(sbt)
}
f.bartlett = compiler::cmpfun(.f.bartlett)


# Estimation of the generalized derivative of spectral density 
# at the origin with the Parzen kernel
.f.parzen = function(g, st, q) {
  
  size = length(g)
  
  if (q == 0) {
    spz = (t(g) %*% g)/size
  } else {
    spz = 0
  }
  
  if (st <= size-1){
    maxlagpz = st
  } else {
    maxlagpz = size-1
  }
  
  jpz = 1
  while (jpz < maxlagpz) {
    xpz  = jpz / maxlagpz
    s0pz = (t(g[1:(size-jpz)]) %*% g[(1+jpz):size])/size
    
    if (xpz <= 1/2) {
      wpz = 1-1-6*xpz^2+6*xpz^3
    } else if(xpz <= 1){
      wpz = 2*(1-xpz)^3
    } else {
      wpz = 0
    }
    
    spz = spz+wpz*(jpz^q)*(s0pz+s0pz)
    
    jpz = jpz + 1
  }
  return(spz)
}
f.parzen = compiler::cmpfun(.f.parzen)

