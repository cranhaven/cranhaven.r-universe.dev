Moments_int <- function(vQ, vTau) {
  
    dM1 = sum(vQ^1 * c(vTau[1], diff(vTau)))
    dM2 = sum(vQ^2 * c(vTau[1], diff(vTau)))
    dM3 = sum(vQ^3 * c(vTau[1], diff(vTau)))
    dM4 = sum(vQ^4 * c(vTau[1], diff(vTau)))
    
    dM2_C = sum((vQ - dM1)^2 * c(vTau[1], diff(vTau)))
    dM3_C = sum((vQ - dM1)^3 * c(vTau[1], diff(vTau)))
    dM4_C = sum((vQ - dM1)^4 * c(vTau[1], diff(vTau)))
    
    vM = c(m1 = dM1,
           m2 = dM2,
           m3 = dM3,
           m4 = dM4
    )
    
    vM_C = c(m1 = dM1,
           m2 = dM2_C,
           m3 = dM3_C,
           m4 = dM4_C
    )
    
  return(list(vM = vM, vM_C = vM_C))
}

MomentsDMQ <- function(Fit) {
  
  vTau = Fit$vTau
  mQ = t(Fit$lFilter$mQ)
  
  lMoments = (apply(mQ, 1, Moments_int, vTau = vTau))
  
  mMoments = do.call(rbind, lapply(lMoments, function(x) x$vM))
  mCenterdMoments = do.call(rbind, lapply(lMoments, function(x) x$vM_C))
  
  vSkew = mCenterdMoments[, 3]/(mCenterdMoments[, 2]^(3/2))
  vKurt = mCenterdMoments[, 4]/(mCenterdMoments[, 2]^2)
  
  lOut = list(mMoments = mMoments,
              mCenterdMoments = mCenterdMoments,
              vSkew = vSkew,
              vKurt = vKurt)
  
  return(lOut)
  
}
