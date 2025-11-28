

SimulateDMQ <- function(iT, vQ_0, vTau, iTau_star, vPn, 
                        ScalingType = "InvSqrt", fSim = NULL) {
  
  if(is.null(fSim)) {
    fSim = Sim_C
  }
  
  dBeta = vPn["beta"]
  dAlpha = vPn["alpha"]
  dGamma = vPn["gamma"]
  dPhi = vPn["phi"]
  
  vVar = Variances(vTau, iTau_star)
  
  lSim = Simulate_DMQ_C(iT, vTau, vQ_0, fSim, iTau_star, dBeta, dAlpha,
                      dGamma, dPhi, ScalingType, vVar) 
  
  vY = lSim$vY
  mQ = t(lSim$mQ)
  
  lOut = list(vY = vY,
              mQ = mQ)
  
  return(lOut)
  
}




