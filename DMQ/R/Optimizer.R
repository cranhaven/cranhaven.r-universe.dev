
DMQ_Optimizer <- function(vPn, vY, vTau, iTau_star, vQ_0, FixReference, 
                          FixOthers, ScalingType, vVar, smooth, 
                          hessian_computation = FALSE, ...) {
  if(hessian_computation) {
    smooth = TRUE
  }
  if (FixReference) {
    vPn = c(vPn, alpha = 0, beta = 0)
  }
  if (FixOthers) {
    vPn = c(vPn, gamma = 0, phi = 0)
  }

  # iTau_star in zero based indexing 
  dLoss = try(FilterDMQ(vY, vTau, vQ_0, iTau_star - 1,
                     dBeta = vPn["beta"],
                     dAlpha = vPn["alpha"],
                     dGamma = vPn["gamma"],
                     dPhi = vPn["phi"], ScalingType = ScalingType,
                     vVar = vVar, 
                     smooth = smooth)$dLoss)
  
  if (!is.numeric(dLoss)) {
    dLoss = 1e10
  }
  
  if (!is.finite(dLoss)) {
    dLoss = 1e10
  }
  
  return(dLoss)

}
# 
# DMQ_Optimizer <- function(vPn, vY, vTau, iTau_star, vQ_0, FixReference, 
#                           FixOthers, ScalingType, vVar, smooth, 
#                           hessian_computation = FALSE, ...) {
#   if(hessian_computation) {
#     smooth = TRUE
#   }
#   if (FixReference) {
#     vPn = c(vPn, alpha = 0, beta = 0)
#   }
#   if (FixOthers) {
#     vPn = c(vPn, gamma = 0, phi = 0)
#   }
#   
#   # iTau_star in zero based indexing 
#   dLoss = try(DMQ:::FilterDMQ(vY, vTau, vQ_0, iTau_star - 1,
#                         dBeta = vPn["beta"],
#                         dAlpha = vPn["alpha"],
#                         dGamma = vPn["gamma"],
#                         dPhi = vPn["phi"], ScalingType = ScalingType,
#                         vVar = vVar, 
#                         smooth = smooth)$dLoss)
#   
#   foo = DMQ:::FilterDMQ(vY, vTau, vQ_0, iTau_star - 1,
#                   dBeta = vPn["beta"],
#                   dAlpha = vPn["alpha"],
#                   dGamma = vPn["gamma"],
#                   dPhi = vPn["phi"], ScalingType = ScalingType,
#                   vVar = vVar, 
#                   smooth = smooth)
#   
#   foo$mQ[, 1:2]
#   foo$mEta[, 1:2]
#   
#   if (!is.numeric(dLoss)) {
#     dLoss = 1e10
#   }
#   
#   print(dLoss)
#   print(vPn)
#   
#   
#   return(dLoss)
#   
# }
