
ForecastDMQ<- function(Fit, H) {

  iT = ncol(Fit$lFilter$mEta) - 1
  iJ = length(Fit$vTau)
  iTau_star = Fit$iTau_star

  vXi_bar = Fit$lFilter$mEta[, 1]
  vTau = Fit$vTau
  vEta_tp1 = Fit$lFilter$mEta[, iT + 1]
  dQ_tp1 = Fit$lFilter$mQ[iTau_star, iT + 1]
  dQ_bar = Fit$lFilter$mQ[iTau_star, 1]

  dPhi = Fit$vPn["phi"]
  dGamma = Fit$vPn["gamma"]
  dBeta = Fit$vPn["beta"]
  dAlpha = Fit$vPn["alpha"]

  ScalingType = Fit$ScalingType
  vScaling = Fit$lFilter$vScaling

  #iTau_star - 1 since XiPrediction is zero based
  mXiPred = XiPrediction(vXi_bar, vTau, H, vEta_tp1,
                         dPhi, dGamma, iTau_star - 1, vScaling)

  vReferencePred = ReferencePrediction(H, dQ_bar, dBeta, dAlpha, dQ_tp1)

  mQ_pred = matrix(NA, H, iJ)
  mQ_pred[, iTau_star] = vReferencePred

  for (j in (iTau_star-1):1) {
    mQ_pred[, j] = mQ_pred[, j+1] - mXiPred[, j]
  }
  for (j in (iTau_star + 1):iJ) {
    mQ_pred[, j] = mQ_pred[, j - 1] + mXiPred[, j]
  }

  return(mQ_pred)

}
