EstimateDMQ <- function(vY, vTau, iTau_star = NULL, vPn_Starting = NULL, 
                        FixReference = FALSE, FixOthers = FALSE,  
                        ScalingType = "InvSqrt", vQ_0 = NULL, 
                         fn.optimizer = fn.DEoptim, 
                        cluster = NULL, smooth = NULL, ...) {
  
  if(iTau_star < 1) {
    stop("iTau_star must be greather or equal to 1")
  }
  if(iTau_star > length(vTau)) {
    stop("iTau_star must be smaller or equal to the length of vTau")
  }
  
  if (is.null(smooth)) {
    if(identical(fn.optimizer, fn.DEoptim)) {
      smooth = FALSE 
    } else {
      smooth = TRUE
    }
  }

  if (is.null(iTau_star)) {
    iTau_star = which.min(abs(vTau - 0.5))
  }

  vVar = Variances(vTau, iTau_star)

  if (is.null(vQ_0)) vQ_0 = quantile(vY, vTau)

  if (is.null(vPn_Starting)) {
    vPn = c(beta = 0.95,
            alpha = 0.05,
            phi   = 0.94,
            gamma = 0.10)
  } else {
    vPn = vPn_Starting
  }

  if (FixReference) {
    vPn = vPn[-which(names(vPn) %in% c("beta", "alpha"))]
  }
  if (FixOthers) {
    vPn = vPn[-which(names(vPn) %in% c("gamma", "phi"))]
  }

  LB = Lower_Fun()[names(vPn)]
  UB = Upper_Fun()[names(vPn)]

  optimizer = fn.optimizer(par0 = vPn, vY = vY, FUN = DMQ_Optimizer,
                           LB = LB, UB = UB, vTau = vTau,  
                           iTau_star = iTau_star, vQ_0 = vQ_0,
                           FixReference = FixReference, ScalingType = ScalingType, 
                           vVar = vVar, smooth = smooth, FixOthers = FixOthers, 
                           cluster = cluster, ...)

  vPn = optimizer$pars

  Inference = significanceFun(optimizer$hessian, vPn, length(vY))

  if (FixReference) {
    vPn = c(vPn, alpha = 0, beta = 0)
  }
  if (FixOthers) {
    vPn = c(vPn, gamma = 0, phi = 0)
  }

  lFilter = FilterDMQ(vY, vTau, vQ_0, iTau_star - 1,
                       dBeta = vPn["beta"],
                       dAlpha = vPn["alpha"],
                       dGamma = vPn["gamma"],
                       dPhi = vPn["phi"], ScalingType = ScalingType, 
                      vVar = vVar, smooth = FALSE) # here is always smooth = FALSE

  iJ = length(vTau)

  return(list(lFilter = lFilter,
              vPn = vPn,
              optimizer = optimizer,
              vTau = vTau,
              iTau_star = iTau_star,
              vQ_0 = vQ_0,
              FixReference = FixReference,
              FixOthers = FixOthers,
              ScalingType = ScalingType,
              vVar = vVar,
              Inference = Inference,
              smooth = smooth))

}

UpdateDMQ <- function(Fit, vY) {

  vPn = Fit$vPn
  vTau = Fit$vTau
  iTau_star = which.min(abs(vTau - 0.5)) - 1
  vQ_0 = Fit$vQ_0
  ScalingType = Fit$ScalingType
  vVar = Fit$vVar

  lFilter = FilterDMQ(vY, vTau, vQ_0, iTau_star,
                       dBeta = vPn["beta"],
                       dAlpha = vPn["alpha"],
                       dGamma = vPn["gamma"],
                       dPhi = vPn["phi"],
                       ScalingType = ScalingType, 
                      vVar = vVar, smooth = FALSE)

  Fit$lFilter = lFilter

  return(Fit)

}

