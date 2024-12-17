par.f.test = function(mod,
                       data = list(),
                       nh,
                       q = rep(0,dim(nh)[1]),
                       sig.level = 0.05,
                       details = FALSE,
                       hyp = TRUE){

  if (inherits(mod, "formula")) { # Wenn Formel übergeben ...
    mod = ols(mod, data = data)
  }

  nh = as.matrix(nh)
  if (dim(nh)[2] == 1) nh = t(nh) # If a t-test
  L = dim(nh)[1]
  SSR = sum(mod$residuals^2)
  df = mod$df.residual # T-K-1
  X = model.matrix(mod) # Extrahiere Praediktoren (inkl alpha)
  XXi = chol2inv(chol(t(X)%*%X))
  bhat = coef(mod)
  coefnames = names(bhat)

  # Calculate F-Value
  f.val = t(nh %*% bhat-q) %*% chol2inv(chol(nh %*% XXi %*% t(nh))) %*% (nh %*% bhat-q)/(L*SSR/df)

  ## Generate Hypotheses
  if (hyp) {
  H = matrix(NA, dim(nh)[1], 2L)
  for (j in 1:L){ # for all hypotheses
    h = ""
    R = nh[j,] # get row j of nh-matrix
    for (i in 1:length(coefnames)){
      tmp = coefnames[i]
      if (R[i] != 0){ # If coef not zero...
        tmp = paste(as.character(abs(R[i])),"*",tmp, sep = "")
        tmp = paste(if ((R[i]>0) & (h != "")) " + " else if (R[i]<0) " - ", tmp, sep = "")
      } else {tmp = ""}# If coef zero, then no name
      h = paste(h, tmp, sep = "")
    } # end of inner for
    h = c(paste(h, " = ", q[j], sep = ""),paste(h, " <> ", q[j], sep = ""))
    H[j,] = h
  } # end of outer for
  dimnames(H) = list(1:L, c("H0:", "H1:"))
  } else {
    H = NULL
  }

  ## Generate other data
  f.crit = qf(1 - sig.level, L, df)
  p.val = 1 - pf(f.val, L, df)

  test.result = if (p.val < sig.level) "rejected" else "not rejected"
  results = data.frame(f.value = f.val,
                       crit.value = f.crit,
                       p.value = p.val,
                       sig.level = sig.level,
                       H0 = test.result,
                       row.names = "")

  out = list()
  attr(out, "title") = "F-Test on multiple linear combinations of parameters"
  out$hyp = H # Null and alternative hypothesis
  out$nh = nh # Null and alternative hypothesis (Matrix form)
  out$q = q # q-value (default: 0)
  out$results = results # Basic test results
  out$mod = mod # Model behind the test
  out$SSR.H0 = SSR*(f.val*L/df + 1)
  out$SSR.H1 = SSR
  out$nulldist = list(type = "f", df = c(L,df))

  attr(out, "direction") = "right"
  attr(out, "details") = if (details) {T} else {F}
  attr(out, "type") = "htest"
  attr(out, "test.type") = "ftest"
  class(out) = c("desk")

  return(out)
}
