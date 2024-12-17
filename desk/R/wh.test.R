wh.test = function(mod,
                   data = list(),
                   sig.level = 0.05,
                   details = FALSE,
                   hyp = TRUE){

  if (!inherits(mod, "formula")) { # Wenn Modell uebergeben ...
    mf = model.frame(mod)
    mf[,1] = mod$residuals^2
  }
  else { # Wenn Formel uebergeben ...
    mf = model.frame(mod, data = data)
    mf[,1] = lm.fit(model.matrix(mod, data = data), model.response(mf))$residuals^2
  }

  y.name = colnames(mf)[1]
  X.term = paste(colnames(mf)[2:ncol(mf)], collapse = " + ")
  X2.term = colnames(mf)[2:ncol(mf)] # Names of exogenous vars.
  k = length(X2.term) # Number of exogenous vars.
  if (k >= 2) {
    for(i in 1:k){# make vars squared
      X2.term[i] = paste("I(", X2.term[i], "^2)", sep = "")
    }
    X2.term = paste(X2.term, collapse = " + ")
    form = as.formula(paste(y.name," ~ ", X2.term, "+ (", X.term,")^2 ", sep = ""))
  } else { # if only one variable, no interaction
    form = as.formula(paste(y.name," ~ ", X2.term, "+ (", X2.term,")^2 ", sep = ""))
  }

  mod = ols(form, data = mf)

  # Generate aux. reg. table
  hreg = matrix(NA, mod$ncoef, 4)
  hreg[,1] = mod$coefficients
  hreg[,2] = sqrt(diag(mod$vcov))
  hreg[,3] = mod$coefficients/sqrt(diag(mod$vcov))
  hreg[,4] = 2*pt(-abs(hreg[,3]), df = mod$df.residual)
  colnames(hreg) = c("Coeff.", "Std. Err.", "t-value", "p-value")
  rownames(hreg) = names(mod$coefficients)

  # Generate aux. reg. details
  stats = matrix(NA, 4L, 1L)
  stats[1] = mod$nobs
  stats[2] = mod$df.residual
  stats[3] = mod$ssr
  stats[4] = mod$sig.squ
  dimnames(stats) = list(c("Number of observations", "Degrees of freedom", "Sum of squ. resid.", "sigma2 (est.)")," ")

  # Calculate White test statistic (identical to Koenker)
  wh = mod$r.squ * mod$nobs

  if (hyp){
    H = c("sig2(i) = sig2 (Homosked.)", "sig2(i) <> sig2 (Heterosked.)")
    names(H) = c("H0:", "H1:")
    H = t(H)
  } else {
    H = NULL
  }

  df = mod$ncoef - sum(mod$has.const)
  p.value = 1 - pchisq(wh, df)

  ## Generate other data
  test.result = if (p.value < sig.level) "rejected" else "not rejected"
  results = data.frame(chi.value = wh,
                       crit.value = qchisq(1 - sig.level, df),
                       p.value =  p.value,
                       sig.level = sig.level,
                       H0 = test.result,
                       row.names = "")

  out = list()
  attr(out, "title") = "White test for heteroskedastic errors"
  out$hyp = H # Null and alternative hypothesis
  out$results = results # Basic test results
  out$hreg = hreg
  out$stats = stats
  out$nulldist = list(type = "chisq", df = df)

  attr(out, "direction") = "right"
  attr(out, "details") = if (details) {T} else {F}
  attr(out, "type") = "htest"
  attr(out, "test.type") = "whtest"
  class(out) = c("desk")

  return(out)
}
