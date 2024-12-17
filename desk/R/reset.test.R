reset.test = function (mod,
                       data = list(),
                       m = 2,
                       sig.level = 0.05,
                       details = FALSE,
                       hyp = TRUE){

  power = 2:(m+1)
  if (!inherits(mod, "formula")) { # Wenn Modell übergeben ...
    X = model.matrix(terms(mod), model.frame(mod))
    y = model.response(model.frame(mod))
  }
  else { # Wenn Formel übergeben ...
    mf = model.frame(mod, data = data)
    y = model.response(mf)
    X = model.matrix(mod, data = data)
  }

  n = nrow(X) # Number of observations
  k = ncol(X) # Number of coefs

  y.hat = lm.fit(X, y)$fitted
  Z = matrix(t(sapply(y.hat, "^", power)), nrow = n)

  XZ = cbind(X, Z)
  L = ncol(Z) # Number of extended coefficients (gamma)
  SSR0 = sum(lm.fit(X, y)$residuals^2)
  SSR1 = sum(lm.fit(XZ, y)$residuals^2)
  df1 = L
  df2 = n - (k + L)
  f.val = (df2/df1) * ((SSR0 - SSR1)/SSR1)

  if (hyp){
    H = c("gammas = 0 (linear)", "gammas <> 0 (non-linear)")
    names(H) = c("H0:", "H1:")
    H = t(H)
  } else {
    H = NULL
  }

  f.crit = qf(1 - sig.level, df1, df2)
  p.val = 1 - pf(f.val, df1, df2)

  test.result = if (p.val < sig.level) "rejected" else "not rejected"
  results = data.frame(f.value = f.val,
                       crit.value = f.crit,
                       p.value = p.val,
                       sig.level = sig.level,
                       H0 = test.result,
                       row.names = "")

  out = list()
  attr(out, "title") = "RESET Method for nonlinear functional form"
  out$hyp = H # Null and alternative hypothesis
  out$results = results # Basic test results
  out$SSR0 = SSR0 # SSR NH-model
  out$SSR1 = SSR1 # SSR extended model
  out$L = L # Number of lin. comb. tested
  out$nulldist = list(type = "f", df = c(df1,df2))

  attr(out, "direction") = "right"
  attr(out, "details") = if (details) {T} else {F}
  attr(out, "type") = "htest"
  attr(out, "test.type") = "resettest"
  class(out) = c("desk")

  return(out)
}
