bp.test = function(mod,
                   data = list(),
                   varmod = NULL,
                   koenker = TRUE,
                   sig.level = 0.05,
                   details = FALSE,
                   hyp = TRUE){

  if (!inherits(mod, "formula")) { # Wenn Modell uebergeben ...
    X = model.matrix(terms(mod), model.frame(mod))
    y = model.response(model.frame(mod))
    Z = if (is.null(varmod)) {X} else {model.matrix(varmod, data = data)}
  }
  else { # Wenn Formel uebergeben ...
    mf = model.frame(mod, data = data)
    y = model.response(mf)
    X = model.matrix(mod, data = data)
    Z = if (is.null(varmod)) {X} else {model.matrix(varmod, data = data)}
  }

  # Wenn varmod nicht alle Exogenen enthaelt...
  if (!(all(c(row.names(X) %in% row.names(Z), row.names(Z) %in%
                row.names(X))))) {
    allnames = row.names(X)[row.names(X) %in% row.names(Z)]
    X = X[allnames, ]
    Z = Z[allnames, ]
    y = y[allnames]
  }

  n = nrow(X) # Number of observations
  k = ncol(X) # Number of coefs

  ## Original model
  resid = lm.fit(X, y)$residuals # residuals from original model
  w = resid^2/mean(resid^2) # standardized residuals

  # Introduce constant to aux model if orig. model had no intercept
  if(any(X[,1] != 1)){Z = cbind(1,Z)}

  ## Auxiliary model
  aux = lm.fit(Z, w)
  ssr = sum(aux$residuals^2)
  sig.hat = ssr/aux$df.residual
  VC = sig.hat * chol2inv(chol(t(Z) %*% Z))

  ## Test statistic
  if (koenker){
    bp = n * Sxy(aux$fitted)/Sxy(w) # n * R^2
    title = "Breusch-Pagan test (Koenker's version)"
  } else {
    # bp = bp * (n-1)/n * var(resid^2) / (2*((n-1)/n * var(resid))^2)
    title = "Breusch-Pagan test (original version)"
    bp = (Sxy(w) - Sxy(aux$resid))/2
  }

  # Generate aux. reg. table
  hreg = matrix(NA, ncol(Z), 4)
  hreg[,1] = aux$coefficients
  hreg[,2] = sqrt(diag(VC))
  hreg[,3] = aux$coefficients/sqrt(diag(VC))
  hreg[,4] = 2*pt(-abs(hreg[,3]), df = aux$df.residual)
  colnames(hreg) = c("coef", "std.err", "t.value", "p.value")
  rownames(hreg) = names(aux$coefficients)

  # Generate aux. reg. details
  stats = matrix(NA, 4L, 1L)
  stats[1] = length(y)
  stats[2] = aux$df.residual
  stats[3] = ssr
  stats[4] = sig.hat
  dimnames(stats) = list(c("Number of observations", "Degrees of freedom", "Sum of squ. resid.", "sigma2 (est.)")," ")

  if (hyp){
    H = c("sig2(i) = sig2 (Homosked.)", "sig2(i) <> sig2 (Heterosked.)")
    names(H) = c("H0:", "H1:")
    H = t(H)
  } else {
    H = NULL
  }

  df = ncol(Z) - 1
  chi.crit = qchisq(1 - sig.level, df)
  p.val = 1 - pchisq(bp, df)


  ## Generate other data
  test.result = if (p.val < sig.level) "rejected" else "not rejected"
  results = data.frame(chi.value = bp,
                       crit.value = chi.crit,
                       p.value = p.val,
                       sig.level = sig.level,
                       H0 = test.result,
                       row.names = "")

  out = list()
  attr(out, "title") = title
  out$hyp = H # Null and alternative hypothesis
  out$results = results # Basic test results
  out$hreg = hreg
  out$stats = stats
  out$nulldist = list(type = "chisq", df = df)

  attr(out, "direction") = "right"
  attr(out, "details") = if (details) {T} else {F}
  attr(out, "type") = "htest"
  attr(out, "test.type") = "bptest"
  class(out) = c("desk")

  return(out)
}
