ols = function(formula, data = list(), na.action = NULL, contrasts = NULL, details = FALSE, ...){

  # simulate "lm"-call for stargazer compatibility
  cl = match.call()
  cl[1] = as.call(list(quote(lm)))

  dname = paste(deparse(substitute(data)))

  mf = model.frame(formula, data = data, drop.unused.levels = TRUE) # Data matrix
  mt = attr(mf, "terms") # Model terms
  y = model.response(mf, "numeric") # Response data
  #y = as.matrix(y)
  #colnames(y) = colnames(mf)[1]
  X = model.matrix(mt, mf, contrasts) # Regressor data
  n = nrow(X) # Number of observations
  k = ncol(X) # Number of coefs in the model
  df = n - k # Number of coefs in the model

  # Perfect Collinearity
  qr.decomp <- qr(X)
  diag.upper <- diag(qr.decomp$qr)[1:min(dim(X))]
  if (any(abs(diag.upper) < 1e-10)) {
    stop("There is the problem of perfect collinearity.", call. = F)
  }

  # Regression
  if (!is.empty.model(mt)) {
    out = as.list(lm.fit(X, y, singular.ok = TRUE, ...))
    ssr = sum(out$residuals^2)
    sig.squ = ssr/out$df.residual
    bhat = out$coefficients
    coefnames = names(bhat)
    XXi = chol2inv(chol(t(X) %*% X))
    VC = sig.squ * XXi
    dimnames(VC) = list(coefnames,coefnames)

    out$modform = paste(deparse(mt), collapse = "")
    attr(out, "title") = if(nchar(out$modform) < 50){
      paste("OLS-Regression of model", paste("", out$modform, "", sep = ""))} else {
      "OLS-Regression"}
    out$model.formula = as.formula(out$modform)

    # Regression table splitted
    #out$coef = out$coefficients
    out$std.err = sqrt(diag(VC))
    out$t.value = out$coef/out$std.err
    out$p.value = 2*pt(-abs(out$t.value), df = df)

    # Data
    out$data = mf # Used dataframe
    out$data.name = dname # Name of used dataframe
    out$response = y
    out$model.matrix = X

    # Some statistics
    out$ssr = ssr
    out$sig.squ = sig.squ
    out$vcov = VC
    out$r.squ = Sxy(out$fitted.values)/Sxy(y)
    out$adj.r.squ = 1-(1-out$r.squ)*(n-1)/df

    # Misc
    out$nobs = n
    out$ncoef = k
    out$has.const = if (attr(mt,"intercept") == 0){F} else if (attr(mt,"intercept") == 1){T}

    # F Test
    if(out$has.const){nh = diag(k)[-1,]; k0 = 1} else {nh = diag(k); k0 = 0}
    if (k == 2) {nh = t(as.matrix(nh))}
    if((k-k0) > 0){
    out$f.value = t(nh %*% bhat) %*% chol2inv(chol(nh %*% XXi %*% t(nh))) %*% (nh %*% bhat)/((k-k0)*ssr/df)
    out$f.pvalue = 1-pf(out$f.val, k-k0, df)
    }

    # for lm compatibility
    out$na.action = attr(mf, "na.action")
    out$contrasts = attr(X, "contrasts")
    out$xlevels = .getXlevels(mt, mf)
    out$call = cl
    out$terms = mt
    out$model = mf
    #out$offset = offset
  }
  else {
    stop("Empty models not supported.", call. = F)
  }

  out = out[order(names(out))]

  attr(out, "details") = if (details) {T} else {F}
  attr(out, "type") = "ols"
  class(out) = c("desk", "lm")

  return(out)
}
