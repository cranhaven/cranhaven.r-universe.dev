ivr = function(formula,
               data = list(),
               endog,
               iv,
               contrasts = NULL,
               details = FALSE,
               ...){

  dname = paste(deparse(substitute(data)))

  # Model setup
  mf = model.frame(formula, data = data, drop.unused.levels = TRUE) # Data matrix
  mt = attr(mf, "terms") # Model terms
  y = model.response(mf, "numeric") # Response data
  X = model.matrix(mt, mf, contrasts) # Regressor data
  n = nrow(X) # Number of observations
  k = ncol(X) # Number of coefs in the model
  df = n - k # Number of coefs in the model

  getEnv = function(x) {
    #xobj = deparse(substitute(x))
    gobjects = ls(envir = .GlobalEnv)
    envirs = gobjects[sapply(gobjects, function(x) is.environment(get(x)))]
    envirs = c('.GlobalEnv', envirs)
    xin = sapply(envirs, function(e) x %in% ls(envir = get(e)))
    envirs[xin]
  }

  # Check whether exogenous regressors are a subset of "iv"
   if ( length(colnames(X)[- which(colnames(X) %in% c("(Intercept)", endog))]) != 0 ) {
   if (any(colnames(X)[- which(colnames(X) %in% c("(Intercept)", endog))] %in% iv)) {
     stop("For the option iv, exogenous regressors should not be listed by the user.
             They are included automatically in the first stage regression.", call. = F)
   }
  }

  # Check whether "endog" is a subset of "X", i.e. only regressors are used
  if (!all(endog %in% colnames(X))) {
    stop("For the option endog, only regressors are allowed.", call. = F)
  }

  # Get model-matrix of instruments Z (and exogenous regressors)
  Xnames = colnames(X)
  iv.data = if (missing(data)) do.call(cbind, mget(iv, envir = as.environment(getEnv(iv)))) else data[, iv]
  iv.data = as.matrix(na.omit(iv.data))
  Z = as.matrix(cbind(X[,- which(colnames(X) %in% endog)], iv.data))
  colnames(Z) = c(Xnames[- which(colnames(X) %in% endog)], iv)

  # Fist stage regression: exog. and endog. regressors X on exog. regressors and instruments Z
  if(!is.null(Z)) { # If instruments defined ...
    if(ncol(Z) < ncol(X)) warning("Underidentified: More regressors than instruments")
    aux.reg = lm.fit(Z, X, ...)
    XZ = as.matrix(aux.reg$fitted.values)
    colnames(XZ) = colnames(X)
  } else { # If no instruments defined ...
    XZ = X
  }

  # Second stage regression
  main.reg = lm.fit(XZ, y, ...)

  ## Initialize list
  out = list()

  # Fitted values
  yhat = drop(X %*% main.reg$coefficients)
  names(yhat) = names(y)
  out$fitted.values = yhat

  # Residuals and SSR
  out$residuals = y - yhat
  out$ssr = sum(out$residuals^2)

  # Est. error variance
  out$sig.squ = out$ssr/main.reg$df.residual

  # Coefficients
  out$coefficients = main.reg$coefficients

  # Variance-covariance matrices
  # Unscaled:
  out$ucov = chol2inv(main.reg$qr$qr[1:k, 1:k, drop = FALSE])
  dimnames(out$ucov) = list(names(main.reg$coefficients),names(main.reg$coefficients))
  # Scaled:
  out$vcov = out$sig.squ * out$ucov

  # Regression table splitted
  out$std.err = sqrt(diag(out$vcov))
  out$t.values = out$coef/out$std.err
  out$p.values = 2*pt(-abs(out$t.value), df = df)

  # Data
  out$data = mf # Used dataframe
  out$data.name = dname # Name of used dataframe
  out$response = y
  out$model.matrix = XZ

  # Some statistics
  ## R-squared
  if (attr(mt,"intercept") == 1) {
    Syy = sum((y - mean(y))^2)
    dfi = 1
  } else if (attr(mt,"intercept") == 0) {
    Syy = sum(y^2)
    dfi = 0
  }
  out$r.squ = 1 - out$ssr/Syy
  out$adj.r.squ = 1 - (1 - out$r.squ) * ((n - dfi)/main.reg$df.residual)

  # Misc
  out$nobs = n
  out$ncoef = k
  out$df = df
  out$has.const = if (attr(mt,"intercept") == 0){F} else if (attr(mt,"intercept") == 1){T}
  out$instrumented = endog
  out$exogenous = colnames(X)[- which(colnames(X) %in% c("(Intercept)", endog))]
  out$instruments = iv

  # First stage diagnostics (Weakness of instruments)
  fsd = matrix(NA, length(endog), 3)
  for(i in 1:length(endog)) {
    bhat = aux.reg$coef[,endog[i]]
    ZZi = sum(aux.reg$resid[,endog[i]]^2) / aux.reg$df.resid * chol2inv(chol(t(Z) %*% Z))
    if(any(is.nan(ZZi))) {out$f.instr <- out$p.instr <- NA} else {
      nh = as.matrix(diag(ncol(Z))[- which(!(colnames(Z) %in% iv)),], ncol = ncol(Z))
      if (dim(nh)[2] == 1) nh = t(nh)
      Rb = nh %*% bhat
      fsd[i, 1] = as.numeric(t(Rb) %*% chol2inv(chol(nh %*% ZZi %*% t(nh))) %*% (Rb)/nrow(nh))
      fsd[i, 2] = 1 - pf(fsd[i, 1], nrow(nh), aux.reg$df.res)}
    # Shea's partial R^2
      ols.reg = lm.fit(X, y, ...)
      sig2.ols = sum(ols.reg$residuals^2)/ols.reg$df.residual
      V.ols = diag(sig2.ols * chol2inv(chol(t(X) %*% X)))
      names(V.ols) = names(ols.reg$coef)
      V.2sls = diag(out$vcov)
      shea = (V.ols/V.2sls * out$sig.squ/sig2.ols)[endog]
      fsd[, 3] = shea
  }
  dimnames(fsd) = list(endog, c("F-value", "p-value", "Shea's R2"))
  out$fsd = fsd

  out$f.instr = out$fsd[,1]
  out$p.instr = out$fsd[,2]
  out$shea = shea

  # Wu-Hausman test (omitted variable version of Hausman)
  if(length(endog) > 1L){
    aux.reg = lm.fit(Z, X[, which(colnames(X) %in% endog)])
  }
  xfit = as.matrix(aux.reg$fitted[, which((colnames(aux.reg$fitted) %in% endog))])
  colnames(xfit) = paste("fit", colnames(xfit), sep = "_")
  Z = cbind(X, xfit)
  aux.reg = lm.fit(Z, y)
  bhat = aux.reg$coef
  ZZi = sum(aux.reg$resid^2)/aux.reg$df.resid * chol2inv(chol(t(Z) %*% Z))
  if(any(is.nan(ZZi))) {out$f.hausman <- out$p.hausman <- NA} else {
    nh = diag(ncol(Z))
    nh = as.matrix(nh[- (1:(dim(X)[2])),])
    if (dim(nh)[2] == 1) nh = t(nh)
    Rb = nh %*% bhat
    out$f.hausman = as.numeric(t(Rb) %*% chol2inv(chol(nh %*% ZZi %*% t(nh))) %*% (Rb)/nrow(nh))
    out$p.hausman = 1 - pf(out$f.hausman, nrow(nh), aux.reg$df.res)
  }

  # order output alphabetically
  out = out[order(names(out))]

  # Model formula
  out$modform = paste(deparse(mt), collapse = "")
  attr(out, "title") = if(nchar(out$modform) < 50){
    paste("2SLS-Regression of model", paste("", out$modform, "", sep = ""))
  } else {
    "2SLS Instrument Variable Regression"}
  out$model.formula = as.formula(out$modform)
  out$terms = mt

  # Further attributes
  attr(out, "details") = if (details) {T} else {F}
  attr(out, "type") = "ivr"
  class(out) = c("desk")

  return(out)
}
