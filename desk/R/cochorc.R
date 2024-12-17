cochorc = function(mod, data = list(), iter = 10, tol = 0.0001, pwt = TRUE, details = FALSE){

  if (!inherits(mod, "formula")) { # Wenn Modell übergeben ...
    mf = model.frame(mod)
    X = model.matrix(terms(mod), model.frame(mod))
    y = model.response(model.frame(mod))
  }
  else { # Wenn Formel übergeben ...
    mf = model.frame(mod, data = data)
    y = model.response(mf)
    X = model.matrix(mod, data = data)
    mod = lm.fit(X,y)
  }

  coefnames = names(mod$coefficients)
  y = as.matrix(y)
  colnames(y) = colnames(mf)[1]

  k = ncol(X) # Number of coefs
  n = nrow(X) # Number of observations

  # Prais-Winsten transformation of data (including first obs.)
  pw.trans = function(x, rho){
    pw = sqrt(1 - rho^2) # Prais-Winsten transformation for first observation
    x.trans = rbind(pw * x[1,], as.matrix(x[2:n,]) - as.matrix(rho * x[1:(n-1),]))
    colnames(x.trans) = colnames(x)
    rownames(x.trans) = 1:n
    return(x.trans)
  }

  # Init vector/matrix
  coor = c()

  rho = 0
  # Start iterated regressions
  for (i in 1:iter){

    # Calculate residuals
    u = y - X %*% mod$coefficients

    rho.before = rho
    rho = sum(u[1:(n-1)] * u[2:n])/sum(u[1:(n-1)]^2) # OLS estimator for rho
    niter = i

    # Transform y-vector and x-matrix
    y_trans = pw.trans(y, rho)
    X_trans = pw.trans(X, rho)

    # If no Prais-Winsten Transformation, delete first observations
    if (pwt == F){
      y_trans = y_trans[-1]
      X_trans = X_trans[-1,]
    }

    # OLS estimation
    mod = lm.fit(X_trans, y_trans)
    coor[ (i*(k + 2) - (k + 1)) : (i*(k + 2))] = c(rho, sum(mod$residuals^2), mod$coefficients)

    # Check if convergence archieved
    if (abs(rho - rho.before) < tol) {
      break
    }
  }
  # Convert vector to matrix
  coor = matrix(coor, nrow = niter, ncol = k+2, byrow = TRUE)
  dimnames(coor) = list(1:niter, c("rho.hat", "SSR", colnames(X)))
  if(niter == iter){
    warning("Rho does not converge within given number of iterations and tolerance level.
Please increase max. number of iterations (iter) or increase tolerance level (tol).", call. = F)}

  # Get optimal regression
  rho.opt = as.numeric(coor[niter, 1])
  y.trans = pw.trans(y, rho.opt) # optimal transformed y values
  X.trans = pw.trans(X, rho.opt) # optimal transformed x values (incl. z)
  mod = ols(y.trans ~ X.trans - 1)

  # Generate regression table
  regtab = matrix(NA, mod$ncoef, 4)
  regtab[,1] = mod$coef
  regtab[,2] = mod$std.err
  regtab[,3] = mod$t.value
  regtab[,4] = mod$p.value
  colnames(regtab) = c("coef", "std.err.", "t.value", "p.value")
  rownames(regtab) = coefnames

  out = list()
  attr(out, "title") = "Cochrane-Orcutt estimation given AR(1)-autocorrelated errors"

  out$results = regtab
  out$niter = niter
  out$rho.opt = rho.opt
  out$y.trans = y.trans # optimal transformed y values
  out$X.trans = X.trans # optimal transformed x values (incl. z)
  out$resid = mod$resid
  out$all.regs = coor # iteration results

  attr(out, "details") = if (details) {T} else {F}
  attr(out, "type") = "cochorc"
  class(out) = c("desk")

  return(out)
}
