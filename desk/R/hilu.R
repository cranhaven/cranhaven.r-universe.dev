hilu = function(mod, data = list(), range = seq(-1, 1, 0.01), details = FALSE){

  if (!inherits(mod, "formula")) { # Wenn Modell übergeben ...
    mf = model.frame(mod)
    X = model.matrix(terms(mod), model.frame(mod))
    y = model.response(model.frame(mod))
  }
  else { # Wenn Formel übergeben ...
    mf = model.frame(mod, data = data)
    y = model.response(mf)
    X = model.matrix(mod, data = data)
  }

  if (range(range)[1] < -1 | range(range)[2] > 1) stop("Valid rho values are between -1 and 1. Please select a different range.", call. = F)

  y = as.matrix(y)
  colnames(y) = colnames(mf)[1]

  k = ncol(X) # Number of coefs
  n = nrow(X) # Number of observations
  r = length(range) # Number of regression to be performed

  # Prais-Winsten transformation of data (including first obs.)
  pw.trans = function(x, rho){
    pw = sqrt(1 - rho^2) # Prais-Winsten transformation for first observation
    x.trans = rbind(pw * x[1,], as.matrix(x[2:n,]) - as.matrix(rho * x[1:(n-1),]))
    colnames(x.trans) = colnames(x)
    rownames(x.trans) = 1:n
    return(x.trans)
  }

  # Init matrix
  hilu = matrix(NA, r, k+2, dimnames = list(NULL, c("rho", "SSR", colnames(X))))
  for (i in 1:r){
    rho = range[i]
    y_trans = pw.trans(y, rho) # Transform y-vector
    X_trans = pw.trans(X, rho) # Transform x-matrix (incl. z)
    hilu.mod = lm.fit(X_trans, y_trans) # Estimation of transformed model
    hilu[i,] = c(rho, sum(hilu.mod$residuals^2), hilu.mod$coefficients)
  }

  #results = t(hilu[which(hilu[,"SSR"] == min(hilu[,"SSR"])),])
  #rownames(results) = ""

  # Get optimal regression
  idx.opt = which(hilu[,"SSR"] == min(hilu[,"SSR"]))
  rho.opt = hilu[which(hilu[,"SSR"] == min(hilu[,"SSR"])),][1]
  if (rho.opt == -1 | rho.opt == 1) stop("No inner SSR minimum found! Check if your data is AR(1) autocorrelated.", call. = F)
  y.trans = pw.trans(y, rho.opt) # optimal transformed y values
  X.trans = pw.trans(X, rho.opt) # optimal transformed x values (incl. z)
  mod = lm.fit(X.trans, y.trans)

  ssr = sum(mod$residuals^2)
  sig.hat = ssr/mod$df.residual
  VC = sig.hat * chol2inv(chol(t(X) %*% X))

  # Generate regression table
  regtab = matrix(NA, k, 4)
  regtab[,1] = mod$coef
  regtab[,2] = sqrt(diag(VC))
  regtab[,3] = mod$coef/sqrt(diag(VC))
  regtab[,4] = 2*pt(-abs(regtab[,3]), df = mod$df.residual)
  colnames(regtab) = c("coeff.", "std. err.", "t-value", "p-value")
  rownames(regtab) = names(mod$coef)

  out = list()
  attr(out, "title") = "Hildreth and Lu estimation given AR(1)-autocorrelated errors"

  # select estimation which has minimal SSR
  out$results = regtab
  out$idx.opt = idx.opt
  out$nregs = length(range)
  out$rho.opt = rho.opt
  out$y.trans = pw.trans(y, out$rho.opt) # optimal transformed y values
  out$X.trans = pw.trans(X, out$rho.opt) # optimal transformed x values (incl. z)
  out$all.regs = hilu
  out$rho.vals = range

  attr(out, "details") = if (details) {T} else {F}
  attr(out, "type") = "hilu"
  class(out) = c("desk")

  return(out)
}
