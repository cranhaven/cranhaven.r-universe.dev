bc.model = function(mod, data = list(), range = seq(-2,2,0.1), details = FALSE){

  if (!inherits(mod, "formula")) { # Wenn Modell übergeben ...
    mf = model.frame(mod)
    X = model.matrix(terms(mod), mf)
    y = model.response(model.frame(mod))
  }
  else { # Wenn Formel übergeben ...
    mf = model.frame(mod, data = data)
    y = model.response(mf)
    X = model.matrix(mod, data = data)
  }
  xname = colnames(mf)[2]
  n = nrow(X) # Number of observations
  k = ncol(X) # Number of coefs

  if (k > 2) stop("Only one predictor allowed.")

  # Generate grid
  PARA = expand.grid(lambda.y = range, lambda.x = range)
  y = y/exp(mean(log(y))) # standardize y values

  A = matrix(NA, nrow(PARA),1)
  for (i in 1:dim(PARA)[1]){
    Z = X
    Z[,xname] = def.log(Z[,xname], PARA[i,2])
    tmp.mod = lm.fit(Z, def.log(y,PARA[i,1]))
    Srr = sum((tmp.mod$resid - mean(tmp.mod$resid))^2)
    A[i] = Srr
  }

  idxmin = which.min(A) # finde Minimum

  min.mod = ols(def.log(y, PARA[idxmin,1]) ~ def.log(X[,2], PARA[idxmin,2]))
  names(min.mod$coefficients) = colnames(X)
  attr(min.mod, "title") = "Model exhibiting minimal SSR:"

  out = list()
  out$results = min.mod
  out$lambda = PARA[idxmin,]
  out$nregs = dim(PARA)[1]
  out$idx.opt = idxmin
  out$val.opt = A[idxmin,1]

  attr(out, "details") = if (details) {T} else {F}
  attr(out, "type") = "bcmodel"
  class(out) = c("desk")

return(out)
}
