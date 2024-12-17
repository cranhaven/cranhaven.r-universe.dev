hcc = function(mod, data = list(), digits = 4) {

  if (!inherits(mod, "formula")) { # Wenn Modell übergeben ...
    X = model.matrix(terms(mod), model.frame(mod))
    y = model.response(model.frame(mod))
    resid = mod$resid
  }
  else { # Wenn Formel übergeben ...
    mf = model.frame(mod, data = data)
    y = model.response(mf)
    X = model.matrix(mod, data = data)
    resid = lm.fit(X,y)$resid
  }

  XXi = chol2inv(chol(t(X) %*% X))
  out = XXi %*% t(X) %*% diag(resid^2) %*%  X %*% XXi
  return(round(out, digits))
}
