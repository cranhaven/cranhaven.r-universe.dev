cisv <- 
  function(lmobj) {
    X <- model.matrix(lmobj)
    D <- 1/sqrt(diag(t(X)%*%X))
    Xs <- X%*%diag(D)
    condition.index <- svd(Xs)$d[1]/svd(Xs)$d
    singular.vectors <- t(svd(Xs)$v)
    colnames(singular.vectors) <- labels(coefficients(lmobj))
    return(cbind(condition.index, singular.vectors))
  }
