procx = function(X){
  # procx pre-processes X such that 
  # - continuous variables are standardized 
  # - dichotomous variables are left as is
  # ---------------------------------------------
  X0 = X
  Tmp = apply(X, 2, unique)
  if(is.matrix(Tmp)){
    idx.d = which(apply(Tmp, 2, length) == 2)
  }
  else if(is.list(Tmp)){
    idx.d = which(lapply(Tmp, length) == 2)
  }
  if(length(idx.d) == 0){
    X = as.matrix(scale(X))
    mx = attr(X, "scaled:center")
    sdx = attr(X, "scaled:scale")
  }
  else{
    Xc = X[ , -idx.d] 
    Xc = as.matrix(scale(Xc))
    X[ , -idx.d] = Xc
    mx = rep(0, ncol(X))
    sdx = rep(1, ncol(X))
    mx[-idx.d] = attr(Xc, "scaled:center")
    sdx[-idx.d] = attr(Xc, "scaled:scale")
  }
  output = list(
    Xoriginal = X0,
    X = X,
    mx = mx,
    sdx = sdx
  )
  return(output)
}