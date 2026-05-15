# A function to compute Observed best prediction method
# variance estimation component for FH model
# The arguments of this function are
# (i)   response vector
# (ii)  design matrix of covariate
# (iii) sampling variance vector
#' @rdname varfh
#' @export
varOBP= function(formula, data, D, na_rm=FALSE, na_omit=FALSE){
  model_frame <- model.frame(formula, data)
  Y <- model.response(model_frame)
  X <- model.matrix(formula, data)
  D = as.vector(D)
  qfun = function(A){
    # Handling missing values
    if (na_rm) {
      # Remove rows with missing values from X, Y, and adjust ni accordingly
      complete_cases <- complete.cases(X, Y)
      X <- X[complete_cases, ]
      Y <- Y[complete_cases]
      D <- D[complete_cases]
    } else if (na_omit) {
      # Stop if there are any missing values
      if (anyNA(X) || anyNA(Y) || anyNA(D)) {
        stop("Input contains missing values (NA). Please handle missing data before proceeding.")
      }
    }
    # Defines object funtion for Observed best prediction method
    m = nrow(X)
    gamma2 = diag((D/(A+D))^2)
    betahat = solve(t(X) %*% gamma2 %*% X) %*% (t(X) %*% gamma2 %*% Y)
    res0 = Y - X %*% betahat
    sum0 = sum(D/(A+D))
    temp = t(res0) %*% gamma2 %*% res0
    return(temp + 2 * A *sum0)
  }
  Ahat = nlminb(sd(Y), qfun, lower = 0)$par
  return(Ahat)
}

# The arguments of this function are
# (i)   response vector
# (ii)  design matrix of covariate
# (iii) sampling variance vector
# (Vi)  estimated variance component
# The function below gives the Prasad & Rao (1990) MSPE estimator
#' @rdname mspeFHlin
#' @export
mspeFHPR = function(formula, data, D, var.method = "default", na_rm=FALSE, na_omit=FALSE){
  # default variance component estimation method is moment estimator ("MOM")
  if(var.method != "MOM" & var.method != "default" ) stop("var.method is not available")
  model_frame <- model.frame(formula, data)
  Y <- model.response(model_frame)
  X <- model.matrix(formula, data)
  D = as.vector(D)
  
  # Handling missing values
  if (na_rm) {
    # Remove rows with missing values from X, Y, and adjust ni accordingly
    complete_cases <- complete.cases(X, Y)
    X <- X[complete_cases, ]
    Y <- Y[complete_cases]
    D <- D[complete_cases]
  } else if (na_omit) {
    # Stop if there are any missing values
    if (anyNA(X) || anyNA(Y) || anyNA(D)) {
      stop("Input contains missing values (NA). Please handle missing data before proceeding.")
    }
  }
  
  A = smallarea::prasadraoest(Y, X, D)$estimate
  m = length(D)
  if(A < 0){A = 0}
  V.inv = diag(1/(A+D))
  p1 = apply(sapply(1:m, function(tt) V.inv[tt, tt] * X[tt, ] %*% t(X[tt, ]), simplify = "array"), 1:2, sum)
  p2 = apply(sapply(1:m, function(tt) V.inv[tt, tt] * X[tt, ] * Y[tt], simplify = "array"), 1, sum)
  betA = solve(p1)%*%p2
  m = length(Y); p = ncol(X)
  g1iA = g2iA = g3iA = c()
  for(i in 1:m){
    g1iA[i] = A * D[i] / ( A+D[i] )
    g2iA[i] = ( ( D[i] / ( A + D[i] ) ) ^ 2) * t( X [i,]) %*% solve(t( X ) %*% diag((A + D)^(-1)) %*%  X  ) %*%  X [i,]
    varA    = 2 * m^(-1)*(A^2 + 2 * A * sum(D) / m + sum( D^2 ) / m )
    g3iA[i] = ( D[i]^2 ) / ( ( A + D[i] )^3 ) * varA
  }
  mspe = g1iA + g2iA + g3iA
  return(list(MSPE = mspe, bhat = betA, Ahat = A))
}

# The arguments of this function are
# (i)   response vector
# (ii)  design matrix of covariate
# (iii) sampling variance vector
# (Vi)  estimated variance component
# The function below gives the Datta & Lahiri (2000) MSPE estimator
#' @rdname mspeFHlin
#' @export
mspeFHDL=function(formula, data, D, var.method = "default", na_rm = FALSE, na_omit = FALSE){
  # default variance component estimation method is restricted maximum 
  model_frame <- model.frame(formula, data)
  Y <- model.response(model_frame)
  X <- model.matrix(formula, data)
  D = as.vector(D)
  
  # Handling missing values
  if (na_rm) {
    # Remove rows with missing values from X, Y, and adjust ni accordingly
    complete_cases <- complete.cases(X, Y)
    X <- X[complete_cases, ]
    Y <- Y[complete_cases]
    D <- D[complete_cases]
  } else if (na_omit) {
    # Stop if there are any missing values
    if (anyNA(X) || anyNA(Y) || anyNA(D)) {
      stop("Input contains missing values (NA). Please handle missing data before proceeding.")
    }
  }
  
  # likelihood estimator ("REML")
  if(var.method == "REML" | var.method == "default" ){
    A = smallarea::resimaxilikelihood(Y, X, D, 1000)$estimate
  }
  else if(var.method == "ML"){
    A = smallarea::maximlikelihood(Y, X, D)$estimate
  } else stop("var.method is not available")
  if(A < 0){A = 0}
  m = length(D)
  V.inv = diag(1/(A+D))
  p1 = apply(sapply(1:m, function(tt) V.inv[tt, tt] * X[tt, ] %*% t(X[tt, ]), simplify = "array"), 1:2, sum)
  p2 = apply(sapply(1:m, function(tt) V.inv[tt, tt] * X[tt, ] * Y[tt], simplify = "array"), 1, sum)
  betA = solve(p1)%*%p2
  m = length(Y); p = ncol(X)
  g1iA = g2iA = g3iA = c()
  for(i in 1:m){
    g1iA[i] = A * D[i]/( A + D[i] )
    temp = matrix(0,p,p)
    for (u in 1:m) {
      temp = temp +  X [u,] %*% t( X [u,])/(A + D[u])
    }
    g2iA[i] = (( D[i] / (A + D[i] ))^2) * t( X [i,] ) %*% solve( temp ) %*%X [i,]
    g3iA[i] = ( D[i]^2)/((A + D[i])^3)/(sum((A + D)^-2))
  }
  mspe = g1iA + g2iA + 2 * g3iA
  return(list(MSPE = mspe, bhat = betA, Ahat = A))
}


# The arguments of this function are
# (i)   response vector
# (ii)  design matrix of covariate
# (iii) sampling variance vector
# (Vi)  estimated variance component
# The function below gives the Datta & Rao & Smith (2005) MSPE estimator
#' @rdname mspeFHlin
#' @export
mspeFHDRS = function(formula, data, D, var.method = "default",na_rm = FALSE, na_omit = FALSE){
  if(var.method != "EB" & var.method != "default" ) stop("var.method is not available")
  # default variance component estimation method is empirical bayesian estimator ("EB")
  model_frame <- model.frame(formula, data)
  Y <- model.response(model_frame)
  X <- model.matrix(formula, data)
  D = as.vector(D)
  
  # Handling missing values
  if (na_rm) {
    # Remove rows with missing values from X, Y, and adjust ni accordingly
    complete_cases <- complete.cases(X, Y)
    X <- X[complete_cases, ]
    Y <- Y[complete_cases]
    D <- D[complete_cases]
  } else if (na_omit) {
    # Stop if there are any missing values
    if (anyNA(X) || anyNA(Y) || anyNA(D)) {
      stop("Input contains missing values (NA). Please handle missing data before proceeding.")
    }
  }
  
  A = smallarea::fayherriot(Y, X, D)$estimate
  if(A < 0){A = 0}
  m = length(D)
  V.inv = diag(1/(A+D))
  p1 = apply(sapply(1:m, function(tt) V.inv[tt, tt] * X[tt, ] %*% t(X[tt, ]), simplify = "array"), 1:2, sum)
  p2 = apply(sapply(1:m, function(tt) V.inv[tt, tt] * X[tt, ] * Y[tt], simplify = "array"), 1, sum)
  betA = solve(p1)%*%p2
  m = length(Y); p = ncol(X)
  g1iA = g2iA = g3iA = c()
  trsum1 = sum((A + D )^(-1))
  trsum2 = sum((A + D )^(-2))
  B =  D /( D + A)
  b = 2*(m * trsum2 - trsum1^2)/(trsum1^3)
  B2b = B^2*b
  for(i in 1:m){
    g1iA[i] = A * D [i]/(A + D [i])
    temp = matrix(0, p, p)
    for (u in 1:m) {
      temp = temp +  X [u,] %*% t( X [u,]) / (A +  D [u])
    }
    g2iA[i] = (( D [i]/(A +  D [i]))^2) * t( X [i,]) %*% solve(temp) %*%  X [i,]
    g3iA[i] = 2 * m * ( D [i]^2) * (A +  D [i])^(-3) / (trsum1^2)
  }
  mspe = g1iA + g2iA + 2 * g3iA - B2b
  return(list(MSPE = mspe, bhat = betA, Ahat = A))
}

# The arguments of this function are
# (i)   response vector
# (ii)  design matrix of covariate
# (iii) sampling variance vector
# (Vi)  estimated variance component
# The function below gives the Modified PR (Liu,2005) MSPE estimator
#' @rdname mspeFHlin
#' @export
mspeFHMPR = function(formula, data, D, var.method = "default", na_rm = FALSE, na_omit = FALSE){
  if(var.method != "OBP" & var.method != "default" ) stop("var.method is not available")
  # default variance component estimation method is observed best prediction 
  model_frame <- model.frame(formula, data)
  Y <- model.response(model_frame)
  X <- model.matrix(formula, data)
  D = as.vector(D)
  
  # Handling missing values
  if (na_rm) {
    # Remove rows with missing values from X, Y, and adjust ni accordingly
    complete_cases <- complete.cases(X, Y)
    X <- X[complete_cases, ]
    Y <- Y[complete_cases]
    D <- D[complete_cases]
  } else if (na_omit) {
    # Stop if there are any missing values
    if (anyNA(X) || anyNA(Y) || anyNA(D)) {
      stop("Input contains missing values (NA). Please handle missing data before proceeding.")
    }
  }
  
  # estimator ("OBP")
  A = varOBP(formula, data, D)
  if(A < 0){A = 0}
  m = length(Y); p = ncol(X)
  V.inv = diag(1/(A+D))
  p1 = apply(sapply(1:m, function(tt) V.inv[tt, tt] * X[tt, ] %*% t(X[tt, ]), simplify = "array"), 1:2, sum)
  p2 = apply(sapply(1:m, function(tt) V.inv[tt, tt] * X[tt, ] * Y[tt], simplify = "array"), 1, sum)
  betA = solve(p1)%*%p2
  AD = A + D
  AD2 = AD^2
  hatr = D / AD; hatr2 = hatr^2
  S0m = sum(hatr2); S1m = sum(hatr2 / AD); S2m = sum(hatr2 / AD2)
  res   = numeric(m)
  for(jj in 1:m){res[jj] = Y[jj] - sum(X[jj, ] * betA)}
  temp = hatr^3 * D
  U0m = p * sum(temp); U1m = p * sum(temp / AD)
  hatr4 = hatr^4
  temp = hatr4 * (res^4 / AD2 - 1)
  V0m = sum(temp)
  V1m = sum(temp / AD)
  tm = sum(1 / AD2)
  Tm = sum(res^4 / AD2) - 3 * m
  hata = Tm / (tm * S1m) * hatr4 / AD2
  hatc = A * hatr
  hatbd = hatr2 * (2 * U1m / (S0m * S1m) + 3 * V1m / S1m^2 -
                     3 * S2m * V0m / S1m^3 +
                     2 * V0m / (AD * S1m^2))
  mspe = -2 * hata + hatbd + hatc
  return(list(MSPE = mspe, bhat = betA, Ahat = A))
}


# The arguments of this function are
# (i)   response vector
# (ii)  design matrix of covariate
# (iii) sampling variance vector
# (Vi)  method used to calculate MSPE
# (V)   method used to calculate variance component
# The function below gives the assembled MSPE estimator, different 
# MSPE estimation methods has different available variance estimation
# methods. Default MSPE estimation methods is Prasad & Rao.
# This function returns a list with:
# (i)  MSPE estimator
# (ii) variance component estimator

#' @export
mspeFHlin = function(formula, data, D, method = "PR", var.method = "default", na_rm = FALSE, na_omit = FALSE){
  # calculate MSPE through several linearization approximation method
  model_frame <- model.frame(formula, data)
  Y <- model.response(model_frame)
  X <- model.matrix(formula, data)
  D = as.vector(D)

  # Handling missing values
  if (na_rm) {
    # Remove rows with missing values from X, Y, and adjust ni accordingly
    complete_cases <- complete.cases(X, Y)
    X <- X[complete_cases, ]
    Y <- Y[complete_cases]
    D <- D[complete_cases]
  } else if (na_omit) {
    # Stop if there are any missing values
    if (anyNA(X) || anyNA(Y) || anyNA(D)) {
      stop("Input contains missing values (NA). Please handle missing data before proceeding.")
    }
  }
  
  m = length(Y); p = ncol(X)
  if(m != nrow(X)){stop( "length of response doesnot match rowlength of designmatrix" )}
  else{if(m != length(D)){stop( "length of response does not match with the number of variances of the random effects" )}
    else{
      if(method == "PR"){
        # PR mspe approximation method (Prasad and Rao 1990)
        result = mspeFHPR(formula, data, D, var.method, na_rm, na_omit)
        return(result)
      }
      if(method == "DL"){
        # DL mspe approximation method (Datta and Lahiri 2000)
        result = mspeFHDL(formula, data, D, var.method, na_rm, na_omit)
        return(result)
      }
      if(method == "DRS"){
        # DRS mspe approximation method (Datta ,Rao ,Smith 2005)
        result = mspeFHDRS(formula, data, D, var.method, na_rm, na_omit)
        return(result)
      }
      if(method == "MPR"){
        # MPR mspe approximation method (Liu 2020)
        result = mspeFHMPR(formula, data, D, var.method, na_rm, na_omit)
        return(result)
      }
      # Available Linearization method includes "PR","DL","DRS","MPR".
      else stop( "method is not available" )
    }
  }
}


