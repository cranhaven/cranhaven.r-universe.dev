


reg.diff = function(X,
                    y,
                    strata,
                    fraction = NULL,
                    nbfraction = 100,
                    nopenalize = NULL,
                    BACK = TRUE,
                    standardize = FALSE,
                    maxit = 100,
                    maxitB = 500,
                    thr = 1e-10,
                    tol = 1e-10,
                    epsilon = 0.0001,
                    trace = TRUE,
                    log = TRUE,
                    adaptive = FALSE,
                    separate = FALSE,
                    ols = FALSE,
                    p.fact = NULL,
                    remove = FALSE) {
  if (length(y) != nrow(X))
    stop("Please ensure that each observation has predictors and response")
  
  # Matrix of differences
  x_rec <- X
  y_rec <- y
  x <- X[y == 1,] - X[y == 0,]
  
  
  if (missing(strata)) {
    stop("'strata' is missing")
  } else{
    y1 = aggregate(as.data.frame(y), by = list(strata), function(u)
      u[1] - u[2])$y
    
    if (any(y1 != 1))
      stop("Response vector should be 1 case and 1 control in each strata, starting by the case")
  }
  
  
  #Algorithme IRLS-LASSOSHOOTING
  #require(lassoshooting)
  x <- as.matrix(x)
  n <- dim(x)[1]
  
  #invariable covariates are removed
  sds <- stand(x, n, unit = F)
  if (remove) {
    remove <- (sds == 0)
  }
  else{
    remove <- (sds <= -1000)
  }    #don't remove invariable covariates
  x <- x[,!remove]
  m <- dim(x)[2]
  
  # stabilize/standardize.
  if (standardize) {
    x <- x / matrix(sds[!remove], n, m, byrow = T)
  }
  
  #Calculate regularization parameter for the 'lassoshooting'
  if (is.null(fraction)) {
    if (missing(nbfraction)) {
      nbfraction = 100
    }
    
    
    fraction = frac(
      epsilon = epsilon,
      log = log,
      x = x,
      n = n,
      m = m,
      nbfraction = nbfraction
    )
  } else{
    nbfraction = length(fraction)
  }
  
  
  if (adaptive & is.null(p.fact)) {
    warning("p.fact required")
  }
  
  #Estimation of coefficient for each value of fraction
  nb_coef_non_nuls <- c()
  beta <- matrix(0, nbfraction, length(remove))
  nz   <- 0
  W    <- rep(0, n)
  
  if (!all(remove)) {
    betanew <- matrix(0, nbfraction, m)
    nbf     <- nbfraction
    for (i in (1:nbf)) {
      if (trace) {
        if (mod(i, 40) == 0) {
          cat("fraction ", i, "\n")
        }
      }
      if (i == 1) {
        betaold <- rep(0, m)
      } else {
        betaold <- betanew[(i - 1),]
      }
      fold <- likelihood.diff(x, betaold)
      a    <- 0
      
      while (a < maxit) {
        a <- a + 1
        
        z <- rep(0, n)
        
        z <- x %*% betaold + 1 / sigmoid(x, betaold)
        lambda <- sigmoid(x, betaold) * (1 - sigmoid(x, betaold))
        X <- sqrt(as.vector(lambda)) * x
        Y <- sqrt(as.vector(lambda)) * z
        rm(z)
        
        if (adaptive == TRUE) {
          gamma = (
            lassoshooting(
              X = X,
              y = Y,
              lambda = fraction[i],
              penaltyweight = p.fact,
              thr = thr,
              nopenalize = nopenalize
            )
          )$coefficient
          rm(X)
          rm(Y)
        } else {
          XtX = t(X) %*% X
          Xty = t(X) %*% Y
          rm(X)
          rm(Y)
          gamma = (
            lassoshooting(
              XtX = XtX,
              Xty = Xty,
              lambda = fraction[i],
              thr = thr,
              nopenalize = nopenalize
            )
          )$coefficient
          rm(XtX)
          rm(Xty)
        }
        
        #Backtracking-line search
        if (BACK) {
          step  <- gamma - betaold
          t     <- 1
          delta <- grad.diff(x, betaold) %*% step
          likelihood_diff_old <- likelihood.diff(x, betaold)
          for (l in (1:maxitB)) {
            gamma <- betaold + t * step
            if (likelihood.diff(x, gamma) <= ( likelihood_diff_old + 0.3 * t * delta))
              break
            t <- 0.9 * t
          }
          betanew[i,] = (1 - t) * betaold + t * gamma
        } else{
          betanew[i,] = gamma
        }
        
        fnew = likelihood.diff(x, betanew[i,])
        if (abs(fnew - fold) / abs(fnew) < tol)
          break
        betaold = betanew[i,]
        fold = fnew
      }
      
      nb_coef_non_nuls[i] = sum(betanew[i,] != 0)
    }
    dimnames(betanew)[2] = dimnames(x)[2]
    
    if (standardize) {
      for (i in seq(length(fraction)))
        betanew[i, betanew[i,] != 0] <-
          betanew[i, betanew[i,] != 0] / sds[betanew[i,] != 0]
    }
    
    beta[,!remove] = betanew
    nz = apply(betanew, 1, function(x)
      sum(x != 0))
    W = lambda
  }
  
  list(
    beta = betanew,
    fraction = fraction,
    nz = nb_coef_non_nuls,
    W = lambda,
    x_rec = x_rec,
    arg = list(
      y = y_rec,
      strata = strata,
      standardize = standardize,
      fraction = fraction,
      nopenalize = nopenalize,
      adaptive = adaptive,
      separate = separate,
      ols = ols,
      maxit = maxit,
      maxitB = maxitB,
      thr = thr,
      tol = tol,
      epsilon = epsilon,
      log = log,
      trace = trace,
      p.fact = p.fact
    )
  )
}


#tools
#sigmoid function
sigmoid <- function(x, beta) {
  1 / (1 + exp(-x %*% beta))
}

#likelihood function
likelihood.diff <- function(x, beta) {
  sum(-log(1 / (1 + exp(-x %*% beta))))
}

# gradient function
grad.diff <- function(x, beta) {
  colSums(sweep(
    -x,
    MARGIN = 1,
    STATS = (1 - 1 / (1 + exp(-x %*% beta))),
    FUN = "*"
  ))
}

#Calcul of fraction
frac <- function(epsilon, log, x, n, m, nbfraction) {
  fracmax = max(abs((t(x) %*% rep(1 / 2, n))))
  fracmin = fracmax * epsilon
  if (log == TRUE) {
    fraction = exp(seq(
      from = log(fracmax),
      to = log(fracmin),
      length.out = nbfraction
    ))
  } else {
    fraction = seq(from = fracmax,
                   to = fracmin,
                   length.out = nbfraction)
  }
  
  return(fraction)
}


# stabilize/standardize
stand <- function(x, n, unit = T) {
  vars <- apply(x, 2, var) * (n - 1) / n
  if (unit) {
    vars[vars == 0] <- 1
  }
  sds <- sqrt(vars)
  
  return(sds)
}

mod <- function(x, m) {
  t1 <- floor(x / m)
  return(x - t1 * m)
}
