

reg.diff1M = function(X,
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
                      coefnuls = FALSE,
                      log = TRUE,
                      adaptive = FALSE,
                      separate = FALSE,
                      ols = FALSE,
                      p.fact = NULL,
                      remove = FALSE) {
  #Algorithme IRLS-LASSOSHOOTING
  #require(data.table)
  
  x_rec = X
  y_rec = y
  strata_rec = strata
  if (length(y) != nrow(X))
    stop("Please ensure that each observation has predictors and response")
  
  
  if (missing(strata)) {
    stop("'strata' is missing")
  } else{
    y1 = aggregate(as.data.frame(y), by = list(strata), function(u)
      u[1] - u[-1])$y
    
    if (any(y1 != 1))
      stop("Response vector should be 1 case and 1 control in each strata, starting by the case")
  }

  ust <- unique(strata)
  
  # require(foreach)
  x <- foreach(i = ust, .combine = rbind) %do% apply(X[strata == i, ], 2, function(u)
    u[1] - u[-1])
  
  M <- as.numeric(table(strata)[1] - 1)
  
  # CHANGE of strata
  strata <- sort(rep(ust, M))
  
  # strata index
  strata_ust  <- as.vector(ust)
  strata_freq <- list(c())
  for (i in strata_ust) {
    strata_freq[[i]] <- which(strata==i)
  }
  strata_table <- list(strata_ust, strata_freq)

  x <- as.matrix(x)
  d <- dim(x)
  n <- d[1]
  m <- d[2]
  N <- length(ust)

  # library(lassoshooting)
  # stabilize/standardize.
  if (standardize) {
    sds = stand1M(x, n)
    x <- x / matrix(sds, nrow(x), ncol(x), byrow = T)
  }
  
  # calcul of regularization parameters
  if (is.null(fraction)) {
    if (missing(nbfraction)) {
      nbfraction = 100
    }
    
    fraction = frac1M(
      epsilon = epsilon,
      log = log,
      x = x,
      n = n,
      m = m,
      nbfraction = nbfraction,
      M = M
    )
  } else{
    nbfraction = length(fraction)
  }

  if (adaptive & is.null(p.fact)) {
    warning("p.fact required")
  }
  
  #Estimation of coefficients for each regularization parameter
  nb_coef_non_nuls <- c()
  betanew <- matrix(0, nbfraction, m)

  for (i in (1:nbfraction)) {
    if (trace) {
      if (mod1M(i, 40) == 0) {
        cat("fraction ", i, "\n")
      }
    }
    if (i == 1) {
      betaold <- rep(0, m)
    } else {
      betaold <- betanew[(i - 1), ]
    }
    a = 0
    fold = likelihood.diff1M(x, betaold, strata_table)

    while (a < maxit) {
      a <- a + 1
      z <- rep(0, n)
      x_local      <- exp(-x %*% betaold)
      sumbys_local_T <- (1 + sumbys(x_local, strata_table, r = T))

      z      <- x %*% betaold + sumbys_local_T
      lambda <- x_local     / ( sumbys_local_T ^ 2)
      rm(x_local)
      
      lambda_vector_sqrt <- sqrt(as.vector(lambda))
      
      X <- lambda_vector_sqrt * x
      Y <- lambda_vector_sqrt * z
      rm(z, lambda_vector_sqrt)
      
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
        XtX <- t(X) %*% X
        Xty <- t(X) %*% Y
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
        delta <- grad.diff1M(x, betaold, strata_table) %*% step
        likelihood_old <- likelihood.diff1M(x, betaold, strata_table)
        for (l in (1:maxitB)) {
          gamma <- betaold + t * step
          if (likelihood.diff1M(x, gamma, strata_table) <= ( likelihood_old + 0.3 * t * delta))
            break
          t <- 0.9 * t
        }
        betanew[i, ] <- (1 - t) * betaold + t * gamma
      } else{
        betanew <- gamma
      }

      fnew <- likelihood.diff1M(x, betanew[i, ], strata_table)
      
      criteria3 <- abs(fnew - fold) / abs(fnew)
      if (criteria3 < tol)
        break
      betaold <- betanew[i, ]
      fold    <- fnew
      
    }
    
    nb_coef_non_nuls[i] <- sum(betanew[i, ] != 0)
  }
  
  dimnames(betanew)[2] <- dimnames(x)[2]

  if (standardize) {
    for (i in seq(length(fraction)))
      betanew[i, betanew[i, ] != 0] <- betanew[i, betanew[i, ] != 0] / sds[betanew[i, ] != 0]
  }
  
  list(
    beta = betanew,
    fraction = fraction,
    nz = nb_coef_non_nuls,
    W = lambda,
    x_rec = x_rec,
    arg = list(
      y = y_rec,
      strata = strata_rec,
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


#==sum by strata, repeated each the number of controls if necessary
sumbys <- function(x, strata_table, r = TRUE) {
  ust <- strata_table[[1]]
  feq <- strata_table[[2]]

  z<-matrix(NA,nrow=length(ust),ncol=1)
  cont<-0
  for(i in ust){
    cont=cont+1
    z[cont,1]<-sum(x[feq[[i]]])
  }

  if (r) {
    z <- rep(z, each = (length(x) / length(ust)))
  }
  return(z)
}

#===Calcul sigmoid function
sigmoid1M <- function(x, beta, strata_table, r = TRUE) {
  1 / (1 + sumbys(exp(-x %*% beta), strata_table, r = r))
}

#===Calcul of likelihood
likelihood.diff1M <- function(x, beta, strata_table) {
  sum(log(1 + sumbys(exp(-x %*% beta), strata_table, r = F)))
}

#===Calcul of gradient
grad.diff1M <- function(x, beta, strata_table) {
  U = exp(-x %*% beta) / (1 + sumbys(exp(-x %*% beta), strata_table))
  res = as.vector(-t(x) %*% U)
  return(res)
}

#===Calcul of fraction, M is the number of controls
frac1M <- function(epsilon, log, x, n, m, nbfraction, M) {
  fracmax <- max(abs((t(x) %*% rep(1 / (M + 1), n))))
  fracmin <- fracmax * epsilon
  if (log == TRUE) {
    fraction <- exp(seq(from = log(fracmax), to = log(fracmin), length.out = nbfraction ))
  } else {
    fraction <- seq(from = fracmax, to = fracmin, length.out = nbfraction)
  }
  
  return(fraction)
}

#===stabilize/standardize
stand1M <- function(x, n) {
  vars <- apply(x, 2, var) * (n - 1) / n
  vars[vars == 0] <- 1
  sds <- sqrt(vars)
  
  return(sds)
}

mod1M <- function(x, m) {
  t1 <- floor(x / m)
  return(x - t1 * m)
}
