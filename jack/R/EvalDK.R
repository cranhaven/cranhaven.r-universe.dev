#' @importFrom gmp as.bigq is.bigq factorialZ
NULL

JackEvalNum <- function(x, lambda, alpha){
  jac <- function(m, k, mu, nu, beta){
    if(length(nu) == 0L || nu[1L]==0L || m == 0L) return(1)
    if(length(nu) > m && nu[m+1L] > 0L) return(0)
    if(m == 1L) return(x[1L]^nu[1L] * prod(alpha*seq_len(nu[1L]-1L)+1))
    if(k == 0L && !is.na(s <- S[.N(lambda,nu),m])) return(s)
    i <- max(1L, k)
    s <- jac(m-1L, 0L, nu, nu, 1) * beta * x[m]^(sum(mu)-sum(nu))
    while(length(nu) >= i && nu[i] > 0L){
      if(length(nu) == i && nu[i] > 0L || nu[i] > nu[i+1L]){
        .nu <- nu; .nu[i] <- nu[i]-1L
        gamma <- beta * .betaratio(mu, nu, i, alpha)
        if(nu[i] > 1L){
          s <- s + jac(m, i, mu, .nu, gamma)
        }else{
          s <- s + jac(m-1L, 0L, .nu, .nu, 1) * gamma *
            x[m]^(sum(mu)-sum(.nu))
        }
      }
      i <- i + 1L
    }
    if(k == 0L) S[.N(lambda,nu),m] <- s
    return(s)
  }
  S <- matrix(NA_real_, nrow = .N(lambda,lambda), ncol = length(x))
  jac(length(x), 0L, lambda, lambda, 1)
}

JackEvalQ <- function(x, lambda, alpha){
  jac <- function(m, k, mu, nu, beta){
    if(length(nu) == 0L || nu[1L]==0L || m == 0L){
      return(as.bigq(1L))
    }
    if(length(nu) > m && nu[m+1L] > 0L) return(as.bigq(0L))
    if(m == 1L) return(x[1L]^nu[1L] * prod(alpha*seq_len(nu[1L]-1L)+1))
    if(k == 0L && !is.na(s <- S[.N(lambda,nu),m])) return(s)
    i <- max(1L,k)
    s <- jac(m-1L, 0L, nu, nu, as.bigq(1L)) * beta * x[m]^(sum(mu)-sum(nu))
    while(length(nu) >= i && nu[i] > 0L){
      if(length(nu) == i|| nu[i] > nu[i+1L]){
        .nu <- nu; .nu[i] <- nu[i]-1L
        gamma <- beta * .betaratio(mu, nu, i, alpha)
        if(nu[i] > 1L){
          s <- s + jac(m, i, mu, .nu, gamma)
        }else{
          s <- s + jac(m-1L, 0L, .nu, .nu, as.bigq(1L)) * gamma *
            x[m]^(sum(mu)-sum(.nu))
        }
      }
      i <- i + 1L
    }
    if(k == 0L) S[.N(lambda,nu),m] <- s
    return(s)
  }
  S <- as.bigq(matrix(NA_integer_, nrow = .N(lambda,lambda), ncol = length(x)))
  jac(length(x), 0L, lambda, lambda, as.bigq(1L))
}

JackEval <- function(x, lambda, alpha){
  stopifnot(isPartition(lambda))
  lambda <- as.integer(lambda)
  gmp <- is.bigq(x) || is.bigq(alpha)
  if(gmp){
    stopifnot(is.bigq(x), is.bigq(alpha))
    JackEvalQ(x, lambda, alpha)
  }else{
    JackEvalNum(x, lambda, alpha)
  }
}


ZonalEvalNum <- function(x, lambda){
  jack <- JackEvalNum(x, lambda, alpha = 2)
  jlambda <- sum(logHookLengths(lambda, alpha = 2))
  n <- sum(lambda)
  exp(n*log(2) + lfactorial(n) - jlambda) * jack
}

ZonalEvalQ <- function(x, lambda){
  jack <- JackEvalQ(x, lambda, alpha= as.bigq(2L))
  jlambda <- prod(hookLengths_gmp(lambda, alpha = as.bigq(2L)))
  n <- sum(lambda)
  as.bigq(2L)^n * as.bigq(factorialZ(n)) / jlambda * jack
}

ZonalEval <- function(x, lambda){
  stopifnot(isPartition(lambda))
  lambda <- as.integer(lambda)
  gmp <- is.bigq(x)
  if(gmp){
    ZonalEvalQ(x, lambda)
  }else{
    ZonalEvalNum(x, lambda)
  }
}

SchurEvalNum <- function(x, lambda){
  sch <- function(m, k, nu){
    if(length(nu) == 0L || nu[1L]==0L || m == 0L){
      return(1)
    }
    if(length(nu) > m && nu[m+1L] > 0L) return(0)
    if(m == 1L) return(x[1L]^nu[1L])
    if(!is.na(s <- S[.N(lambda,nu),m])) return(s)
    s <- sch(m-1L, 1L, nu)
    i <- k
    while(length(nu) >= i && nu[i] > 0L){
      if(length(nu) == i || nu[i] > nu[i+1L]){
        .nu <- nu; .nu[i] <- nu[i]-1L
        if(nu[i] > 1L){
          s <- s + x[m] * sch(m, i, .nu)
        }else{
          s <- s + x[m] * sch(m-1L, 1L, .nu)
        }
      }
      i <- i + 1L
    }
    if(k == 1L) S[.N(lambda,nu),m] <- s
    return(s)
  }
  S <- matrix(NA_real_, nrow = .N(lambda,lambda), ncol = length(x))
  sch(length(x), 1L, lambda)
}

SchurEvalQ <- function(x, lambda){
  sch <- function(m, k, nu){
    if(length(nu) == 0L || nu[1L]==0 || m == 0L){
      return(as.bigq(1L))
    }
    if(length(nu) > m && nu[m+1L] > 0L) return(as.bigq(0L))
    if(m == 1L) return(x[1L]^nu[1L])
    if(!is.na(s <- S[.N(lambda,nu),m])) return(s)
    s <- sch(m-1L, 1L, nu)
    i <- k
    while(length(nu) >= i && nu[i] > 0L){
      if(length(nu) == i || nu[i] > nu[i+1L]){
        .nu <- nu; .nu[i] <- nu[i]-1L
        if(nu[i] > 1L){
          s <- s + x[m] * sch(m, i, .nu)
        }else{
          s <- s + x[m] * sch(m-1L, 1L, .nu)
        }
      }
      i <- i + 1L
    }
    if(k == 1L) S[.N(lambda,nu),m] <- s
    return(s)
  }
  S <- as.bigq(matrix(NA_integer_, nrow = .N(lambda,lambda), ncol = length(x)))
  sch(length(x), 1L, lambda)
}

SchurEval <- function(x, lambda){
  stopifnot(isPartition(lambda))
  lambda <- as.integer(lambda)
  gmp <- is.bigq(x)
  if(gmp){
    SchurEvalQ(x, lambda)
  }else{
    SchurEvalNum(x, lambda)
  }
}


ZonalQEvalNum <- function(x, lambda){
  jack <- JackEvalNum(x, lambda, alpha = 1/2)
  jlambda <- sum(logHookLengths(lambda, alpha = 1/2))
  n <- sum(lambda)
  exp(-n*log(2) + lfactorial(n) - jlambda) * jack
}

ZonalQEvalQ <- function(x, lambda){
  jack <- JackEvalQ(x, lambda, alpha = as.bigq(1L,2L))
  jlambda <- prod(hookLengths_gmp(lambda, alpha = as.bigq(1L,2L)))
  n <- sum(lambda)
  as.bigq(1L,2L)^n * as.bigq(factorialZ(n)) / jlambda * jack
}

ZonalQEval <- function(x, lambda){
  stopifnot(isPartition(lambda))
  lambda <- as.integer(lambda)
  gmp <- is.bigq(x)
  if(gmp){
    ZonalQEvalQ(x, lambda)
  }else{
    ZonalQEvalNum(x, lambda)
  }
}
