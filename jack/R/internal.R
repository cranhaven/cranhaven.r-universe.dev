#' @importFrom partitions conjugate parts
#' @importFrom gmp as.bigq is.bigq
#' @importFrom utils tail head
#' @importFrom qspray qlone
NULL

pairing <- function(lambdas) {
  mapply(
    function(lambda1, lambda2) {
      list(lambda1, lambda2)
    },
    tail(lambdas, -1L), head(lambdas, -1L),
    USE.NAMES = FALSE, SIMPLIFY = FALSE
  )
}

lastSubpartition <- function(w, lambda) {
  if(w == 0L || length(lambda) == 0L) {
    integer(0L)
  } else {
    k <- lambda[1L]
    if(w <= k) {
      w
    } else {
      c(k, lastSubpartition(w - k, tail(lambda, -1L)))
    }
  }
}

isDecreasing <- function(x) {
  all(diff(x) <= 0)
}

isIncreasing <- function(x) {
  all(diff(x) >= 0)
}

.rg <- function(start, end) {
  if(start <= end) {
    start:end
  } else {
    integer(0L)
  }
}

#' @importFrom utils head
#' @noRd
removeTrailingZeros <- function(x) {
  n <- length(x)
  while(x[n] == 0 && n > 0L) {
    n <- n - 1L
  }
  head(x, n)
}

Columns <- function(M) {
  lapply(seq_len(ncol(M)), function(j) {
    M[, j]
  })
}

partitionAsString <- function(lambda) {
  paste0("[", toString(lambda), "]")
}

fromPartitionAsString <- function(string) {
  string <- gsub("(\\[|\\])", "", string)
  as.integer(strsplit(string, ",", fixed = TRUE)[[1L]])
}

isInteger <- function(n){
  is.vector(n) && is.numeric(n) &&
    length(n) == 1L && !is.na(n) && as.integer(n) == n
}

isPositiveInteger <- function(n){
  is.vector(n) && is.numeric(n) && length(n) == 1L && !is.na(n) && floor(n) == n
}

# isStrictlyPositiveInteger <- function(n){
#   isPositiveInteger(n) && n != 0
# }

isPartition <- function(lambda){
  length(lambda) == 0L ||
    all(floor(lambda) == lambda) && all(diff(lambda) <= 0)
}

dualPartition <- function(lambda){
  conjugate(lambda)
}

logHookLengths <- function(lambda, alpha){
  i <- rep(seq_along(lambda), times = lambda)
  j <- unlist(sapply(lambda, seq_len, simplify = FALSE))
  lambdaPrime <- dualPartition(lambda)
  upperHL <- lambdaPrime[j] - i + alpha*(lambda[i] - j + 1L)
  lowerHL <- lambdaPrime[j] - i + 1 + alpha*(lambda[i] - j)
  log(c(upperHL, lowerHL))
}

hookLengths_gmp <- function(lambda, alpha){
  i <- rep(seq_along(lambda), times = lambda)
  j <- unlist(sapply(lambda, seq_len, simplify = FALSE))
  lambdaPrime <- dualPartition(lambda)
  alpha <- as.bigq(alpha)
  upperHL <- lambdaPrime[j] - i + alpha*(lambda[i] - j + 1L)
  lowerHL <- lambdaPrime[j] - i + 1L + alpha*(lambda[i] - j)
  rbind(lowerHL, upperHL)
}

#' @importFrom gmp factorialZ
#' @noRd
JackCcoefficient <- function(lambda, alpha) {
  if(length(lambda) == 0L) {
    as.bigq(1L)
  } else {
    k <- sum(lambda)
    jlambda <- prod(hookLengths_gmp(lambda, alpha))
    as.bigq(alpha)^k * factorialZ(k) / jlambda
  }
}

JackPcoefficient <- function(lambda, alpha) {
  if(length(lambda) == 0L){
    as.bigq(1L)
  } else {
    1L / prod(hookLengths_gmp(lambda, alpha)[1L, ])
  }
}

JackQcoefficient <- function(lambda, alpha) {
  if(length(lambda) == 0L){
    as.bigq(1L)
  } else {
    1L / prod(hookLengths_gmp(lambda, alpha)[2L, ])
  }
}

#' @importFrom qspray qlone qone
#' @noRd
symbolicJackPcoefficientInverse <- function(lambda) {
  if(length(lambda) == 0L) {
    return(qone())
  }
  i <- rep(seq_along(lambda), times = lambda)
  j <- unlist(sapply(lambda, seq_len, simplify = FALSE))
  lambdaPrime <- as.bigq(dualPartition(lambda))
  lambda <- as.bigq(lambda)
  alpha <- qlone(1L)
  out <- 1L
  for(k in seq_along(i)) {
    out <- out *
      (lambdaPrime[j[k]] - i[k] + 1L + (lambda[i[k]] - j[k])*alpha)
  }
  out
}
symbolicJackQcoefficientInverse <- function(lambda){
  if(length(lambda) == 0L) {
    return(qone())
  }
  i <- rep(seq_along(lambda), times = lambda)
  j <- unlist(sapply(lambda, seq_len, simplify = FALSE))
  lambdaPrime <- as.bigq(dualPartition(lambda))
  lambda <- as.bigq(lambda)
  alpha <- qlone(1L)
  out <- 1L
  for(k in seq_along(i)) {
    out <- out *
      (lambdaPrime[j[k]] - i[k] + alpha*(lambda[i[k]] - j[k] + 1L))
  }
  out
}

symbolicJackCcoefficient <- function(lambda) {
  k <- sum(lambda)
  alpha <- qlone(1L)
  jlambda <- symbolicJackPcoefficientInverse(lambda) *
    symbolicJackQcoefficientInverse(lambda)
  factorialZ(k) * alpha^k / jlambda
}

.Blog <- function(nu, lambda, mu, alpha){
  if(all(nu == 0L)) return(0)
  i <- rep(seq_along(nu), times = nu)
  j <- unlist(sapply(nu, seq_len, simplify = FALSE))
  nuPrime <- dualPartition(nu)
  lambdaPrime <- dualPartition(lambda); l1 <- length(lambdaPrime)
  muPrime <- dualPartition(mu); l2 <- length(muPrime)
  n <- length(i)
  out <- numeric(n)
  for(k in 1L:n){
    ii <- i[k]; jj <- j[k]
    lambdaPj <- if(jj <= l1) lambdaPrime[jj] else 0
    muPj <- if(jj <= l2) muPrime[jj] else 0
    out[k] <- if(lambdaPj == muPj){
      nuPrime[jj] - ii + alpha*(nu[ii] - jj + 1L)
    }else{
      nuPrime[jj] - ii + 1 + alpha*(nu[ii] - jj)
    }
  }
  sum(log(out))
}

.beta <- function(lambda, mu, alpha){
  if(all(lambda == mu)) return(1)
  exp(.Blog(lambda, lambda, mu, alpha) - .Blog(mu, lambda, mu, alpha))
}

.betaratio <- function(kappa, mu, k, alpha){
  t <- k - alpha*mu[k]
  s <- seq_len(k)
  u <- t + 1 - s + alpha*kappa[s]
  s <- seq_len(k-1L)
  v <- t - s + alpha*mu[s]
  s <- seq_len(mu[k]-1L)
  w <- vapply(s, function(i) sum(mu >= i), integer(1L)) - t - alpha*s #conjugate(mu)[s] - t - alpha*s
  alpha * prod(u/(u+alpha-1)) * prod((v+alpha)/v) * prod((w+alpha)/w)
}

.B_gmp <- function(nu, lambda, mu, alpha){
  if(all(nu == 0L)) return(as.bigq(1L))
  i <- rep(seq_along(nu), times = nu)
  j <- unlist(sapply(nu, seq_len, simplify = FALSE))
  nuPrime <- as.bigq(dualPartition(nu))
  lambdaPrime <- dualPartition(lambda); l1 <- length(lambdaPrime)
  muPrime <- dualPartition(mu); l2 <- length(muPrime)
  n <- length(i)
  out <- as.bigq(integer(n))
  for(k in 1L:n){
    ii <- i[k]; jj <- j[k]
    lambdaPj <- if(jj <= l1) lambdaPrime[jj] else 0L
    muPj <- if(jj <= l2) muPrime[jj] else 0L
    out[k] <- if(lambdaPj == muPj){
      nuPrime[jj] - ii + alpha*(nu[ii] - jj + 1L)
    }else{
      nuPrime[jj] - ii + 1L + alpha*(nu[ii] - jj)
    }
  }
  sum(prod(out))
}

.beta_gmp <- function(lambda, mu, alpha){
  if(all(lambda == mu)) return(as.bigq(1L))
  .B_gmp(lambda, lambda, mu, alpha) / .B_gmp(mu, lambda, mu, alpha)
}

.N <- function(lambda, mu){
  n <- length(lambda)
  M <- sapply(1L:n, function(i) prod(tail(lambda+1L, n-i)))
  sum(mu * M)
}

#####
listOfPartitions <- function(n) {
  if(n == 0L) {
    list(integer(0L))
  } else {
    apply(parts(n), 2L, removeTrailingZeros, simplify = FALSE)
  }
}

isDominated <- function(mu, lambda){
  n <- sum(lambda)
  # assumption: n == sum(mu)
  # if(sum(mu) != n) {
  #   return(FALSE)
  # }
  lambda <- lambda[seq_len(match(0L, lambda, nomatch = length(lambda)+1L)-1L)]
  lambda <- c(lambda, rep(0L, n-length(lambda)))
  mu <- mu[seq_len(match(0L, mu, nomatch = length(mu)+1L)-1L)]
  for(i in seq_along(mu)){
    if(sum(mu[1L:i]) > sum(lambda[1L:i])){
      return(FALSE)
    }
  }
  TRUE
}

dominatedPartitions <- function(lambda){
  allParts <- parts(sum(lambda))
  allParts[, apply(allParts, 2L, isDominated, lambda = lambda),
           drop = FALSE]
}

listOfDominatedPartitions <- function(lambda) {
  n <- length(lambda)
  if(n == 0L) {
    return(list(integer(0L)))
  }
  go <- function(h, w, dds, e) {
    if(w == 0L) {
      list(integer(0L))
    } else {
      arange <- seq_len(min(h, dds[1L] - e))
      do.call(c, lapply(arange, function(a) {
        L <- go(a, w-a, dds[-1L], e+a)
        lapply(L, function(as) {
          c(a, as)
        })
      }))
    }
  }
  weight <- sum(lambda)
  dsums <- c(cumsum(lambda), rep(weight, weight - n))
  go(lambda[1L], weight, dsums, 0L)
}

betweenPartitions <- function(mu, lambda){
  doms <- dominatedPartitions(lambda)
  n <- sum(mu)
  doms[, apply(doms, 2L, function(p){
    isDominated(mu,p) && !all(c(mu, rep(0L, n-length(mu))) == p)
  }), drop = FALSE]
}

.rho <- function(lambda) sum(lambda*(lambda-seq_along(lambda)))

.rhoQ <- function(lambda) sum(lambda*(lambda-4*seq_along(lambda)))

#####
.n <- function(lambda){
  sum(seq_len(length(lambda)-1L) * tail(lambda, -1L))
}

.e <- function(lambda, alpha){
  if(is.bigq(alpha)){
    alpha * as.bigq(.n(dualPartition(lambda))) - .n(lambda)
  }else{
    alpha * .n(dualPartition(lambda)) - .n(lambda)
  }
}

.eSymbolic <- function(lambda){
  .n(dualPartition(lambda))*qlone(1) - .n(lambda)
}


fromString <- function(string) {
  as.integer(strsplit(string, ",", fixed = TRUE)[[1L]])
}

####
#' @importFrom mvp mvp as.mvp
#' @importFrom gmp asNumeric as.bigq
#' @noRd
as_mvp_spray <- function(s) {
  if(length(s) == 0L) return(as.mvp(0))
  powers <- s[["index"]]
  m <- nrow(powers)
  n <- ncol(powers)
  vars <- replicate(m, paste0("x_", 1L:n), simplify = FALSE)
  powers <- lapply(1L:m, function(i) powers[i, ])
  mvp(vars, powers, s[["value"]])
}

as_mvp_qspray <- function(s) {
  vars <- lapply(s@powers, function(exponents) paste0("x_", seq_along(exponents)))
  mvp(vars, s@powers, asNumeric(as.bigq(s@coeffs)))
}

#' @importFrom qspray as.qspray qzero qone
#' @importFrom spray zero one lone
NULL

