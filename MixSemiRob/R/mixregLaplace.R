#' Robust Mixture Regression with Laplace Distribution
#'
#' `mixregLap' provides robust estimation for a mixture of linear regression models
#' by assuming that the error terms follow the Laplace distribution (Song et al., 2014).
#'
#' @usage
#' mixregLap(x, y, C = 2, nstart = 20, tol = 1e-05)
#'
#' @param x an n by p matrix of observations (one observation per row). The intercept will be automatically added to \code{x}.
#' @param y an n-dimensional vector of response variable.
#' @param C number of mixture components. Default is 2.
#' @param nstart number of initializations to try. Default is 20.
#' @param tol stopping criteria (threshold value) for the EM algorithm. Default is 1e-05.
#'
#' @return A list containing the following elements:
#'   \item{beta}{C by (p + 1) matrix of estimated regression coefficients.}
#'   \item{sigma}{C-dimensional vector of estimated component standard deviations.}
#'   \item{pi}{C-dimensional vector of estimated mixing proportions.}
#'   \item{lik}{final likelihood.}
#'   \item{run}{total number of iterations after convergence.}
#'
#' @seealso \code{\link{mixregT}} for robust estimation with t-distribution.
#'
#' @references
#'   Song, W., Yao, W., and Xing, Y. (2014). Robust mixture regression model fitting by Laplace distribution.
#'   Computational Statistics & Data Analysis, 71, 128-137.
#'
#' @export
#' @examples
#' data(tone)
#' y = tone$tuned          # length(y) = 160
#' x = tone$stretchratio   # length(x) = 160
#' k = 160
#' x[151:k] = 0
#' y[151:k] = 5
#' \donttest{est_lap = mixregLap(x, y, 2)}
mixregLap <- function(x, y, C = 2, nstart = 20, tol = 1e-05){

  #SK-- Used functions: mixlap_one

  k = C
  n = length(y)
  x = matrix(x, nrow = n)
  a = dim(x)
  p = a[2] + 1
  n1 = 2 * p
  lh = rep(0, nstart)
  bet = matrix(rep(0, k * p), nrow = k)
  sig = 0

  for(j in seq(k)){
    ind = sample(1:n, n1)
    X = cbind(rep(1, n1), x[ind, ])
    bet[j, ] = ginv(t(X) %*% X) %*% t(X) %*% y[ind]
    sig = sig + sum((y[ind] - X %*% bet[j, ])^2)
  }

  pr = rep(1/k, k)
  sig = sig/n1/k
  sig[sig < 0.1] <- 0.5
  sig[sig > 3] <- 1
  est = mixlap_one(x = x, y = y, bet, sig, pr, k, tol)
  lh[1] = est$lik #est$likelihood

  for(i in seq(nstart - 1)){
    bet = matrix(rep(0, k * p), nrow = k)
    sig = 0

    for(j in seq(k)){
      ind = sample(1:n, n1)
      X = cbind(rep(1, n1), x[ind, ])
      bet[j, ] = MASS::ginv(t(X) %*% X) %*% t(X) %*% y[ind]
      sig = sig + sum((y[ind] - X %*% bet[j, ])^2)
      sig[sig < 0.1] <- 0.5
      sig[sig > 3] <- 1
    }

    pr = rep(1/k, k)
    sig = sig/n1/k
    sig[sig < 0.1] <- 0.5
    sig[sig > 3] <- 1
    temp = mixlap_one(x, y, bet, sig, pr, k, tol)
    lh[i + 1] = temp$lik
    if(lh[i + 1] > est$lik & min(temp$theta[, p + 2]) > min(0.05, est$theta[, p + 2])){
      est = temp
    }
  }

  #colnames(est$theta) <- c("beta.intercept", "beta.slope", "sigma", "prop")
  #est$all.lik = lh #SK-- 9/4/2023
  #est$inilikelihoodseq = lh

  npar <- ncol(est$theta)
  out <- list(beta = est$theta[, - c(npar - 1, npar)],
              sigma = est$theta[, npar - 1],
              pi = est$theta[, npar],
              lik = est$lik,
              run = est$run)
  return(out)
}


#-------------------------------------------------------------------------------
# Internal functions
#-------------------------------------------------------------------------------
# Mixture regression fitting by Laplace distribution
# mixlap_one estimates the mixture regression parameters based on ONE initial value.
mixlap_one <- function(x, y, bet, sig, pr, m = 2, tol = 1e-05) {

  run <- 0
  n <- length(y)
  x <- matrix(x, nrow = n)
  X <- cbind(rep(1, n), x)

  if (length(sig) > 1) { # the case when the variance is unequal
    r <- matrix(rep(0, m * n), nrow = n)
    lh <- 0

    for (j in seq(m)) {
      r[, j] <- y - X %*% bet[j, ]
      r[abs(r) < 0.0001] <- 0.0001
      lh <- lh + (pr[j] / (sqrt(2) * sig[j])) * exp(-(sqrt(2) * abs(r[, j])) / sig[j])
    }

    lh <- sum(log(lh))

    # E-step
    repeat { ## repeat left
      prest <- c(bet, sig, pr)
      run <- run + 1
      plh <- lh
      pk <- matrix(rep(0, m * n), nrow = n)
      delta <- matrix(rep(0, m * n), nrow = n)

      for (j in seq(m)) {
        pk[, j] <- pr[j] * pmax(10^(-300), (exp(-(sqrt(2) * abs(r[, j])) / sig[j])) / sig[j])
        delta[, j] <- sig[j] / (sqrt(2) * abs(r[, j]))
      }

      pk <- pk / matrix(rep(apply(pk, 1, sum), m), nrow = n)

      # M-step
      np <- apply(pk, 2, sum)
      pr <- np / n
      lh <- 0

      for (j in seq(m)) {
        w1 <- diag(pk[, j])
        w2 <- diag(delta[, j])
        w <- w1 %*% w2
        w_star <- matrix(rep(0, n), nrow = n)

        for (i in seq(n)) {
          w_star[i, ] <- w[i, i]
        }

        bet[j, ] <- ginv(t(X) %*% w %*% X) %*% t(X) %*% w %*% y
        r[, j] <- y - X %*% bet[j, ]
        r[abs(r) < 0.0001] <- 0.0001

        sig[j] <- sqrt(2 * t(w_star) %*% (r[, j]^2) / np[j])
        sig[sig < 0.1] <- 0.5        ## truncate the extremely small sigma
        sig[sig > 3] <- 1           ## truncate the extremely large sigma

        lh <- lh + (pr[j] / (sqrt(2) * sig[j])) * exp(-(sqrt(2) * abs(r[, j])) / sig[j])
      }

      lh <- sum(log(lh))
      dif <- lh - plh

      if (dif < tol | run > 500) {
      #if (dif < 10^(-5) | run > 500) {
        break
      }
    }
  } else {  # the case when the variance is equal
    r <- matrix(rep(0, m * n), nrow = n)
    lh <- 0

    for (j in seq(m)) {
      r[, j] <- y - X %*% bet[j, ]
      r[abs(r) < 0.0001] <- 0.0001
      lh <- lh + (pr[j] / (sqrt(2) * sig)) * exp(-(sqrt(2) * abs(r[, j])) / sig)
    }

    lh <- sum(log(lh))

    # E-steps
    repeat { ## repeat left
      prest <- c(bet, sig, pr)
      run <- run + 1
      plh <- lh
      pk <- matrix(rep(0, m * n), nrow = n)
      delta <- matrix(rep(0, m * n), nrow = n)

      for (j in seq(m)) {
        pk[, j] <- pr[j] * pmax(10^(-300), (exp(-(sqrt(2) * abs(r[, j])) / sig)) / sig)
        delta[, j] <- sig / (sqrt(2) * abs(r[, j]))
      }

      pk <- pk / matrix(rep(apply(pk, 1, sum), m), nrow = n)

      # M-step
      np <- apply(pk, 2, sum)
      pr <- np / n
      w_star <- matrix(rep(0, n * m), nrow = n)

      for (j in seq(m)) {
        w1 <- diag(pk[, j])
        w2 <- diag(delta[, j])
        w <- w1 %*% w2

        for (i in seq(n)) {
          w_star[i, j] <- w[i, i]
        }

        bet[j, ] <- ginv(t(X) %*% w %*% X) %*% t(X) %*% w %*% y
        r[, j] <- y - X %*% bet[j, ]
        r[abs(r) < 0.0001] <- 0.0001
      }

      sig <- sqrt(2 * sum(w_star * (r^2)) / n)
      sig[sig < 0.1] <- 0.5        ## truncate the extremely small sigma
      sig[sig > 3] <- 1           ## truncate the extremely large sigma

      lh <- 0

      for (j in seq(m)) {
        lh <- lh + (pr[j] / (sqrt(2) * sig)) * exp(-(sqrt(2) * abs(r[, j])) / sig)
      }

      lh <- sum(log(lh))
      dif <- lh - plh

      if (dif < 10^(-5) | run > 100) {
        break
      }
    }

    sig <- sig * rep(1, m) # the case when the variance is equal
  }

  est <- list(theta = matrix(c(bet, sig, pr), nrow = m), lik = lh, run = run)
  #est <- list(theta = matrix(c(bet, sig, pr), nrow = m), lik = lh, run = run, diflik = dif)
  est
}
