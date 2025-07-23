#' Parameter Estimation of Normal Mixture Using EM Algorithm
#'
#' `EMnormal' is used to estimate the parameters of a univariate or multivariate
#' normal mixture model using the expectation-maximization (EM) algorithm.
#' The result can be used as the initial value for the \code{\link{mixLogconc}} and
#' \code{\link{mixLogconcHD}} function.
#'
#' @usage
#' EMnormal(x, C = 2, nstart = 20, tol = 1e-05)
#'
#' @param x an n by p data matrix where n is the number of observations and
#'   p is the dimension of the data.
#' @param C number of mixture components. Default is 2.
#' @param nstart number of initializations to try. Default is 20.
#' @param tol stopping criteria (threshold value) for the EM algorithm. Default is 1e-05.
#'
#' @return A list containing the following elements:
#'   \item{loglik}{final log-likelihood.}
#'   \item{pi}{estimated mixing proportions.}
#'   \item{mu}{estimated component means.}
#'   \item{sigma}{estimated component standard deviation or covariance matrix.}
#'
#' @seealso \code{\link{mixLogconc}}, \code{\link{mixLogconcHD}}
#'
#' @export
#' @examples
#' #-----------------------------------------------------------------------------------------#
#' # Univariate Case
#' #-----------------------------------------------------------------------------------------#
#' x = matrix(rnorm(100, 2, sqrt(2)), nrow = 100)
#' x[1:60] = x[1:60] + 5
#' ini = EMnormal(x)
EMnormal <- function(x, C = 2, nstart = 20, tol = 1e-05){
  # computes the parameter estimates of a (possibly multivariate)
  # Gaussian mixture with k components using EM.
  # The observations are the row vectors of x.
  # Uses random initializations for 20 restarts.
  # Stopping criterion: relative change in log likelihood < 10^(-8)
  k = C
  x = as.matrix(x)
  n = nrow(x)
  d = ncol(x)

  tau = matrix(numeric(n * k), nrow = n)  # membership weight matrix
  lik = tau
  bestloglik = -Inf
  loglik = 1

  for(restarts in 1:nstart){
    # Initialize random starting parameters:
    mu = mvtnorm::rmvnorm(k, apply(x, 2, mean), stats::cov(x))  # mu is kxd
    sigma = array(rep(1, d * d * k), dim = c(d, d, k))
    for(m in 1:k){
      sigma[, , m] = stats::cov(x)
    }
    pi = matrix(rep(1/k, k), nrow = 1)

    for(iters in 1:1000){  # max. number of iterations
      # E-step:
      for(m in 1:k){
        tau[, m] = mvtnorm::dmvnorm(x, mu[m, ], as.matrix(sigma[, , m]))
      }
      tau = (matrix(rep(1, n), nrow = n) %*% pi) * tau
      tau = tau/(tau %*% matrix(rep(1, k * k), nrow = k))

      # M-step:
      for(m in 1:k){
        w = tau[, m]/sum(tau[, m])
        mu[m, ] = t(w) %*% x
        xcenter = x - matrix(rep(1, n), nrow = n) %*% mu[m, ]
        sigma[, , m] = t(xcenter) %*% (xcenter * (w %*% matrix(rep(1, d), nrow = 1)))
        lik[, m] = mvtnorm::dmvnorm(x, mu[m, ], as.matrix(sigma[, , m]))
      }

      pi = matrix(apply(tau, 2, sum)/n, nrow = 1)
      oldloglik = loglik
      loglik = sum(log(lik %*% t(pi)))

      if(iters > 1 && (loglik - oldloglik)/abs(oldloglik) < tol){
        break
      }
    }

    if(loglik > bestloglik){
      bestloglik = loglik
      bestpi = pi
      bestmu = mu
      bestsigma = sigma
    }
  }

  parameters = list(loglik = bestloglik, pi = bestpi, mu = bestmu, sigma = bestsigma)
  parameters
}

#' Clustering with Mixtures of Log-concave Distributions using EM Algorithm (Univariate)
#'
#' `mixLogconc' is used to estimate the parameters of a mixture of univariate
#' log-concave distributions.
#'
#' @usage
#' mixLogconc(x, C = 2, ini = NULL, nstart = 20, tol = 1e-05)
#'
#' @param x an n by 1 data matrix where n is the number of observations.
#' @param C number of mixture components. Default is 2.
#' @param ini initial value for the EM algorithm. Default value is NULL, which
#'   obtains the initial value using the \code{\link{EMnormal}} function. It can be a list
#'   with the form of \code{list(pi, mu, sigma)}, where \code{pi} is a 1 by C matrix
#'   of mixing proportions, \code{mu} is a C by 1 matrix of component means, and
#'   \code{sigma} is a p by p by 1 array of standard deviations or covariance matrices
#'   of \code{C} mixture components.
#' @param nstart number of initializations to try. Default is 20.
#' @param tol stopping criteria (threshold value) for the EM algorithm. Default is 1e-05.
#'
#' @return A list containing the following elements:
#'   \item{loglik}{final log-likelihood.}
#'   \item{pi}{estimated mixing proportions.}
#'   \item{f}{component densities at x.}
#'
#' @seealso \code{\link{EMnormal}}, \code{\link{mixLogconcHD}}
#'
#' @references
#'   Chang, G. T., and Walther, G. (2007). Clustering with mixtures of log-concave
#'   distributions. Computational Statistics & Data Analysis, 51(12), 6242-6251.
#'
#'   Hu, H., Wu, Y., and Yao, W. (2016). Maximum likelihood estimation of the mixture
#'   of log-concave densities. Computational Statistics & Data Analysis, 101, 137-147.
#'
#' @export
#'
#' @importFrom mvtnorm rmvnorm dmvnorm
#'
#' @examples
#' set.seed(4)
#' x = matrix(rnorm(100, 2, sqrt(2)), nrow = 100)
#' x[1:60] = x[1:60] + 5
#' EMlogc = mixLogconc(x, C = 2)
mixLogconc <- function(x, C = 2, ini = NULL, nstart = 20, tol = 1e-05){

  k = C

  if(is.null(ini)){
    ini = EMnormal(x, k, nstart, tol)
  }

  x = as.matrix(x)
  n = nrow(x)
  tau = matrix(numeric(n * k), nrow = n)  # membership probabilities
  f = tau  # component densities at the x's

  # Initialize using the membership probs. from EMnormal:
  for(m in 1:k){
    f[, m] = stats::dnorm(x, ini$mu[m], sqrt(ini$sigma[m]))
  }
  pi = ini$pi

  for(iters in 1:5){
    # E-step:
    for(i in 1:n){
      tau[i, ] = pi * f[i, ]/as.numeric(f[i, ] %*% t(pi))
    }

    # M-step:
    for(m in 1:k){
      f[, m] = exp(lcmle(x, tau[,m]))
    }

    pi = matrix(apply(tau, 2, sum)/n, nrow = 1)
    loglik = sum(log(f %*% t(pi)))
  }

  parameters = list(loglik = loglik, pi = pi, f = f)
  parameters
}

#' Clustering with Mixtures of Log-concave Distributions using EM Algorithm (Multivariate)
#'
#' `mixLogconcHD' is used to estimate the parameters of a mixture of multivariate
#' log-concave distributions. The correlation structure among components is
#' calculated by the normal copula.
#'
#' @usage
#' mixLogconcHD(x, C, ini = NULL, nstart = 20, tol = 1e-05, maxiter = 100)
#'
#' @param x an n by p data matrix where n is the number of observations and
#'   p is the dimension of the data.
#' @param C number of mixture components.
#' @param ini initial value for the EM algorithm. Default value is NULL, which
#'   obtains the initial value using the \code{\link{EMnormal}} function. It can be a list
#'   with the form of \code{list(pi, mu, sigma)}, where \code{pi} is a 1 by C matrix
#'   of mixing proportions, \code{mu} is a C by p matrix of component means, and
#'   \code{sigma} is a p by p by C array of standard deviations or covariance matrices
#'   of \code{C} mixture components.
#' @param nstart number of initializations to try. Default is 20.
#' @param tol stopping criteria (threshold value) for the EM algorithm. Default is 1e-05.
#' @param maxiter maximum number of iterations for the EM algorithm. Default is 100.
#'
#' @return A list containing the following elements:
#'   \item{loglik}{final log-likelihood.}
#'   \item{pi}{estimated mixing proportions.}
#'   \item{f}{component densities at x.}
#'   \item{sigma}{estimated standard deviation or covariance matrix.}
#'
#' @seealso \code{\link{mixLogconc}}
#'
#' @references
#'   Chang, G. T., and Walther, G. (2007). Clustering with mixtures of log-concave
#'   distributions. Computational Statistics & Data Analysis, 51(12), 6242-6251.
#'
#'   Hu, H., Wu, Y., and Yao, W. (2016). Maximum likelihood estimation of the mixture
#'   of log-concave densities. Computational Statistics & Data Analysis, 101, 137-147.
#'
#' @export
#'
#' @importFrom mvtnorm rmvnorm dmvnorm
#'
#' @examples
#'
#' x = mvtnorm::rmvnorm(100, c(0, 0), matrix(c(2, 1, 1, 2), nrow = 2))
#' x = matrix(x, nrow = 100)
#' x[1:60, ] = x[1:60, ] + 5
#' \donttest{EMlogc = mixLogconcHD(x, C = 2)}
mixLogconcHD <- function(x, C, ini = NULL, nstart = 20, tol = 1e-05, maxiter = 100){
  # Does EM by computing the logconcave MLE for each marginal in
  # each component in the M-step. Then use the normal copula to
  # model the correlation structure in each component:
  # Transform each marginal to a normal using the estimated cdf.
  # Initializes using the output EMnorm of EM with normal components.
  # Observations are given as row vectors of x. k is the number of components.
  #fh:logconcave marginal component densities at x.
  #Fh:cdfs of the fh.
  k = C
  if (is.null(ini)){
    ini = EMnormal(x, k, nstart, tol)
  }
  x = as.matrix(x)
  n = nrow(x)
  d = ncol(x)

  tau = matrix(numeric(n * k), nrow = n)          # membership weight matrix
  f = tau                                         # component densities at the x's
  fh = array(rep(1, n * d * k), dim = c(n, d, k)) # logconcave marginal component densities at the x's
  Fh = fh                                         # cdfs of the fh
  sigma = array(rep(1, d * d * k), dim = c(d, d,k ))# covariances of the transformed marginals

  # Initialize using the membership probs. from EMnormal:
  for(m in 1:k){
    f[, m] = dmvnorm(x, ini$mu[m, ], as.matrix(ini$sigma[, , m]))
  }
  pi = ini$pi

  for(iters in 1:maxiter){
    # E-step:
    tau = (matrix(rep(1, n), nrow = n) %*% pi) * f
    tau = tau/(tau %*% matrix(rep(1, k * k), nrow = k))

    # M-step:
    for(m in 1:k){
      J = matrix(rep(1, n), nrow = n)     # initialize Jacobian
      Y = matrix(rep(0, n * d), nrow = n) # Y is transformed data

      for(j  in 1:d){
        fh[, j, m] = exp(lcmle(x[, j], tau[, m]))
        xs = sort(x[, j], index.return = TRUE)$x
        idx = sort(x[, j], index.return = TRUE)$ix
        fhs = fh[idx, j, m]  # xs is sorted data, fhs corresponds to x
        cdfincrements = (xs[2:n] - xs[1:(n - 1)]) * (fhs[2:n] + fhs[1:(n - 1)])/2
        cdf = cumsum(c(0, cdfincrements))
        # scale so that cdf(x1)=1/(n+1), cdf(xn)=n/(n+1):
        Fh[idx, j, m] = 1/(n + 1) + cdf/cdf[n] * (n - 1)/(n + 1)
        for(l in 1:n){
          Y[l, j] = finv(Fh[l, j, m])
        }
        J = J * fh[, j, m]/stats::dnorm(Y[, j])
      }

      w = tau[, m]/sum(tau[, m])
      ycenter = Y - matrix(rep(1, n), nrow = n) %*% (t(w) %*% Y)
      sigma[, , m] = t(ycenter) %*% (ycenter * (w %*% matrix(rep(1, d), nrow = 1)))
      f[, m] = dmvnorm(Y, sigma = as.matrix(sigma[, , m])) * J
    }

    pi = matrix(apply(tau, 2, sum)/n, nrow = 1)
    loglik = sum(log(f %*% t(pi)))
  }

  parameters = list(loglik = loglik, pi = pi, f = f, sigma = sigma)#, fh = fh, Fh = Fh)
  parameters
}


#-------------------------------------------------------------------------------
# Internal functions
#-------------------------------------------------------------------------------
lcmle <- function(x, w) {
  # Computes logMLE of a log-concave density using ICMA for f=exp(log) piecewise
  # linear, based on data x with weights w.
  # Merges data that are tied or too close (< std(x)/1000).
  # ICMA works on a cone of increasing arguments, so work with s(n:-1:1) and
  # undo in calls to loglikelihood, grad loglikelihood. Return grad(n:-1:1) in latter.

  # Sort data and take care of ties and data that are too close:
  x = as.matrix(x)
  n = max(dim(x))
  x = t(x)
  w = t(w)
  w = w / sum(w)
  mu = sum(w * x)
  sigma = sqrt(sum(w * (x - mu)^2))
  eps = sigma / 1000  # eps/1000 is merging threshold
  xsort = sort(x, index.return = TRUE)$x
  idx = sort(x, index.return = TRUE)$ix
  wsort = w[idx]
  xx = xsort[1]
  ww = wsort[1]
  nn = 1

  for (i in 2:n) {
    if (xsort[i] - xx[nn] < eps) {
      ww[nn] = ww[nn] + wsort[i]
    } else {
      xx = cbind(xx, xsort[i])
      ww = cbind(ww, wsort[i])
      nn = nn + 1
    }
  }

  ww = ww / sum(ww)

  # Fit normal to initialize:
  xx1 = xx[1] - sigma / 10  # xx1 is a dummy variable
  xx = cbind(xx1, xx)
  d = min(-10, -((xx1 - mu) / sigma)^2)  # log f at dummy xx(1) = xx1
  ww = cbind(1, ww)  # ww(1) is a dummy weight
  cw = cumsum(ww[seq(nn + 1, 2, -1)])
  cw = cw[seq(nn, 1, -1)] * diff(as.numeric(xx))
  swork = 1:nn  # Construct starting values for slopes:
  swork[1] = (log(stats::dnorm(xx[2], mu, sigma)) - d) / (xx[2] - xx[1])

  for (i in 2:nn) {
    swork[i] = (log(stats::dnorm(xx[i + 1], mu, sigma) / stats::dnorm(xx[i], mu, sigma))) / (xx[i + 1] - xx[i])
  }

  # Start ICMA:
  repeatICMA = 1

  while (repeatICMA > 0) {  # Is the dummy d = log f(xx1) not small enough?
    d = 4 * d
    swork[1] = swork[1] - 3/4 * d / (xx[2] - xx[1])  # Adjust d and swork(1)
    swork = swork[seq(nn, 1, -1)]  # Working value is isotonic
    swork = ICMA(xx, d, swork, cw)  # Compute MLE
    swork = swork[seq(nn, 1, -1)]  # Working value swork is isotonic

    if (swork[1] > 1.1 * swork[2]) {
      repeatICMA = 0  # d is small enough
    }
  }

  # Evaluate logMLE at xx:
  logf = matrix(rep(0, n), nrow = 1)
  logf[1] = d

  for (i in 2:(nn + 1)) {
    logf[i] = logf[i - 1] + swork[i - 1] * (xx[i] - xx[i - 1])
  }

  logf = logf[2:(nn + 1)]  # Ignore dummy variable xx1
  xx = xx[2:(nn + 1)]

  # Evaluate logMLE at xsort:
  logfsort = matrix(rep(0, n), nrow = 1)
  logfsort[1] = logf[1]
  xxidx = 1

  for (i in 2:n) {
    if (xsort[i] - xx[xxidx] < eps) {
      logfsort[i] = logf[xxidx]
    } else {
      xxidx = xxidx + 1
      logfsort[i] = logf[xxidx]
    }
  }

  # Evaluate logMLE at x:
  logf = matrix(rep(0, n), nrow = 1)
  logf[idx] = logfsort  # Undo the sorting
  logf
}

ICMA <- function(x, d, s, cw) {
  # Modified ICMA using Hessian weights and further modifications:
  # Also terminates if the gain in phi is < eta, or if more than 500 iterations;
  # Bounds weights below at 10^-3 to avoid problems when using w^(-1);
  # Performs at most 6 bisections for the line search.

  eta <- 0.00001  # Accuracy parameter
  eps <- 0.01     # Line search parameter
  x = as.matrix(x)
  n = max(dim(x)) - 1

  # s: starting value
  grad = gradphi(x, d, s, cw)
  w = mmax(0.001, grgrphi(x, d, s))    # Second derivatives as weights

  t1 = abs(sum(s * grad))
  t2 = abs(sum(grad))
  t3 = min(cumsum(grad[seq(n, 1, -1)]))
  ctr = 0
  gain = 1

  while ((t1 > eta || t2 > eta || t3 < eta) && ctr < 500 && gain > eta) {
    oldphi = phi(x, d, s, cw)
    ytil = MINLOWSET(c(s - (w^(-1)) * grad, w))

    if (phi(x, d, ytil, cw) < phi(x, d, s, cw) + eps * sum(grad * (ytil - s))) {
      s = ytil
    } else {
      lam = 1
      ss = 0.5
      z = ytil
      ctr2 = 0
      t4 = (phi(x, d, z, cw) < phi(x, d, s, cw) + (1 - eps) * sum(grad * (z - s)))
      t5 = (phi(x, d, z, cw) > phi(x, d, s, cw) + eps * sum(grad * (z - s)))

      while ((t4 > 0.5 || t5 > 0.5) && ctr2 < 6) {
        if (t4 > 0.5) {
          lam = lam + ss
        } else {
          lam = lam - ss
        }

        z = s + lam * (ytil - s)
        ss = ss / 2
        t4 = (phi(x, d, z, cw) < phi(x, d, s, cw) + (1 - eps) * sum(grad * (z - s)))
        t5 = (phi(x, d, z, cw) > phi(x, d, s, cw) + eps * sum(grad * (z - s)))
        ctr2 = ctr2 + 1
      }

      s = z
    }

    grad = gradphi(x, d, s, cw)
    w = mmax(0.001, grgrphi(x, d, s))
    t1 = abs(sum(s * grad))
    t2 = abs(sum(grad))
    t3 = min(cumsum(grad[seq(n, 1, -1)]))
    ctr = ctr + 1
    gain = oldphi - phi(x, d, s, cw)
  }

  y = s
  if (ctr > 499) {
    warning("Number of iterations in ICMA exceeded")
  }

  y
}

gradphi <- function(x, d, s, cw) {
  # Computes the gradient of phi at s.
  # NOTE: Switches the argument s to s(n:-1:1), consequently switches the result(n:-1:1).

  x = as.matrix(x)
  n = max(dim(x)) - 1
  s = s[seq(n, 1, -1)]
  a = matrix(rep(0, n + 1), nrow = 1)
  y = matrix(rep(0, n), nrow = 1)
  x1sq = x[1]^2

  for (i in 2:(n + 1)) {
    a[i] = a[i - 1] + s[i - 1] * (x[i] - x[i - 1])
  }
  t = (x[n + 1] - x[n]) * exp(d + a[n + 1])
  y[n] = 0.5 * t * (x[n + 1] - x[n])

  for (k in seq(n - 1, 2, -1)) {
    t = t + (x[k + 2] - x[k]) * exp(d + a[k + 1])
    y[k] = 0.5 * t * (x[k + 1] - x[k])
  }

  t = (x[3] - x[2]) * exp(d + a[2])
  y[1] = 0.5 * t * (x[2] - x[1])
  y = y - cw
  y = y[seq(n, 1, -1)]

  y
}

mmax <- function(a, b) {
  # Find the element-wise maximum between vectors a and b

  n = length(b)
  c = numeric()

  for (i in 1:n) {
    if (a > b[i]) {
      c[i] = a
    } else {
      c[i] = b[i]
    }
  }

  return(c)
}

grgrphi <- function(x, d, s) {
  # Computes the diagonal of the second derivative of phi at s.
  # NOTE: Switches the argument s to s(n:-1:1), consequently switches the result(n:-1:1)

  x = as.matrix(x)
  n = max(dim(x)) - 1
  s = s[seq(n, 1, -1)]
  a = matrix(rep(0, n + 1), nrow = 1)
  y = matrix(rep(0, n), nrow = 1)
  x1sq = x[1] * x[1]

  for (i in 2:(n + 1)) {
    a[i] = a[i - 1] + s[i - 1] * (x[i] - x[i - 1])
  }
  t = (x[n + 1] - x[n]) * exp(d + a[n + 1])
  y[n] = 0.5 * t * (x[n + 1] - x[n])^2

  for (k in seq(n - 1, 2, -1)) {
    t = t + (x[k + 2] - x[k]) * exp(d + a[k + 1])
    y[k] = 0.5 * t * (x[k + 1] - x[k])^2
  }

  t = (x[3] - x[2]) * exp(d + a[2])
  y[1] = 0.5 * t * (x[2] - x[1])^2
  y = y[seq(n, 1, -1)]

  y
}

phi <- function(x, d, s, cw) {
  # Computes -(likelihood - integrated likelihood) for log-concave + ct^2,
  # linearized, for f = exp(log) is piecewise linear.
  # NOTE: Switches argument s to s(n:-1:1)

  x = as.matrix(x)
  n = max(dim(x)) - 1
  s = s[seq(n, 1, -1)]
  a = matrix(rep(0, n + 1), nrow = 1)
  x1sq = x[1]^2
  a[2] = s[1] * (x[2] - x[1])
  y = (x[3] - x[2]) * exp(d + a[2])

  for (i in 3:n) {
    a[i] = a[i - 1] + s[i - 1] * (x[i] - x[i - 1])
    y = y + (x[i + 1] - x[i - 1]) * exp(d + a[i])
  }

  a[n + 1] = a[n] + s[n] * (x[n + 1] - x[n])
  y = y + (x[n + 1] - x[n]) * exp(d + a[n + 1])
  y = 0.5 * y - sum(s * cw)

  y
}

MINLOWSET <- function(gw) {
  # Minimum lower sets algorithm, computes isotonic regression of g w.r.t. weights w.
  # i.e., slope of greatest convex minorant (alternative to PAVA)

  n = length(gw) / 2
  g = gw[1:n]
  w = gw[(n + 1):(2 * n)]
  curr = 1

  while (curr < n + 0.5) {
    h = cumsum(g[curr:n] * w[curr:n]) / cumsum(w[curr:n])
    a = min(h[seq(n - curr + 1, 1, -1)])
    ind = which.min(h[seq(n - curr + 1, 1, -1)])
    ind = n + 1 - ind
    g[curr:ind] = seq(a, a, ind - curr + 1)
    curr = ind + 1
  }

  y = g
  y
}

#' Inverse fcdf
#'
#' Calculate cdf for normal distribution
#'
#' @param x observation we want to calculate the normal cdf for.
#'
#' @return pnorm value
#'
#' @importFrom GoFKernel inverse
#' @noRd
fcdf = function(x) {
  y = stats::pnorm(x)
  y
}
finv <- GoFKernel::inverse(fcdf)
