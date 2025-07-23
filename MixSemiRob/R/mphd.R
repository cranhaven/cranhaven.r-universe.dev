#' Semiparametric Mixture Model by Minimizing Profile Hellinger Distance
#'
#' `mixMPHD' provides an efficient and robust estimation of a mixture of unknown
#' location-shifted symmetric distributions using a semiparamatric method (Wu et al., 2017).
#' As of version 1.1.0, 'mixMPHD' supports a two-component model, which is defined as
#' \deqn{h(x;\boldsymbol{\theta},f) = \pi f(x-\mu_1)+(1-\pi)f(x-\mu_2),}
#' where \eqn{\boldsymbol{\theta}=(\pi,\mu_1,\mu_2)^{\top}} is the parameter to estimate,
#' \eqn{f} is an unknown density function that is symmetric at zero.
#' The parameters are estimated by minimizing the profile Hellinger distance (MPHD)
#' between the parametric model and a non-parametric density estimate.
#'
#' @usage
#' mixMPHD(x, sigma.known = NULL, ini = NULL)
#'
#' @param x a vector of observations.
#' @param sigma.known standard deviation of one component (if known). Default is NULL.
#' @param ini initial values for the parameters. Default is NULL, which obtains the initial values
#'   using the \code{\link{mixOnekn}} function. It can be a list with the form of \code{list(mu, pi, sigma)}, where
#'   \code{mu} is a vector of component means,
#'   \code{pi} is a vector of component mixing proportions,
#'   \code{sigma} is a vector of component standard deviations.
#'
#' @return A list containing the following elements:
#'   \item{lik}{final likelihood.}
#'   \item{pi}{estimated mixing proportion.}
#'   \item{sigma}{estimated component standard deviation. Only returned when \code{sigma.known} is not provided.}
#'   \item{mu}{estimated component mean.}
#'   \item{run}{total number of iterations after convergence.}
#'
#' @seealso \code{\link{mixOnekn}} for initial value calculation.
#'
#' @references
#'   Wu, J., Yao, W., and Xiang, S. (2017). Computation of an efficient and robust estimator
#'   in a semiparametric mixture model. Journal of Statistical Computation and Simulation, 87(11), 2128-2137.
#'
#' @importFrom stats nlminb
#'
#' @export
#' @examples
#' # Model: X ~ 0.3*N(0, 1) + 0.7*N(3, 1)
#' set.seed(4)
#' n = 100
#' p = 0.3
#' n1 = rbinom(1, n, p)
#' sigma1 = 1
#' sigma2 = 1
#' x1 = rnorm(n1, mean = 0, sd = sigma1)
#' x2 = rnorm(n - n1, mean = 3, sd = sigma2)
#' x = c(x1, x2)
#' ini = mixOnekn(x, sigma1)
#' mixMPHDest = mixMPHD(x, sigma1, ini = ini)
mixMPHD <- function(x, sigma.known = NULL, ini = NULL){

  true = NULL
  if(is.null(sigma.known)){ #true = c(p,sigma,mu); #SK---9/3/2023???
    out = mhdem1_x(x, ini, true)
  } else { #true = c(p,mu);
    out = mhde2m1_x(x, sigma.known, ini, true)
  }

  return(out)
}

#' Two-component Normal Mixture Estimation with One Known Component
#'
#' `mixOnekn' is used for the estimation of the following two-component mixture model:
#' \deqn{h(x;\boldsymbol{\theta},f) = \pi f(x-\mu_1)+(1-\pi)f(x-\mu_2),}
#' where \eqn{\boldsymbol{\theta}=(\pi,\mu_1,\mu_2)^{\top}} is the parameter to estimate,
#' \eqn{f} is an unknown density function that is symmetric at zero.
#' The parameters are estimated by assuming \eqn{f} is the normal density and the
#' first component has a mean of 0.
#' This function can be used to obtain initial values for the \code{\link{mixMPHD}} function.
#'
#' @usage
#' mixOnekn(x, sigma.known = NULL)
#'
#' @param x a vector of observations.
#' @param sigma.known standard deviation of the first component (if known). Default is NULL,
#'   which calculates the component standard deviations using the given observations \code{x}.
#'
#' @return A list containing the following elements:
#'   \item{mu}{estimated 2 component means, where the first mean is 0.}
#'   \item{sigma}{estimated 2 component standard deviations.}
#'   \item{pi}{estimated 2 mixing proportions.}
#'   \item{lik}{final likelihood.}
#'
#' @seealso \code{\link{mixMPHD}}
#'
#' @export
#' @examples
#' # see examples for the `mixMPHD' function.
mixOnekn <- function(x, sigma.known = NULL){

  sig <- sigma.known

  n = length(x)
  s2 = stats::var(x) * (n - 1)/n
  sx = sort(x)
  numini = 10
  mugrid = seq(from = mean(x), to = sx[round(n * 0.95)], length.out = 10)

  l = numeric()
  mu = matrix(rep(0, 2 * numini), nrow = numini)
  sigma = mu
  prop = mu

  # run mixonecompkn based on ten intial values
  if(is.null(sig)){

    for(i in 1:numini){
      muini = c(0, mugrid[1])
      ind1 = which(x^2 < (x - muini[2])^2)
      ind2 = which(x^2 > (x - muini[2])^2)
      sigmaini = c(stats::sd(x[ind1]), stats::sd(x[ind2]))
      propini = c(length(ind1), length(ind2))/n
      temp = mixonecompkn(x, muini, sigmaini, propini)
      l[i] = temp$lh
      mu[i, ] = temp$mu
      sigma[i, ] = temp$sigma
      prop[i, ] = temp$pi
    }
  } else {

    for(i in 1:numini){
      muini = c(0, mugrid[1])
      ind1 = which(x^2 < (x - muini[2])^2)
      ind2 = which(x^2 > (x - muini[2])^2)
      sigmaini = c(sig, stats::sd(x[ind2]))
      propini = c(length(ind1), length(ind2))/n
      temp = mixonecompkn(x, muini, sigmaini, propini, sig)
      l[i] = temp$lh
      mu[i, ] = temp$mu
      sigma[i, ] = temp$sigma
      prop[i,] = temp$pi
    }
  }

  # choose the converged value which has the largest likelihood
  #y = sort((-l),index.return = T)$x;
  id = sort((- l), index.return = TRUE)$ix
  id = id[1]
  mu = mu[id, ]
  prop = prop[id, ]
  sigma = sigma[id, ]
  #a = sort((mu),index.return = T)$x;
  b = sort((mu), index.return = TRUE)$ix

  out = list(mu = mu[b], sigma = sigma[b], pi = prop[b], lik = l[id])
  return(out)
}


#-------------------------------------------------------------------------------
# Internal functions
#-------------------------------------------------------------------------------
# Reliable and extremely fast kernel density estimator for one-dimensional data
# Gaussian kernel is assumed and the bandwidth is chosen automatically
# dct1d;fixed_point
# Kernel Density Estimation Bandwidth Calculation
kdebw <- function(y, n = NULL, MIN = NULL, MAX = NULL) {
  if (is.null(n)) {
    n = 2^12
  }
  n = 2^ceiling(log2(n))

  if (is.null(MIN) || is.null(MAX)) {
    minimum = min(y)
    maximum = max(y)
    Range = maximum - minimum
    MIN = minimum - Range / 10
    MAX = maximum + Range / 10
  }

  # Set up the grid over which the density estimate is computed
  R = MAX - MIN
  dx = R / (n - 1)
  xmesh = MIN + seq(from = 0, to = R, by = dx)
  N = length(y)

  # Bin the data uniformly using the grid defined above
  initial_data = graphics::hist(y, xmesh, plot = FALSE)$counts / N
  a = dct1d(initial_data)  # Discrete cosine transform of initial data
  I = as.matrix(c(1:(n - 1))^2)
  a2 = as.matrix((a[2:length(a)] / 2)^2)

  # Find the bandwidth using optimization
  #ft1=function(t){
  #  y=fixed_point(abs(t),I,a2,N)-abs(t)
  #}
  #t_star=abs(stats::uniroot(ft1,c(-0.1,1)))
  ft2 <- function(t) {
    y = abs(fixed_point(t, I, a2, N) - t)
  }
  #if(is.null(t_star) || is.infinite(t_star)){
  t_star = stats::optimize(ft2, c(0, 0.1))$minimum
  #}

  bandwidth = sqrt(t_star) * R

  return(bandwidth)
}

# Fixed Point Calculation
fixed_point <- function(t, I, a2, N) {
  functional <- function(df, s) {
    K0 = prod(seq(1, 2 * s - 1, 2)) / sqrt(2 * pi)
    const = (1 + (1/2)^(s + 1/2)) / 3
    t = (2 * const * K0 / N / df)^(2 / (3 + 2 * s))
    f = 2 * pi^(2 * s) * sum(I^s * a2 * exp(-I * pi^2 * t))
    return(f)
  }

  f = numeric()
  f[5] = 2 * pi^10 * sum(I^5 * a2 * exp(-I * pi^2 * t))
  for (s in seq(4, 2, -1)) {
    f[s] = functional(f[s + 1], s)
  }
  time = (2 * N * sqrt(pi) * f[2])^(-2 / 5)
  out = (t - time) / time

  return(out)
}

# 1D Discrete Cosine Transform
dct1d <- function(data) {
  data = c(data, 0)
  n = length(data)
  weight = c(1, 2 * exp(-1i * seq(1, (n - 1), 1) * pi / (2 * n)))
  data = c(data[seq(1, n, 2)], data[seq(n, 2, -2)])
  out = Re(weight * stats::fft(data))

  return(out)
}

# Interpolate the Response Curve at xgrid
interpcut <- function(x, y, xgrid) {
  a = sort(x, index.return = TRUE)$x
  b = sort(x, index.return = TRUE)$ix
  x = a
  y = y[b]
  c = which(duplicated(x) == FALSE)
  x = x[c]
  y = y[c]
  my = 0 # If xgrid is out of bound, we set them equal to 0.
  i = which(xgrid > max(x))
  j = which(xgrid < min(x))
  out = stats::approx(x, y, xgrid)$y
  out[i] = my
  out[j] = my
  out = pmax(0, out)

  return(out)
}

# One-Component Kernel Density Estimation
mixonecompkn <- function(x, mu, sigma, prop, sig = NULL) {
  trimprop = 0.05  # The proportion of samples being trimmed.
  n = length(x)
  acc = 10^(-5) * n
  oldx = x
  n1 = ceiling(n * (1 - trimprop))
  mu = c(0, 3)
  sigma = c(1, 1)
  prop = c(0.5, 0.5)

  if (is.null(sig)) {
    stop = 0
    run = 0
    dif = 10
    difml = 10
    ml = 10^8

    # Use EM algorithm to calculate the MLE
    while (stop == 0) {
      denm = t(cbind(as.matrix(stats::dnorm(oldx, mu[1], sigma[1])), as.matrix(stats::dnorm(oldx, mu[2], sigma[2]))))
      f = apply(prop * denm, 2, sum)
      #a = sort((-f),index.return = T)$x
      b = sort((-f), index.return = TRUE)$ix
      x = oldx[b[1:n1]]
      x = matrix(x, nrow = 1)
      run = run + 1
      denm = denm[, b[1:n1]]
      f = f[b[1:n1]]
      # lh[run] = sum(log(f));# use lh to record the sequence of lh
      lh = c(lh, sum(log(f))) # updated oct23 2022 by Xin Shen to make lh a vector instead of ts to fix a bug

      if (run > 2) {
        denom = lh[run - 1] - lh[run - 2]
        if (denom == 0) {
          stop = 1
        } else {
          c = (lh[run] - lh[run - 1]) / denom # convergence rate of Em algorithm
          if (c > 0 && c < 1) {
            preml = ml
            ml = lh[run - 1] + (lh[run] - lh[run - 1]) / (1 - c) # use Aitken acceleration to predict the maximum likelihood value
            dif = ml - lh[run]
            difml = abs(preml - ml)
          }
        }
      }

      # Algorithm stopped when the direction derivative was smaller than acc and
      # the difference of the predicted maximum likelihood value and the current likelihood value is small than 0.005
      if (dif < 0.001 && difml < 0.0001 && run > 20 || run > 200 || is.nan(min(mu)) == TRUE) {
        stop = 1
      } else {
        # E-step, classification probability of x from each component
        p = matrix(rep(prop, n1), ncol = n1) * denm / rbind(f, f)
        sp1 = sum(p[1,])
        # M-step
        mu = c(0, x %*% p[2, ] / sum(p[2, ]))
        prop = apply(p, 1, sum) / n1
        sigma = sqrt(apply((rbind(x, x) - matrix(rep(mu, n1), nrow = 2))^2 * p, 1, sum) / apply(p, 1, sum))
        sigma = pmax(0.1 * max(sigma), sigma)
      }
    }

    #a = sort((mu),index.return = T)$x
    b = sort((mu), index.return = TRUE)$ix
    out = list(mu = mu[b], sigma = sigma[b], pi = prop[b], lh = lh[run])

  } else {
    stop = 0
    run = 0
    dif = 10
    difml = 10
    ml = 10^8

    while (stop == 0) {
      denm = t(cbind(as.matrix(stats::dnorm(oldx, mu[1], sigma[1])), as.matrix(stats::dnorm(oldx, mu[2], sigma[2]))))
      f = apply(prop * denm, 2, sum)
      #a = sort((-f),index.return = T)$x
      b = sort((-f), index.return = TRUE)$ix
      x = oldx[b[1:n1]]
      x = matrix(x, nrow = 1)
      run = run + 1
      denm = denm[, b[1:n1]]
      f = f[b[1:n1]]
      # lh[run] = sum(log(f));#use lh to record the sequence of lh
      lh = c(lh, sum(log(f))) # updated oct23 2022 by Xin Shen to make lh a vector instead of ts to fix a bug

      if (run > 2) {
        denom = lh[run - 1] - lh[run - 2]
        if (denom == 0) {
          stop = 1
        } else {
          c = (lh[run] - lh[run - 1]) / denom
          if (c > 0 && c < 1) {
            preml = ml
            ml = lh[run - 1] + (lh[run] - lh[run - 1]) / (1 - c) # use Aitken acceleration to predict the maximum likelihood value
            dif = ml - lh[run]
            difml = abs(preml - ml)
          }
        }
      }

      # Algorithm stopped when the direction derivative was smaller than acc and
      # the difference of the predicted maximum likelihood value and the current likelihood value is small than 0.005
      if (dif < 0.001 && difml < 0.0001 && run > 20 || run > 200 || is.nan(min(mu)) == TRUE) {
        stop = 1
      } else {
        # E-step, classification probability of x from each component
        p = matrix(rep(prop, n1), ncol = n1) * denm / rbind(f, f)
        # M-step
        mu = c(0, x %*% p[2, ] / sum(p[2, ]))
        prop = apply(p, 1, sum) / n1
        sigma = c(sig, pmax(0.1 * sig, sqrt((x - mu[2])^2 %*% p[2, ] / sum(p[2, ]))))
      }
    }

    #a = sort((mu),index.return = T)$x
    b = sort((mu), index.return = TRUE)$ix
    out = list(mu = mu[b], sigma = sigma[b], pi = prop[b], lh = lh[run])
  }

  return(out)
}

# uses Em algorithm to estimate k components norm mixture model with equal unknown variance
# we can also get p the classification probability matrix
# rsample;mmax
mixnveq <- function(x, k = NULL, ini = NULL) {
  if (is.null(k)) {
    k = 2
  }

  x = as.vector(x)
  n = length(x)
  acc = 10^(-5) * n
  a = ceiling(n/2)
  s2 = stats::var(x) * (n - 1) / n
  sigma = matrix(rep(NaN, 10), nrow = 5)
  sx = sort(x)
  lowvar = ((range(x)[2] - range(x)[1]) / 38)^2

  if (k < 2) {
    warning("The number of components (second input) is less than 2")
  } else if (k == 2) {
    # Choose five initial values
    prop = 1/2 * matrix(rep(1, 10), nrow = 5)  # Set initial value of pi to 1/2 for each component
    mu = rbind(
      rsample(x, 2),
      rsample(x, 2),
      cbind(min(x), max(x)),
      cbind(mean(sx[1:a]), mean(sx[(a+1):n])),
      cbind(mean(x) - 0.5 * stats::sd(x), mean(x) + 0.5 * stats::sd(x))
    )
    sigma[(1:2), ] = matrix(rep(sqrt(stats::runif(2, 0, 1) * s2), 2), ncol = 2)
    sigma[(3:5), ] = matrix(
      rep(sqrt(mmax(lowvar, s2 - diag(stats::var(t(mu[(3:5), ])))/k)), 2),
      ncol = 2
    )

    if (is.null(ini) == FALSE) {
      mu = rbind(mu, ini$mu)
      sigma = rbind(sigma, ini$sigma)
      prop = rbind(prop, ini$pi)
    }

    numini = dim(mu)[1]

    # Run EM algorithm for a while for the five initial values
    l = rep(NaN, numini)
    for (i in 1:numini) {
      stop = 0
      run = 0
      l2 = -10^8
      while (stop == 0) {
        run = run + 1
        denm = rbind(
          stats::dnorm(x, mu[i, 1], sigma[i, 1]),
          stats::dnorm(x, mu[i, 2], sigma[i, 2])
        )
        f = prop[i,] %*% denm
        l1 = l2
        l2 = sum(log(f))
        dif = l2 - l1

        if (dif < 10^(-5) * n && run > 30 || dif < 10^(-8) * n) {
          stop = 1
        } else {
          p = matrix(rep(prop[i,], n), ncol = n) * denm / rbind(f, f)  # E step, classification probability of x from each component
          mu[i, ] = x %*% t(p) / apply(p, 1, sum)  # M step for mu
          prop[i, ] = apply(p, 1, sum) / n  # M step for prop
          sigma[i, ] = sqrt(
            sum(sum(p * (rbind(x, x) - matrix(rep(mu[i,], n), ncol = n))^2)) / n
          ) * rep(1, 2)  # M step for sigma
        }
      }
      l[i] = l2
    }

    # Choose the initial value of the 4, which has the largest likelihood
    I = sort(l, index.return = TRUE)$ix
    id = I[numini]
    mu = mu[id, ]
    prop = prop[id, ]
    sigma = sigma[id, ]
    stop = 0
    run = 0
    dif = 10
    difml = 10
    ml = 10^8

    # Use EM algorithm to calculate the MLE
    while (stop == 0) {
      denm = rbind(
        stats::dnorm(x, mu[1], sigma[1]),
        stats::dnorm(x, mu[2], sigma[2])
      )
      f = prop %*% denm
      dd = max(apply((denm - rbind(f, f))/rbind(f, f), 1, sum))
      run = run + 1
      lh[run] = sum(log(f))  # Use lh to record the sequence of lh

      if (run > 2) {
        denom = lh[run - 1] - lh[run - 2]
        if (denom == 0) {
          stop = 1
        } else {
          c = (lh[run] - lh[run - 1]) / denom  # Convergence rate of EM algorithm
          if (c > 0 && c < 1) {
            preml = ml
            ml = lh[run - 1] + (lh[run] - lh[run - 1]) / (1 - c)  # Use Aitken acceleration to predict the maximum likelihood value
            dif = ml - lh[run]
            difml = abs(preml - ml)
          }
        }
      }

      # Algorithm stopped when the direction derivative was smaller than acc and the difference of the predicted maximum likelihood value and the current likelihood value is smaller than 0.005
      if (dif < 0.001 && difml < 0.0001 && dd < acc) {
        stop = 1
      } else {
        p = matrix(rep(prop, n), ncol = n) * denm / rbind(f, f)  # E-step, classification probability of x from each component
        mu = x %*% t(p) / apply(p, 1, sum)  # M step for mu
        prop = apply(p, 1, sum) / n  # M step for pi
        sigma = sqrt(
          sum(sum(p * (t(matrix(rep(x, n), ncol = n)) - matrix(rep(mu, n), ncol = n))^2))
        ) * rep(1, 2)  # M step for sigma
      }
    }

    out = list(mu = mu, sigma = sigma, pi = prop, lh = lh[run], p = p)

  } else {  # The situation when k > 2
    mu = matrix(rep(NaN, 4 * k), nrow = 4)
    sigma = matrix(rep(NaN, 4 * k), nrow = 4)
    prop = 1/k * matrix(rep(1, 4 * k), nrow = 4)
    mu[1,] = rsample(x, k)
    mu[2,] = rsample(x, k)
    mu[(3:4), 1] = rbind(sx[1], mean(sx[1:ceiling(n/k)]))
    for (i in 1:(k - 1)) {
      mu[(3:4), (i + 1)] = rbind(
        sx[ceiling(i * n / (k - 1))],
        mean(sx[(ceiling(i * n / k) + 1):ceiling((i + 1) * n / k)])
      )
    }
    sigma[(1:2), ] = matrix(rep(sqrt(stats::runif(2, 0, 1) * s2), k), ncol = k)
    sigma[3, ] = sqrt(
      mmax(lowvar, s2 - stats::var(mu[3, ]) * (k - 1) / k)
    ) * rep(1, k)
    sigma[4, ] = sqrt(
      mmax(lowvar, s2 - stats::var(mu[4, ]) * (k - 1) / k)
    ) * rep(1, k)

    if (is.null(ini) == FALSE) {
      mu = rbind(mu, ini$mu)
      sigma = rbind(sigma, ini$sigma)
      prop = rbind(prop, ini$pi)
    }

    numini = dim(mu)[1]

    # Run EM algorithm for a while for the five initial values
    l = rep(NaN, numini)
    for (i in 1:numini) {
      stop = 0
      run = 0
      l2 = -10^8
      while (stop == 0) {
        run = run + 1
        denm = matrix(rep(NaN, n * k), nrow = k)
        for (j in 1:k) {
          denm[j, ] = stats::dnorm(x, mu[i, j], sigma[i, j])
        }
        f = prop[i,] %*% denm
        l1 = l2
        l2 = sum(log(f))
        dif = l2 - l1

        if (dif < 10^(-5) * n && run > 30 || dif < 10^(-8) * n) {
          stop = 1
        } else {
          p = matrix(rep(prop[i,], n), ncol = n) * denm / t(matrix(rep(f, k), ncol = k))
          mu[i, ] = x %*% t(p) / apply(p, 1, sum)
          prop[i, ] = apply(p, 1, sum) / n
          sigma[i, ] = sqrt(sum(sum(p * (t(matrix(rep(x, k), ncol = k)) - matrix(rep(mu[i,], n), ncol = n))^2))) * rep(1, k)
        }
      }
      l[i] = l2
    }

    # Choose the initial value of the five which has the largest likelihood
    I = sort(l, index.return = TRUE)$ix
    id = I[numini]
    mu = mu[id, ]
    prop = prop[id, ]
    sigma = sigma[id, ]
    stop = 0
    run = 0
    dif = 10
    difml = 10
    ml = 10^8

    # Use EM algorithm to calculate the MLE
    while (stop == 0) {
      for (j in 1:k) {
        denm[j,] = stats::dnorm(x, mu[j], sigma[j])
      }
      f = prop %*% denm
      dd = max(apply((denm - t(matrix(rep(f, k), ncol = k)))/t(matrix(rep(f, k), ncol = k)), 1, sum))
      run = run + 1
      lh[run] = sum(log(f))  # Use lh to record the sequence of lh

      if (run > 2) {
        c = (lh[run] - lh[run - 1]) / (lh[run - 1] - lh[run - 2])  # Convergence rate of EM algorithm
        if (c > 0 && c < 1) {
          preml = ml
          ml = lh[run - 1] + (lh[run] - lh[run - 1]) / (1 - c)  # Use Aitken acceleration to predict the maximum likelihood value
          dif = ml - lh[run]
          difml = abs(preml - ml)
        }
      }

      # Algorithm stopped when the direction derivative was smaller than acc and the difference of the predicted maximum likelihood value and the current likelihood value is smaller than 0.005
      if (dif < 0.001 && difml < 0.0001 && dd < acc) {
        stop = 1
      } else {
        p = matrix(rep(prop, n), ncol = n) * denm / t(matrix(rep(f, k), ncol = k))
        mu = x %*% t(p) / apply(p, 1, sum)
        prop = apply(p, 1, sum) / n
        sigma = sqrt(
          sum(
            sum(
              p * (
                t(matrix(rep(x, k), ncol = k)) - matrix(rep(mu, n), ncol = n)
              )^2
            )
          )
        ) * rep(1, k)
      }
    }

    out = list(mu = mu, sigma = sigma, pi = prop, lh = lh[run], p = p)
  }

  return(out)
}

#true = c(p,mu);
mhde2m1_x <- function(x, sigma, ini = NULL, true = NULL) {
  stopiter <- 30
  k <- 2
  n <- length(x)
  x <- matrix(x, nrow = n)
  h <- kdebw(x, 2^14)

  if (is.null(ini)) {
    ini <- mixnveq(x, k)
  }

  if (ini$mu[2] > ini$mu[1]) {
    prop <- ini$pi[1]
    mu <- ini$mu[2]
  } else {
    prop <- ini$pi[2]
    mu <- ini$mu[1]
  }
  est <- c(prop, mu)

  xgridmin <- min(x) - 5 * h
  xgridmax <- max(x) + 5 * h
  lxgrid <- 100
  xgrid <- seq(from = xgridmin, to = xgridmax, length.out = lxgrid)
  hspace <- (xgridmax - xgridmin) / lxgrid
  acc <- 10^(-5) / hspace

  # Nonparametric estimator
  deng <- function(t) {
    y <- apply(exp(-(matrix(rep(x, length(t)), nrow = n) - t(matrix(rep(t, n), ncol = n)))^2 / 2 / h^2), 2, mean) / h / sqrt(2 * pi)
  }
  dengx <- deng(xgrid)^(1/2)

  # Calculate the MHDE using temp
  dif <- acc + 1
  numiter <- 0
  fval <- 10^10

  while (dif > acc && numiter < stopiter) {
    numiter <- numiter + 1
    pfval <- fval

    denf1 <- function(t) {
      y <- stats::dnorm(t, 0, sigma)
    }

    # Find alpha and M by iteration
    difa <- 1
    step <- 0
    a <- 1
    while (difa > 10^(-3) && step < 20) {
      prea <- a
      step <- step + 1

      mfun <- function(t) {
        y <- a * deng(t) > prop * denf1(t)
      }
      temp <- function(t) {
        y <- denf1(t) * mfun(t)
      }
      temp1 <- function(t) {
        y <- deng(t) * mfun(t)
      }

      a <- min((prop * stats::integrate(temp, xgridmin, xgridmax)$value + 1 - prop) / max(stats::integrate(temp1, xgridmin, xgridmax)$value, 1 - prop), 1)
      difa <- abs(prea - a)
    }
    if (a > 0.99) {
      a <- 1
    }

    # Given theta update f
    denfmu <- (mmax(0, a * deng(xgrid) - prop * denf1(xgrid)) + mmax(0, a * deng(2 * mu - xgrid) - prop * denf1(2 * mu - xgrid))) / 2 / (1 - prop)
    preest <- est
    # Given f, update theta
    denf <- function(t) {
      y <- interpcut(c(xgrid - mu, mu - xgrid), c(denfmu, denfmu), t)
    }
    obj <- function(t) {
      y <- sum(((min(0.95, max(t[1], 0.05)) * stats::dnorm(xgrid, 0, sigma) + (1 - min(0.95, max(t[1], 0.05))) * denf(xgrid - min(max(x), max(0, t[2]))))^(1/2) - dengx)^2)
    }

    fmin <- nlminb(start = preest, objective = obj)
    est <- fmin$par
    fval <- fmin$objective
    est <- c(min(est[1], 0.95), min(est[2], max(x)))
    est <- c(max(est[1], 0.05), max(est[2], 0))
    dif <- pfval - fval
    if (dif < 0) {
      est <- preest
      fval <- pfval
    }

    prop <- est[1]
    mu <- est[2]
  }

  res <- list(fval = fval, pi = prop, mu = mu, numiter = numiter)
  #sk-- 09/02/2023
  out <- list(lik = fval, pi = prop, mu = mu, run = numiter)
  #out <- list(fval = fval, pi = prop, mu = mu, numiter = numiter)

  if (is.null(true)) {
    out <- out
  } else { # Calculate the MHDE using true
    dif <- acc + 1
    numiter <- 0
    fval <- 10^10
    est <- true
    prop <- true[1]
    mu <- true[2]

    while (dif > acc && numiter < stopiter) {
      numiter <- numiter + 1
      pfval <- fval

      denf1 <- function(t) {
        y <- stats::dnorm(t, 0, sigma)
      }

      # Find alpha and M by iteration
      difa <- 1
      step <- 0
      a <- 1
      while (difa > 10^(-3) && step < 20) {
        prea <- a
        step <- step + 1

        mfun <- function(t) {
          y <- a * deng(t) > prop * denf1(t)
        }
        temp <- function(t) {
          y <- denf1(t) * mfun(t)
        }
        temp1 <- function(t) {
          y <- deng(t) * mfun(t)
        }

        a <- min((prop * stats::integrate(temp, xgridmin, xgridmax)$value + 1 - prop) / max(stats::integrate(temp1, xgridmin, xgridmax)$value, 1 - prop), 1)
        difa <- abs(prea - a)
      }
      if (a > 0.99) {
        a <- 1
      }

      # Given theta update f
      denfmu <- (mmax(0, a * deng(xgrid) - prop * denf1(xgrid)) + mmax(0, a * deng(2 * mu - xgrid) - prop * denf1(2 * mu - xgrid))) / 2 / (1 - prop)
      preest <- est
      # Given f, update theta
      denf <- function(t) {
        y <- interpcut(c(xgrid - mu, mu - xgrid), c(denfmu, denfmu), t)
      }
      obj <- function(t) {
        y <- sum(((min(0.95, max(t[1], 0.05)) * stats::rnorm(xgrid, 0, sigma) + (1 - min(0.95, max(t[1], 0.05))) * denf(xgrid - min(max(x), max(0, t[2]))))^(1/2) - dengx)^2)
      }

      fmin <- nlminb(start = preest, objective = obj)
      est <- fmin$par
      fval <- fmin$objective
      est <- c(min(est[1], 0.95), min(est[2], max(x)))
      est <- c(max(est[1], 0.05), max(est[2], 0))
      dif <- pfval - fval
      if (dif < 0) {
        est <- preest
        fval <- pfval
      }

      prop <- est[1]
      mu <- est[2]
    }

    if (res$fval < fval) {
      #sk-- 09/02/2023
      out <- list(lik = res$fval, pi = res$pi, mu = res$mu, run = res$numiter)
      #out <- list(pi = res$pi, mu = res$mu, numiter = res$numiter, initialtrue = 0)
    } else {
      #sk-- 09/02/2023
      out <- list(lik = fval, pi = prop, mu = mu, run = numiter)
      #out <- list(pi = prop, mu = mu, numiter = numiter, initialtrue = 1)
    }
  }

  return(out)
}

#true = c(p,sigma,mu);
mhdem1_x <- function(x, ini = NULL, true = NULL) {
  stopiter <- 30
  k <- 2
  n <- length(x)
  x <- matrix(x, nrow = n)
  h <- kdebw(x, 2^14)

  if (is.null(ini)) {
    ini <- mixnveq(x, k)
  }

  if (ini$mu[2] > ini$mu[1]) {
    prop <- ini$pi[1]
    mu <- ini$mu[2]
    sigma <- ini$sigma[1]
  } else {
    prop <- ini$pi[2]
    mu <- ini$mu[1]
    sigma <- ini$sigma[2]
  }
  est <- c(prop, sigma, mu)

  xgridmin <- min(x) - 5 * h
  xgridmax <- max(x) + 5 * h
  lxgrid <- 100
  xgrid <- seq(from = xgridmin, to = xgridmax, length.out = lxgrid)
  hspace <- (xgridmax - xgridmin) / lxgrid
  acc <- 10^(-5) / hspace

  # Nonparametric estimator
  deng <- function(t) {
    y <- apply(exp(-(matrix(rep(x, length(t)), nrow = n) - t(matrix(rep(t, n), ncol = n)))^2 / 2 / h^2), 2, mean) / h / sqrt(2 * pi)
  }
  dengx <- deng(xgrid)^(1/2)

  dif <- acc + 1
  numiter <- 0
  fval <- 10^10

  while (dif > acc && numiter < stopiter) {
    numiter <- numiter + 1
    pfval <- fval

    denf1 <- function(t) {
      y <- stats::dnorm(t, 0, sigma)
    }

    # Find alpha and M by iteration
    difa <- 1
    step <- 0
    a <- 1
    while (difa > 10^(-3) && step < 20) {
      prea <- a
      step <- step + 1

      mfun <- function(t) {
        y <- a * deng(t) > prop * denf1(t)
      }
      temp <- function(t) {
        y <- denf1(t) * mfun(t)
      }
      temp1 <- function(t) {
        y <- deng(t) * mfun(t)
      }

      a <- min((prop * stats::integrate(temp, xgridmin, xgridmax)$value + 1 - prop) / max(stats::integrate(temp1, xgridmin, xgridmax)$value, 1 - prop), 1)
      difa <- abs(prea - a)
    }
    if (a > 0.99) {
      a <- 1
    }

    # Given theta update f
    denfmu <- (mmax(0, a * deng(xgrid) - prop * denf1(xgrid)) + mmax(0, a * deng(2 * mu - xgrid) - prop * denf1(2 * mu - xgrid))) / 2 / (1 - prop)
    preest <- est
    # Given f, update theta
    denf <- function(t) {
      y <- interpcut(c(xgrid - mu, mu - xgrid), c(denfmu, denfmu), t)
    }
    obj <- function(t) {
      y <- sum(((min(0.95, max(t[1], 0.05)) * stats::dnorm(xgrid, 0, min(stats::sd(x), max(0.1 * stats::sd(x), t[2]))) + (1 - min(0.95, max(t[1], 0.05))) * denf(xgrid - min(max(x), max(0, t[3]))))^(1/2) - dengx)^2)
    }

    fmin <- stats::nlminb(start = preest, objective = obj)
    est <- fmin$par
    fval <- fmin$objective
    est <- c(min(est[1], 0.95), min(est[2], stats::sd(x)), min(est[3], max(x)))
    est <- c(max(est[1], 0.05), max(est[2], stats::sd(x) * 0.1), max(est[3], 0))
    dif <- pfval - fval
    if (dif < 0) {
      est <- preest
      fval <- pfval
    }

    prop <- est[1]
    sigma <- est[2]
    mu <- est[3]
  }


  res <- list(fval = fval, pi = prop, sigma = sigma, mu = mu, numiter = numiter)
  #sk-- 09/02/2023
  out <- list(lik = fval, pi = prop, sigma = sigma, mu = mu, run = numiter)
  #out <- list(fval = fval, pi = prop, sigma = sigma, mu = mu, numiter = numiter)

  if (is.null(true)) {
    out <- out
  } else {
    dif <- acc + 1
    numiter <- 0
    fval <- 10^10
    est <- true
    prop <- true[1]
    sigma <- true[2]
    mu <- true[3]

    while (dif > acc && numiter < stopiter) {
      numiter <- numiter + 1
      pfval <- fval

      denf1 <- function(t) {
        y <- stats::dnorm(t, 0, sigma)
      }

      # Find alpha and M by iteration
      difa <- 1
      step <- 0
      a <- 1
      while (difa > 10^(-3) && step < 20) {
        prea <- a
        step <- step + 1

        mfun <- function(t) {
          y <- a * deng(t) > prop * denf1(t)
        }
        temp <- function(t) {
          y <- denf1(t) * mfun(t)
        }
        temp1 <- function(t) {
          y <- deng(t) * mfun(t)
        }

        a <- min((prop * stats::integrate(temp, xgridmin, xgridmax)$value + 1 - prop) / max(stats::integrate(temp1, xgridmin, xgridmax)$value, 1 - prop), 1)
        difa <- abs(prea - a)
      }
      if (a > 0.99) {
        a <- 1
      }

      # Given theta update f
      denfmu <- (mmax(0, a * deng(xgrid) - prop * denf1(xgrid)) + mmax(0, a * deng(2 * mu - xgrid) - prop * denf1(2 * mu - xgrid))) / 2 / (1 - prop)
      preest <- est
      # Given f, update theta
      denf <- function(t) {
        y <- interpcut(c(xgrid - mu, mu - xgrid), c(denfmu, denfmu), t)
      }
      obj <- function(t) {
        y <- sum(((min(0.95, max(t[1], 0.05)) * stats::dnorm(xgrid, 0, min(stats::sd(x), max(0.1 * stats::sd(x), t[2]))) + (1 - min(0.95, max(t[1], 0.05))) * denf(xgrid - min(max(x), max(0, t[3]))))^(1/2) - dengx)^2)
      }

      fmin <- stats::nlminb(start = preest, objective = obj)
      est <- fmin$par
      fval <- fmin$objective
      est <- c(min(est[1], 0.95), min(est[2], stats::sd(x)), min(est[3], max(x)))
      est <- c(max(est[1], 0.05), max(est[2], stats::sd(x) * 0.1), max(est[3], 0))
      dif <- pfval - fval
      if (dif < 0) {
        est <- preest
        fval <- pfval
      }

      prop <- est[1]
      sigma <- est[2]
      mu <- est[3]
    }

    if (res$fval < fval) {
      #SK-- 09/02/2023
      out <- list(lik = res$fval, pi = res$pi, sigma = res$sigma, mu = res$mu, run = res$numiter)
      #out <- list(pi = res$pi, sigma = res$sigma, mu = res$mu, numiter = res$numiter, initialtrue = 0)
    } else {
      #SK-- 09/02/2023
      out <- list(lik = fval, pi = prop, sigma = sigma, mu = mu, run = numiter)
      #out <- list(pi = prop, sigma = sigma, mu = mu, numiter = numiter, initialtrue = 1)
    }
  }

  return(out)
}


#-------------------------------------------------------------------------------
# Keep unused functions (SK, 08/31/2023)
#-------------------------------------------------------------------------------

# Sigma known
# ini: the initial values for mu and prop
# h:bandwidth. h=1.06*n^(-1/5) by default
# kdebw;mixnveq;mmax;interpcut
mhde2m1 <- function(x, p, sigma, mu, ini = NULL) {
  stopiter = 30
  k = 2
  n = length(x)
  x = matrix(x, nrow = n)
  h = kdebw(x, 2^14)
  true = c(p, mu)

  if (is.null(ini)) {
    ini = mixnveq(x, k)
  }

  if (ini$mu[2] > ini$mu[1]) {
    prop = ini$pi[1]
    mu = ini$mu[2]
  } else {
    prop = ini$pi[2]
    mu = ini$mu[1]
  }
  est = c(prop, mu)

  xgridmin = min(x) - 5 * h
  xgridmax = max(x) + 5 * h
  lxgrid = 100
  xgrid = seq(from = xgridmin, to = xgridmax, length.out = lxgrid)
  hspace = (xgridmax - xgridmin) / lxgrid
  acc = 10^(-5) / hspace

  # Nonparametric estimator
  deng <- function(t) {
    y = apply(exp(-(matrix(rep(x, length(t)), nrow = n) - t(matrix(rep(t, n), ncol = n)))^2 / 2 / h^2), 2, mean) / h / sqrt(2 * pi)
  }
  dengx = deng(xgrid)^(1/2)  # (hn_hat)^(1/2)

  # Calculate the MHDE using temp
  dif = acc + 1
  numiter = 0
  fval = 10^10

  while (dif > acc && numiter < stopiter) {
    numiter = numiter + 1
    pfval = fval

    denf1 <- function(t) {
      y = stats::dnorm(t, 0, sigma)
    }

    # Find alpha and M by iteration
    difa = 1
    step = 0
    a = 1
    while (difa > 10^(-3) && step < 20) {
      prea = a
      step = step + 1

      mfun <- function(t) {
        y = a * deng(t) > prop * denf1(t)
      }
      temp <- function(t) {
        y = denf1(t) * mfun(t)
      }
      temp1 <- function(t) {
        y = deng(t) * mfun(t)
      }
      # Integrate
      a = min((prop * stats::integrate(temp, xgridmin, xgridmax)$value + 1 - prop) / max(stats::integrate(temp1, xgridmin, xgridmax)$value, 1 - prop), 1)
      difa = abs(prea - a)
    }
    if (a > 0.99) {
      a = 1
    }

    # Given theta, update f
    denfmu = (mmax(0, a * deng(xgrid) - prop * denf1(xgrid)) + mmax(0, a * deng(2 * mu - xgrid) - prop * denf1(2 * mu - xgrid))) / 2 / (1 - prop)
    preest = est
    # Given f, update theta
    denf <- function(t) {
      y = interpcut(c(xgrid - mu, mu - xgrid), c(denfmu, denfmu), t)
    }
    obj <- function(t) {
      y = sum(((min(0.95, max(t[1], 0.05)) * stats::dnorm(xgrid, 0, sigma) + (1 - min(0.95, max(t[1], 0.05))) * denf(xgrid - min(max(x), max(0, t[2]))))^(1/2) - dengx)^2)
    }

    fmin = nlminb(start = preest, objective = obj)
    est = fmin$par
    fval = fmin$objective
    est = c(min(est[1], 0.95), min(est[2], max(x)))
    est = c(max(est[1], 0.05), max(est[2], 0))
    dif = pfval - fval
    if (dif < 0) {
      est = preest
      fval = pfval
    }

    prop = est[1]
    mu = est[2]
  }

  res = list(fval = fval, pi = prop, mu = mu, numiter = numiter)

  # Calculate the MHDE using true
  dif = acc + 1
  numiter = 0
  fval = 10^10
  est = true
  prop = true[1]
  mu = true[2]

  while (dif > acc && numiter < stopiter) {
    numiter = numiter + 1
    pfval = fval

    denf1 <- function(t) {
      y = stats::dnorm(t, 0, sigma)
    }

    # Find alpha and M by iteration
    difa = 1
    step = 0
    a = 1
    while (difa > 10^(-3) && step < 20) {
      prea = a
      step = step + 1

      mfun <- function(t) {
        y = a * deng(t) > prop * denf1(t)
      }
      temp <- function(t) {
        y = denf1(t) * mfun(t)
      }
      temp1 <- function(t) {
        y = deng(t) * mfun(t)
      }

      a = min((prop * stats::integrate(temp, xgridmin, xgridmax)$value + 1 - prop) / max(stats::integrate(temp1, xgridmin, xgridmax)$value, 1 - prop), 1)
      difa = abs(prea - a)
    }
    if (a > 0.99) {
      a = 1
    }

    # Given theta, update f
    denfmu = (mmax(0, a * deng(xgrid) - prop * denf1(xgrid)) + mmax(0, a * deng(2 * mu - xgrid) - prop * denf1(2 * mu - xgrid))) / 2 / (1 - prop)
    preest = est
    # Given f, update theta
    denf <- function(t) {
      y = interpcut(c(xgrid - mu, mu - xgrid), c(denfmu, denfmu), t)
    }
    obj <- function(t) {
      y = sum(((min(0.95, max(t[1], 0.05)) * stats::dnorm(xgrid, 0, sigma) + (1 - min(0.95, max(t[1], 0.05))) * denf(xgrid - min(max(x), max(0, t[2]))))^(1/2) - dengx)^2)
    }

    fmin = nlminb(start = preest, objective = obj)
    est = fmin$par
    fval = fmin$objective
    est = c(min(est[1], 0.95), min(est[2], max(x)))
    est = c(max(est[1], 0.05), max(est[2], 0))
    dif = pfval - fval
    if (dif < 0) {
      est = preest
      fval = pfval
    }

    prop = est[1]
    mu = est[2]
  }

  if (res$fval < fval) {
    out = list(pi = res$pi, mu = res$mu, numiter = res$numiter, initialtrue = 0)
  } else {
    out = list(pi = prop, mu = mu, numiter = numiter, initialtrue = 1)
  }

  return(out)
}

# MHDE sigma unknown
# ini: the initial values for mu and prop.
# h:bandwidth. h=1.06*n^(-1/5) by default.
# kdebw;mixnveq;mmax;interpcut
mhdem1 <- function(x, p, sigma, mu, ini = NULL) {
  stopiter = 30
  k = 2
  n = length(x)
  x = matrix(x, nrow = n)
  h = kdebw(x, 2^14)
  true = c(p, sigma, mu)

  if (is.null(ini)) {
    ini = mixnveq(x, k)
  }

  if (ini$mu[2] > ini$mu[1]) {
    prop = ini$pi[1]
    mu = ini$mu[2]
    sigma = ini$sigma[1]
  } else {
    prop = ini$pi[2]
    mu = ini$mu[1]
    sigma = ini$sigma[2]
  }
  est = c(prop, sigma, mu)

  xgridmin = min(x) - 5 * h
  xgridmax = max(x) + 5 * h
  lxgrid = 100
  xgrid = seq(from = xgridmin, to = xgridmax, length.out = lxgrid)
  hspace = (xgridmax - xgridmin) / lxgrid
  acc = 10^(-5) / hspace

  # Nonparametric estimator
  deng <- function(t) {
    y = apply(exp(-(matrix(rep(x, length(t)), nrow = n) - t(matrix(rep(t, n), ncol = n)))^2 / 2 / h^2), 2, mean) / h / sqrt(2 * pi)
  }
  dengx = deng(xgrid)^(1/2)

  # Calculate the MHDE using temp
  dif = acc + 1
  numiter = 0
  fval = 10^10

  while (dif > acc && numiter < stopiter) {
    numiter = numiter + 1
    pfval = fval

    denf1 <- function(t) {
      y = stats::dnorm(t, 0, sigma)
    }

    # Find alpha and M by iteration
    difa = 1
    step = 0
    a = 1
    while (difa > 10^(-3) && step < 20) {
      prea = a
      step = step + 1

      mfun <- function(t) {
        y = a * deng(t) > prop * denf1(t)
      }
      temp <- function(t) {
        y = denf1(t) * mfun(t)
      }
      temp1 <- function(t) {
        y = deng(t) * mfun(t)
      }

      a = min((prop * stats::integrate(temp, xgridmin, xgridmax)$value + 1 - prop) / max(stats::integrate(temp1, xgridmin, xgridmax)$value, 1 - prop), 1)
      difa = abs(prea - a)
    }
    if (a > 0.99) {
      a = 1
    }

    # Given theta, update f
    denfmu = (mmax(0, a * deng(xgrid) - prop * denf1(xgrid)) + mmax(0, a * deng(2 * mu - xgrid) - prop * denf1(2 * mu - xgrid))) / 2 / (1 - prop)
    preest = est
    # Given f, update theta
    denf <- function(t) {
      y = interpcut(c(xgrid - mu, mu - xgrid), c(denfmu, denfmu), t)
    }
    obj <- function(t) {
      y = sum(((min(0.95, max(t[1], 0.05)) * stats::dnorm(xgrid, 0, min(stats::sd(x), max(0.1 * stats::sd(x), t[2]))) + (1 - min(0.95, max(t[1], 0.05))) * denf(xgrid - min(max(x), max(0, t[3]))))^(1/2) - dengx)^2)
    }

    fmin = nlminb(start = preest, objective = obj)
    est = fmin$par
    fval = fmin$objective
    est = c(min(est[1], 0.95), min(est[2], stats::sd(x)), min(est[3], max(x)))
    est = c(max(est[1], 0.05), max(est[2], stats::sd(x) * 0.1), max(est[3], 0))
    dif = pfval - fval
    if (dif < 0) {
      est = preest
      fval = pfval
    }

    prop = est[1]
    sigma = est[2]
    mu = est[3]
  }

  res = list(fval = fval, pi = prop, sigma = sigma, mu = mu, numiter = numiter)

  # Calculate the MHDE using true
  dif = acc + 1
  numiter = 0
  fval = 10^10
  est = true
  prop = true[1]
  sigma = true[2]
  mu = true[3]

  while (dif > acc && numiter < stopiter) {
    numiter = numiter + 1
    pfval = fval

    denf1 <- function(t) {
      y = stats::dnorm(t, 0, sigma)
    }

    # Find alpha and M by iteration
    difa = 1
    step = 0
    a = 1
    while (difa > 10^(-3) && step < 20) {
      prea = a
      step = step + 1

      mfun <- function(t) {
        y = a * deng(t) > prop * denf1(t)
      }
      temp <- function(t) {
        y = denf1(t) * mfun(t)
      }
      temp1 <- function(t) {
        y = deng(t) * mfun(t)
      }

      a = min((prop * stats::integrate(temp, xgridmin, xgridmax)$value + 1 - prop) / max(stats::integrate(temp1, xgridmin, xgridmax)$value, 1 - prop), 1)
      difa = abs(prea - a)
    }
    if (a > 0.99) {
      a = 1
    }

    # Given theta, update f
    denfmu = (mmax(0, a * deng(xgrid) - prop * denf1(xgrid)) + mmax(0, a * deng(2 * mu - xgrid) - prop * denf1(2 * mu - xgrid))) / 2 / (1 - prop)
    preest = est
    # Given f, update theta
    denf <- function(t) {
      y = interpcut(c(xgrid - mu, mu - xgrid), c(denfmu, denfmu), t)
    }
    obj <- function(t) {
      y = sum(((min(0.95, max(t[1], 0.05)) * stats::dnorm(xgrid, 0, min(stats::sd(x), max(0.1 * stats::sd(x), t[2]))) + (1 - min(0.95, max(t[1], 0.05))) * denf(xgrid - min(max(x), max(0, t[3]))))^(1/2) - dengx)^2)
    }

    fmin = nlminb(start = preest, objective = obj)
    est = fmin$par
    fval = fmin$objective
    est = c(min(est[1], 0.95), min(est[2], stats::sd(x)), min(est[3], max(x)))
    est = c(max(est[1], 0.05), max(est[2], stats::sd(x) * 0.1), max(est[3], 0))
    dif = pfval - fval
    if (dif < 0) {
      est = preest
      fval = pfval
    }

    prop = est[1]
    sigma = est[2]
    mu = est[3]
  }

  if (res$fval < fval) {
    out = list(pi = res$pi, sigma = res$sigma, mu = res$mu, numiter = res$numiter, initialtrue = 0)
  } else {
    out = list(pi = prop, sigma = sigma, mu = mu, numiter = numiter, initialtrue = 1)
  }

  return(out)
}
