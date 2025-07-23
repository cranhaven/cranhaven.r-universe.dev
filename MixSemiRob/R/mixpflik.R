#' Profile Likelihood Method for Normal Mixture with Unequal Variance
#'
#' `mixpf' is used to estimate the following \eqn{C}-component univariate normal mixture model,
#' using the profile likelihood method (Yao, 2010), with the assumption that the ratio of
#' the smallest variance to the largest variance is \eqn{k}:
#' \deqn{f(x;\boldsymbol{\theta}) = \sum_{j=1}^C\pi_j\phi(x;\mu_j,\sigma_j^2),}
#' where \eqn{\boldsymbol{\theta}=(\pi_1,\mu_1,\sigma_1,..,\pi_{C},\mu_C,\sigma_C)^{\top}}
#' is the parameter to estimate, \eqn{\phi(\cdot;\mu,\sigma^2)} is the normal density with a
#' mean of \eqn{\mu} and a standard deviation of \eqn{\sigma}, and \eqn{\pi}'s are mixing
#' proportions that sum up to 1.
#' Once the results are obtained, one can also find the maximum likelihood estimate (MLE) of \eqn{k} by
#' plotting the likelihood vs. \eqn{k} for different \eqn{k} values and finding the maximum
#' interior mode in the likelihood. See examples below.
#'
#' @usage
#' mixpf(x, k = 0.5, C = 2, nstart = 20)
#'
#' @param x a vector of observations.
#' @param k ratio of the smallest variance to the largest variance. Default is 0.5.
#' @param C number of mixture components. Default is 2.
#' @param nstart number of initializations to try. Default is 20.
#'
#' @return A list containing the following elements:
#'   \item{mu}{vector of estimated component means.}
#'   \item{sigma}{vector of estimated component standard deviations.}
#'   \item{pi}{vector of estimated mixing proportions.}
#'   \item{lik}{final likelihood.}
#'
#' @references
#'   Yao, W. (2010). A profile likelihood method for normal mixture with unequal variance.
#'   Journal of Statistical Planning and Inference, 140(7), 2089-2098.
#'
#' @export
#' @examples
#' set.seed(4)
#' n = 100
#' u = runif(n, 0, 1)
#' x2 = (u <= 0.3) * rnorm(n, 0, 0.5) + (u > 0.3) * rnorm(n, 1.5, 1)
#'
#' # please set ngrid to 200 to get a smooth likelihood curve
#' ngrid = 5
#' grid = seq(from = 0.01, to = 1, length = ngrid)
#' likelihood = numeric()
#' for(i in 1:ngrid){
#'   k = grid[i]
#'   est = mixpf(x2, k)
#'   lh = est$lik
#'   likelihood[i] = lh
#' }
#'
#' # visualize likelihood to find the best k
#' plot(grid, likelihood, type = "l", lty = 2, xlab = "k", ylab = "profile log-likelihood")
mixpf <- function(x, k = 0.5, C = 2, nstart = 20) {
  m = C                    # Initialize variables and parameters
  x = as.matrix(x)

  if (dim(x)[1] > dim(x)[2]) {  # Transpose x if necessary for consistency
    x = t(x)
  }

  numini = nstart

  n = length(x)             # Calculate the length of x
  sx = sort(x)              # Sort x
  vk = k                    # Copy k for iteration
  acc = 10^(-6) * n         # Accuracy threshold
  pm = permfull(1:m)        # Permutation matrix
  lpm = max(dim(pm)[1], dim(pm)[2])  # Maximum dimension of the permutation matrix
  likelihood = numeric()    # Initialize the likelihood vector

  for (t in 1:length(vk)) {  # Iterate through the values of vk (possibly just one)
    k = vk[t]
    prop = 1/m * matrix(rep(1, lpm * numini * m), ncol = m)  # Initialize prop matrix
    sig = matrix(rep(0, lpm * numini * m), ncol = m)         # Initialize sig matrix
    mu = matrix(rep(0, numini * m), ncol = m)                # Initialize mu matrix

    for (i in 1:(numini - 1)) {   # Generate initial values for mu
      mu[i, ] = rsample(x, m)
    }

    mu[numini, ] = sx[round((1:m) / m * n + 0.0001)]  # Set the last row of mu
    # based on sorted x values

    for (i in 1:(lpm - 1)) {  # Expand mu matrix to cover all permutations
      mu = rbind(mu, mu[(1:numini), pm[i, ]])
    }

    sig[, (1:(m - 1))] = matrix(rep(sqrt(0.5 * stats::var(as.numeric(x))), lpm * numini * (m - 1)), ncol = m - 1)
    sig[, m] = sig[, 1] * k

    lh = numeric()  # Initialize likelihood vector

    for (i in 1:(lpm * numini)) {  # Loop over permutations and initializations
      stop = 0
      run = 0
      l2 = -10^8

      while (stop == 0) {  # EM Algorithm: Expectation Maximization
        run = run + 1
        denm = matrix(rep(0, n * m), nrow = m)

        for (j in 1:m) {
          denm[j, ] = stats::dnorm(x, mu[i, j], sig[i, j])
        }

        f = prop[i, ] %*% denm
        l1 = l2
        l2 = sum(log(f))
        dif = l2 - l1

        if (dif < 10^(-6) * n && run > 30 || dif < 10^(-8) * n || run > 1000) {
          stop = 1
        } else {
          p = matrix(rep(prop[i, ], n), ncol = n) * denm / t(matrix(rep(f, m), ncol = m))
          nc = apply(p, 1, sum)
          mu[i, ] = x %*% t(p) / nc
          prop[i, ] = nc / n
          s = numeric()

          for (j in 1:m) {
            s[j] = (x - mu[i, j])^2 %*% p[j, ]
          }

          b = sort(s / nc, index.return = TRUE)$ix
          ind = c(1, 1)
          q = 10^10

          if (s[b[1]] / nc[b[1]] < k^2 * s[b[m]] / nc[b[m]]) {
            for (l in 1:(m - 1)) {
              minn = m
              j = 1
              sigtemp = numeric()

              while (j < min(minn, m - l + 1)) {
                temp = (sum(s[b[1:l]]) + k^2 * sum(s[b[(m - j + 1):m]])) / (sum(nc[b[1:l]]) + sum(nc[b[(m - j + 1):m]]))

                if (temp < s[b[l + 1]] / nc[b[l + 1]] && temp > k^2 * s[b[m - j]] / nc[b[m - j]]) {
                  minn = min(minn, j)
                  sigtemp[b[1:l]] = matrix(rep(sqrt(temp), l), nrow = 1)

                  if ((l + 1) <= (m - j)) {
                    sigtemp[b[(l + 1):(m - j)]] = sqrt(s[b[(l + 1):(m - j)]] / nc[b[(l + 1):(m - j)]])
                  }

                  sigtemp[b[(m - j + 1):m]] = matrix(rep(sqrt(temp) / k, j), nrow = 1)
                  oldq = q
                  sigtemp = matrix(sigtemp, nrow = 1)
                  q = log(sigtemp) %*% nc + s %*% t(1 / (sigtemp^2)) / 2

                  if (q < oldq) {
                    ind = c(1, j)
                    break
                  } else {
                    j = j + 1
                  }
                }
              }
            }

            l = ind[1]
            j = ind[2]
            temp = (sum(s[b[1:l]]) + k^2 * sum(s[b[(m - j + 1):m]])) / (sum(nc[b[1:l]]) + sum(nc[b[(m - j + 1):m]]))
            sig[i, b[1:l]] = matrix(rep(sqrt(temp), l), nrow = 1)

            if ((l + 1) <= (m - j)) {
              sig[i, b[(l + 1):(m - j)]] = sqrt(s[b[(l + 1):(m - j)]] / nc[b[(l + 1):(m - j)]])
            }

            sig[i, b[(m - j + 1):m]] = matrix(rep(sqrt(temp) / k, j), nrow = 1)
          } else {
            ind = c(1, 2)
            q = 10^10

            for (l in 1:(m - 1)) {
              sigtemp = numeric()

              for (j in (l + 1):m) {
                temp = (s[b[l]] + k^2 * s[b[j]]) / (nc[b[l]] + nc[b[j]])

                if (temp < s[b[1]] / nc[b[1]] && temp > k^2 * s[b[m]] / nc[b[m]]) {
                  sigtemp[b[1:m]] = sqrt(s[b[1:m]] / nc[b[1:m]])
                  sigtemp[b[l]] = sqrt(temp)
                  sigtemp[b[j]] = sqrt(temp) / k
                  oldq = q
                  sigtemp = matrix(sigtemp, nrow = 1)
                  q = log(sigtemp) %*% nc + s %*% t(1 / (sigtemp^2)) / 2

                  if (q < oldq) {
                    ind = c(l, j)
                  }
                }
              }
            }

            l = ind[1]
            j = ind[2]
            temp = (s[b[l]] + k^2 * s[b[j]]) / (nc[b[l]] + nc[b[j]])
            sig[i, b[1:m]] = sqrt(s[b[1:m]] / nc[b[1:m]])
            sig[i, b[l]] = sqrt(temp)
            sig[i, b[j]] = sqrt(temp) / k
          }
        }
      }

      for (j in 1:m) {
        denm[j, ] = stats::dnorm(x, mu[i, j], sig[i, j])
      }

      f = prop[i, ] %*% denm
      lh[i] = sum(log(f))
    }

    I = sort(lh, index.return = TRUE)$ix
    id = I[lpm * numini]
    mu = mu[id, ]
    prop = prop[id, ]
    sig = sig[id, ]
    stop = 0
    numiter = 0
    dif = 10
    difml = 10
    ml = 10^8
    lh = numeric()

    while (stop == 0) {  # Iteratively update mu, prop, and sig
      for (j in 1:m) {
        denm[j, ] = stats::dnorm(x, mu[j], sig[j])
      }

      f = prop %*% denm
      numiter = numiter + 1
      lh[numiter] = sum(log(f))

      if (numiter > 2) {
        temp = lh[numiter - 1] - lh[numiter - 2]

        if (temp == 0) {
          dif = 0
          break
        } else {
          c = (lh[numiter] - lh[numiter - 1]) / temp

          if (c > 0 && c < 1) {
            preml = ml
            ml = lh[numiter - 1] + (lh[numiter] - lh[numiter - 1]) / (1 - c)
            dif = ml - lh[numiter]
            difml = abs(preml - ml)
          }
        }
      }

      if (dif < 0.001 && difml < 10^(-8) || numiter > 50000) {
        stop = 1
      } else {
        p = matrix(rep(prop, n), ncol = n) * denm / t(matrix(rep(f, m), ncol = m))
        nc = apply(p, 1, sum)
        mu = x %*% t(p) / nc
        prop = nc / n
        s = numeric()

        for (j in 1:m) {
          s[j] = (x - mu[j])^2 %*% matrix(p[j, ])
        }

        b = sort(s / nc, index.return = TRUE)$ix
        ind = c(1, 1)
        q = 10^10

        if (s[1] / nc[1] < k^2 * s[m] / nc[m]) {
          for (l in 1:(m - 1)) {
            minn = m
            j = 1
            sigtemp = numeric()

            while (j < min(minn, m - l + 1)) {
              temp = (sum(s[b[1:l]]) + k^2 * sum(s[b[(m - j + 1):m]])) / (sum(nc[b[1:l]]) + sum(nc[b[(m - j + 1):m]]))

              if (temp < s[b[l + 1]] / nc[b[l + 1]] && temp > k^2 * s[b[m - j]] / nc[b[m - j]]) {
                minn = min(minn, j)
                sigtemp[b[1:l]] = matrix(rep(sqrt(temp), l), nrow = 1)

                if ((l + 1) <= (m - j)) {
                  sigtemp[b[(l + 1):(m - j)]] = sqrt(s[b[(l + 1):(m - j)]] / nc[b[(l + 1):(m - j)]])
                }

                sigtemp[b[(m - j + 1):m]] = matrix(rep(sqrt(temp) / k, j), nrow = 1)
                oldq = q
                sigtemp = matrix(sigtemp, nrow = 1)
                q = log(sigtemp) %*% nc + s %*% t(1 / (sigtemp^2)) / 2

                if (q < oldq) {
                  ind = c(1, j)
                  break
                } else {
                  j = j + 1
                }
              }
            }
          }

          l = ind[1]
          j = ind[2]
          temp = (sum(s[b[1:l]]) + k^2 * sum(s[b[(m - j + 1):m]])) / (sum(nc[b[1:l]]) + sum(nc[b[(m - j + 1):m]]))
          sig[b[1:l]] = matrix(rep(sqrt(temp), l), nrow = 1)

          if ((l + 1) <= (m - j)) {
            sig[b[(l + 1):(m - j)]] = sqrt(s[b[(l + 1):(m - j)]] / nc[b[(l + 1):(m - j)]])
          }

          sig[b[(m - j + 1):m]] = matrix(rep(sqrt(temp) / k, j), nrow = 1)
        } else {
          ind = c(1, 2)
          q = 10^10

          for (l in 1:(m - 1)) {
            sigtemp = numeric()

            for (j in (l + 1):m) {
              temp = (s[b[l]] + k^2 * s[b[j]]) / (nc[b[l]] + nc[b[j]])

              if (temp < s[b[1]] / nc[b[1]] && temp > k^2 * s[b[m]] / nc[b[m]]) {
                sigtemp[b[1:m]] = sqrt(s[b[1:m]] / nc[b[1:m]])
                sigtemp[b[l]] = sqrt(temp)
                sigtemp[b[j]] = sqrt(temp) / k
                oldq = q
                sigtemp = matrix(sigtemp, nrow = 1)
                q = log(sigtemp) %*% nc + s %*% t(1 / (sigtemp^2)) / 2

                if (q < oldq) {
                  ind = c(l, j)
                }
              }
            }
          }

          l = ind[1]
          j = ind[2]
          temp = (s[b[l]] + k^2 * s[b[j]]) / (nc[b[l]] + nc[b[j]])
          sig[b[1:m]] = sqrt(s[b[1:m]] / nc[b[1:m]])
          sig[b[l]] = sqrt(temp)
          sig[b[j]] = sqrt(temp) / k
        }
      }
    }

    for (j in 1:m) {
      denm[j, ] = stats::dnorm(x, mu[j], sig[j])
    }

    f = prop %*% denm
    likelihood[t] = sum(log(f))
  }

  if (length(vk) == 1) {
    out = list(mu = as.vector(mu), sigma = sig, pi = prop, lik = likelihood)
  } else {
    out = list(lik = likelihood) #?? SK 9/4/2023
  }

  return(out)
}

#-------------------------------------------------------------------------------
# Internal functions
#-------------------------------------------------------------------------------
# Function to find all the permuted values of the vector x
permfull <- function(x) {
  lx = length(x)
  fact = 1:lx

  if (lx == 1) {
    out = x
  } else {
    n = gamma(lx)
    out = matrix(rep(0, lx * n * lx), ncol = lx)

    for (i in 1:lx) {
      # Recursive call to permfull for permutations
      temp = permfull(x[fact != i])
      temp = as.matrix(temp)

      # Populate the output matrix with permuted values
      out[((i - 1) * n + 1):(i * n), ] = cbind(x[i] * matrix(rep(1, n), nrow = n), temp)
    }
  }

  return(out)
}

