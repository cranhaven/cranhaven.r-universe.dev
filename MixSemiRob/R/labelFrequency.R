#' Complete Likelihood Frequency Method for Label Switching
#'
#' `complh' is used to address the label switching problem by maximizing
#' the complete likelihood (Yao, 2015). This method leverages information from the latent
#' component label, which is the label the user used to generate the sample.
#' The function supports both one-dimensional (with equal variances or unequal variances)
#' and multi-dimensional data (with equal variances).
#'
#' @usage
#' complh(est, lat)
#'
#' @param est a list with four elements representing the estimated mixture model,
#'   which can be obtained using the \code{\link{mixnorm}} function. When specified,
#'   it has the form of \code{list(mu, sigma, pi, p)}, where
#'   \code{mu} is a C by p matrix of estimated component means where p is the dimension of data
#'     and C is the number of mixture components,
#'   \code{sigma} is a p by p matrix of estimated common standard deviation for all components
#'     (when the data is multi-dimensional) or a C-dimensional vector of estimated component
#'     standard deviations (when the data is one-dimensional),
#'   \code{pi} is a C-dimensional vector of mixing proportions, and
#'   \code{p} is a C by n matrix of the classification probabilities,
#'   where the (i, j)th element corresponds to the probability of the jth observation
#'   belonging to the ith component.
#' @param lat a C by n zero-one matrix representing the latent component labels for all observations,
#'   where C is the number of components in the mixture model and n is the number of observations.
#'   If the (i, j)th cell is 1, it indicates that the jth observation belongs to the ith component.
#'
#' @return The estimation results adjusted to account for potential label switching problems are returned,
#'   as a list containing the following elements:
#'   \item{mu}{C by p matrix of estimated component means.}
#'   \item{sigma}{C-dimensional vector of estimated component standard deviations (for univariate data) or
#'     p by p matrix of estimated component variance (for multivariate data).}
#'   \item{pi}{C-dimensional vector of estimated mixing proportions.}
#'
#' @seealso \code{\link{distlat}}, \code{\link{mixnorm}}
#'
#' @references
#'   Yao, W. (2015). Label switching and its solutions for frequentist mixture models.
#'   Journal of Statistical Computation and Simulation, 85(5), 1000-1012.
#'
#' @importFrom mvtnorm rmvnorm dmvnorm
#' @export
#' @examples
#' #-----------------------------------------------------------------------------------------#
#' # Example 1: Two-component Univariate Normal Mixture
#' #-----------------------------------------------------------------------------------------#
#' # Simulate the data
#' set.seed(827)
#' n = 200
#' prop = 0.3
#' n1 = rbinom(1, n, prop)
#' mudif = 1.5
#' x1 = rnorm(n1, 0, 1)
#' x2 = rnorm(n - n1, mudif, 1)
#' x = c(x1, x2)
#' pm = c(2, 1, 3, 5, 4)
#'
#' # Use the `mixnorm' function to get the MLE and the estimated classification probabilities
#' out = mixnorm(x, 2)
#'
#' # Prepare latent component label
#' lat = rbind(rep(c(1, 0), times = c(n1, n - n1)),
#'             rep(c(0, 1), times = c(n1, n - n1)))
#'
#' # Fit the complh/distlat function
#' clhest = complh(out, lat)
#' clhest
#' # Result:
#' # mean of the first component: -0.1037359,
#' # mean of the second component: 1.6622397,
#' # sigma is 0.8137515 for both components, and
#' # the proportions for the two components are
#' # 0.3945660 and 0.6054340, respectively.
#' ditlatest = distlat(out, lat)
#'
#' #-----------------------------------------------------------------------------------------#
#' # Example 2: Two-component Multivariate Normal Mixture
#' #-----------------------------------------------------------------------------------------#
#' # Simulate the data
#' n = 400
#' prop = 0.3
#' n1 = rbinom(1, n, prop)
#' pi = c(prop, 1 - prop)
#' mu1 = 0.5
#' mu2 = 0.5
#' mu = matrix(c(0, mu1, 0, mu2), ncol = 2)
#' pm = c(2, 1, 4, 3, 6, 5)
#' sigma = diag(c(1, 1))
#' ini = list(sigma = sigma, mu = mu, pi = pi)
#' x1 = mvtnorm::rmvnorm(n1, c(0, 0), ini$sigma)
#' x2 = mvtnorm::rmvnorm(n - n1, c(mu1, mu2), ini$sigma)
#' x = rbind(x1, x2)
#'
#' # Use the `mixnorm' function to get the MLE and the estimated classification probabilities
#' out = mixnorm(x, 2)
#'
#' # Prepare latent component label
#' lat = rbind(rep(c(1, 0), times = c(n1, n - n1)),
#'             rep(c(0, 1), times = c(n1, n - n1)))
#'
#' # Fit the complh/distlat function
#' clhest = complh(out, lat)
#' distlatest = distlat(out, lat)
#'
#' #-----------------------------------------------------------------------------------------#
#' # Example 3: Three-component Multivariate Normal Mixture
#' #-----------------------------------------------------------------------------------------#
#' # Simulate the data
#' n = 100
#' pi = c(0.2, 0.3, 0.5)
#' ns = stats::rmultinom(1, n, pi)
#' n1 = ns[1]; n2 = ns[2]; n3 = ns[3]
#' mu1 = 1
#' mu2 = 1
#' mu = matrix(c(0, mu1, 2 * mu1, 0, mu2, 2 * mu2), ncol = 2)
#' sigma = diag(c(1, 1))
#' ini = list(sigma = sigma, mu = mu, pi = pi)
#' x1 = mvtnorm::rmvnorm(n1, c(0, 0), ini$sigma)
#' x2 = mvtnorm::rmvnorm(n2, c(mu1, mu2), ini$sigma)
#' x3 = mvtnorm::rmvnorm(n3, c(2 * mu1, 2 * mu2), ini$sigma)
#' x = rbind(x1, x2, x3)
#'
#' # Use the `mixnorm' function to get the MLE and the estimated classification probabilities
#' out = mixnorm(x, 3)
#'
#' # Prepare latent component label
#' lat = rbind(rep(c(1, 0), times = c(n1, n - n1)),
#'             rep(c(0, 1, 0), times = c(n1, n2, n3)),
#'             rep(c(0, 1), times = c(n - n3, n3)))
#'
#' # Fit the complh/distlat function
#' clhest = complh(out, lat)
#' distlatest = distlat(out, lat)
complh <- function(est, lat){

  #s = dim(est$mu)
  s = ncol(est$mu) #SK-- 9/4/2023
  m = length(est$pi) #SK-- 9/4/2023
  #m = length(est$mu) # the number of components
  p = est$p
  pm = permfull(1:m)
  numpm = max(dim(pm))

  #SK-- 9/4/2023
  if(is.vector(est$mu) | ncol(est$mu) == 1){
  #if(s[1] == 1){
    ## one dimensional data

    if(est$sigma[1] != est$sigma[2]){
      # unequal variance
      est = c(est$mu, est$sigma, est$pi)
      pm = cbind(pm, pm + m, pm + 2 * m)
      dist = numeric()
      for(j in 1:numpm){
        dist[j] = sum(log(p[pm[j, 1:m], ]) * lat)
      }
      ind = sort(dist, index.return = TRUE)$ix
      #if(ind[numpm] != 1){
      #  est = est[pm[ind[numpm], ]]
      #}
      out <- list(mu = est$mu[pm[ind[numpm], 1:m], ],
                  sigma = est$sigma[pm[ind[numpm], 1:m], ],
                  pi = est$pi[pm[ind[numpm], 1:m]])

    } else {
      # equal variance
      #est = c(est$mu, est$sigma[1], est$pi)
      pm = cbind(pm, matrix(rep(m + 1, numpm), ncol = 1), pm + m + 1)
      dist = numeric()
      for(j in 1:numpm){
        dist[j] = sum(log(p[pm[j, 1:m], ]) * lat)
      }
      ind = sort(dist, index.return = TRUE)$ix
      #if(ind[numpm] != 1){
      #  est = est[pm[ind[numpm], ]]
      #}
      out <- list(mu = est$mu[pm[ind[numpm], 1:m], ],
                  sigma = est$sigma,
                  pi = est$pi[pm[ind[numpm], 1:m]])
    }

  } else {
    ## high dimensional data with equal covariance
    #temp = t(est$mu[, 1])
    #for(i in 2:s){
    #  temp = cbind(temp, t(est$mu[, i]))
    #}
    #temp = c(temp,est$pi)
    #est = temp
    temp = pm
    for(i in 2:(s + 1)){
      temp = cbind(temp, pm + (i - 1) * m)
    }
    pm = temp
    dist = numeric()
    for(j in 1:numpm){
      dist[j] = sum(log(p[pm[j, 1:m], ]) * lat)
    }
    ind = sort(dist, index.return = TRUE)$ix
    #if(ind[numpm] != 1){
    #  est = est[pm[ind[numpm], ]]
    #}
    out <- list(mu = est$mu[pm[ind[numpm], 1:m], ],
                sigma = est$sigma,
                pi = est$pi[pm[ind[numpm], 1:m]])
  }

  return(out)
  #return(list(mu = est[1:m], sigma = est[(m + 1):(2 * m)], pi = est[(2 * m + 1):(3 * m)])) wrong
}

#' Euclidean Distance Based Labeling Method for Label Switching
#'
#' `distlat' is used to address the label switching problem by minimizing
#' the distance between the classification probabilities and the latent component label,
#' which is the label used by the user to generate the sample (Yao, 2015).
#' The function supports both one-dimensional (with equal variances or unequal variances)
#' and multi-dimensional data (with equal variances).
#'
#' @usage
#' distlat(est, lat)
#'
#' @param est a list with four elements representing the estimated mixture model,
#'   which can be obtained using the \code{\link{mixnorm}} function. When specified,
#'   it has the form of \code{list(mu, sigma, pi, p)}, where
#'   \code{mu} is a C by p matrix of estimated component means where p is the dimension of data
#'     and C is the number of mixture components,
#'   \code{sigma} is a p by p matrix of estimated common standard deviation for all components
#'     (when the data is multi-dimensional) or a C-dimensional vector of estimated component
#'     standard deviations (when the data is one-dimensional),
#'   \code{pi} is a C-dimensional vector of mixing proportions, and
#'   \code{p} is a C by n matrix of the classification probabilities,
#'   where the (i, j)th element corresponds to the probability of the jth observation
#'   belonging to the ith component.
#' @param lat a C by n zero-one matrix representing the latent component labels for all observations,
#'   where C is the number of components in the mixture model and n is the number of observations.
#'   If the (i, j)th cell is 1, it indicates that the jth observation belongs to the ith component.
#'
#' @return The estimation results adjusted to account for potential label switching problems are returned,
#'   as a list containing the following elements:
#'   \item{mu}{C by p matrix of estimated component means.}
#'   \item{sigma}{C-dimensional vector of estimated component standard deviations (for univariate data) or
#'     p by p matrix of estimated component variance (for multivariate data).}
#'   \item{pi}{C-dimensional vector of estimated mixing proportions.}
#'
#' @seealso \code{\link{complh}}, \code{\link{mixnorm}}
#'
#' @references
#'   Yao, W. (2015). Label switching and its solutions for frequentist mixture models.
#'   Journal of Statistical Computation and Simulation, 85(5), 1000-1012.
#'
#' @export
#' @examples
#' # See examples for the `complh' function.
distlat <- function(est, lat){

  s = dim(est$mu)
  m = length(est$pi) #SK-- 9/4/2023

  #SK-- 9/4/2023
  if(is.vector(est$mu) | ncol(est$mu) == 1){
    #if(s[1] == 1){
    ## one dimensional data
    #m = length(est$mu) # the number of components
    pm = permfull(1:m)
    numpm = max(dim(pm))
    p = est$p

    if(est$sigma[1] != est$sigma[2]){
      # unequal variance
      est = c(est$mu, est$sigma, est$pi)
      pm = cbind(pm, pm + m, pm + 2 * m)
      dist = numeric()
      for(j in 1:numpm){
        dist[j] = sum(log(p[pm[j, 1:m], ]) * lat)
      }
      ind = sort(dist, index.return = TRUE)$ix
      if(ind[numpm] != 1){
        est = est[pm[ind[numpm], ]]
      }

    } else {
      # equal variance
      est = c(est$mu, est$sigma[1], est$pi)
      pm = cbind(pm,matrix(rep(m + 1, numpm), ncol = 1), pm + m + 1)
      dist = numeric()
      for(j in 1:numpm){
        dist[j] = sum(log(p[pm[j, 1:m], ]) * lat)
      }
      ind = sort(dist, index.return = TRUE)$ix
      if(ind[numpm] != 1){
        est = est[pm[ind[numpm], ]]
      }
    }

  } else {
    ## high dimensional data with equal covariance
    #m = length(est$pi)
    p = est$p
    pm = permfull(1:m)
    numpm = max(dim(pm))
    s = s[2]
    temp = t(est$mu[, 1])
    for(i in 2:s){
      temp = cbind(temp,t(est$mu[, i]))
    }
    temp = c(temp, est$pi)
    est = temp
    temp = pm
    for(i in 2:(s + 1)){
      temp = cbind(temp, pm + (i - 1) * m)
    }
    pm = temp
    dist = numeric()
    for(j in 1:numpm){
      dist[j] = sum(log(p[pm[j, 1:m], ]) * lat)
    }
    ind = sort(dist, index.return = TRUE)$ix
    if(ind[numpm] != 1){
      est = est[pm[ind[numpm], ]]
    }
  }

  #SK-- 9/4/2023
  #out = list(est = est, obj = dist[ind[numpm]], listobj = dist)
  return(est)
  #return(list(mu = est[1:m], sigma = est[(m + 1):(2 * m)], pi = est[(2 * m + 1):(3 * m)])) wrong
}

#' Parameter Estimation for Uni- or Multivariate Normal Mixture Models
#'
#' `mixnorm' is used to estimate parameters of a normal mixture model with equal variance.
#' The function supports both one-dimensional and multi-dimensional data.
#'
#' @usage
#' mixnorm(x, C = 2, sigma.known = NULL, ini = NULL, tol = 1e-05)
#'
#' @param x an n by p matrix of observations where n is the number of observations and
#'   s is the dimension of data.
#' @param C number of mixture components. Default is 2.
#' @param sigma.known a vector or matrix of component standard deviations. Default is NULL, which means
#'   the standard deviations are unknown.
#' @param ini initial values for the parameters. Default is NULL, which randomly sets the initial values
#'   using the given observations. If specified, it can be a list with the form of \code{list(mu, pi, sigma)}, where
#'   \code{mu} is a vector of C component means,
#'   \code{pi} is a vector of C mixing proportions, and
#'   \code{sigma} is a vector of C component standard deviations (this element is only needed when \code{sigma.known} is not given).
#' @param tol stopping criteria for the algorithm. Default is 1e-05.
#'
#' @return A list containing the following elements:
#'   \item{mu}{estimated component means.}
#'   \item{sigma}{estimated component standard deviations. Only returned when \code{sigma.known} is not specified.}
#'   \item{pi}{estimated mixing proportions.}
#'   \item{p}{matrix containing estimated classification probabilities where the (i, j)th element is the probability of
#'   the jth observation belonging to the ith component.}
#'   \item{lik}{final likelihood.}
#'
#' @seealso \code{\link{complh}}, \code{\link{distlat}}
#'
#' @importFrom mvtnorm dmvnorm rmvnorm
#' @export
#' @examples
#' # See examples for the `complh' function.
mixnorm <- function(x, C = 2, sigma.known = NULL, ini = NULL, tol = 1e-05){

  acc <- tol

  k = C
  x = as.matrix(x)
  s = dim(x)

  if(min(s) > 1){
  #-----------------------------------------------------------------------------------------#
  # (i) Multi-dimensional data
  #-----------------------------------------------------------------------------------------#
    if(is.null(k)){
      k = 2
      equal = 1
    } else {
      k = as.matrix(k)
      temp = dim(k)
      if(max(temp) == 1){
        equal = 1
      } else {
        equal = 2
      }
    }
    main = mixmnorm(x, k, equal)

  }
  else {
  #-----------------------------------------------------------------------------------------#
  # (ii) One-dimensional data
  #-----------------------------------------------------------------------------------------#

    if(is.null(acc)){
      acc = 0.001
    }
    if(is.null(k)){
      k = 2
    }
    if(s[1] > 1){
      x = t(x)
    }

    if(length(k) > 1){
      #-----------------------------------------------------------------------------------------#
      # (ii-1) when the equal variance is known
      #-----------------------------------------------------------------------------------------#
      sigma = sigma.known
      #k = length(sigma)
      n = length(x)
      acc = 10^(-5) * n
      sx = sort(x)

      if(k == 2){
        #-----------------------------------------------------------------------------------------#
        # (ii-1-a) when C = 2
        #-----------------------------------------------------------------------------------------#
        prop = matrix(rep(1/2, 10 * 2), nrow = 10)
        stdmix = sqrt(max(10^(-4) * stats::var(x), stats::var(x) - mean(sigma^2)))
        mu = rbind(sort(rsample(x, 2)), sort(rsample(x, 2)), sort(rsample(x, 2)),
                   sort(rsample(x, 2)), sort(rsample(x, 2)), sort(rsample(x, 2)),
                   cbind(mean(x) - stdmix, mean(x) + stdmix),
                   cbind(min(x), max(x)),
                   cbind(mean(sx[1:round(n/2 + 0.0001)]), mean(sx[(round(n/2) + 1 + 0.0001):n])),
                   cbind(mean(x) - 0.5 * stats::sd(x), mean(x) + 0.5 * stats::sd(x)))
        if(!is.null(ini)){
          mu = rbind(mu, ini$mu)
          prop = rbind(prop, ini$pi)
        }

        numini = dim(mu)[1]
        l = rep(NaN, numini)
        for(i in 1:numini){
          stop = 0; run = 0; l2 = -10^8
          while(stop == 0){
            run = run + 1
            denm = rbind(stats::dnorm(x, mu[i, 1], sigma[1]), stats::dnorm(x, mu[i, 2], sigma[2]))
            f = prop[i, ] %*% denm
            l1 = l2; l2 = sum(log(f)); dif = l2-l1

            if(dif < 10^(-4) * n && run > 15 || dif < 10^(-8) * n || run > 1000){
              stop = 1
            } else {
              p = matrix(rep(prop[i, ], n), ncol = n) * denm/t(matrix(rep(f, 2), ncol = 2))
              mu[i, ] = x %*% t(p)/apply(p, 1, sum)
              prop[i, ] = apply(p, 1, sum)/n
            }
          }
          l[i] = l2
        }

        I = sort(l, index.return = TRUE)$ix
        id = I[numini]
        mu = mu[id, ]
        prop = prop[id, ]
        stop = 0; run = 0; dif = 10; difml = 10; ml = 10^8

        while(stop == 0){
          denm = rbind(stats::dnorm(x, mu[1], sigma[1]), stats::dnorm(x, mu[2], sigma[2]))
          f = prop %*% denm
          run = run + 1
          lh[run] = sum(log(f))

          if(run > 2){

            if(lh[run - 1] - lh[run - 2] == 0){
              stop = 1
            } else {
              c = (lh[run] - lh[run - 1])/(lh[run - 1] - lh[run - 2])
              if(c > 0 && c < 1){
                preml = ml
                ml = lh[run - 1] + (lh[run] - lh[run - 1])/(1 - c)
                dif = ml - lh[run]
                difml = abs(preml - ml)
              }
            }
          }

          if(dif < acc && difml < 10^(-4) || run > 50000){
            stop = 1
          } else {
            p = matrix(rep(prop[i, ], n), ncol = n) * denm/t(matrix(rep(f, 2), ncol = 2))
            mu[i, ] = x %*% t(p)/apply(p, 1, sum)
            prop[i, ] = apply(p, 1, sum)/n
          }
        }

        main = list(mu = matrix(mu, nrow = k), pi = prop, p = p, lik = lh[run]) #,inilh=l,iniid=id)

      }
      else {
        #-----------------------------------------------------------------------------------------#
        # (ii-1-a) when C > 2
        #-----------------------------------------------------------------------------------------#
        prop = matrix(rep(1/k, 10 * k), nrow = 10)
        mu = rbind(sort(rsample(x, k)), sort(rsample(x, k)), sort(rsample(x, k)), sort(rsample(x, k)),
                   sort(rsample(x, k)), sort(rsample(x, k)), sort(rsample(x, k)), sort(rsample(x, k)))
        mmu = rbind(sx[1], mean(sx[1:round(n/k + 0.0001)]))
        for(i in 1:(k - 1)){
          mmu = cbind(mmu,
                      rbind(sx[round(i * n/(k - 1) + 0.0001)],
                            mean(sx[(round(i * n/k) + 1 + 0.0001):round((i + 1) * n/k + 0.0001)])))
        }
        mu = rbind(mu, mmu)
        if(!is.null(ini)){
          mu = rbind(mu, ini$mu)
          prop = rbind(prop, ini$pi)
        }

        numini = dim(mu)[1]
        l = rep(NaN, numini)
        for(i in 1:numini){
          stop = 0; run=0; l2 = -10^8
          while(stop == 0){
            run = run + 1
            denm = matrix(rep(0, n * k), nrow = k)
            for(j in 1:k){
              denm[j, ] = stats::dnorm(x, mu[i, j], sigma[j])
            }
            f = prop[i, ] %*% denm
            l1 = l2; l2 = sum(log(f)); dif = l2 - l1

            if(dif < 10^(-4) * n && run > 15 || dif < 10^(-8) * n || run > 1000){
              stop = 1
            } else {
              p = matrix(rep(prop[i, ], n), ncol = n) * denm/t(matrix(rep(f, k), ncol = k))
              mu[i, ] = x %*% t(p)/apply(p, 1, sum)
              prop[i, ] = apply(p, 1, sum)/n
            }
          }
          l[i] = l2
        }

        I = sort(l, index.return = TRUE)$ix
        id = I[numini]
        mu = mu[id, ]
        prop = prop[id, ]
        stop = 0; run = 0; dif = 10; difml = 10; ml = 10^8

        while(stop == 0){
          for(j in 1:k){
            denm[j, ] = stats::dnorm(x, mu[j], sigma[j])
          }
          f = prop %*% denm
          run = run + 1
          lh[run] = sum(log(f))

          if(run > 2){

            if(lh[run - 1] - lh[run - 2] == 0){
              stop = 1
            } else {
              c = (lh[run] - lh[run - 1])/(lh[run - 1] - lh[run - 2])
              if(c > 0 && c < 1){
                preml = ml
                ml = lh[run - 1] + (lh[run] - lh[run - 1])/(1 - c)
                dif = ml - lh[run]
                difml = abs(preml - ml)
              }
            }
          }

          if(dif < acc && difml < 10^(-4) || run > 50000){
            stop = 1
          } else {
            p = matrix(rep(prop, n), ncol = n) * denm/t(matrix(rep(f, k), ncol = k))
            mu = x %*% t(p)/apply(p, 1, sum)
            prop = apply(p, 1, sum)/n
          }
        }

        main = list(mu = matrix(mu, nrow = k), pi = prop, p = p, lik = lh[run]) #,inilh=l,iniid=id)
      }

    }
    else {
      #-----------------------------------------------------------------------------------------#
      # (ii-2) when the equal variance is unknown
      #-----------------------------------------------------------------------------------------#

      n = length(x)
      acc = acc * n
      a = round(n/2 + 0.0001) # Xin Shen updated acc from 10^(-5) to acc
      s2 = stats::var(as.numeric(x)) * (n - 1)/n
      sx = sort(x)
      lowvar = ((range(x)[2] - range(x)[1])/35)^2

      if(k == 2){
        #-----------------------------------------------------------------------------------------#
        # (ii-2-a) when C = 2
        #-----------------------------------------------------------------------------------------#
        prop = matrix(rep(1/2, 10 * 2), nrow = 10)
        mu = rbind(sort(rsample(x, 2)), sort(rsample(x, 2)), sort(rsample(x, 2)),
                   sort(rsample(x, 2)), sort(rsample(x, 2)), sort(rsample(x, 2)), sort(rsample(x, 2)),
                   cbind(min(x), max(x)),
                   cbind(mean(sx[1:a]), mean(sx[(a + 1 + 0.0001):n])),
                   cbind(mean(x) - 0.5 * stats::sd(x), mean(x) + 0.5 * stats::sd(x)))
        sigma = matrix(rep(sqrt(stats::runif(7, 0, 1) * s2), 2), nrow = 7)
        sigma = rbind(sigma, matrix(rep(sqrt(mmax(lowvar, s2 - diag(stats::var(t(mu[(8:10), ])))/k)), 2), ncol = 2))

        if(!is.null(ini)){
          mu = rbind(mu, ini$mu)
          prop = rbind(prop, ini$pi)
          sigma = rbind(sigma, ini$sigma)
        }

        numini = dim(mu)[1]
        l = rep(NaN, numini)
        for(i in 1:numini){
          stop = 0; run = 0; l2 = -10^8
          while(stop == 0){
            run = run + 1
            denm = rbind(stats::dnorm(x, mu[i, 1], sigma[i, 1]), stats::dnorm(x, mu[i, 2], sigma[i, 2]))
            f = prop[i,] %*% denm
            l1 = l2; l2 = sum(log(f)); dif = l2 - l1

            if(dif < 10^(-4) * n && run > 30 || dif < 10^(-8) * n || run > 1000){
              stop = 1
            } else {
              p = matrix(rep(prop[i, ], n), ncol = n) * denm/t(matrix(rep(f, 2), ncol = 2))
              mu[i, ] = x %*% t(p)/apply(p, 1, sum)
              prop[i, ] = apply(p, 1, sum)/n
              sigma[i, ] = sqrt(sum(sum(p * (rbind(x, x) - matrix(rep(mu[i, ], n), ncol = n))^2))/n) * rep(1, 2)
            }
          }
          l[i] = l2
        }

        I = sort(l, index.return = TRUE)$ix
        id = I[numini]
        mu = mu[id, ]
        prop = prop[id, ]
        sigma = sigma[id, ]
        stop = 0; run = 0; dif = 10; difml = 10; ml = 10^8

        while(stop == 0){
          denm = rbind(stats::dnorm(x, mu[1], sigma[1]), stats::dnorm(x, mu[2], sigma[2]))
          f = prop %*% denm
          run = run + 1
          lh[run] = sum(log(f))

          if(run > 2){

            if(lh[run - 1] - lh[run - 2] == 0){
              stop = 1
            } else {
              c = (lh[run] - lh[run - 1])/(lh[run - 1] - lh[run - 2])
              if(c > 0 && c < 1){
                preml = ml
                ml = lh[run - 1] + (lh[run] - lh[run - 1])/(1 - c)
                dif = ml - lh[run]
                difml = abs(preml - ml)
              }
            }
          }

          if(dif < acc && difml < 10^(-4) || run > 50000){
            stop = 1
          } else {
            p = matrix(rep(prop, n), ncol = n) * denm/t(matrix(rep(f, 2), ncol = 2))
            mu = x %*% t(p)/apply(p, 1, sum)
            prop = apply(p, 1, sum)/n
            sigma = sqrt(sum((rbind(x, x) - matrix(rep(mu, n), nrow = 2))^2 * p)/n) * rep(1, 2)
          }
        }

        main = list(mu = matrix(mu, nrow = k), pi = prop, sigma = sigma, p = p, lik = lh[run]) #inilh=l,iniid=id)

      }
      else {
        #-----------------------------------------------------------------------------------------#
        # (ii-2-b) when C > 2
        #-----------------------------------------------------------------------------------------#
        prop = matrix(rep(1/k, 10 * k), nrow = 10)
        mu = rbind(sort(rsample(x, k)), sort(rsample(x, k)), sort(rsample(x, k)), sort(rsample(x, k)),
                   sort(rsample(x, k)), sort(rsample(x, k)), sort(rsample(x, k)), sort(rsample(x, k)))
        mmu = rbind(sx[1], mean(sx[1:round(n/k + 0.0001)]))
        for(i in 1:(k - 1)){
          mmu = cbind(mmu,
                      rbind(sx[round(i * n/(k - 1) + 0.0001)],
                            mean(sx[(round(i * n/k) + 1 + 0.0001):round((i + 1) * n/k + 0.0001)])))
        }
        mu = rbind(mu, mmu)
        sigma = matrix(rep(sqrt(stats::runif(8, 0, 1) * s2), k), nrow = 8)
        sigma = rbind(sigma, matrix(rep(sqrt(mmax(lowvar, s2 - stats::var(mu[9, ]) * (k - 1)/k)), k), ncol = k))
        sigma = rbind(sigma, matrix(rep(sqrt(mmax(lowvar, s2 - stats::var(mu[10, ]) * (k - 1)/k)), k), ncol = k))

        if(!is.null(ini)){
          mu = rbind(mu, ini$mu)
          prop = rbind(prop, ini$pi)
          sigma = rbind(sigma, ini$sigma)
        }

        numini = dim(mu)[1]
        l = rep(NaN, numini)
        for(i in 1:numini){
          stop = 0; run=0; l2 = -10^8
          while(stop == 0){
            run = run + 1
            denm = matrix(rep(0, n * k), nrow = k)
            for(j in 1:k){
              denm[j, ] = stats::dnorm(x, mu[i, j], sigma[i, j])
            }
            f = prop[i, ] %*% denm
            l1 = l2; l2 = sum(log(f)); dif = l2 - l1

            if(dif < 10^(-4) * n && run > 15 || dif < 10^(-8) * n || run > 1000){
              stop = 1
            } else {
              p = matrix(rep(prop[i, ], n), ncol = n) * denm/t(matrix(rep(f, k), ncol = k))
              mu[i, ] = x %*% t(p)/apply(p, 1, sum)
              prop[i, ] = apply(p, 1, sum)/n
              sigma[i, ] = sqrt(sum(sum(p * (t(matrix(rep(x, k), ncol = k)) - matrix(rep(mu[i, ], n), ncol = n))^2))/n) * rep(1, k)
            }
          }
          l[i] = l2
        }

        I = sort(l, index.return = TRUE)$ix
        id = I[numini]
        mu = mu[id, ]
        prop = prop[id, ]
        sigma = sigma[id, ]
        stop = 0; run = 0; dif = 10; difml = 10; ml = 10^8; lh=numeric()

        while(stop == 0){
          for(j in 1:k){
            denm[j, ] = stats::dnorm(x, mu[j], sigma[j])
          }
          f = prop %*% denm
          run = run + 1
          lh[run] = sum(log(f))

          if(run > 2){

            if(lh[run - 1] - lh[run - 2] == 0){
              stop = 1
            } else {
              c = (lh[run] - lh[run - 1])/(lh[run - 1] - lh[run - 2])
              if(c > 0 && c < 1){
                preml = ml
                ml = lh[run - 1] + (lh[run] - lh[run - 1])/(1 - c)
                dif = ml - lh[run]
                difml = abs(preml - ml)
              }
            }
          }

          if(dif < acc && difml < 10^(-4) || run > 50000){
            stop = 1
          } else {
            p = matrix(rep(prop, n), ncol = n) * denm/t(matrix(rep(f, k), ncol = k))
            mu = x %*% t(p)/apply(p, 1, sum)
            prop = apply(p, 1, sum)/n
            sigma = sqrt(sum(sum(p * (t(matrix(rep(x, k), ncol = k)) - matrix(rep(mu, n), ncol = n))^2))/n) * rep(1, k)
          }
        }

        main = list(mu = matrix(mu, nrow = k), pi = prop, sigma = sigma, p = p, lik = lh[run]) # inilh=l,iniid=id)

      }
    }
  }

  return(main)
}


#-------------------------------------------------------------------------------
# Internal functions
#-------------------------------------------------------------------------------
#' Parameter Estimation for Multivariate Normal Mixture
#'
#' `mixmnorm' is used to estimate parameters of multivariate normal mixtures
#' using the EM-algorithm based on MLE.
#'
#' @param x observations matrix with one observation per row.
#' @param sigma_k initial value for standard deviation. Default is NULL.
#' @param equal If the covariate matrix of each component is equal, possible values: 0,1,2. See details.
#'
#'@details
#'equal: When equal = 0, fit k component normal mixture with unknown covariance matrix; when equal = 1,
#'fit k component normal mixture with unknown equal covariance matrix; when equal = 2, fit k component normal mixture with covariance matrix is known.
#'
#' @return A list containing the following elements:
#'   \item{mu}{estimated means of each component}
#'   \item{pi}{estimated proportion of each component}
#'   \item{sigma}{estimated sd of each component, reported when sigma is not specified.}
#'   \item{p}{the classification probability matrix with (k,l)th element is the probability of kth observation belongs to lth component.}
#'   \item{lh}{the likelihood of the estimations}
#'
#' @examples
#' #See example in complh
#' @seealso \code{\link{complh}} can take the result from mixmnorm
#' @seealso \code{\link{distlat}} can take the result from mixmnorm
#' @seealso \code{\link{mixnorm}} for univariate input
#' @noRd
mixmnorm <- function(x, sigma_k = NULL, equal = NULL) {
  if (is.null(sigma_k) && is.null(equal)) {
    k = 2
    equal = 1
  }

  if (!is.null(sigma_k)) {
    #-----------------------------------------------------------------------------------------#
    # (i-1) when sigma is known
    #-----------------------------------------------------------------------------------------#
    temp = dim(as.matrix(sigma_k))
    if (temp[1] < temp[2]) {
      sigma_k = t(sigma_k)
      temp = dim(sigma_k)
    }

    if (temp[1] > 1) {
      if (temp[1] == temp[2]) {
        sigma = matrix(rep(sigma_k, 2), nrow = 2)
        k = 2
        if (is.null(equal) == FALSE) {
          k = equal
        }
      } else {
        sigma = sigma_k
        k = temp[1] / temp[2]
      }
      equal = 2
    } else {
      k = sigma_k
      if (is.null(equal)) {
        equal = 1
      }
    }
  }

  if (NCOL(x) > NROW(x)) {
    n = NCOL(x)
    s = NROW(x)
    x = t(x)
  } else {
    n = NROW(x)
    s = NCOL(x)
  }

  if (equal == 0) {
    # Fit k-component normal mixture with unknown covariance matrix
    mu = rbind(rsample(x, k), rsample(x, k), rsample(x, k), rsample(x, k), rsample(x, k))
    prop = matrix(rep(1/k, 5 * k), nrow = 5)
    sigma = matrix(rep(0, k * s * 5 * s), ncol = s)

    for (i in 1:5) {
      sigma[(k * s * (i - 1) + 1):(i * s * k), ] = t(matrix(rep(diag(mmax(0.5, stats::runif(s, 0, 1)) * diag(stats::var(x))), k), nrow = s))
    }

    l = rep(NaN, 5)

    for (i in 1:5) {
      stop = 0
      run = 0
      l2 = -10^8

      while (stop == 0) {
        run = run + 1
        denm = matrix(rep(0, n * k), nrow = k)

        for (j in 1:k) {
          denm[j, ] = mvtnorm::dmvnorm(x, mu[(k * (i - 1) + j), ], sigma[(((i - 1) * k + j - 1) * s + 1):(((i - 1) * k + j) * s), ])
        }

        f = prop[i, ] %*% denm
        l1 = l2
        l2 = sum(log(f))
        dif = l2 - l1

        if (dif < 10^(-5) * n && run > 20 || dif < 10^(-8) * n || run > 10000) {
          stop = 1
        } else {
          p = matrix(rep(prop[i, ], n), ncol = n) * denm / t(matrix(rep(f, k), ncol = k))
          mu[(k * (i - 1) + 1):(k * i), ] = p %*% x / apply(p, 1, sum)
          prop[i, ] = apply(p, 1, sum) / n

          for (j in 1:k) {
            sigma[(((i - 1) * k + j - 1) * s + 1):(((i - 1) * k + j) * s), ] = t(x - t(matrix(rep(mu[k * (i - 1) + j, ], n), nrow = 2))) %*%
              (matrix(rep(p[j, ], s), ncol = s) * (x - t(matrix(rep(mu[k * (i - 1) + j, ], n), nrow = 2)))) / sum(p[j, ])
          }
        }

        if (min(prop[i, ]) < 4 / n) {
          mu[(k * i - 1 + 1):(k * i), ] = rsample(x, k)
          prop[i, ] = rep(1/k, k)
          sigma[((i - 1) * k * s + 1):(i * k * s), ] = t(matrix(rep(diag(mmax(0.05, stats::runif(s, 0, 1)) * diag(stats::var(x))), k), nrow = s))
          stop = 0
          run = 0
        }
      }
      l[i] = l2
    }

    I = sort(l, index.return = T)$ix #y = sort(l,index.return = T)$x;
    id = I[5]

    mu = mu[(k * (id - 1) + 1):(k * id), ]
    prop = prop[id, ]
    sigma = sigma[(k * s * (id - 1) + 1):(k * s * id), ]
    stop = 0
    run = 0
    dif = 10
    difml = 10
    ml = 10^8
    lh = numeric()

    while (stop == 0) {
      denm = matrix(rep(0, n * k), nrow = k)

      for (j in 1:k) {
        denm[j, ] = dmvnorm(x, mu[j, ], sigma[((j - 1) * s + 1):(j * s), ])
      }

      f = prop %*% denm
      run = run + 1
      lh[run] = sum(log(f)) # use lh to record the sequence of lh

      if (run > 3) {
        if ((lh[run - 1] - lh[run - 2]) < 10^(-8)) {
          warning("Likelihood in EM algorithm is not increasing.")
        }

        if (lh[run - 1] - lh[run - 2] == 0) {
          stop = 1
        } else {
          c = (lh[run] - lh[run - 1]) / (lh[run - 1] - lh[run - 2]) # convergence rate of Em algorithm

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
      if (dif < 0.001 && difml < 10^(-6) || run > 100) {
        stop = 1
      } else {
        p = matrix(rep(prop, n), ncol = n) * denm / t(matrix(rep(f, k), ncol = k))
        mu = p %*% x / apply(p, 1, sum)
        prop = apply(p, 1, sum) / n

        for (j in 1:k) {
          sigma[((j - 1) * s + 1):(j * s), ] = t(x - t(matrix(rep(mu[j, ], n), nrow = 2))) %*%
            (matrix(rep(p[j, ], s), ncol = s) * (x - t(matrix(rep(mu[j, ], n), nrow = 2)))) / sum(p[j, ])
        }
      }

      if (min(prop) < 2 / n) {
        est = mixmnorm(x, sigma_k, equal)
      } else {
        est = list(mu = mu, sigma = sigma, pi = prop, lh = lh[run], p = p)
      }
    }

    est = est
  } else if (equal == 1) {
    # Unknown equal covariance matrix
    if (k == 2) {
      prop = 1/2 * matrix(rep(1, 4 * 2), nrow = 4)
      b = sort(x[, 1], index = T)$ix
      c = sort(x[, 2], index = T)$ix
      sigma = matrix(rep(0, 2 * s * 4), ncol = 2)
      mu = rbind(rsample(x, k), rsample(x, k), x[b[1], ], x[b[n], ], x[c[1], ], x[c[n], ])
      sigma[1:(2 * s), ] = rbind(diag(mmax(0.1, stats::runif(s, 0, 1)) * diag(stats::var(x))),
                                 diag(mmax(0.1, stats::runif(s, 0, 1)) * diag(stats::var(x))))
      sigma[(2 * s + 1):(4 * s), ] = rbind(diag(mmax(0.1, diag(stats::var(x)) * (n - 1) / n - diag(stats::var(mu[5:6, ])) / 2)),
                                           diag(mmax(0.1, diag(stats::var(x)) * (n - 1) / n - diag(stats::var(mu[7:8, ])) / 2)))
      l = rep(NaN, 4)

      for (i in 1:4) {
        stop = 0
        run = 0
        l2 = -10^8

        while (stop == 0) {
          run = run + 1
          denm = rbind(mvtnorm::dmvnorm(x, mu[(2 * i - 1), ], sigma[((i - 1) * s + 1):(i * s), ]),
                       mvtnorm::dmvnorm(x, mu[(2 * i), ], sigma[((i - 1) * s + 1):(i * s), ]))
          f = prop[i, ] %*% denm
          l1 = l2
          l2 = sum(log(f))
          dif = l2 - l1

          if (dif < 10^(-5) * n && run > 20 || dif < 10^(-8) * n || run > 1000) {
            stop = 1
          } else {
            p = matrix(rep(prop[i, ], n), ncol = n) * denm / rbind(f, f)
            mu[(2 * i - 1):(2 * i), ] = p %*% x / apply(p, 1, sum)
            prop[i, ] = apply(p, 1, sum) / n
            sigma[((i - 1) * s + 1):(i * s), ] =
              (t(x - t(matrix(rep(mu[2 * i - 1, ], n), nrow = 2))) %*%
                 (matrix(rep(p[1, ], s), ncol = s) * (x - t(matrix(rep(mu[2 * i - 1, ], n), nrow = 2)))) +
                 t(x - t(matrix(rep(mu[2 * i, ], n), nrow = 2))) %*%
                 (matrix(rep(p[2, ], s), ncol = s) * (x - t(matrix(rep(mu[2 * i, ], n), nrow = 2)))))/n
          }
        }
        l[i] = l2
      }

      I = sort(l, index.return = T)$ix #y = sort(l,index.return = T)$x
      id = I[4]
      mu = rbind(mu[(2 * id - 1), ], mu[(2 * id), ])
      prop = prop[id, ]
      sigma = sigma[(s * (id - 1) + 1):(s * id), ]
      stop = 0
      run = 0
      dif = 10
      difml = 10
      ml = 10^8
      lh = numeric()

      while (stop == 0) {
        denm = rbind(mvtnorm::dmvnorm(x, mu[1, ], sigma), mvtnorm::dmvnorm(x, mu[2, ], sigma))
        f = prop %*% denm
        run = run + 1
        lh[run] = sum(log(f)) # use lh to record the sequence of lh

        if (run > 2) {
          if ((lh[run - 1] - lh[run - 2]) < 10^(-8)) {
            warning("Likelihood in EM algorithm is not increasing.")
          }

          if (lh[run - 1] - lh[run - 2] == 0) {
            stop = 1
          } else {
            c = (lh[run] - lh[run - 1]) / (lh[run - 1] - lh[run - 2]) # convergence rate of Em algorithm
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
        if (dif < 0.0001 && difml < 10^(-7) || run > 50000) {
          stop = 1
        } else {
          p = matrix(rep(prop, n), ncol = n) * denm / rbind(f, f)
          mu = p %*% x / apply(p, 1, sum)
          prop = apply(p, 1, sum) / n
          sigma = (t(x - t(matrix(rep(mu[1, ], n), nrow = 2))) %*%
                     (matrix(rep(p[1, ], s), ncol = s) * (x - t(matrix(rep(mu[1, ], n), nrow = 2)))) +
                     t(x - t(matrix(rep(mu[2, ], n), nrow = 2))) %*%
                     (matrix(rep(p[2, ], s), ncol = s) * (x - t(matrix(rep(mu[2, ], n), nrow = 2)))))/n
        }
      }
      p = matrix(rep(prop, n), ncol = n) * denm / rbind(f, f)
      est = list(mu = mu, sigma = sigma, pi = prop, lik = lh[run], p = p)
    } else {
      mu = rbind(rsample(x, k), rsample(x, k), rsample(x, k), rsample(x, k), rsample(x, k))
      prop = matrix(rep(1/k, 5 * k), nrow = 5)
      sigma = matrix(rep(0, s * 5 * 2), ncol = 2)

      for (i in 1:5) {
        sigma[(s * (i - 1) + 1):(i * s), ] = diag(mmax(0.1, stats::runif(s, 0, 1)) * diag(stats::var(x)))
      }

      l = rep(NaN, 5)
      for (i in 1:5) {
        stop = 0
        run = 0
        l2 = -10^8

        while (stop == 0) {
          run = run + 1
          denm = matrix(rep(0, n * k), nrow = k)
          for (j in 1:k) {
            denm[j, ] = mvtnorm::dmvnorm(x, mu[(k * (i - 1) + j), ], sigma[((i - 1) * s + 1):(i * s), ])
          }
          f = prop[i, ] %*% denm
          l1 = l2
          l2 = sum(log(f))
          dif = l2 - l1

          if (dif < 10^(-5) * n && run > 20 || dif < 10^(-8) * n || run > 10000) {
            stop = 1
          } else {
            p = matrix(rep(prop[i, ], n), ncol = n) * denm / t(matrix(rep(f, k), ncol = k))
            mu[(k * (i - 1) + 1):(k * i), ] = p %*% x / apply(p, 1, sum)
            prop[i, ] = apply(p, 1, sum) / n
            temp = 0

            for (j in 1:k) {
              temp = temp + t(x - t(matrix(rep(mu[k * (i - 1) + j, ], n), nrow = 2))) %*%
                (matrix(rep(p[j, ], s), ncol = s) * (x - t(matrix(rep(mu[k * (i - 1) + j, ], n), nrow = 2))))
            }
            sigma[((i - 1) * s + 1):(i * s), ] = temp / n
          }
        }
        l[i] = l2
      }
      I = sort(l, index.return = T)$ix #y = sort(l,index.return = T)$x
      id = I[5]
      mu = mu[(k * (id - 1) + 1):(k * id), ]
      prop = prop[id, ]
      sigma = sigma[(s * (id - 1) + 1):(s * id), ]
      stop = 0
      run = 0
      dif = 10
      difml = 10
      ml = 10^8
      lh = numeric()

      while (stop == 0) {
        denm = matrix(rep(0, n * k), nrow = k)
        for (j in 1:k) {
          denm[j, ] = mvtnorm::dmvnorm(x, mu[j, ], sigma)
        }
        f = prop %*% denm
        run = run + 1
        lh[run] = sum(log(f)) # use lh to record the sequence of lh

        if (run > 2) {
          if (lh[run - 1] - lh[run - 2] == 0) {
            stop = 1
          } else {
            c = (lh[run] - lh[run - 1]) / (lh[run - 1] - lh[run - 2]) # convergence rate of Em algorithm
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
        if (dif < 0.0001 && difml < 10^(-7) || run > 50000) {
          stop = 1
        } else {
          p = matrix(rep(prop, n), ncol = n) * denm / t(matrix(rep(f, k), ncol = k))
          mu = p %*% x / apply(p, 1, sum)
          prop = apply(p, 1, sum) / n
          temp = 0

          for (j in 1:k) {
            temp = temp + t(x - t(matrix(rep(mu[j, ], n), nrow = 2))) %*%
              (matrix(rep(p[j, ], s), ncol = s) * (x - t(matrix(rep(mu[j, ], n), nrow = 2))))
          }
          sigma = temp / n
        }
      }
      p = matrix(rep(prop, n), ncol = n) * denm / t(matrix(rep(f, k), ncol = k))
      est = list(mu = mu, sigma = sigma, pi = prop, p = p, lik = lh[run]) #,inilh=l,iniid=id)
    }
  } else {
    # Covariance matrix is known
    mu = rbind(rsample(x, k), rsample(x, k), rsample(x, k), rsample(x, k), rsample(x, k))
    prop = matrix(rep(1/k, 5 * k), nrow = 5)
    l = rep(NaN, 5)

    for (i in 1:5) {
      stop = 0
      run = 0
      l2 = -10^8

      while (stop == 0) {
        run = run + 1
        denm = matrix(rep(0, n * k), nrow = k)

        for (j in 1:k) {
          denm[j, ] = mvtnorm::dmvnorm(x, mu[(k * (i - 1) + j), ], sigma[((j - 1) * s + 1):(j * s), ])
        }
        f = prop[i, ] %*% denm
        l1 = l2
        l2 = sum(log(f))
        dif = l2 - l1

        if (dif < 10^(-5) * n && run > 20 || dif < 10^(-8) * n || run > 10000) {
          stop = 1
        } else {
          p = matrix(rep(prop[i, ], n), ncol = n) * denm / t(matrix(rep(f, k), ncol = k))
          mu[(k * (i - 1) + 1):(k * i), ] = p %*% x / apply(p, 1, sum)
          prop[i, ] = apply(p, 1, sum) / n
          sigma[(((i - 1) * s + 1):(i * s)), ] = matrix(0, s, s)

          for (j in 1:k) {
            sigma[(((i - 1) * s + 1):(i * s)), ] = sigma[(((i - 1) * s + 1):(i * s)), ] +
              t(x - t(matrix(rep(mu[k * (i - 1) + j, ], n), nrow = 2))) %*%
              (matrix(rep(p[j, ], s), ncol = s) * (x - t(matrix(rep(mu[k * (i - 1) + j, ], n), nrow = 2)))) / sum(p[j, ])
          }
          sigma[(((i - 1) * s + 1):(i * s)), ] = sigma[(((i - 1) * s + 1):(i * s)), ] / n
        }
      }
      l[i] = l2
    }

    I = sort(l, index.return = T)$ix #y = sort(l,index.return = T)$x
    id = I[5]
    mu = mu[(k * (id - 1) + 1):(k * id), ]
    prop = prop[id, ]
    sigma = sigma[(s * (id - 1) + 1):(s * id), ]
    stop = 0
    run = 0
    dif = 10
    difml = 10
    ml = 10^8
    lh = numeric()

    while (stop == 0) {
      denm = matrix(rep(0, n * k), nrow = k)

      for (j in 1:k) {
        denm[j, ] = mvtnorm::dmvnorm(x, mu[j, ], sigma[((j - 1) * s + 1):(j * s), ])
      }

      f = prop %*% denm
      run = run + 1
      lh[run] = sum(log(f)) # use lh to record the sequence of lh

      if (run > 2) {
        if (lh[run - 1] - lh[run - 2] == 0) {
          stop = 1
        } else {
          c = (lh[run] - lh[run - 1]) / (lh[run - 1] - lh[run - 2]) # convergence rate of Em algorithm

          if (c > 0 && c < 1) {
            preml = ml
            ml = lh[run - 1] + (lh[run] - lh[run - 1]) / (1 - c) # use Aitken acceleration to predict the maximum likelihood value
            dif = ml - lh[run]
            difml = abs(preml - ml)
          }
        }
      }

      # Algorithm stopped when the direction derivative was smaller than acc and the
      # difference of the predicted maximum likelihood value and the current likelihood value is small than 0.005
      if (dif < 0.0001 && difml < 10^(-7) || run > 50000) {
        stop = 1
      } else {
        p = matrix(rep(prop, n), ncol = n) * denm / t(matrix(rep(f, k), ncol = k))
        mu = p %*% x / apply(p, 1, sum)
        prop = apply(p, 1, sum) / n
        sigma = matrix(0, s, s)

        for (j in 1:k) {
          sigma = sigma + t(x - t(matrix(rep(mu[j, ], n), nrow = 2))) %*%
            (matrix(rep(p[j, ], s), ncol = s) * (x - t(matrix(rep(mu[j, ], n), nrow = 2))))
        }

        sigma = sigma / n
      }
    }
    p = matrix(rep(prop, n), ncol = n) * denm / t(matrix(rep(f, k), ncol = k))
    est = list(mu = mu, sigma = sigma, pi = prop, p = p, lik = lh[run])
  }

  return(est)
}

permfull <- function(x) {
  lx <- length(x)
  fact <- 1:lx
  if (lx == 1) {
    out <- x
  } else {
    n <- gamma(lx)
    out <- matrix(rep(0, lx * n * lx), ncol = lx)
    for (i in 1:lx) {
      temp <- permfull(x[fact != i])
      temp <- as.matrix(temp)
      out[((i - 1) * n + 1):(i * n), ] <- cbind(x[i] * matrix(rep(1, n), nrow = n), temp)
    }
  }
  return(out)
}

mmax <- function(a, b) {
  n <- length(b)
  c <- numeric()
  for (i in 1:n) {
    if (a > b[i]) {
      c[i] <- a
    } else {
      c[i] <- b[i]
    }
  }
  return(c)
}

rsample <- function(x, n = NULL, replace = NULL) {
  x <- as.matrix(x)
  s <- dim(x)
  if (s[1] == 1) {
    x <- t(x)
  }
  s <- dim(x)
  dim1 <- s[1]
  dim2 <- s[2]

  if (is.null(replace)) {
    replace <- 0
  }
  if (is.null(n)) {
    n <- dim1
  }

  if (replace == 0) {
    if (n > dim1) {
      stop("ERROR: The number of samples to be chosen is bigger than that of x in the situation without replacement")
    } else {
      b <- sample(dim1)
      a <- x[b[1:n], ]
    }
  } else {
    a <- x[sample(1:dim1, size = n), ]
  }

  if (s[2] == 1) {
    a <- t(a)
  }

  return(a)
}
