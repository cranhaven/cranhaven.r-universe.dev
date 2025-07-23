#' Goodness of Fit Test for Finite Mixture Models
#'
#' `mixTest' is used to perform a goodness-of-fit test for finite mixture models (Wichitchan et al., 2019).
#' It returns five types of goodness-of-fit statistics and determines the number of components
#' in the mixture model based on the Kolmogorov-Smirnov (KS) statistic.
#' The test is performed using bootstrapping.
#'
#' @usage
#' mixTest(x, alpha = 0.10, C.max = 10, nboot = 500, nstart = 5)
#'
#' @param x a vector of observations.
#' @param alpha significance level of the test.
#' @param C.max maximum number of mixture components considered in the test.
#'   The test is performed for 2 to \code{C.max} components.
#' @param nboot number of bootstrap resampling. Default is 500.
#' @param nstart number of initializations to try. Default if 5.
#'
#' @return
#'
#'   A list containing the following elements:
#'   \item{GOFstats}{vector of test statistics calculated from data, in the order \code{of c(ks, cvm, kui, wat, ad)}.}
#'   \item{ks}{vector of the Kolmogorov-Smirnov (KS) statistic for each bootstrap sample.}
#'   \item{cvm}{vector of the Cramer-Von Mises statistic for each bootstrap sample.}
#'   \item{kui}{vector of the Kuiper's statistic for each bootstrap sample.}
#'   \item{wat}{vector of the Watson statistic for each bootstrap sample.}
#'   \item{ad}{vector of the Anderson Darling statistic for each bootstrap sample.}
#'   \item{result}{vector of test results based on the KS statistic.
#'     If the kth element in the vector is 1, k-component mixture model is significant
#'     based on the KS statistic; If 0, otherwise. See examples for details.}
#'
#' @references
#'    Wichitchan, S., Yao, W., and Yang, G. (2019). Hypothesis testing for finite mixture models.
#'    Computational Statistics & Data Analysis, 132, 180-189.
#'
#' @importFrom mixtools rnormmix
#' @export
#' @examples
#' n = 100
#' mu = c(-2.5, 0)
#' sd = c(0.8, 0.6)
#' n1 = rbinom(n, 1, 0.3)
#' x = c(rnorm(sum(n1), mu[1], sd[1]), rnorm(n - sum(n1), mu[2], sd[2]))
#'
#' # The result shows that two-component mixture model is statistically significant based on the KS.
#' \donttest{out = mixTest(x, alpha = 0.10, C.max = 10, nboot = 500, nstart = 5)}
mixTest <- function(x, alpha = 0.10, C.max = 10, nboot = 500, nstart = 5){

  alp <- alpha
  k <- C.max
  ite <- nboot
  init <- nstart

  n = length(x)
  e = seq(1:n)
  e_cdf <- e/n
  e_cdf1 <- (e - 1)/n
  e_cd <- (2 * e - 1)/(2 * n)

  pro = rep(0, k)

  ### test 1 component ###
  cdf <- stats::pnorm(sort(x), mean(x), stats::sd(x))
  DP <- max(abs(e_cdf - cdf))
  DM <- max(abs(cdf - e_cdf1))
  ks <- max(DP, DM)
  cvm <- sum((cdf - e_cd)^2) + (1/(12 * n))
  kui <- DP + DM
  wat <- cvm - n * ((mean(cdf) - 0.5)^2)
  ad <- - n - ((sum((2 * (1:n) - 1) * (log(cdf) + log(1 - sort(cdf, decreasing = TRUE))))/n))
  real <- c(ks, cvm, kui, wat, ad)

  ks <- cvm <- kui <- wat <- ad <- c()

  ### Bootstrap ###
  for (i in 1:ite){
    xboot <- stats::rnorm(n, mean(x), stats::sd(x))
    cdf <- stats::pnorm(sort(xboot), mean(xboot), stats::sd(xboot))

    DP <- max(abs(e_cdf - cdf))
    DM <- max(abs(cdf - e_cdf1))
    ks[i] <- max(DP, DM)
    cvm[i] <- sum((cdf - e_cd)^2) + (1/(12 * n))
    kui[i] <- DP + DM
    wat[i] <- cvm[i] - n * ((mean(cdf) - 0.5)^2)
    ad[i] <- - n - ((sum((2 * (1:n) - 1) * (log(cdf) + log(1 - sort(cdf, decreasing = TRUE))))/n))
  }

  if(real[1] < sort(ks)[(1 - alp) * ite]){
    pro[1] <- pro[1] + 1
  } else {
    ### test k component ###
    for(kk in 2:k){
      out = test_k_comp(x, kk, e, ite, init)
      if(out$real[1] < sort(out$ks)[(1 - alp) * ite]){
        pro[kk] <- pro[kk] + 1
        ks = out$ks; cvm = out$cvm; kui = out$kui; wat = out$wat; ad = out$ad
      }
      if(out$real[1] < sort(out$ks)[(1 - alp) * ite]){
        break
      }
    }
  }

  res = list(sample_stats = real, ks = ks, cvm = cvm, kui = kui, wat = wat, ad = ad, result = pro)
  return(res)

}


#-------------------------------------------------------------------------------
# Internal functions
#-------------------------------------------------------------------------------
test_k_comp <- function(x, k, e, ite, init) {
  n <- length(x) # Added sep18 2022 Xin Shen to fix the error message of 'no visible binding for global variable ‘n’'
  nor <- function(x) {sqrt(sum(x^2))}

  e_cdf <- e / n
  e_cdf1 <- (e - 1) / n
  e_cd <- (2 * e - 1) / (2 * n)

  m <- matrix(stats::rnorm(k * init, mean(x), stats::sd(x)), init, k)
  s <- matrix(rep(stats::sd(x)), init, k)
  p <- matrix(rep(1 / k), init, k)

  #### EM ######
  thetaah <- matrix(0, init, (3 * k + 1))
  cd <- c()
  for (j in 1:init) {
    res <- mixtools::normalmixEM(x, lambda = p[j, ], mu = m[j, ],
                                 sigma = s[j, ], epsilon = 1e-06,
                                 maxit = 5000, maxrestarts = 1000)

    ### Label Swiching ###
    rl <- res$lambda
    rm <- res$mu
    rs <- res$sigma
    est <- c(rl[order(rm)], rm[order(rm)], rs[order(rm)])
    estt <- t(matrix(est, nrow = k))

    for (kk in 1:(3 * k)) {
      thetaah[j, kk] <- estt[kk]
    }
    thetaah[j, 3 * k + 1] <- res$loglik

    la1 <- 0
    for (kk in 1:k) {
      lakk <- thetaah[j, 3 * kk - 2] * stats::pnorm(sort(x), thetaah[j, 3 * kk - 1], thetaah[j, 3 * kk])
      la1 <- la1 + lakk
      la1
    }

    cd[j] <- sum((la1 - e_cd)^2) + (1 / (12 * n))
  }

  lab <- which.min(cd)

  theta <- c(thetaah[lab, 1:(3 * k)])

  cdf <- 0
  for (kk in 1:k) {
    cdfkk <- theta[3 * kk - 2] * stats::pnorm(sort(x), theta[3 * kk - 1], theta[3 * kk])
    cdf <- cdf + cdfkk
    cdf
  }

  DP <- max(abs(e_cdf - cdf))
  DM <- max(abs(cdf - e_cdf1))
  ks <- max(DP, DM)
  cvm <- sum((cdf - e_cd)^2) + (1 / (12 * n))
  kui <- DP + DM
  wat <- cvm - n * ((mean(cdf) - 0.5)^2)
  ad <- -n - ((sum((2 * (1:n) - 1) * (log(cdf) + log(1 - sort(cdf, decreasing = TRUE)))) / n))
  real <- c(ks, cvm, kui, wat, ad)

  ks <- c()
  cvm <- c()
  kui <- c()
  wat <- c()
  ad <- c()
  true <- theta

  for (i in 1:ite) {
    lambda <- numeric()
    mu <- numeric()
    sigma <- numeric()
    for (kk in 1:k) {
      lambda[kk] <- true[3 * kk - 2]
      mu[kk] <- true[3 * kk - 1]
      sigma[kk] <- true[3 * kk]
    }

    xb <- rnormmix(n, lambda, mu, sigma)

    m <- matrix(stats::rnorm(k * init, mean(xb), stats::sd(xb)), init, k)
    s <- matrix(rep(stats::sd(xb)), init, k)
    p <- matrix(rep(1 / k), init, k)

    #### EM ######
    thetaa <- matrix(0, init, 3 * k + 1)
    cd <- c()
    for (j in 1:init) {
      res <- mixtools::normalmixEM(xb, lambda = p[j, ], mu = m[j, ],
                                   sigma = s[j, ], epsilon = 1e-06,
                                   maxit = 5000, maxrestarts = 1000)

      ### Label Swiching ###
      if (k == 2) {
        est <- matrix(0, 2, 6)
        est[1, ] <- c(res$lambda[1], res$mu[1], res$sigma[1],
                      res$lambda[2], res$mu[2], res$sigma[2])
        est[2, ] <- c(res$lambda[2], res$mu[2], res$sigma[2],
                      res$lambda[1], res$mu[1], res$sigma[1])

        distance <- c(nor(est[1, ] - true), nor(est[2, ] - true))
        label <- which.min(distance)

        for (kk in 1:(3 * k)) {
          thetaa[j, kk] <- est[label, kk]
        }
      }

      if (k == 3) {
        est <- matrix(0, 6, 9)
        est[1, ] <- c(res$lambda[1], res$mu[1], res$sigma[1],
                      res$lambda[2], res$mu[2], res$sigma[2],
                      res$lambda[3], res$mu[3], res$sigma[3])
        est[2, ] <- c(res$lambda[1], res$mu[1], res$sigma[1],
                      res$lambda[3], res$mu[3], res$sigma[3],
                      res$lambda[2], res$mu[2], res$sigma[2])
        est[3, ] <- c(res$lambda[2], res$mu[2], res$sigma[2],
                      res$lambda[1], res$mu[1], res$sigma[1],
                      res$lambda[3], res$mu[3], res$sigma[3])
        est[4, ] <- c(res$lambda[2], res$mu[2], res$sigma[2],
                      res$lambda[3], res$mu[3], res$sigma[3],
                      res$lambda[1], res$mu[1], res$sigma[1])
        est[5, ] <- c(res$lambda[3], res$mu[3], res$sigma[3],
                      res$lambda[1], res$mu[1], res$sigma[1],
                      res$lambda[2], res$mu[2], res$sigma[2])
        est[6, ] <- c(res$lambda[3], res$mu[3], res$sigma[3],
                      res$lambda[2], res$mu[2], res$sigma[2],
                      res$lambda[1], res$mu[1], res$sigma[1])

        distance <- c(nor(est[1, ] - true), nor(est[2, ] - true),
                      nor(est[3, ] - true), nor(est[4, ] - true),
                      nor(est[5, ] - true), nor(est[6, ] - true))
        label <- which.min(distance)

        for (kk in 1:(3 * k)) {
          thetaa[j, kk] <- est[label, kk]
        }
      }

      if (k >= 4) {
        rl <- res$lambda
        rm <- res$mu
        rs <- res$sigma
        est <- c(rl[order(rm)], rm[order(rm)], rs[order(rm)])
        estt <- t(matrix(est, nrow = k))

        for (kk in 1:(3 * k)) {
          thetaa[j, kk] <- estt[kk]
        }
      }

      thetaa[j, 3 * k + 1] <- res$loglik

      la2 <- 0
      for (kk in 1:k) {
        lakk <- thetaa[j, 3 * kk - 2] * stats::pnorm(sort(xb), thetaa[j, 3 * kk - 1], thetaa[j, 3 * kk])
        la2 <- la2 + lakk
        la2
      }

      cd[j] <- sum((la2 - e_cd)^2) + (1 / (12 * n))
    }

    lab <- which.min(cd)

    theta <- c(thetaa[lab, 1:(3 * k)])

    cdf <- 0
    for (kk in 1:k) {
      cdfkk <- theta[3 * kk - 2] * stats::pnorm(sort(xb), theta[3 * kk - 1], theta[3 * kk])
      cdf <- cdf + cdfkk
      cdf
    }

    DP <- max(abs(e_cdf - cdf))
    DM <- max(abs(cdf - e_cdf1))
    ks[i] <- max(DP, DM)
    cvm[i] <- sum((cdf - e_cd)^2) + (1 / (12 * n))
    kui[i] <- DP + DM
    wat[i] <- cvm[i] - n * ((mean(cdf) - 0.5)^2)
    ad[i] <- -n - ((sum((2 * (1:n) - 1) * (log(cdf) + log(1 - sort(cdf, decreasing = TRUE)))) / n))
  }

  out <- list(real = real, ks = ks, cvm = cvm, kui = kui, wat = wat, ad = ad)
  out
}
