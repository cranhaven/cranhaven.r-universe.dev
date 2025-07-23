#' Semiparametric Mixture of Binomial Regression with a Degenerate Component
#' with Time-Varying Proportion and Time-Varying Success Probability
#'
#' `semimrBin' is used for semiparametric estimation of a mixture of binomial distributions
#' with one degenerate component, with time-varying proportions and time-varying success probability
#' (Cao and Yao, 2012).
#'
#' @usage
#' semimrBin(t, x, N, tg = NULL, tune = 1, tol = 1e-02)
#'
#' @details
#' The semiparametric mixture of binomial regression model is as follows:
#' \deqn{w(t) \times B(N,p(t))+(1-w(t))\times B(N,0),}
#' where \eqn{B(N,p)} is the probability mass function of a binomial distribution
#' with the number of trials \eqn{N} and the success probability \eqn{p}.
#' Here, the second component is a degenerate distribution with mass 1 on 0.
#' The time-varying proportion \eqn{w(t)} and success probability \eqn{p(t)} for the binomial components
#' are estimated by the kernel regression with some bandwidth.
#'
#' @param t a vector of time variable along which \eqn{w(t)} and \eqn{p(t)} vary.
#'   See details for the explanations of those notations.
#' @param x a vector of observed number of successes. The length of \code{t} and \code{x} must be the same.
#' @param N a scalar, specifying the number of trials for the Binomial distribution.
#' @param tg grid points of time used in the kernel regression for the estimation of \eqn{w(t)} and \eqn{p(t)}.
#'   Default is NULL, and 100 equally spaced grid points will automatically generated
#'   using the minimum and maximum values of \code{t}.
#' @param tune a scalar related to the bandwidth selection and local estimation. Default is 1.
#'   If greater than 0.2, the bandwidth is found based on the method in Cao and Yao (2012).
#'   If smaller than or equal to 0.2, this value is used as the percentage of data included in local estimation.
#' @param tol stopping criteria for the algorithm.
#'
#' @return A list containing the following elements:
#'   \item{pt}{estimated time-varying success probabilities for the first component.}
#'   \item{wt}{estimated time-varying proportions for the first component.}
#'   \item{h}{bandwidth for the kernel regression. The bandwidth calculation can be found in Section 4 of Cao and Yao (2012).}
#'
#' @seealso \code{\link{semimrBinOne}}, \code{\link{semimrBinFull}}
#'
#' @references
#'   Cao, J. and Yao, W. (2012). Semiparametric mixture of binomial regression with a degenerate component.
#'   Statistica Sinica, 27-46.
#'
#' @export
#' @examples
#' n = 100
#' tg = seq(from = 0, to = 1, length.out = 50)
#' t = seq(from = 0, to = 1, length.out = n)
#' pt = 0.5 * (1 - cos(2 * pi * t))
#' b = rbinom(n, 1, 0.2)
#' y = apply(X = matrix(pt), 1, rbinom, n = 1, size = 7)
#' y = ifelse(b == 1, 0, y)
#' ft = semimrBin(t = t, x = y, N = 7, tg = tg)
semimrBin <- function(t, x, N, tg = NULL, tune = 1, tol = 1e-02) {

  if (length(x) != length(t)) {
    errorCondition("x and t are in different sizes")
  }

  if (is.null(tg)) {
    u = seq(from = min(t), to = max(t), length = 100)
  }

  # make x and t row vector
  x = as.matrix(x)
  t = as.matrix(t)
  if (dim(x)[1] == 1) {
    x = x
    t = t
  } else {
    x = t(x)
    t = t(t)
  }

  # Create initial values for w and pt, where pt records the estimate of p(t)
  # evaluated at all observed t values.
  # p recrods the classification probability that each observation from the first component.
  lx = length(x)
  temp = 1:lx
  ind1 = temp[x == 0]
  ind2 = temp[x != 0]
  w = length(ind1)/lx
  pt = mean(x[ind2])/N
  p = numeric()
  p[ind1] = w/(w + (1 - w) * gamma(N + 1)/gamma(x[ind1] + 1)/gamma(N + 1 - x[ind1]) * pt^x[ind1] * (1 - pt)^(N - x[ind1]))
  p[ind2] = 0
  ltg = length(tg)
  #acc = 0.01
  acc <- tol

  if (tune > 0.2) { # the final bandwidth will be h*tune
    subx = x[ind2]/N
    subt = t[ind2] # subx and subt must from the second component
    lsx = length(subx)
    hx = stats::median(abs(subx - stats::median(subx)))/0.6745 * (4/3/lsx)^0.2
    hy = stats::median(abs(subt - stats::median(subt)))/0.6745 * (4/3/lsx)^0.2
    h = sqrt(hy * hx)
    #SK-- 9/4/2023
    acc1 = min(tol, tol * h)
    #if (is.null(acc1)) {
    #  acc1 = min(0.01, 0.01 * h)
    #}
    difh = acc1 + 1

    while (difh > acc1) {
      preh = h
      PT = numeric()
      W = numeric()
      for (i in 1:ltg) {
        dif = 1
        while (dif > acc) {
          prew = w
          # M-step to find pt and w
          temp = stats::dnorm(t, tg[i], h) * (1 - p)
          pt = sum(temp * x) / (sum(temp)) / N
          temp1 = stats::dnorm(t, tg[i], h) %*% p
          w = sum(temp1) / (sum(temp1) + sum(temp))
          dif = abs(w - prew)
          if (length(dif) > 1) {
            dif = min(dif)
          }
          p[ind1] = w/(w + (1 - w) * gamma(N + 1)/gamma(x[ind1] + 1)/gamma(N + 1 - x[ind1]) * pt^x[ind1] * (1 - pt)^(N - x[ind1]))
        }
        PT[i] = pt
        W[i] = 1 - w
      }
      out = list(pt = PT, wt = W)

      w = stats::approx(tg, out$w, t)$y
      pt = stats::approx(tg, out$pt, t)$y
      p[ind1] = w[ind1]/(w[ind1] + (1 - w[ind1]) * gamma(N + 1)/gamma(x[ind1] + 1)/gamma(N + 1 - x[ind1]) * pt[ind1]^x[ind1] * (1 - pt[ind1])^(N - x[ind1]))
      lsx = lx - sum(p)
      mx = (1 - p) %*% t(x)/lsx
      mx = as.numeric(mx)
      mt = (1 - p) %*% t(t)/lsx
      mt = as.numeric(mt)
      hx = sqrt((1 - p) %*% t(x - mx)^2/lsx)/N * (4/3/lsx)^0.2
      hy = sqrt((1 - p) %*% t(t - mt)^2/lsx)/N * (4/3/lsx)^0.2
      h = sqrt(hy * hx)
      difh = abs(h - preh)
      difh = as.numeric(difh)
    }

  } else {
    PT = numeric()  #SK-- 9/4/2023
    W = numeric()  #SK-- 9/4/2023
    # tune means the percentage of data included for local estimation
    h = tune * (range(t)[2] - range(t)[1])
    for (i in 1:ltg) {
      dif = 1
      while (dif > acc) {
        prew = w
        temp = stats::dnorm(t, tg[i], h) * (1 - p)
        pt = sum(temp * x)/(sum(temp))/N
        temp1 = stats::dnorm(t, tg[i], h) %*% p
        w = sum(temp1) / (sum(temp1) + sum(temp))
        dif = abs(w - prew)
        p[ind1] = w/(w + (1 - w) * gamma(N + 1)/gamma(x[ind1] + 1)/gamma(N + 1 - x[ind1]) * pt^x[ind1] * (1 - pt)^(N - x[ind1]))
      }
      PT[i] = pt
      W[i] = 1 - w
      out = list(pt = PT, wt = W)
    }
  }

  out$h = as.numeric(h)
  return(out)
}

#' Semiparametric Mixture of Binomial Regression with a Degenerate Component
#' with Constant Proportion and Time-Varying Success Probability (One-step Backfitting)
#'
#' `semimrBinOne' implements the one-step backfitting method (Cao and Yao, 2012)
#' for semiparametric estimation of a mixture of binomial distributions with one degenerate component,
#' with constant proportion and time-varying success probability.
#'
#' @usage
#' semimrBinOne(t, x, N, tg = NULL, tune = 1, tol = 1e-02)
#'
#' @details
#' The semiparametric mixture of binomial regression model is as follows:
#' \deqn{w \times B(N,p(t))+(1-w)\times B(N,0),}
#' where \eqn{B(N,p)} is the probability mass function of a binomial distribution
#' with the number of trials \eqn{N} and the success probability \eqn{p}.
#' Here, the second component is a degenerate distribution with mass 1 on 0.
#' The time-varying success probability \eqn{p(t)} for the binomial components
#' are estimated by the kernel regression using one-step estimation for faster computation with some bandwidth.
#'
#' @param t a vector of time variable along which \eqn{p(t)} varies.
#' @param x a vector of observed number of successes. The length of \code{t} and \code{x} must be the same.
#' @param N a scalar, specifying the number of trials for the Binomial distribution.
#' @param tg grid points of time used in the kernel regression for the estimation of \eqn{p(t)}.
#'   Default is NULL, and 100 equally spaced grid points will automatically generated
#'   using the minimum and maximum values of \code{t}.
#' @param tune a scalar, specifying the percentage of data included in local estimation.
#'    related to the bandwidth selection and local estimation. Default is 1.
#' @param tol stopping criteria for the algorithm.
#'
#' @return A list containing the following elements:
#'   \item{pt}{estimated time-varying success probabilities for the first component.}
#'   \item{w}{estimated constant proportion for the first component.}
#'   \item{h}{bandwidth for the kernel regression. The bandwidth calculation can be found in Section 4 of Cao and Yao (2012).}
#'
#' @seealso \code{\link{semimrBin}}, \code{\link{semimrBinFull}}
#'
#' @references
#'   Cao, J. and Yao, W. (2012). Semiparametric mixture of binomial regression with a degenerate component.
#'   Statistica Sinica, 27-46.
#'
#' @export
#' @examples
#' nobs = 50
#' tobs = seq(from = 0, to = 1, length.out = nobs)
#' pi1Tru = 0.4
#' ptTru = 0.3 * (1.5 + cos(2 * pi * tobs))
#' nfine = nobs
#' tfine = seq(from = 0, to = 1, length.out = nfine)
#' b = rbinom(nobs, size = 1, pi1Tru)
#' yobs = apply(X = matrix(ptTru), 1, rbinom, n = 1, size = 7)
#' yobs = ifelse(b == 1, 0, yobs)
#' ftonestep = semimrBinOne(t = tobs, x = yobs, N = 7, tg = tfine)
semimrBinOne <- function(t, x, N, tg = NULL, tune = 1, tol = 1e-02){

  if(length(x) != length(t)){
    errorCondition("x and t are in different sizes")
  }

  #SK-- 9/4/2023
  if (is.null(tg)) {
    u = seq(from = min(t), to = max(t), length = 100)
  }

  # make x and t row vector
  x = as.matrix(x)
  t = as.matrix(t)
  if(dim(x)[1] == 1){
    x = x; t = t
  }else{
    x = t(x); t = t(t)
  }

  lx = length(x)
  temp = 1:lx
  ind1 = temp[x == 0]; ind2 = temp[x != 0]
  out = semimrBin(t, x, N, tg, tune, 1)
  pt = out$pt
  w = mean(out$wt)
  p = numeric()
  p[ind2] = 0
  acc = tol
  #acc = 0.001
  dif = 1

  while(dif > acc){
    prew = w
    # M-step to find pt and w
    p[ind1] = w/(w + (1 - w) * gamma(N + 1)/gamma(x[ind1] + 1)/gamma(N + 1 - x[ind1]) * pt[ind1]^x[ind1]*(1 - pt[ind1])^(N - x[ind1]))
    w = sum(p)/lx
    dif = abs(w - prew)
  }
  lsx = lx - sum(p)
  mx = (1 - p) %*% t(x)/lsx; mx = as.numeric(mx)
  mt = (1 - p) %*% t(t)/lsx; mt = as.numeric(mt)
  hx = sqrt((1 - p) %*% t(x - mx)^2/lsx)/N * (4/3/lsx)^0.2
  hy = sqrt((1 - p) %*% t(t - mt)^2/lsx)/N * (4/3/lsx)^0.2
  h = sqrt(hy * hx)
  run = 0
  dif = 1

  while(dif > acc && run < 1000){
    prep = pt
    run = run + 1
    # M step to find pt and w
    pt = numeric()
    for(i in 1:lx){
      temp = stats::dnorm(t, t[i], h) * (1 - p)
      pt[i] = sum(temp * x)/(sum(temp))/N
    }
    dif = max(abs(pt - prep))
    p[ind1] = w/(w + (1 - w) * gamma(N + 1)/gamma(x[ind1] + 1)/gamma(N + 1 - x[ind1]) * pt[ind1]^x[ind1] * (1 - pt[ind1])^(N - x[ind1]))
  }

  pt = stats::approx(t, pt, tg)$y
  out = list(pt = pt, w = w, h = as.numeric(h))
  return(out)
}

#' Semiparametric Mixture of Binomial Regression with a Degenerate Component
#' with Constant Proportion and Time-Varying Success Probability (Backfitting)
#'
#' `semimrBinFull' implements the backfitting method (Cao and Yao, 2012)
#' for semiparametric estimation of a mixture of binomial distributions with one degenerate component,
#' with constant proportion and time-varying success probability \eqn{p}.
#'
#' @usage
#' semimrBinFull(t, x, N, tg = NULL, tune = 1, tol = 1e-02)
#'
#' @details
#' The semiparametric mixture of binomial regression model is as follows:
#' \deqn{w \times (N,p(t))+(1-w)\times B(N,0),}
#' where \eqn{B(N,p)} is the probability mass function of a binomial distribution
#' with the number of trials \eqn{N} and the success probability \eqn{p}.
#' Here, the second component is a degenerate distribution with mass 1 on 0.
#' The time-varying success probability \eqn{p(t)} for the binomial components
#' are estimated by the kernel regression using a full iterative backfitting procedure with some bandwidth.
#'
#' @param t a vector of time variable along which \eqn{p(t)} varies.
#' @param x a vector of observed number of successes. The length of \code{t} and \code{x} must be the same.
#' @param N a scalar, specifying the number of trials for the Binomial distribution.
#' @param tg grid points of time used in the kernel regression for the estimation of \eqn{p(t)}.
#'   Default is NULL, and 100 equally spaced grid points will automatically generated
#'   using the minimum and maximum values of \code{t}.
#' @param tune a scalar, specifying the percentage of data included in local estimation.
#'    related to the bandwidth selection and local estimation. Default is 1.
#' @param tol stopping criteria for the algorithm.
#'
#' @return A list containing the following elements:
#'   \item{pt}{estimated time-varying success probabilities for the first component.}
#'   \item{w}{estimated constant proportion for the first component.}
#'   \item{h}{bandwidth for the kernel regression. The bandwidth calculation can be found in Section 4 of Cao and Yao (2012).}
#'
#' @seealso \code{\link{semimrBin}}, \code{\link{semimrBinOne}}
#'
#' @references
#'   Cao, J. and Yao, W. (2012). Semiparametric mixture of binomial regression with a degenerate component.
#'   Statistica Sinica, 27-46.
#'
#' @export
#' @examples
#' nobs = 50
#' tobs = seq(from = 0, to = 1, length.out = nobs)
#' pi1Tru = 0.4
#' ptTru = 0.3 * (1.5 + cos(2 * pi * tobs))
#' nfine = nobs
#' tfine = seq(from = 0, to = 1, length.out = nfine)
#' b = rbinom(nobs, size = 1, pi1Tru)
#' yobs = apply(X = matrix(ptTru), 1, rbinom, n = 1, size = 7)
#' yobs = ifelse(b == 1, 0, yobs)
#' ftfull = semimrBinFull(t = tobs, x = yobs, N = 7, tg = tfine)
semimrBinFull <- function(t, x, N, tg = NULL, tune = 1, tol = 1e-02){

  if(length(x) != length(t)){
    errorCondition("x and t are in different sizes")
  }
  #if(is.null(tune)){
  #  tune = 1
  #}

  #SK-- 9/4/2023
  if (is.null(tg)) {
    u = seq(from = min(t), to = max(t), length = 100)
  }

  # make x and t row vector
  x = as.matrix(x)
  t = as.matrix(t)
  if(dim(x)[1] == 1){
    x = x; t = t
  }else{
    x = t(x); t = t(t)
  }

  lx = length(x)
  temp = 1:lx
  ind1 = temp[x == 0]; ind2 = temp[x != 0]
  out = semimrBin(t, x, N, tg, tune = tune)
  pt = out$pt
  w = mean(out$wt)
  p = numeric()
  p[ind2] = 0
  acc = tol
  #acc = 0.001
  dif1 = 1
  numiter = 0

  while(dif1 > acc * 0.01 && numiter < 500){
    prew1 = w
    dif = 1
    numiter = numiter + 1

    while(dif > acc){
      prew = w
      # M step to find pt and w
      p[ind1] = w/(w + (1 - w) * gamma(N + 1)/gamma(x[ind1] + 1)/gamma(N + 1 - x[ind1]) * pt[ind1]^x[ind1] * (1 - pt[ind1])^(N - x[ind1]))
      w = sum(p)/lx
      dif = abs(w - prew)
    }
    lsx = lx - sum(p)
    mx = (1 - p) %*% t(x)/lsx; mx = as.numeric(mx)
    mt = (1 - p) %*% t(t)/lsx; mt = as.numeric(mt)
    hx = sqrt((1 - p) %*% t(x - mx)^2/lsx)/N * (4/3/lsx)^0.2
    hy = sqrt((1 - p) %*% t(t - mt)^2/lsx)/N * (4/3/lsx)^0.2
    h = sqrt(hy * hx)
    dif = 1
    run = 0

    while(dif > acc && run < 1000){
      prep = pt
      run = run + 1
      # M step to find pt and w
      pt = numeric()
      for(i in 1:lx){
        temp = stats::dnorm(t, t[i], h) * (1 - p)
        pt[i] = sum(temp * x)/(sum(temp))/N
      }
      dif = max(abs(pt - prep))
      p[ind1] = w/(w + (1 - w) * gamma(N + 1)/gamma(x[ind1] + 1)/gamma(N + 1 - x[ind1]) * pt[ind1]^x[ind1] * (1 - pt[ind1])^(N - x[ind1]))
    }
    dif1 = abs(prew1 - w)
  }

  pt = stats::approx(t, pt, tg)$y
  out = list(pt = pt, w = w, h = as.numeric(h))
  return(out)
}
