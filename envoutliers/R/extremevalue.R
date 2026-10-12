#' Return level estimation - Only intended for developer use
#'
#' Estimation of return level for a given threshold value using Peaks Over Threshold model.
#' The function is called by \code{\link{KRDetect.outliers.EV}} and is not intended for use by regular users of the package.
#'
#' @param r a return period.
#' @param u a threshold value.
#' @param sigma_u a scale parameter of Generalized Pareto distribution.
#' @param xi a shape parameter of Generalized Pareto distribution.
#' @param lambda_u a relative frequency of the number of threshold value exceedances.
#' @param theta an extremal index.
#' @details This function computes the estimate of return level for a given threshold value using Peaks Over Threshold model.
#' The function is exported for developer use only. It does not perform any checks on inputs since it is only convenience function used within \code{\link{KRDetect.outliers.EV}}.
#' @return A numeric value of return lever corresponding to return period \code{r}
#' @references Coles S (2001). An Introduction to Statistical Modeling of Extreme Values. 3 edition. London: Springer. ISBN 1-85233-459-2.
#'
#' Pickands J (1975). Statistical inference using extreme order statistics. The Annals of Statistics, 3(1), 119-131.
return.level.est <- function(r,
                             u,
                             sigma_u,
                             xi,
                             lambda_u,
                             theta) {
  z_r = u + (sigma_u / xi) * ((lambda_u ^ (-1) * (1 - (1 - r ^ (-1)) ^ (theta ^ (-1)))) ^ (-xi) - 1)

  return(z_r)
}

#' Extremal index estimation (Gomes, 1993) - Only intended for developer use
#'
#' Estimation of an extremal index using the block maxima approach suggested by (Gomes, 1993).
#' The function is called by \code{\link{KRDetect.outliers.EV}} and is not intended for use by regular users of the package.
#'
#' @param x a numeric vector of observations.
#' @param block.length a numeric value giving the length of blocks.
#' @details This function computes the estimate of extremal index suggested by (Gomes, 1993).
#' The function is exported for developer use only. It does not perform any checks on inputs since it is only convenience function used within \code{\link{KRDetect.outliers.EV}}.
#' @return A numeric value of an extremal index estimate
#' @references Gomes M (1993). On the estimation of parameter of rare events in environmental time series. In Statistics for the Environment, volume 2 of Water Related Issues, pp. 225-241. Wiley.
#'
#' Heffernan JE, Stephenson AG (2016). ismev: An Introduction to Statistical Modeling of Extreme Values. R package version 1.41, URL http://CRAN.R-project.org/package=ismev.
#' @importFrom ismev gev.fit
extremal.index.gomes <- function(x,
                                 block.length) {
  blocks = seq(1, length(x), block.length)
  k = length(blocks)

  # stationary data
  max.stat = rep(0, k)
  for (i in 1:k) {
    if (i < k) {
      max.stat[i] = max(x[c(blocks[i]:(blocks[(i + 1)] - 1))])
    } else {
      max.stat[i] = max(x[c(blocks[i]:length(x))])
    }
  }

  # permuted data
  max.perm = rep(0, k)
  set.seed(1)
  x.permut = sample(x)

  for (i in 1:k) {
    if (i <  k) {
      max.perm[i] = max(x.permut[c(blocks[i]:(blocks[(i + 1)] - 1))])
    } else {
      max.perm[i] = max(x.permut[c(blocks[i]:length(x))])
    }
  }

  # gev.fit for stationary data
  gev.model = gev.fit(max.stat, show = FALSE)
  stat.sigma = gev.model$mle[2]
  stat.mu = gev.model$mle[1]

  # gev.fit for permuted data
  gev.model = gev.fit(max.perm, show = FALSE)
  perm.sigma = gev.model$mle[2]
  perm.mu = gev.model$mle[1]

  # extremal index estimation
  xi = (perm.sigma - stat.sigma) / (perm.mu - stat.mu)
  extremal.index = min(1, (perm.sigma / stat.sigma) ^ (-1 / xi))

  return(extremal.index)
}

#' Extremal index estimation (Ferro and Segers, 2003) - Only intended for developer use
#'
#' Estimation of an extremal index using the Intervals estimator suggested in (Ferro and Segers, 2003).
#' The function is called by \code{\link{KRDetect.outliers.EV}} and is not intended for use by regular users of the package.
#'
#' @param x a numeric vector of observations.
#' @param u a numeric value giving threshold
#' @details This function computes the estimate of extremal index suggested in (Ferro and Segers, 2003).
#' The function is exported for developer use only. It does not perform any checks on inputs since it is only convenience function used within \code{\link{KRDetect.outliers.EV}}.
#' @return a numeric value of an extremal index estimate
#' @references Ferro, CAT, Segers, J (2003). Inference for Cluster of Extreme Values. Journal of Royal Statistical Society, Series B, 65(2), 545-556.
extremal.index.intervals <- function(x,
                                     u) {
  j = which(x > u)
  T = diff(j)

  if (max(T) <= 2) {
    theta = min(1, (2 * (sum(T)) ^ 2) / (length(T) * sum(T ^ 2)))
  } else {
    theta = min(1, (2 * (sum(T - 1)) ^ 2) / (length(T) * sum((T - 1) * (T - 2))))
  }

  return(theta)
}

#' Extremal index estimation (Holesovsky and Fusek, 2020) - Only intended for developer use
#'
#' Estimation of an extremal index using the censored estimator suggested in (Holesovsky and Fusek, 2020).
#' The function is called by \code{\link{KRDetect.outliers.EV}} and is not intended for use by regular users of the package.
#'
#' @param x a numeric vector of observations.
#' @param u a numeric value giving threshold.
#' @param D a nonnegative integer giving the value of D parameter (Holesovsky and Fusek, 2020)
#' @details This function computes the censored estimate of extremal index suggested in (Holesovsky and Fusek, 2020).
#' The function is exported for developer use only. It does not perform any checks on inputs since it is only convenience function used within \code{\link{KRDetect.outliers.EV}}.
#' @return a numeric value of an extremal index estimate
#' @references Holesovsky, J, Fusek, M (2020). Estimation of the Extremal Index Using Censored Distributions. Extremes, DOI: 10.1007/s10687-020-00374-3.
#' @importFrom stats optimize
extremal.index.censored <- function(x,
                                    u,
                                    D) {
  j = which(x > u)
  T = diff(j)
  N = length(j)
  n = length(x)
  Nc = length(T[T > D])
  d = length(j) * D / length(x)
  T.sort = sort(T)

  log.likelihood = function(theta) {
    logl = (N - 1 - Nc) * log(1 - theta * exp(-theta * d)) + 2 * Nc * log(theta) - theta * sum((N / n) * T.sort[(N - Nc) : (N - 1)])

    return(logl)
  }

  result = optimize(log.likelihood, c(0, 1), maximum = TRUE)

  return(result$maximum)
}

#' Extremal index estimation (Suveges and Davison, 2010) - Only intended for developer use
#'
#' Estimation of an extremal index using the K-gaps estimator suggested in (Suveges and Davison, 2010).
#' The function is called by \code{\link{KRDetect.outliers.EV}} and is not intended for use by regular users of the package.
#'
#' @param x a numeric vector of observations.
#' @param u a numeric value giving threshold.
#' @param K a nonnegative integer giving the value of K parameter (Suveges and Davison, 2010).
#' @details This function computes the K-gaps estimate of extremal index suggested in (Suveges and Davison, 2010).
#' The function is exported for developer use only. It does not perform any checks on inputs since it is only convenience function used within \code{\link{KRDetect.outliers.EV}}.
#' @return a numeric value of an extremal index estimate
#' @references Suveges, M, Davison, AC (2010). Model Misspecification in Peaks Over Threshold Analysis. The Annals of Applied Statistics, 4(1), 203-221.
#' @importFrom stats optimize
extremal.index.Kgaps <- function(x,
                                 u,
                                 K) {
  theta0 = extremal.index.intervals(x, u)
  j = which(x > u)
  T = diff(j)
  N = length(j)
  n = length(x)
  S = rep(0, length(T))

  for (i in 1:length(T)) {
    S[i] = max(T[i] - K, 0)
  }

  Nc = length(S[S != 0])

  log.likelihood = function(theta) {
    logl = (N - 1 - Nc) * log(1 - theta) + 2 * Nc * log(theta) - theta * sum((N / n) * S)

    return(logl)
  }

  result = optimize(log.likelihood, c(0, 1), maximum = TRUE)

  return(result$maximum)
}

#' Extremal index estimation (Northrop, 2015) - Only intended for developer use
#'
#' Estimation of an extremal index using the sliding blocks estimator suggested in (Northrop, 2015).
#' The function is called by \code{\link{KRDetect.outliers.EV}} and is not intended for use by regular users of the package.
#'
#' @param x a numeric vector of observations.
#' @param b a numeric value giving the length of blocks. Default is \code{b = round(sqrt(n))}.
#' @details This function computes the sliding blocks estimate of extremal index suggested in (Northrop, 2015).
#' The function is exported for developer use only. It does not perform any checks on inputs since it is only convenience function used within \code{\link{KRDetect.outliers.EV}}.
#' @return a numeric value of an extremal index estimate
#' @references Northrop, PJ (2015). An Efficient Semiparametric Maxima Estimator of the Extremal Index. Extremes, 18, 585-603.
extremal.index.sliding.blocks <- function(x,
                                          b = round(sqrt(length(x)))) {
  n = length(x)
  m = n - b + 1
  M = rep(0, m)

  for (i in 1:length(M)) {
    M[i] = max(x[i : (i + b - 1)])
  }

  x.sorted = sort(x, decreasing = TRUE)
  R = rep(0, m)

  for (i in 1:length(R)) {
    R[i] = mean(which(M[i] == x.sorted))
  }

  F = rep(0, m)

  for (i in 1:length((F))) {
    if (R[i] <= n - b) {
      F[i] = (n - b + 1 - R[i]) / (n - b + 1)
    }

    if (R[i] > n - b) {
      F[i] = (1) / (n - b + m + 1)
    }
  }

  theta = 1 / mean(-b * log(F))

  return(theta)
}

#' Extremal index estimation (Smith and Weissman, 1994) - Only intended for developer use
#'
#' Estimation of an extremal index using the runs estimator suggested in (Smith and Weissman, 1994).
#' The function is called by \code{\link{KRDetect.outliers.EV}} and is not intended for use by regular users of the package.
#'
#' @param x a numeric vector of observations.
#' @param u a numeric value giving threshold.
#' @param r a positive integer giving the value of runs parameter (Smith and Weissman, 1994).
#' @details This function computes the runs estimate of extremal index suggested in (Smith and Weissman, 1994).
#' The function is exported for developer use only. It does not perform any checks on inputs since it is only convenience function used within \code{\link{KRDetect.outliers.EV}}.
#' @return a numeric value of an extremal index estimate
#' @references Smith, RL, Weissman, I (1994). Estimating the Extremal Index. Journal of the Royal Statistical Society, Series B, 56, 515-529.
extremal.index.runs <- function(x,
                                u,
                                r) {
  n = length(x)
  N = length(x[x > u])
  C = rep(0, n - r)

  for (i in 1:length(C)) {
    if ((x[i] > u) & (length(which(x[(i + 1) : (i + r)] <= u)) == r)) {
      C[i] = 1
    }
  }

  theta = sum(C / N)

  return(theta)
}

#' Moment estimates of GP distribution parameters - Only intended for developer use
#'
#' Moment estimates of shape and scale parameters of GP distribution using the approach presented in (de Haan and Ferreira, 2006).
#' The function is called by \code{\link{KRDetect.outliers.EV}} and is not intended for use by regular users of the package.
#'
#' @param x a numeric vector of observations.
#' @param k a positive integer giving the number of top rank statistics (de Haan and Ferreira, 2006). Default is \code{k = round(length(x) * 0.1)}.
#' @details This function computes the moment estimates of shape and scale parameters of GP distribution (de Haan and Ferreira, 2006).
#' The function is exported for developer use only. It does not perform any checks on inputs since it is only convenience function used within \code{\link{KRDetect.outliers.EV}}.
#' @return a numeric vector giving the moment estimates for the scale and shape parameters, resp.
#' a numeric vector giving the standard deviations for the scale and shape parameter estimates, resp.
#' @references de Haan, L, Ferreira, A (2006). Extreme Value Theory: An Introduction. Springer.
Moment.gpd.fit <- function(x,
                           k = round(length(x) * 0.1)) {
  n = length(x)
  x.sort = sort(x)

  Mnj = function(j) {
    sum = 0

    for (i in 0:(k - 1)) {
      sum = sum + (log(x.sort[n - i]) - log(x.sort[n - k])) ^ j
    }

    return(sum/k)
  }

  xi = Mnj(1) + 1 - 0.5 * (1 - (Mnj(1)) ^ 2 / Mnj(2)) ^ (-1) # xi estimate
  sigma = x.sort[n - k] * Mnj(1) * (1 - xi + Mnj(1))  # sigma estimate

  if (xi >= 0) {
    var.xi = (1 / k) * (xi ^ 2 + 1)
    var.sigma = (sigma ^ 2 / k * (xi ^ 2 + 2))
  }

  if (xi < 0) {
    var.xi = (1 / k) * (1 - xi) ^ 2 * (1 - 2 * xi) * (1 - xi + 6 * xi ^ 2) / ((1 - 3 * xi) * (1 - 4 * xi))
    var.sigma = (sigma ^ 2 / k) * (2 - 16 * xi + 51 * xi ^ 2 - 69 * xi ^ 3 + 50 * xi ^ 4 - 24 * xi ^5) / ((1 - 2 * xi) * (1 - 3 * xi) * (1 - 4 * xi))
  }

  est = c(sigma, xi)
  sd.est = c(sqrt(var.sigma), sqrt(var.xi))
  result = list(est = est, sd = sd.est)

  return(result)
}

#' Identification of outliers using extreme value theory
#'
#' Identification of outliers in environmental data using semiparametric method based on kernel smoothing and extreme value theory (Holesovsky et al., 2018). The outliers are identified as observations whose values are exceeded on average once a given period that is specified by the user.
#'
#' @param x data values.
#' Supported data types
#' \itemize{
#'   \item{a numeric vector}
#'   \item{a time series object \code{ts}}
#'   \item{a time series object \code{xts}}
#'   \item{a time series object \code{zoo}}
#' }
#' @param perform.smoothing a logical value specifying if data smoothing is performed. If \code{TRUE} (default), data are smoothed.
#' @param bandwidth.type a character string specifying the type of bandwidth.
#'
#' Possible options are
#' \itemize{
#'   \item{\code{"local"}} {(default) to use local bandwidth}
#'   \item{\code{"global"}} {to use global bandwidth}
#' }
#' @param bandwidth.value a local bandwidth array (for \code{bandwidth.type = "local"}) or global bandwidth value (for \code{bandwidth.type = "global"}) for kernel regression estimation. If \code{bandwidth.type = "NULL"} (default) a data-adaptive local plug-in (Herrmann, 1997) (for \code{bandwidth.type = "local"}) or data-adaptive global plug-in (Gasser et al., 1991) (for \code{bandwidth.type = "global"}) bandwidth is used instead.
#' @param kernel.order a nonnegative integer giving the order of the optimal kernel (Gasser et al., 1985) used for smoothing.
#'
#' Possible options are
#' \itemize{
#'   \item{\code{kernel.order = 2}} {(default)}
#'   \item{\code{kernel.order = 4}}
#' }
#' @param gpd.fit.method a character string specifying the method used for the estimate of the scale and shape parameters of GP distribution.
#'
#' Possible options are
#' \itemize{
#'   \item{\code{"mle"}} {(default) for maximum likelihood estimates (Coles, 2001)}
#'   \item{\code{"moment"}} {for moment estimates (de Haan and Ferreira2006)}
#' }
#' @param threshold.min a threshold value for residuals with low values, that is used to find the maximum likelihood estimates of shape and scale parameters of GP distribution and selected types of extremal index estimates (specifically: Intervals estimator (Ferro and Segers, 2003), censored estimator, (Holesovsky and Fusek, 2020), K-gaps estimator (Suveges and Davison, 2010), runs estimator (Smith and Weissman, 1994)). If \code{threshold.min = NULL} (default), threshold is estimated as 90\% quantile of smoothing residuals.
#' @param threshold.max a threshold value for residuals with high values, that is used to find the maximum likelihood estimates of shape and scale parameters of GP distribution and selected types of extremal index estimates (specifically: Intervals estimator (Ferro and Segers, 2003), censored estimator, (Holesovsky and Fusek, 2020), K-gaps estimator (Suveges and Davison, 2010), runs estimator (Smith and Weissman, 1994)). If \code{threshold.max = NULL} (default), threshold is estimated as 90\% quantile of smoothing residuals.
#' @param k.min a positive integer for residuals with low values giving the number of largest order statistics used to find the moment estimates (de Haan and Ferreira, 2006) of shape and scale parameters of GP distribution. Default is \code{k.min = round(length(x) * 0.1)}.
#' @param k.max a positive integer for residuals with high values giving the number of largest order statistics used to find the moment estimates (de Haan and Ferreira, 2006) of shape and scale parameters of GP distribution. Default is \code{k.max = round(length(x) * 0.1)}.
#' @param extremal.index.min a numeric value giving the extremal index for identification of outliers with extremely low value. If \code{extremal.index.min = NULL} (default), the extremal index is estimated using the method specified by the parameter \code{extremal.index.type}.
#' @param extremal.index.max a numeric value giving the extremal index for identification of outliers with extremely high value. If \code{extremal.index.max = NULL} (default), the extremal index is estimated using the method specified by the parameter \code{extremal.index.type}.
#' @param extremal.index.type a character string specifying the type of extremal index estimate.
#'
#' Possible options are
#' \itemize{
#'   \item{\code{"block.maxima"}} {(default) for block maxima estimator (Gomes, 1993).}
#'   \item{\code{"intervals"}} {for intervals estimator (Ferro and Segers, 2003).}
#'   \item{\code{"censored"}} {for censored estimator (Holesovsky and Fusek, 2020).}
#'   \item{\code{"Kgaps"}} {for K-gaps estimator (Suveges and Davison, 2010).}
#'   \item{\code{"sliding.blocks"}} {for sliding blocks estimator (Northrop, 2015).}
#'   \item{\code{"runs"}} {for runs estimator (Smith and Weissman, 1994).}
#' }
#' @param block.length.min a numeric value for residuals with low values giving the length of blocks for estimation of extremal index. Only required for \code{extremal.index.type = "block.maxima"} and \code{extremal.index.type = "sliding.blocks"}. Default is \code{block.length.min = round(sqrt(length(x)))}.
#' @param block.length.max a numeric value for residuals with high values giving the length of blocks for estimation of extremal index. Only required for \code{extremal.index.type = "block.maxima"} and \code{extremal.index.type = "sliding.blocks"}. Default is \code{block.length.max = round(sqrt(length(x)))}.
#' @param D.min a nonnegative integer for residuals with low values giving the value of D parameter used for censored extremal index estimate (Holesovsky and Fusek, 2020). Only required for \code{extremal.index.type = "censored"}.
#' @param D.max a nonnegative integer for residuals with high values giving the value of D parameter used for censored extremal index estimate (Holesovsky and Fusek, 2020). Only required for \code{extremal.index.type = "censored"}.
#' @param K.min a nonnegative integer for residuals with low values giving the value of K parameter used for K-gaps extremal index estimate (Suveges and Davison, 2010). Only required for \code{extremal.index.type = "Kgaps"}.
#' @param K.max a nonnegative integer for residuals with high values giving the value of K parameter used for K-gaps extremal index estimate (Suveges and Davison, 2010). Only required for \code{extremal.index.type = "Kgaps"}.
#' @param r.min a positive integer for residuals with low values giving the value of runs parameter of runs extremal index estimate (Smith and Weissman, 1994). Only required for \code{extremal.index.type = "runs"}.
#' @param r.max a positive integer for residuals with high values giving the value of runs parameter of runs extremal index estimate (Smith and Weissman, 1994). Only required for \code{extremal.index.type = "runs"}.
#' @param return.period a positive numeric value giving return period. Default is \code{r = 120}, which means that observations whose values are exceeded on average once every 120 observations are detected as outliers.
#' @details This function identifies outliers in time series using two-step procedure (Holesovsky et al., 2018). The procedure consists of kernel smoothing and extreme value estimation of high threshold exceedances for smoothing residuals.
#' Outliers with both extremely high and extremely low values are identified.
#' Crucial for the method is the choice of return period - parameter defining the criterion for outliers detection.
#' The outliers with extremely high values are detected as observations whose values are exceeded on average once a given return.period of observations. Analogous, the outliers with extremely low values are identified.
#' @return A \code{"KRDetect"} object which contains a list with elements:
#' \item{method.type}{a character string giving the type of method used for outlier idetification}
#' \item{x}{a numeric vector of observations}
#' \item{index}{a numeric vector of index design points assigned to individual observations}
#' \item{smoothed}{a numeric vector of estimates of the kernel regression function (smoothed data)}
#' \item{GPD.fit.method}{the method used for the estimate of the scale and shape parameters of GP distribution}
#' \item{extremal.index.type}{the type of extremal index estimate used for the identification of outliers}
#' \item{sigma.min}{a numeric value giving scale parameter of Generalised Pareto distribution used for identification of outliers with extremely low value}
#' \item{sigma.max}{a numeric value giving scale parameter of Generalised Pareto distribution used for identification of outliers with extremely high value}
#' \item{xi.min}{a numeric value giving shape parameter of Generalised Pareto distribution used for identification of outliers with extremely low value}
#' \item{xi.max}{a numeric value giving shape parameter of Generalised Pareto distribution used for identification of outliers with extremely high value}
#' \item{lambda_u.min}{a numeric value giving relative frequency of the number of threshold value exceedances and identification of outliers with extremely low value. The value of the parameter is returned only for \code{gpd.fit.method = "mle"}.}
#' \item{lambda_u.max}{a numeric value giving relative frequency of the number of threshold value exceedances and identification of outliers with extremely high value. The value of the parameter is returned only for \code{gpd.fit.method = "mle"}.}
#' \item{extremal.index.min}{a numeric value giving extremal index used for identification of outliers with extremely low value}
#' \item{extremal.index.max}{a numeric value giving extremal index used for identification of outliers with extremely high value}
#' \item{threshold.min}{a numeric value giving threshold value used for identification of outliers with extremely low value.}
#' \item{threshold.max}{a numeric value giving threshold value used for identification of outliers with extremely high value.}
#' \item{return.level.min}{a numeric value giving return level used for identification of outliers with extremely low value}
#' \item{return.level.max}{a numeric value giving return level used for identification of outliers with extremely high value}
#' \item{outlier.min}{a logical vector specyfing the identified outliers with extremely low value. \code{TRUE} means that corresponding observation from vector \code{x} is detected as outlier}
#' \item{outlier.max}{a logical vector specyfing the identified outliers with extremely high value. \code{TRUE} means that corresponding observation from vector \code{x} is detected as outlier}
#' \item{outlier}{a logical vector specyfing the identified outliers with both extremely low and extremely high value. \code{TRUE} means that corresponding observation from vector \code{x} is detected as outlier}
#' @references Holesovsky J, Campulova M, Michalek J (2018). Semiparametric Outlier Detection in Nonstationary Times Series: Case Study for Atmospheric Pollution in Brno, Czech Republic. Atmospheric Pollution Research, 9(1).
#'
#' Theo Gasser, Alois Kneip & Walter Koehler (1991) A flexible and fast method for automatic smoothing. Journal of the American Statistical Association 86, 643-652. https://doi.org/10.2307/2290393
#'
#' E. Herrmann (1997) Local bandwidth choice in kernel regression estimation. Journal of Graphical and Computational Statistics 6, 35-54.
#'
#' Herrmann E, Maechler M (2013). lokern: Kernel Regression Smoothing with Local or Global Plug-in Bandwidth. R package version 1.1-5, URL http://CRAN.R-project.org/package=lokern.
#'
#' Gasser, T, Muller, H-G, Mammitzsch, V (1985). Kernels for nonparametric curve estimation. Journal of the Royal Statistical Society, B Met., 47(2), 238-252.
#'
#' Gomes M (1993). On the estimation of parameter of rare events in environmental time series. In Statistics for the Environment, volume 2 of Water Related Issues, pp. 225-241. Wiley.
#'
#' Ferro, CAT, Segers, J (2003). Inference for Cluster of Extreme Values. Journal of Royal Statistical Society, Series B, 65(2), 545-556.
#'
#' Holesovsky, J, Fusek, M (2020). Estimation of the Extremal Index Using Censored Distributions. Extremes, In Press.
#'
#' Suveges, M, Davison, AC (2010). Model Misspecification in Peaks Over Threshold Analysis. The Annals of Applied Statistics, 4(1), 203-221.
#'
#' Northrop, PJ (2015). An Efficient Semiparametric Maxima Estimator of the Extremal Index. Extremes, 18, 585-603.
#'
#' Smith, RL, Weissman, I (1994). Estimating the Extremal Index. Journal of the Royal Statistical Society, Series B, 56, 515-529.
#'
#' Heffernan JE, Stephenson AG (2016). ismev: An Introduction to Statistical Modeling of Extreme Values. R package version 1.41, URL http://CRAN.R-project.org/package=ismev.
#'
#' Coles S (2001). An Introduction to Statistical Modeling of Extreme Values. 3 edition. London: Springer. ISBN 1-85233-459-2.
#'
#' de Haan, L, Ferreira, A (2006). Extreme Value Theory: An Introduction. Springer.
#'
#' Pickands J (1975). Statistical inference using extreme order statistics. The Annals of Statistics, 3(1), 119-131.
#' @examples data("mydata", package = "openair")
#' x = mydata$o3[format(mydata$date, "%m %Y") == "12 2002"]
#' result = KRDetect.outliers.EV(x)
#' summary(result)
#' plot(result)
#' plot(result, plot.type = "min")
#' plot(result, plot.type = "max")
#' @importFrom ismev gpd.fit
#' @importFrom stats na.omit quantile
#' @export
KRDetect.outliers.EV <- function(x,
                                 perform.smoothing = TRUE,
                                 bandwidth.type = "local",
                                 bandwidth.value = NULL,
                                 kernel.order = 2,
                                 gpd.fit.method = "mle",
                                 threshold.min = NULL,
                                 threshold.max = NULL,
                                 k.min = round(length(na.omit(x)) * 0.1),
                                 k.max = round(length(na.omit(x)) * 0.1),
                                 extremal.index.min = NULL,
                                 extremal.index.max = NULL,
                                 extremal.index.type = "block.maxima",
                                 block.length.min = round(sqrt(length(na.omit(x)))),
                                 block.length.max = round(sqrt(length(na.omit(x)))),
                                 D.min = NULL,
                                 D.max = NULL,
                                 K.min = NULL,
                                 K.max = NULL,
                                 r.min = NULL,
                                 r.max = NULL,
                                 return.period = 120) {
  # input data check
  if (any(c("ts", "zoo", "xts") %in% class(x))) {
    x = as.numeric(x)
  }

  if (!bandwidth.type %in% c("local", "global")) {
    stop("Invalid bandwidth type")
  }

  if (!kernel.order %in% c(2, 4)) {
    stop("Invalid kernel order")
  }

  if (!gpd.fit.method %in% c("mle", "moment")) {
    stop("Invalid GPD fit method")
  }

  if (!extremal.index.type %in% c("block.maxima", "intervals", "censored", "Kgaps", "sliding.blocks", "runs")) {
    stop("Invalid extremal index type")
  }

  if (extremal.index.type == "Kgaps") {
    if (is.null(K.min)) {
      stop("Parameter K.min cannot be NULL for extremal index type 'Kgaps'")
    }

    if (K.min < 0) {
      stop("Parameter K.min must be non negative integer for extremal index type 'Kgaps'")
    }

    if (K.min != round(K.min)) {
      stop("Parameter K.min must be integer for extremal index type 'Kgaps'")
    }

    if (is.null(K.max)) {
      stop("Parameter K.max cannot be NULL for extremal index type 'Kgaps'")
    }

    if (K.max < 0) {
      stop("Parameter K.max must be non negative integer for extremal index type 'Kgaps'")
    }

    if (K.max != round(K.max)) {
      stop("Parameter K.max must be integer for extremal index type 'Kgaps'")
    }
  }

  if (extremal.index.type == "censored") {
    if (is.null(D.min)) {
      stop("Parameter D.min cannot be NULL for extremal index type 'censored'")
    }

    if (D.min < 0) {
      stop("Parameter D.min must be non negative integer for extremal index type 'censored'")
    }

    if (D.min != round(D.min)) {
      stop("Parameter D.min must be integer for extremal index type 'censored'")
    }

    if (is.null(D.max)) {
      stop("Parameter D.max cannot be NULL for extremal index type 'censored'")
    }

    if (D.max < 0) {
      stop("Parameter D.max must be non negative integer for extremal index type 'censored'")
    }

    if (D.max != round(D.max)) {
      stop("Parameter D.max must be integer for extremal index type 'censored'")
    }
  }

  if (extremal.index.type == "runs") {
    if (is.null(r.min)) {
      stop("Parameter r.min cannot be NULL for extremal index type 'runs'")
    }

    if (r.min <= 0) {
      stop("Parameter r.min must be positive integer for extremal index type 'runs'")
    }

    if (r.min != round(r.min)) {
      stop("Parameter r.min must be integer for extremal index type 'runs'")
    }

    if (is.null(r.max)) {
      stop("Parameter r.max cannot be NULL for extremal index type 'runs'")
    }

    if (r.max <= 0) {
      stop("Parameter r.max must be positive integer for extremal index type 'runs'")
    }

    if (r.max != round(r.max)) {
      stop("Parameter r.max must be integer for extremal index type 'runs'")
    }
  }

  data.input = data.frame(x)
  data.input$index = c(1:dim(data.input)[1])
  data = na.omit(data.input)

  if (dim(data)[1] < 4) {
    stop("Not enough data")
  }

  data$smoothed = NA

  if (perform.smoothing) {
    data$smoothed = smoothing(data$index, data$x, bandwidth.type, bandwidth.value, kernel.order)$data.smoothed
    data$residuals = data$x - data$smoothed
  } else {
    data$residuals = data$x
  }

  if (gpd.fit.method == "moment") {
    if (sort(data$residuals)[length(data$residuals) - k.max] <= 0) {
      stop("The value of 'k.max' parameter is too high and the moment estimates (de Haan and Ferreira, 2006) of shape and scale parameters of GP distribution can not be estimated (negative values in parameters of log() function are generated). Lower value of 'k.max' parameter is required.")
    }

    if (sort(-data$residuals)[length(data$residuals) - k.min] <= 0) {
      stop("The value of 'k.min' parameter is too high and the moment estimates (de Haan and Ferreira, 2006) of shape and scale parameters of GP distribution can not be estimated (negative values in parameters of log() function are generated). Lower value of 'k.min' parameter is required.")
    }
  }

  # threshold value computing
  if (is.null(threshold.min)) {
    threshold.min = quantile(-data$residuals, 0.9)
  }

  if (is.null(threshold.max)) {
    threshold.max = quantile(data$residuals, 0.9)
  }

  # extremal index estimation
  if (is.null(extremal.index.min)) {
    if (extremal.index.type == "block.maxima") {
      extremal.index.min = extremal.index.gomes(x = (-data$residuals), block.length = block.length.min)
    }

    if (extremal.index.type == "intervals") {
      extremal.index.min = extremal.index.intervals(x = (-data$residuals), u = threshold.min)
    }

    if (extremal.index.type == "censored") {
      extremal.index.min = extremal.index.censored(x = (-data$residuals), u = threshold.min, D = D.min)
    }

    if (extremal.index.type == "Kgaps") {
      extremal.index.min = extremal.index.Kgaps(x = (-data$residuals), u = threshold.min, K = K.min)
    }

    if (extremal.index.type == "sliding.blocks") {
      extremal.index.min = extremal.index.sliding.blocks(x = (-data$residuals), b = block.length.min)
    }

    if (extremal.index.type == "runs") {
      extremal.index.min = extremal.index.runs(x = (-data$residuals), u = threshold.min, r = r.min)
    }
  }

  if (is.null(extremal.index.max)) {
    if (extremal.index.type == "block.maxima") {
      extremal.index.max = extremal.index.gomes(x = data$residuals, block.length = block.length.max)
    }

    if (extremal.index.type == "intervals") {
      extremal.index.max = extremal.index.intervals(x = data$residuals, u = threshold.max)
    }

    if (extremal.index.type == "censored") {
      extremal.index.max = extremal.index.censored(x = data$residuals, u = threshold.max, D = D.max)
    }

    if (extremal.index.type == "Kgaps") {
      extremal.index.max = extremal.index.Kgaps(x = data$residuals, u = threshold.max, K = K.max)
    }

    if (extremal.index.type == "sliding.blocks") {
      extremal.index.max = extremal.index.sliding.blocks(x = data$residuals, b = block.length.max)
    }

    if (extremal.index.type == "runs") {
      extremal.index.max = extremal.index.runs(x = data$residuals, u = threshold.max, r = r.max)
    }
  }

  # maximum likelihood estimates (GPD fit) and return level estimates
  if (gpd.fit.method == "mle") {
    gpd.model.min = gpd.fit(-data$residuals, threshold.min, show = FALSE)
    sigma_u.min = gpd.model.min$mle[1]
    xi.min = gpd.model.min$mle[2]
    lambda_u.min = gpd.model.min$nexc/length(data$residuals)

    gpd.model.max = gpd.fit(data$residuals, threshold.max, show = FALSE)
    sigma_u.max = gpd.model.max$mle[1]
    xi.max = gpd.model.max$mle[2]
    lambda_u.max = gpd.model.max$nexc/length(data$residuals)

    sigma.min = sigma_u.min
    sigma.max = sigma_u.max

    return.level.min = return.level.est(r = return.period, u = threshold.min, sigma_u = sigma_u.min, xi = xi.min, lambda_u = lambda_u.min, theta = extremal.index.min)
    return.level.max = return.level.est(r = return.period, u = threshold.max, sigma_u = sigma_u.max, xi = xi.max, lambda_u = lambda_u.max, theta = extremal.index.max)
  }

  # moment estimates (GPD fit) and return level estimates
  if (gpd.fit.method == "moment") {

    gpd.model.min = Moment.gpd.fit(-data$residuals, k = k.min)
    sigma.min = gpd.model.min$est[1]
    xi.min = gpd.model.min$est[2]

    gpd.model.max = Moment.gpd.fit(data$residuals, k = k.max)
    sigma.max = gpd.model.max$est[1]
    xi.max = gpd.model.max$est[2]

    lambda_u.min = NULL
    lambda_u.max = NULL

    return.level.min = return.level.est(r = return.period, u = sort(-data$residuals)[length(-data$residuals) - k.min], sigma_u = sigma.min, xi = xi.min, lambda_u = k.min/length(-data$residuals), theta = extremal.index.min)
    return.level.max = return.level.est(r = return.period, u = sort(data$residuals)[length(data$residuals) - k.max], sigma_u = sigma.max, xi = xi.max, lambda_u = k.max/length(data$residuals), theta = extremal.index.max)
  }

  # outlier identification
  data$outliers.min = FALSE
  data$outliers.min[-data$residuals > return.level.min] = TRUE

  data$outliers.max = FALSE
  data$outliers.max[data$residuals > return.level.max] = TRUE

  data$outliers = FALSE
  data$outliers[(-data$residuals > return.level.min) | (data$residuals > return.level.max)] = TRUE

  data.output = merge(data.input, data, by = c("index", "x"), all.x = TRUE)
  data.output = data.output[order(data.output$index),]

  result = list(method.type = "extreme value theory",
                x = data.output$x,
                index = data.output$index,
                smoothed = data.output$smoothed,
                GPD.fit.method = gpd.fit.method,
                extremal.index.type = extremal.index.type,
                sigma.min = sigma.min,
                sigma.max = sigma.max,
                xi.min = xi.min,
                xi.max = xi.max,
                lambda_u.min = lambda_u.min,
                lambda_u.max = lambda_u.max,
                extremal.index.min = extremal.index.min,
                extremal.index.max = extremal.index.max,
                threshold.min = threshold.min,
                threshold.max = threshold.max,
                return.level.min = return.level.min,
                return.level.max = return.level.max,
                outlier.min = data.output$outliers.min,
                outlier.max = data.output$outliers.max,
                outlier = data.output$outliers)

  class(result) = "KRDetect"

  return(result)
}

#' Mean residual life (MRL) plot
#'
#' An empirical mean residual life plot (Coles, 2001), including confidence intervals, is produced based on maximum likelihood or moment estimates.
#'
#' @param x data values.
#' Supported data types
#' \itemize{
#'   \item{a numeric vector}
#'   \item{a time series object \code{ts}}
#'   \item{a time series object \code{xts}}
#'   \item{a time series object \code{zoo}}
#' }
#' @param umin the minimum threshold at which the mean residual life function is calculated based on maximum likelihood estimates. Default is \code{umin = quantile(na.omit(x), probs = 0.8)}.
#' @param umax the maximum threshold at which the mean residual life function is calculated based on maximum likelihood estimates. Default is \code{umin = quantile(na.omit(x), probs = 0.95)}.
#' @param kmin the minimum number of largest order statistics for which the mean residual life function is calculated based on moment estimates. Default is \code{kmin = round(length(na.omit(x)) * 0.05)}.
#' @param kmax the maximum number of largest order statistics for which the mean residual life function is calculated based on moment estimates. Default is \code{kmax = round(length(na.omit(x)) * 0.2)}.
#' @param nint the number of points at which the mean residual life function is calculated. Default is \code{nint = 100}.
#' @param conf the confidence coefficient for the confidence intervals depicted in the plot. Default is \code{conf = 0.95}.
#' @param est.method a character string specifying the type of estimates for the scale and shape parameters of GP distribution.
#'
#' Possible options are
#' \itemize{
#'   \item{\code{"mle"}} {(default) to use maximum likelihood estimates (Coles, 2001)}
#'   \item{\code{"moment"}} {to use moment estimates (de Haan and Ferreira2006).}
#' }
#' @param u0 a numeric value giving the threshold meant for a GP approximation of the threshold exceedances. Default is \code{u0 = NULL}.
#' @param k0 a numeric value giving the number \code{(k0-1)} of largest observations meant for a GP approximation. Default is \code{k0 = NULL}.
#' @details The function constructs MRL plot (Coles, 2001) based on maximum likelihood or moment estimates for parameters of GP distribution.
#' The MRL, i.e. the estimates of the mean excess, are expected to change linearly with threshold levels at which the GP model is appropriate.
#' If \code{u0} (or \code{k0}, respectively) is given, a GP mean-threshold dependency line is plotted in addition to the MRL plot (Coles, 2001; Eq. 4.9).
#' Each of the lines provide the user an option to assess the suitability of \code{u0} or \code{k0} as a lower bound for the threshold exceedances (for \code{u0}) or the number of upper order statistics (for \code{k0}) to fit the GP distribution.
#' In case \code{est.method = "mle"} and \code{u0} takes a value, the theoretical GP mean is estimated by the MLE estimates of the GP parameters. For the case \code{est.method = "moment"} and \code{k0} is given, the theoretical GP mean is estimated using the moment estimates.
#' In case \code{est.method = "moment"} the value \code{x(n-k)} on the x-axis of MRL plot denotes the \code{(k + 1)}-th largest observation of the total number of \code{n} observations.
#' @references Theo Gasser, Alois Kneip & Walter Koehler (1991) A flexible and fast method for automatic smoothing. Journal of the American Statistical Association 86, 643-652. https://doi.org/10.2307/2290393
#'
#' E. Herrmann (1997) Local bandwidth choice in kernel regression estimation. Journal of Graphical and Computational Statistics 6, 35-54.
#'
#' Herrmann E, Maechler M (2013). lokern: Kernel Regression Smoothing with Local or Global Plug-in Bandwidth. R package version 1.1-5, URL http://CRAN.R-project.org/package=lokern.
#'
#' Gasser, T, Muller, H-G, Mammitzsch, V (1985). Kernels for nonparametric curve estimation. Journal of the Royal Statistical Society, B Met., 47(2), 238-252.
#'
#' Coles, S (2001). An Introduction to Statistical Modeling of Extreme Values. Springer-Verlag, London, U.K., 208pp.
#'
#' de Haan, L, Ferreira, A (2006). Extreme Value Theory: An Introduction. Springer.
#' @examples data("mydata", package = "openair")
#' x = mydata$o3[format(mydata$date, "%m %Y") == "12 2002"]
#' res = smoothing(y = x)$residuals
#' MRL.plot(res)
#' @importFrom graphics abline lines
#' @importFrom grDevices dev.off
#' @importFrom ismev gpd.fit
#' @importFrom lokern lokerns
#' @importFrom stats quantile
#' @export
MRL.plot <- function(x,
                     umin = quantile(na.omit(x), probs = 0.8),
                     umax = quantile(na.omit(x), probs = 0.95),
                     kmin = round(length(na.omit(x)) * 0.05),
                     kmax = round(length(na.omit(x)) * 0.2),
                     nint = 100,
                     conf = 0.95,
                     est.method = "mle",
                     u0 = NULL,
                     k0 = NULL) {
  # input data check
  if (any(c("ts", "zoo", "xts") %in% class(x))) {
    x = as.numeric(x)
  }

  if (!est.method %in% c("mle", "moment")) {
    stop("Invalid GPD fit method")
  }

  x = na.omit(x)
  n = length(x)

  if (n < 4) {
    stop("Not enough data")
  }

  if (est.method == "moment") {
    if (sort(x)[n - kmax] <= 0) {
      stop("The value of 'kmax' parameter is too high and the moment estimates (de Haan and Ferreira, 2006) of shape and scale parameters of GP distribution can not be estimated (negative values in parameters of log() function are generated). Lower value of 'kmax' parameter is required.")
    }
  }

  alpha = 1 - conf

  if (est.method == "mle") {
    if (is.null(umin)) {
      umin = quantile(x, probs = 0.8)
    }

    if (is.null(umax)) {
      umax = quantile(x, probs = 0.95)
    }

    h = (umax - umin) / nint
    u = seq(umin, umax, h)
    mean.exceed = c(rep(0, length(u)))
    mean.exceed.int = matrix(c(rep(0, length(u) * 2)), ncol = 2)

    for (i in 1:length(u)) {
      exceed = x[x > u[i]]
      mean.exceed[i] = mean(exceed - u[i])
      mean.exceed.int[i,] = c(mean.exceed[i] - qnorm(1 - alpha / 2) * sd(exceed) / sqrt(length(exceed)), mean.exceed[i] + qnorm(1 - alpha / 2) * sd(exceed) / sqrt(length(exceed)))
    }

    plot(u, mean.exceed, ylim = c(min(mean.exceed.int[,1], na.rm = TRUE), max(mean.exceed.int[,2], na.rm = TRUE)), type = "l", ylab = "Mean Excess", main = "MRL plot", mgp = c(2, 0.5, 0))
    lines(u, mean.exceed.int[,1], lty = 2)
    lines(u, mean.exceed.int[,2], lty = 2)

    if (!is.null(u0)) {
      gpd.model = gpd.fit(x, u0, show = FALSE)
      lines(u[u >= u0], ((gpd.model$mle[1] + gpd.model$mle[2] * (u - u0)) / (1 - gpd.model$mle[2]))[u >= u0], col = "red", lty = 1)
      lines(u[u < u0], ((gpd.model$mle[1] + gpd.model$mle[2] * (u - u0)) / (1 - gpd.model$mle[2]))[u < u0], col = "red", lty = 2)
      abline(v = u0, col = "red", lty = 2)
    }
  }

  if (est.method == "moment") {
    h = (kmax - kmin)/nint
    k = unique(round(seq(kmin, kmax, h)))
    mean.exceed = c(rep(0, length(k)))
    mean.exceed.int = matrix(c(rep(0, length(k) * 2)), ncol = 2)
    x.sorted = sort(x)

    for (i in 1:length(k)) {
      threshold = x.sorted[n - k[i]]

      exceed = x.sorted[x.sorted > threshold]
      mean.exceed[i] = mean(exceed - threshold)
      mean.exceed.int[i,] = c(mean.exceed[i] - qnorm(1 - alpha / 2) * sd(exceed) / sqrt(k[i]), mean.exceed[i] + qnorm(1 - alpha/2) * sd(exceed) / sqrt(k[i]))
    }

    plot(x.sorted[n-k], mean.exceed, ylim = c(min(mean.exceed.int[,1], na.rm = TRUE), max(mean.exceed.int[,2], na.rm = TRUE)), type = "l", ylab = "Mean Excess", xlab = "x(n - k)", main = "MRL plot", mgp = c(2, 0.5, 0))
    lines(x.sorted[n-k], mean.exceed.int[,1], lty = 2)
    lines(x.sorted[n-k], mean.exceed.int[,2], lty = 2)

    if (!is.null(k0)) {
      gpd.model = Moment.gpd.fit(x, k0)
      lines(x.sorted[n - k][x.sorted[n - k] >= x.sorted[n - k0]], ((gpd.model$est[1] + gpd.model$est[2] * (x.sorted[n-k] - x.sorted[n - k0])) / (1 - gpd.model$est[2]))[x.sorted[n - k] >= x.sorted[n - k0]], col = "red", lty = 1)
      lines(x.sorted[n - k][x.sorted[n - k] < x.sorted[n - k0]], ((gpd.model$est[1] + gpd.model$est[2] * (x.sorted[n-k] - x.sorted[n - k0])) / (1 - gpd.model$est[2]))[x.sorted[n - k] < x.sorted[n - k0]], col = "red", lty = 2)
      abline(v = x.sorted[n - k0], col = "red", lty = 2)
    }
  }
}

#' Stability plot
#'
#' A stability plot for maximum likelihood or moment estimates of the GP parameters (Coles, 2001), including confidence intervals, at a range of thresholds or number of the largest observations.
#'
#' @param x data values.
#' Supported data types
#' \itemize{
#'   \item{a numeric vector}
#'   \item{a time series object \code{ts}}
#'   \item{a time series object \code{xts}}
#'   \item{a time series object \code{zoo}}
#' }
#' @param umin the minimum threshold at which the mean residual life function is calculated. Default is \code{umin = quantile(na.omit(x), probs = 0.8)}.
#' @param umax the maximum threshold at which the mean residual life function is calculated. Default is \code{ummax = quantile(na.omit(x), probs = 0.95)}.
#' @param kmin the minimum number of largest order statistics for which the mean residual life function is calculated based on moment estimates. Default is \code{kmin = round(length(na.omit(x)) * 0.05)}.
#' @param kmax the maximum number of largest order statistics for which the mean residual life function is calculated based on moment estimates. Default is \code{kmax = round(length(na.omit(x)) * 0.2)}.
#' @param nint the number of points at which the mean residual life function is calculated. Default is \code{nint = 100}.
#' @param conf the confidence coefficient for the confidence intervals depicted in the plot. Default is \code{conf = 0.95}.
#' @param est.method a character string specifying the type of estimates for the scale and shape parameters of GP distribution.
#'
#' Possible options are
#' \itemize{
#'   \item{\code{"mle"}} {(default) to use maximum likelihood estimates (Coles, 2001)}
#'   \item{\code{"moment"}} {to use moment estimates (de Haan and Ferreira, 2006).}
#' }
#' @param u0 a numeric value giving the threshold meant for a GP approximation of the threshold exceedances. Default is \code{u0 = NULL}.
#' @param k0 a numeric value giving the number \code{(k0-1)} of largest observations meant for a GP approximation. Default is \code{k0 = NULL}.
#' @details  The function estimates the GP parameters at a range of thresholds (in case \code{est.method = "mle"}) or a range of upper order statistics (in case of \code{est.method = "moment"}), and shows the sample paths of the estimates.
#' The estimates of the shape or the scale parameter are expected to be constant or to change linearly, respectively, with threshold levels at which the GP model is appropriate.
#' If \code{u0} (or \code{k0}, respectively) is given, a threshold-dependency lines for the particular parameters are plotted in addition. The lines provide the user an option to assess the suitability of \code{u0} or \code{k0} as a lower bound for the threshold exceedances (for \code{u0}) or the number of upper order statistics (for \code{k0}) to fit the GP distribution.
#' In case \code{est.method = "mle"} and \code{u0} takes a value, the theoretical dependency lines for the parameters are evaluated on the basis of MLE estimates. For the case \code{est.method = "moment"} and \code{k0} is given, the dependency lines are estimated using the moment estimators.
#' In case \code{est.method = "moment"} the value \code{x(n-k)} on the x-axis of MRL plot denotes the \code{(k + 1)}-th largest observation of the total number of \code{n} observations.
#' @references Theo Gasser, Alois Kneip & Walter Koehler (1991) A flexible and fast method for automatic smoothing. Journal of the American Statistical Association 86, 643-652. https://doi.org/10.2307/2290393
#'
#' E. Herrmann (1997) Local bandwidth choice in kernel regression estimation. Journal of Graphical and Computational Statistics 6, 35-54.
#'
#' Herrmann E, Maechler M (2013). lokern: Kernel Regression Smoothing with Local or Global Plug-in Bandwidth. R package version 1.1-5, URL http://CRAN.R-project.org/package=lokern.
#'
#' Gasser, T, Muller, H-G, Mammitzsch, V (1985). Kernels for nonparametric curve estimation. Journal of the Royal Statistical Society, B Met., 47(2), 238-252.
#'
#' Coles, S (2001). An Introduction to Statistical Modeling of Extreme Values. Springer-Verlag, London, U.K., 208pp.
#'
#' de Haan, L, Ferreira, A (2006). Extreme Value Theory: An Introduction. Springer.
#' @examples data("mydata", package = "openair")
#' x = mydata$o3[format(mydata$date, "%m %Y") == "12 2002"]
#' res = smoothing(y = x)$residuals
#' stability.plot(res)
#' @importFrom graphics abline lines
#' @importFrom grDevices dev.off
#' @importFrom ismev gpd.fit
#' @export
stability.plot <- function(x,
                           umin = quantile(na.omit(x), probs = 0.8),
                           umax = quantile(na.omit(x), probs = 0.95),
                           kmin = round(length(na.omit(x)) * 0.05),
                           kmax = round(length(na.omit(x)) * 0.2),
                           nint = 100,
                           conf = 0.95,
                           est.method = "mle",
                           u0 = NULL,
                           k0 = NULL) {
  # input data check
  if (any(c("ts", "zoo", "xts") %in% class(x))) {
    x = as.numeric(x)
  }

  if (!est.method %in% c("mle", "moment")) {
    stop("Invalid GPD fit method")
  }

  x = na.omit(x)
  n = length(x)

  if (n < 4) {
    stop("Not enough data")
  }

  if (est.method == "moment") {
    if (sort(x)[n - kmax] <= 0) {
      stop("The value of 'kmax' parameter is too high and the moment estimates (de Haan and Ferreira, 2006) of shape and scale parameters of GP distribution can not be estimated (negative values in parameters of log() function are generated). Lower value of 'kmax' parameter is required.")
    }
  }

  alpha = 1 - conf

  if (est.method == "mle") {
    h = (umax - umin) / nint
    u = seq(umin, umax, h)
    sigma = c(rep(0, length(u)))
    xi = c(rep(0, length(u)))
    sigma.int.est = matrix(c(rep(0, length(u) * 2)), ncol = 2)
    xi.int.est = matrix(c(rep(0, length(u) * 2)), ncol = 2)

    for (i in 1:length(u)) {
      gpd.model = gpd.fit(x, u[i], show = FALSE)
      sigma[i] = gpd.model$mle[1] # scale parameter
      xi[i] = gpd.model$mle[2]    # shape parameter
      sigma.int.est[i,] = c(sigma[i] - qnorm(1 - alpha / 2) * gpd.model$se[1], sigma[i] + qnorm(1 - alpha / 2) * gpd.model$se[1])
      xi.int.est[i,] = c(xi[i] - qnorm(1 - alpha / 2) * gpd.model$se[2], xi[i] + qnorm(1 - alpha / 2) * gpd.model$se[2])
    }

    if (!is.null(u0)) {
      gpd.model = gpd.fit(x, u0, show = FALSE)
      sigma.u0 = gpd.model$mle[1]
      xi.u0 = gpd.model$mle[2]
    }

    split.screen(figs = c(1,2))
    screen(1)
    plot(u, xi, ylim = c(min(xi.int.est[,1], na.rm = TRUE), max(xi.int.est[,2], na.rm = TRUE)), type = "l", ylab = "xi MLE", mgp = c(2, 0.5, 0))
    lines(u, xi.int.est[,1], lty = 2)
    lines(u, xi.int.est[,2], lty = 2)

    if (!is.null(u0)) {
      abline(h = xi.u0[u >= u0], col = "red", lty = 1)
      lines(u[u >= u0], rep(xi.u0, length(u[u >= u0])), col = "red", lty = 1)
      lines(u[u < u0], rep(xi.u0, length(u[u < u0])), col = "red", lty = 2)
      abline(v = u0, col = "red", lty = 2)
    }

    screen(2)
    plot(u, sigma, ylim = c(min(sigma.int.est[,1], na.rm = TRUE), max(sigma.int.est[,2], na.rm = TRUE)), type = "l", ylab = "sigma MLE", mgp = c(2, 0.5, 0))
    lines(u, sigma.int.est[,1], lty = 2)
    lines(u, sigma.int.est[,2], lty = 2)

    if (!is.null(u0)) {
      lines(u[u >= u0], (sigma.u0 + xi.u0 * (u - u0))[u >= u0], col = "red", lty = 1)
      lines(u[u < u0], (sigma.u0 + xi.u0 * (u - u0))[u < u0], col = "red", lty = 2)
      abline(v = u0, col = "red", lty = 2)
    }

    close.screen(all.screens = TRUE)
  }

  if (est.method == "moment") {
    h = (kmax - kmin) / nint
    k = unique(round(seq(kmin, kmax, h)))
    sigma = c(rep(0, length(k)))
    xi = c(rep(0, length(k)))
    sigma.int.est = matrix(c(rep(0, length(k) * 2)), ncol = 2)
    xi.int.est = matrix(c(rep(0, length(k) * 2)), ncol = 2)

    for (i in 1:length(k)) {
      gpd.model = Moment.gpd.fit(x, k[i])
      sigma[i] = gpd.model$est[1] # scale parameter
      xi[i] = gpd.model$est[2]    # shape parameter
      sigma.int.est[i,] = c(sigma[i] - qnorm(1 - alpha / 2) * gpd.model$sd[1], sigma[i] + qnorm(1 - alpha / 2) * gpd.model$sd[1])
      xi.int.est[i,] = c(xi[i] - qnorm(1 - alpha / 2) * gpd.model$sd[2], xi[i] + qnorm(1 - alpha / 2) * gpd.model$sd[2])
    }

    if (!is.null(k0)) {
      gpd.model = Moment.gpd.fit(x, k0)
      sigma.k0 = gpd.model$est[1]
      xi.k0 = gpd.model$est[2]
    }

    x.sorted = sort(x)
    split.screen(figs = c(1, 2))
    screen(1)
    plot(x.sorted[n - k], xi, ylim = c(min(xi.int.est[,1], na.rm = TRUE), max(xi.int.est[,2], na.rm = TRUE)), type = "l", ylab = "xi MOM", xlab = "x(n - k)", mgp = c(2, 0.5, 0))
    lines(x.sorted[n - k], xi.int.est[,1], lty = 2)
    lines(x.sorted[n - k], xi.int.est[,2], lty = 2)

    if (!is.null(k0)) {
      lines(x.sorted[n - k][x.sorted[n - k] >= x.sorted[n - k0]], rep(xi.k0, length(x.sorted[n - k][x.sorted[n - k] >= x.sorted[n - k0]])), col = "red", lty = 1)
      lines(x.sorted[n - k][x.sorted[n - k] < x.sorted[n - k0]], rep(xi.k0, length(x.sorted[n - k][x.sorted[n - k] < x.sorted[n - k0]])), col = "red", lty = 2)
      abline(v = x.sorted[n - k0], col = "red", lty = 2)
    }

    screen(2)
    plot(x.sorted[n - k], sigma, ylim = c(min(sigma.int.est[,1], na.rm = TRUE), max(sigma.int.est[,2], na.rm = TRUE)), type = "l", ylab = "sigma MOM", xlab = "x(n - k)", mgp = c(2, 0.5, 0))
    lines(x.sorted[n - k], sigma.int.est[,1], lty = 2)
    lines(x.sorted[n - k], sigma.int.est[,2], lty = 2)

    if (!is.null(k0)) {
      lines(x.sorted[n - k][x.sorted[n - k] >= x.sorted[n - k0]], (sigma.k0 + xi.k0 * (x.sorted[n - k] - x.sorted[n - k0]))[x.sorted[n - k] >= x.sorted[n - k0]], col = "red", lty = 1)
      lines(x.sorted[n - k][x.sorted[n - k] < x.sorted[n - k0]], (sigma.k0 + xi.k0 * (x.sorted[n - k] - x.sorted[n - k0]))[x.sorted[n - k] < x.sorted[n - k0]], col = "red", lty = 2)
      abline(v = x.sorted[n - k0], col = "red", lty = 2)
    }

    close.screen(all.screens = TRUE)
  }
}
