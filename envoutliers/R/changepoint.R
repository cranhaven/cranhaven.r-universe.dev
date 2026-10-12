#' Left medcouple (LMC) - Only intended for developer use
#'
#' Calculates left medcouple (MLC).
#' The function is called by \code{\link{KRDetect.outliers.changepoint}} and is not intended for use by regular users of the package.
#'
#' @param x a numeric vector of data values.
#' @details This function computes left medcouple (LMC).
#' The function is exported for developer use only. It does not perform any checks on inputs since it is only a convenience function.
#' @return A numeric value giving left medcouple
#' @references Brys G, Hubert M, Struyf A (2008). Goodness-of-fit tests based on a robust measure of skewness. Computational Statistics, 23(3), 429–442.
#'
#' Todorov V, Filzmoser P (2009). An Object-Oriented Framework for Robust Multivariate Analysis. Journal of Statistical Software, 32(3), 1-47. URL http://www.jstatsoft.org/v32/i03/.
#' @importFrom robustbase mc
#' @importFrom stats median
mc.left <- function(x) {
  result = -mc(x[x < median(x)])

  return (result)
}

#' Right medcouple (RMC) - Only intended for developer use
#'
#' Calculates right medcouple (RMC).
#' The function is called by \code{\link{KRDetect.outliers.changepoint}} and is not intended for use by regular users of the package.
#'
#' @param x a numeric vector of data values.
#' @details This function computes right medcouple (RMC).
#' The function is exported for developer use only. It does not perform any checks on inputs since it is only a convenience function.
#' @return A numeric value giving right medcouple
#' @references Brys G, Hubert M, Struyf A (2008). Goodness-of-fit tests based on a robust measure of skewness. Computational Statistics, 23(3), 429–442.
#'
#' Todorov V, Filzmoser P (2009). An Object-Oriented Framework for Robust Multivariate Analysis. Journal of Statistical Software, 32(3), 1-47. URL http://www.jstatsoft.org/v32/i03/.
#' @importFrom robustbase mc
#' @importFrom stats median
mc.right <- function(x) {
  result = mc(x[x > median(x)])

  return (result)
}

#' Robust medcouple MC-LR test - Only intended for developer use
#'
#' Performs robust medcouple test to evaluate the fit of the data to normal distribution.
#' The function is called by \code{\link{KRDetect.outliers.changepoint}} and is not intended for use by regular users of the package.
#'
#' @param x a numeric vector of data values.
#' @param alpha numeric value giving test significance level.
#' @details This function performs robust medcouple test based on the left and right medcouple (LMC and LRC).
#' The function is exported for developer use only. It does not perform any checks on inputs since it is only a convenience function for robust testing of the normality.
#' @return A list is returned with elements:
#' \item{test.stat}{a numeric value giving the value of test statistics}
#' \item{crit numeric}{a vector of critical values defining rejection region of the test}
#' @references Brys G, Hubert M, Struyf A (2008). Goodness-of-fit tests based on a robust measure of skewness. Computational Statistics, 23(3), 429–442.
#'
#' Todorov V, Filzmoser P (2009). An Object-Oriented Framework for Robust Multivariate Analysis. Journal of Statistical Software, 32(3), 1-47. URL http://www.jstatsoft.org/v32/i03/.
#' @importFrom robustbase mc
#' @importFrom stats qchisq
mc.test <- function(x,
                    alpha = 0.05) {
  n = length(x)
  w1 = mc(x)
  w2 = mc.left(x)
  w3 = mc.right(x)
  w = c(w1, w2, w3)
  omega = c(0, 0.199, 0.199)
  sigma = matrix(c(1.25, 0.323, -0.323, 0.323, 2.62, -0.0123, -0.323, -0.0123, 2.62), ncol = 3, byrow = TRUE)
  sigma.inv = solve(sigma, diag(3))
  test.value = n * (w - omega) %*% sigma.inv %*% matrix(w - omega, ncol = 1)
  crit.left = qchisq(alpha / 2, df = 3)
  crit.right = qchisq(1 - alpha / 2, df = 3)

  result = list(test.stat = test.value,
                crit = c(crit.left, crit.right))

  return(result)
}

#' Box-Cox transformation of data - Only intended for developer use
#'
#' Performs Box-Cox power transformation of the data. The optimal value of power parameter is selected based on profile log-likelihoods.
#' The function is called by \code{\link{KRDetect.outliers.changepoint}} and is not intended for use by regular users of the package.
#'
#' @param x a numeric vector of data values.
#' @details This function computes the Box-Cox power transformation of the data.
#' The function is exported for developer use only. It does not perform any checks on inputs since it is only a convenience function for a transformation of data to normality.
#' The optimal value of a power parameter is estimated based on profile log-likelihoods calculated using \code{boxcox} function implemented in \pkg{MASS} package.
#' @return A list is returned with elements:
#' \item{lambda}{a numeric value giving power parameter}
#' \item{x}{a numeric vector of data values}
#' \item{x.transformed}{a numeric vector of transformed data}
#' @references Box G, Cox D (1964). An analysis of transformations. Journal of the Royal Statistical Society: Series B, 26, 211–234.
#'
#' Venables WN, Ripley BD (2002). Modern Applied Statistics with S. New York, fourth edition. ISBN 0-387-95457-0, URL http://www.stats.ox.ac.uk/pub/MASS4.
#' @importFrom MASS boxcox
boxcoxTransform <- function(x) {
  x = x[!is.na(x)]

  if (length(which(x <= 0.1))) {
    if (abs(min(x)) >= 0.1) {
      x = x + abs(min(x)) * 1.1
    } else {
      x = x + 0.1 * 1.1
    }
  }

  ll.profile.list = boxcox(x ~ c(1:length(x)), lambda = seq(-2, 2, 1/10), plotit = FALSE)
  ll.profile.df = data.frame(x = ll.profile.list$x, y = ll.profile.list$y)
  lambda = ll.profile.df$x[ll.profile.df$y == max(ll.profile.df$y)]

  if (lambda != 0) {
    y = (x ^ lambda - 1) / lambda
  } else {
    y = log(x)
  }

  result = list(lambda = lambda,
                x = x,
                x.transformed = y)

  return(result)
}

#' Outlier detection using Grubbs test - Only intended for developer use
#'
#' Sequential identification of outliers using Grubbs' test.
#' The algorithm first considers the data value with the highest absolute value. If the null hypothesis that such a value is not an outlier is rejected,
#' the considered value is detected as an outlier and excluded from further analysis. Subsequently, a value with the second-highest absolute value is considered, and its quality is again evaluated using the Grubbs test. This procedure is repeated until no outlier is detected.
#' The function is called by \code{\link{KRDetect.outliers.changepoint}} and \code{\link{grubbs.detect}}. The function is not intended for use by regular users of the package.
#'
#' @param x a numeric vector of data values.
#' @param alpha a numeric value giving test significance level.
#' @details This function sequentially identifies outlier data using Grubbs test.
#' The function is exported for developer use only. It does not perform any checks on inputs since it is only convenience function for identification of outlier residuals using Grubbs test.
#' @return A list is returned with elements:
#' \item{result}{A table containing information about identified outliers. The number of rows of the table corresponds to the number of identified outliers. The table has following columns: \code{index} - a numeric value giving the index of detected outlier in the original data, \code{value} - a numeric value of the identified outlier, \code{test.value} - a numeric value of the test statistics, \code{critical.value} - a numeric value giving the test statistics, \code{p.value} - a numeric value giving \emph{p.value} of the test}
#' \item{outliers.exists}{A logical value. \code{TRUE} means that at least one outlier was detected.}
#' @references Grubbs F (1950). Sample criteria for testing outlying observations. The Annals of Mathematical Statistics, 21(1), 27-58.
#' @importFrom stats pt qt sd
grubbs.test <- function(x,
                        alpha = 0.05) {
  result = data.frame(index = NA, value = NA, test.value = NA, critical.value = NA, p.value = NA)
  extreme.omit = c()
  outliers.count = 0
  x.old = x

  repeat {
    N = length(x)
    G1 = (mean(x) - min(x)) / sd(x)
    G2 = (max(x) - mean(x)) / sd(x)
    G = max(G1, G2)
    if (!(is.nan(G1) || is.nan(G2))) {
      critical.value = (N - 1) * sqrt(qt(alpha / N, (N - 2)) ^ 2/(N - 2 + qt(alpha / N, (N - 2)) ^ 2)) / sqrt(N)
      if (G1 >= G2) {
        extreme.type = "min"
      } else {
        extreme.type = "max"
      }
      q = ((G^2 * N^2 - 2 * G^2 * N) / ((N - 1)^2 - G^2 * N)) ^ (1 / 2)
      p.value = (1 - pt(q, N - 2)) * (2 * N)
      if (G > critical.value){
        outliers.count = outliers.count + 1
        result = rbind(result, c(NA, NA, NA, NA, NA))
        result$p.value[outliers.count] = p.value
        result$critical.value[outliers.count] = critical.value
        result$test.value[outliers.count] = G
        if (extreme.type == "min") {
          if (length(which(x.old == min(x))) == 1) {
            result$index[outliers.count] = which(x.old == min(x))
            result$value[outliers.count] = min(x)
            extreme.omit[length(extreme.omit) + 1] = which(x.old == min(x))
          } else {
            for (i in 1:length(which(x.old == min(x)))) {
              result$index[outliers.count] = which(x.old == min(x))[i]
              result$value[outliers.count] = min(x)
              extreme.omit[length(extreme.omit) + 1] = which(x.old == min(x))[i]
              outliers.count = outliers.count + 1
            }
          }
        } else if (extreme.type == "max") {
          if (length(which(x.old == max(x))) == 1) {
            result$index[outliers.count] = which(x.old == max(x))
            result$value[outliers.count] = max(x)
            extreme.omit[length(extreme.omit) + 1] = which(x.old == max(x))
          } else {
            for (i in 1:length(which(x.old == max(x)))) {
              result$index[outliers.count] = which(x.old == max(x))[i]
              result$value[outliers.count] = max(x)
              extreme.omit[length(extreme.omit) + 1] = which(x.old == max(x))[i]
              outliers.count = outliers.count + 1
            }
          }
        }
        x = x.old[-extreme.omit]
      }
      if (G <= critical.value) break
    }
  }

  if (outliers.count > 0) {
    outliers.exist = TRUE
  } else {
    outliers.exist = FALSE
  }

  result = list(result = result[-dim(result)[1],],
                outliers.exists = outliers.exist)

  return(result)
}

#' Parameter L for Chebyshev inequality based outlier detection - Only intended for developer use
#'
#' Finds the value of parameter L defining the criterion for outlier identification using Chebyshev inequality. The parameter is found using Algorithm A1 (Campulova et al., 2018).
#' The function is called by \code{\link{KRDetect.outliers.changepoint}} and is not intended for use by regular users of the package.
#'
#' @param x a numeric vector of data values.
#' @param L.start a numeric value giving the smallest reasonable value of parameter \code{L}.
#' @param eps A numeric value of the \emph{epsilon} parameter. If \code{eps = NULL}, the value is calculated as recommended in Modified Algorithm A1 (Campulova et al., 2018).
#' @details This function finds the value of parameter L defining the criterion for outlier identification using Chebyshev inequality. The algorithm is based on Algorithm A1 described in (Campulova et al., 2018).
#' Nonoutliers are characterised as a homogeneous set of data randomly distributed around zero value.
#' The differences between the data correspond to random fluctuations in the measurements.
#' The algorithm finds the value of the parameter by scanning possible values of \emph{L} and investigating differences of the corresponding nonoutliers.
#' The idea is to choose \emph{L} corresponding to the maximum change of the maximum difference found among the ordered nonoutlier data.
#' The function is exported for developer use only. It does not perform any checks on inputs since it is only a convenience function for finding outlier residuals based on Chebyshev inequality.
#' @return A numeric value giving the parameter \emph{L}
#' @references Campulova M, Michalek J, Mikuska P, Bokal D (2018). Nonparametric algorithm for identification of outliers in environmental data. Journal of Chemometrics, 32, 453-463.
#' @importFrom stats sd
find.L <- function(x,
                   L.start = 2.5,
                   eps = NULL) {
  if (is.null(eps)) {
    eps = (max(abs(x)) / sd(x) - L.start) / 100
  }

  if (eps > 0){

    data = data.frame(x = x, limit.value = NA, is.outlier = NA)
    L = L.start
    data.params = data.frame(L = c(), outliers.count = c(), largest.gap = c())
    index = 0

    repeat {
      index = index + 1
      data$limit.value = L * sd(data$x)
      data$is.outlier[abs(data$x) > data$limit.value] = TRUE
      data$is.outlier[abs(data$x) <= data$limit.value] = FALSE
      outliers.count = length(which(data$is.outlier))
      temp = sort(data$x[which(data$is.outlier == FALSE)])
      largest.gap = max(abs(diff(temp)))
      data.params = rbind(data.params, c(L, outliers.count, largest.gap))
      if (outliers.count == 0) break
      L = L + eps
    }

    names(data.params) = c("L", "outliers.count", "largest.gap")

    if (data.params$outliers.count[1] > 0){

      data.params$largest.gap.diff = NA
      data.params$largest.gap.diff[1:(dim(data.params)[1] - 1)] = diff(data.params$largest.gap)
      L = data.params$L[data.params$largest.gap.diff == max(data.params$largest.gap.diff, na.rm = TRUE)][1]
    } else {
      L = L.start
    }
  } else {
    L = L.start
  }

  return(L)
}

#' Parameter \emph{alpha} for Quantiles of normal distribution based outlier detection - Only intended for developer use
#'
#' Finds the value of parameter \emph{alpha} defining the criterion for outlier identification using quantiles of normal distribution. The parameter is found using Modified algorithm A1 (Campulova et al., 2018.)
#' The function is called by \code{\link{KRDetect.outliers.changepoint}} and is not intended for use by regular users of the package.
#'
#' @param x a numeric vector of data values.
#' @param alpha.start a numeric value giving the largest reasonable value of parameter \emph{alpha}.
#' @param eps a numeric value of the epsilon parameter. If \code{eps = NULL}, the value is calculated as recommended in Modified Algorithm A1 (Campulova et al., 2018).
#' @details This function finds the value of parameter \emph{alpha} defining the criterion for outlier identification using quantiles of normal distribution. The algorithm is based on Modified Algorithm A1 described in (Campulova et al., 2018).
#' Nonoutliers are characterised as a homogeneous set of data randomly distributed around zero value.
#' The differences between the data correspond to random fluctuations in the measurements.
#' The algorithm finds the value of the parameter by scanning possible values of \emph{alpha} and investigating differences of the corresponding nonoutliers.
#' The idea is to choose alpha corresponding to the maximum change of the maximum difference found among the ordered nonoutlier data.
#' The function is exported for developer use only. It does not perform any checks on inputs since it is only convenience function for finding outlier residuals based on quantiles of normal distribution.
#' @return A numeric value giving the parameter \emph{alpha}
#' @references Campulova M, Michalek J, Mikuska P, Bokal D (2018). Nonparametric algorithm for identification of outliers in environmental data. Journal of Chemometrics, 32, 453-463.
#' @importFrom stats pnorm qnorm sd
find.alpha <- function(x,
                       alpha.start = 0.05,
                       eps = NULL) {
  if (is.null(eps)) {
    eps = (alpha.start - (1 - pnorm(max(abs(x)) / sd(x), mean(x), sd(x))) * 2) / 100
  }

  if (eps > 0) {
    data = data.frame(x = x, limit.value.left = NA, limit.value.right = NA, is.outlier = NA)
    alpha = alpha.start
    data.params = data.frame(alpha = c(), outliers.count = c(), largest.gap = c())
    index = 0

    repeat {
      index = index + 1
      data$limit.value.right = qnorm(1 - alpha / 2, mean = mean(data$x), sd = sd(data$x))
      data$limit.value.left = qnorm(alpha / 2, mean = mean(data$x), sd = sd(data$x))
      data$is.outlier[data$x < data$limit.value.left | data$x > data$limit.value.right] = TRUE
      data$is.outlier[data$x > data$limit.value.left & data$x < data$limit.value.right] = FALSE
      outliers.count = length(which(data$is.outlier))
      temp = sort(data$x[which(data$is.outlier == FALSE)])
      largest.gap = max(abs(diff(temp)))
      data.params = rbind(data.params, c(alpha, outliers.count, largest.gap))
      if (outliers.count == 0) break
      alpha = alpha - eps
      if (alpha < 0) alpha = 0
    }

    names(data.params) = c("alpha", "outliers.count", "largest.gap")

    if (data.params$outliers.count[1] > 0) {
      data.params$largest.gap.diff = NA
      data.params$largest.gap.diff[1:(dim(data.params)[1] - 1)] = diff(data.params$largest.gap)
      alpha = data.params$alpha[data.params$largest.gap.diff == max(data.params$largest.gap.diff, na.rm = TRUE)][1]
    } else {
      alpha = alpha.start
    }
  } else {
    alpha = alpha.start
  }

  return(alpha)
}

#' Changepoint analysis - Only intended for developer use
#'
#' Performs changepoint analysis using PELT algorithm or A Nonparametric Approach for Multiple Changepoints.
#' The function is called by \code{\link{KRDetect.outliers.changepoint}} and is not intended for use by regular users of the package.
#'
#' @param x a numeric vector of data values.
#' @param cp.analysis.type a character string specifying the type of changepoint analysis
#'
#' Possible options are
#' \itemize{
#'   \item{\code{"parametric"}} {to perform changepoint analysis using PELT algorithm (Killick et al., 2012)}
#'   \item{\code{"nonparametric"}} {to perform a nonparametric approach for multiple changepoins (Matteson and James, 2014)}
#' }
#' @param pen.value A character string giving the formula for manual penalty used in PELT algorithm.
#' Only required for \code{cp.analysis.type = "parametric"}.
#' @param alpha.edivisive a numeric value giving the moment index used for determining the distance between and within segments in the nonparametric changepoint model.
#' @details This function performs changepoint analysis using parametric or nonparametric approach.
#' The function is exported for developer use only. It does not perform any checks on inputs since it is only convenience function for partitioning smoothing residuals into homogeneous segments.
#' @return A list is returned with elements:
#' \item{x}{a numeric vector of data values}
#' \item{cp.segmet}{an estimated integer membership vector for individual segments}
#' @references Killick R, Fearnhead P, Eckley IA (2012). Optimal detection of changepoints with a linear computational cost. Journal of the American Statistical Association, 107(500), 1590–1598.
#'
#' Matteson D, James N (2014). A Nonparametric Approach for Multiple Change Point Analysis of Multivariate Data. Journal of the American Statistical Association, 109(505), 334–345.
#'
#' Nicholas A. James, David S. Matteson (2014). ecp: An R Package for Nonparametric Multiple Change Point Analysis of Multivariate Data. Journal of Statistical Software, 62(7), 1-25, URL "http://www.jstatsoft.org/v62/i07/".
#'
#' Killick R, Haynes K, Eckley IA (2016). changepoint: An R package for changepoint analysis. R package version 2.2.2, <URL: https://CRAN.R-project.org/package=changepoint>.
#' @importFrom changepoint cpt.var cpts
#' @importFrom ecp e.divisive
changepoint <- function(x,
                        cp.analysis.type,
                        pen.value,
                        alpha.edivisive) {
  if (cp.analysis.type == "parametric"){
    model = cpt.var(x, method = "PELT", penalty = "Manual", pen.value = pen.value)
    cp.segment = c(NA, length(x)) # cp.index will contains index of segment corresponding to individual data

    if (length(cpts(model)) > 0) { # if some changepoints are detected
      segments = c(1, cpts(model)) # assigning detected change points

      for (i in 1:(length(segments) - 1)) { # changepoint indexes are in vector segments
        segment.length = segments[i + 1] - segments[i] # n.segments contains length of j-th segment in concrete day
        cp.segment[segments[i]:segments[i + 1] - 1] = i # assignment to cp.index column
      }

      cp.segment[segments[length(segments)]:length(x)[1]] = i + 1 # assignment to cp.index column for the last segment
    } else { # if no changepoint are detected cp.index contains only 1
      cp.segment = 1
      segments = 1
    }
  } else if(cp.analysis.type == "nonparametric") {
    cp.segment = e.divisive(as.matrix(x, ncol = 1), min.size = 60, alpha = alpha.edivisive)$cluster
  } else {
    stop ("Invalid changepoint analysis type")
  }

  result = list(x = x,
                cp.segment = cp.segment)

  return(result)
}

#' Grubbs test based identification of outliers on segments - Only intended for developer use
#'
#' Identification of outlier data values on individual homogeneous segments using Grubbs test.
#' The function is called by \code{\link{KRDetect.outliers.changepoint}} and is not intended for use by regular users of the package.
#'
#' @param x a numeric vector of data.
#' @param cp.segment an integer membership vector for individual segments.
#' @details This function detects outlier observations on individual segments using Grubbs test.
#' The function is exported for developer use only. It does not perform any checks on inputs since it is only convenience function for identification of outlier residuals.
#' @return A logical vector specifing the identified outliers, \code{TRUE} means that corresponding data value from vector \code{x} is detected as outlier.
#' @references Grubbs F (1950). Sample criteria for testing outlying observations. The Annals of Mathematical Statistics, 21(1), 27-58.
#'
#' Campulova M, Michalek J, Mikuska P, Bokal D (2018). Nonparametric algorithm for identification of outliers in environmental data. Journal of Chemometrics, 32, 453-463.
grubbs.detect <- function(x, cp.segment){
  outlier = c(rep(FALSE, length(x)))
  for (i in 1:max(cp.segment)) {
    if (length(x[(cp.segment == i) & x != 0]) > 2) {
      grubbs.test.result = grubbs.test(x[(cp.segment == i)])
      if (grubbs.test.result$outliers.exists) {
        for (j in grubbs.test.result$result$index) {
          outlier[cp.segment == i][j] = TRUE
        }
      }
    }
  }

  return(outlier)
}

#' Normal distribution based identification of outliers on segments - Only intended for developer use
#'
#' Identification of outlier data values on individual homogeneous segments using quantiles of normal distribution.
#' The function is called by \code{\link{KRDetect.outliers.changepoint}} and is not intended for use by regular users of the package.
#'
#' @param x a numeric vector of data.
#' @param cp.segment an integer membership vector for individual segments.
#' @param alpha.default a numeric value from interval (0,1) of alpha parameter determining the criterion for outlier detection:
#' the limits for outlier observations on individual segments are set as \emph{+/- (alpha/2-quantile of normal distribution with parameters corresponding to data on studied segment) * (sample standard deviation of data on corresponding segment)}
#' If \code{alpha.default = NULL}, its value on individual segments is estimated using Modified Algorithm A1 (Campulova et al., 2018).
#' @details This function detects outlier observations on individual segments using quantiles of normal distribution.
#' The function is exported for developer use only. It does not perform any checks on inputs since it is only convenience function for identification of outlier residuals.
#' @return A list is returned with elements:
#' \item{alpha}{a numeric vector of alpha parameters used for outlier identification on individual segments}
#' \item{outlier}{a logical vector specyfing the identified outliers, \code{TRUE} means that corresponding data value from vector \code{x} is detected as an outlier}
#' @references Campulova M, Michalek J, Mikuska P, Bokal D (2018). Nonparametric algorithm for identification of outliers in environmental data. Journal of Chemometrics, 32, 453-463.
#' @importFrom stats qnorm sd
normal.distr.quantiles.detect <- function(x, cp.segment, alpha.default) {
  outlier = c(rep(FALSE, length(x)))
  alpha = c(rep(NA, max(cp.segment)))
  Q.upper = c(rep(NA, max(cp.segment)))
  Q.lower = c(rep(NA, max(cp.segment)))
  for (i in 1:max(cp.segment)) {
    if (is.null(alpha.default)) {
      alpha[i] = find.alpha(x[(cp.segment == i)])
    } else {
      alpha[i] = alpha.default
    }

    Q.upper[(cp.segment == i)] = qnorm(1 - alpha[i] / 2, mean(x[(cp.segment == i)]), sd = sd(x[(cp.segment == i)]))
    Q.lower[(cp.segment == i)] = qnorm(alpha[i] / 2, mean(x[(cp.segment == i)]), sd = sd(x[(cp.segment == i)]))
  }
  outlier[((x > Q.upper) | (x < Q.lower))] = TRUE
  result = list(alpha = alpha,
                outlier = outlier)
  return(result)
}

#' Chebyshev inequality based identification of outliers on segments - Only intended for developer use
#'
#' Identification of outlier data values on individual homogeneous segments using Chebyshev inequality.
#' The function is called by \code{\link{KRDetect.outliers.changepoint}} and is not intended for use by regular users of the package.
#'
#' @param x a numeric vector of data.
#' @param cp.segment an integer membership vector for individual segments.
#' @param L.default a numeric value of \emph{L} parameter determining the criterion for outlier detection:
#' the limits for outlier observations on individual segments are set as \eqn{+/- L * sample standard deviation of data on the corresponding segment}
#' If \code{L.default = NULL}, its value on individual segments is estimated using Algorithm A1 (Campulova et al., 2018).
#' @details This function detects outlier observations on individual segments using Chebyshev inequality.
#' The function is exported for developer use only. It does not perform any checks on inputs since it is only convenience function for identification of outlier residuals.
#' @return A list is returned with elements:
#' \item{L}{a numeric vector of \emph{L} parameters used for outlier identification on individual segments}
#' \item{outlier}{a logical vector specifing the identified outliers, \code{TRUE} means that corresponding data value from vector \code{x} is detected as outlier}
#' @references Campulova M, Michalek J, Mikuska P, Bokal D (2018). Nonparametric algorithm for identification of outliers in environmental data. Journal of Chemometrics, 32, 453-463.
#' @importFrom stats sd
chebyshev.inequality.detect <- function(x, cp.segment, L.default) {
  outlier = c(rep(FALSE, length(x)))
  limit = c(rep(FALSE, length(x)))
  L = c(rep(NA, max(cp.segment)))

  for (i in 1:max(cp.segment)) {
    if (is.null(L.default)) {
      L[i] = find.L(x[(cp.segment == i)])
    } else {
      L[i] = L.default
    }

    limit[(cp.segment == i)] = L[i] * sd(x[(cp.segment == i)])
  }

  outlier[((x > limit) | (x < -limit))] = TRUE

  result = list(L = L,
                outlier = outlier)
  return(result)
}

#' Segment length control - Only intended for developer use
#'
#' Control of a number of data values on individual segments.
#' In case a number of data values on a segment is too small, the segment is (under the presumption of meeting certain conditions)
#' merged with the previous one. The first segment can be merged with the previous one.
#'
#' @param index a numeric vector of design points.
#' @param x a numeric vector of data.
#' @param cp.segment an integer membership vector for individual segments.
#' @param min.segment.length a numeric value giving minimal required number of observations on segments from changepoint analysis.
#' If a segment contains less than \code{min.segment.length} observations and the variances of data on the segment and the previous one are supposed to be equal (based on Levene´s test (Fox, 2016) for homogeneity of variances), the segment is merged with previous one.
#' Analogous, the first segment can be merged with the second one.
#' @param segment.length.for.merge a numeric value giving giving minimal required number of observations on segments for performing the homogeneity test within changepoint split control.
#' A segment with fewer data than \code{segment.length.for.merge} is merged with the previous one without testing the homogeneity of variances (the first segment is merged with the second one).
#' @details Control of data splitting into segments.
#' If a segment contains less than a given number of observations specified by the user and the variances of data on the segment and the previous one are equally based on the robust version of Levene's test, the segment is merged with previous one.
#' Analogous, the first segment can be merged with the second one.
#' The user can also specify a minimum length of a segment for performing the homogeneity test. A segment with fewer data than this minimal length is merged with the previous one without testing the homogeneity of variances.
#' The function is called by \code{\link{KRDetect.outliers.changepoint}} and is not intended for use by regular users of the package.
#' @return An integer membership vector for individual segments
#' @references Fox J (2016). Applied regression analysis and generalized linear models. 3 edition. Los Angeles: SAGE. ISBN 9781452205663.
#' @importFrom car leveneTest
#' @importFrom stats lm
segment.length.control <- function(index, x, cp.segment, min.segment.length, segment.length.for.merge) {
  i = 1

  repeat {
    i = i + 1
    if (length(x[cp.segment == i]) < min.segment.length) {
      levene.test.p.value = as.data.frame(leveneTest(lm(x[(cp.segment == (i - 1) | cp.segment == i)] ~ as.factor(cp.segment[(cp.segment == (i - 1) | cp.segment == i)])), location = "median"))$`Pr(>F)`[1]
      if (levene.test.p.value > 0.05 | length(x[cp.segment == i]) < segment.length.for.merge) {
        begin = min(which(cp.segment == i))
        cp.segment[begin:length(x)] = cp.segment[begin:length(x)] - 1
        i = i - 1
      }
    }
    if (i == max(cp.segment)) {
      break
    }
  }

  if (length(x[cp.segment == 1]) < min.segment.length) {
    levene.test.p.value = as.data.frame(leveneTest(lm(x[(cp.segment == 1 | cp.segment == 2)] ~ as.factor(cp.segment[(cp.segment == 1 | cp.segment == 2)])), location = "median"))$`Pr(>F)`[1]

    if (levene.test.p.value > 0.05 | length(x[cp.segment == 1]) < segment.length.for.merge) {
      begin = min(which(cp.segment == 2))
      cp.segment[begin:length(x)] = cp.segment[begin:length(x)] - 1
    }
  }

  return(cp.segment)
}

#' Identification of outliers using changepoint analysis
#'
#' Identification of outliers in environmental data using method based on kernel smoothing, changepoint analysis of smoothing residuals and subsequent analysis of residuals on homogeneous segments (Campulova et al., 2018).
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
#' @param perform.cp.analysis a logical value specifying if changepoint analysis is performed. If \code{TRUE} (default), smoothing residuals are partitioned into homogeneous segments.
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
#' @param cp.analysis.type a character string specifying the type of changepoint analysis.
#'
#' Possible options are
#' \itemize{
#'   \item{\code{"parametric"}} {(default) to perform changepoint analysis using PELT algorithm (Killick et al., 2012)}
#'   \item{\code{"nonparametric"}} {to perform a nonparametric approach for multiple changepoins (Matteson and James, 2014)}
#' }
#' @param pen.value a character string giving the formula for manual penalty used in PELT algorithm.
#' Only required for \code{cp.analysis.type = "parametric"}. Default is \code{pen.value = "5*log(n)"}.
#' @param alpha.edivisive a numeric value giving the moment index used for determining the distance between and within segments in nonparametric changepoint model. Default is \code{alpha.edivisive = 0.3}.
#' @param min.segment.length a numeric value giving minimal required number of observations on segments from changepoint analysis.
#' If a segment contains less than \code{min.segment.length} observations and the variances of data on the segment and the previous one are supposed to be equal (based on Levene´s test (Fox, 2016) for homogeneity of variances), the segment is merged with previous one.
#' Analogous, the first segment can be merged with the second one. Default is \code{min.segment.length = 30}.
#' @param segment.length.for.merge a numeric value giving minimal required number of observations on segments for performing the homogeneity test within changepoint split control.
#' A segment with less data than \code{segment.length.for.merge} is merged with the previous one without testing the homogeneity of variances (the first segment is merged with the second one). Default is \code{min.segment.length.for.merge = 15}.
#' @param method a character string specifying the method for identification of outlier residuals.
#'
#' Possible options are
#' \itemize{
#'   \item{\code{"auto"}} {(default) for automatic selection based on the structure of the residuals}
#'   \item{\code{"grubbs.test"}} {for Grubbs test}
#'   \item{\code{"normal.distribution"}} {for quantiles of normal distribution}
#'   \item{\code{"chebyshev.inequality"}} {for chebyshev inequality}
#' }
#' @param prefer.grubbs a logical variable specyfing if Grubbs test for identification of outlier residuals is preferred to quantiles of normal distribution.
#' \code{TRUE} (default) means that Grubbs test is preferred. Only required for \code{method = "auto"}.
#' @param alpha.default a numeric value from interval (0,1) of \emph{alpha} parameter determining the criterion for (residual) outlier detection:
#' the limits for outlier residuals on individual segments are set as \eqn{+/- (alpha/2-quantile of normal distribution with parameters corresponding to residuals on studied segment) * (sample standard deviation of residuals on corresponding segment)}.
#' If \code{alpha.default = NULL} (default), its value on individual segments is estimated using Modified Algorithm A1 (Campulova et al., 2018).
#' @param L.default a numeric value of \emph{L} parameter determining the criterion for outlier (residual) detection:
#' the limits for outlier residuals on individual segments are set as \eqn{+/- L * sample standard deviation of residuals on corresponding segment}.
#' If \code{L.default = NULL} (default), its value on individual segments is estimated using Algorithm A1 (Campulova et al., 2018).
#' @details This function identifies outliers in time series using procedure based on kernel smoothing, changepoint analysis of smoothing residuals and subsequent analysis of residuals on homogeneous segments (Campulova et al., 2018).
#' Three different approaches (Grubbs test, quantiles of normal distribution, Chebyshev inequality), that can be selected automatically based on data structure or specified by the user, can be used to detect outlier residuals.
#' Crucial for the method is the choice of parameters alpha and \emph{L} for quantiles of normal distribution and Chebyshev inequality approach, that define the criterion for outlier detection. These values can be specified by the user
#' or estimated automatically using data driven algorithms (Campulova et al., 2018).
#' @return A \code{"KRDetect"} object which contains a list with elements:
#' \item{method.type}{a character string giving the type of method used for outlier idetification}
#' \item{x}{a numeric vector of observations}
#' \item{index}{a numeric vector of index design points assigned to individual observations}
#' \item{smoothed}{a numeric vector of estimates of the kernel regression function (smoothed data)}
#' \item{changepoints}{an integer membership vector for individual segments}
#' \item{normality.results}{a data.frame of normality results of residuals on individual segments}
#' \item{detection.method}{a character string giving the type of method used for identification of outlier residuals}
#' \item{alpha}{a numeric vector of alpha parameters used for outlier identification on individual segments}
#' \item{L}{a numeric vector of \emph{L} parameters used for outlier identification on individual segments}
#' \item{outlier}{a logical vector specyfing the identified outliers, \code{TRUE} means that corresponding observation from vector \code{x} is detected as outlier}
#' @references Campulova M, Michalek J, Mikuska P, Bokal D (2018). Nonparametric algorithm for identification of outliers in environmental data. Journal of Chemometrics, 32, 453-463.
#'
#' Gasser T, Kneip A, Kohler W (1991). A flexible and fast method for automatic smoothing. Journal of the American Statistical Association, 86, 643–652.
#'
#' Herrmann E (1997). Local bandwidth choice in kernel regression estimation. Journal of Computational and Graphical Statistics, 6(1), 35–54.
#'
#' Eva Herrmann; Packaged for R and enhanced by Martin Maechler (2016). lokern: Kernel Regression Smoothing with Local or Global Plug-in Bandwidth. R package version 1.1-8. https://CRAN.R-project.org/package=lokern.
#'
#' Killick R, Fearnhead P, Eckley IA (2012). Optimal detection of changepoints with a linear computational cost. Journal of the American Statistical Association, 107(500), 1590–1598.
#'
#' Killick R, Haynes K, Eckley IA (2016). changepoint: An R package for changepoint analysis. R package version 2.2.2, <URL: https://CRAN.R-project.org/package=changepoint>.
#'
#' Matteson D, James N (2014). A Nonparametric Approach for Multiple Change Point Analysis of Multivariate Data. Journal of the American Statistical Association, 109(505), 334–345.
#'
#' Nicholas A. James, David S. Matteson (2014). ecp: An R Package for Nonparametric Multiple Change Point Analysis of Multivariate Data. Journal of Statistical Software, 62(7), 1-25, URL "http://www.jstatsoft.org/v62/i07/".
#'
#' Brys G, Hubert M, Struyf A (2008). Goodness-of-fit tests based on a robust measure of skewness. Computational Statistics, 23(3), 429–442.
#'
#' Todorov V, Filzmoser P (2009). An Object-Oriented Framework for Robust Multivariate Analysis. Journal of Statistical Software, 32(3), 1-47. URL http://www.jstatsoft.org/v32/i03/.
#'
#' Box G, Cox D (1964). An analysis of transformations. Journal of the Royal Statistical Society: Series B, 26, 211–234.
#'
#' Venables WN, Ripley BD (2002). Modern Applied Statistics with S. New York, fourth edition. ISBN 0-387-95457-0, URL http://www.stats.ox.ac.uk/pub/MASS4.
#'
#' Grubbs F (1950). Sample criteria for testing outlying observations. The Annals of Mathematical Statistics, 21(1), 27-58.
#'
#' Fox J (2016). Applied regression analysis and generalized linear models. 3 edition. Los Angeles: SAGE. ISBN 9781452205663.
#' @examples data("mydata", package = "openair")
#' x = mydata$o3[format(mydata$date, "%m %Y") == "12 2002"]
#' result = KRDetect.outliers.changepoint(x)
#' summary(result)
#' plot(result)
#' plot(result, show.segments = FALSE)
#' @importFrom changepoint cpt.var cpts
#' @importFrom ecp e.divisive
#' @importFrom lokern glkerns lokerns
#' @importFrom stats lm na.omit sd
#' @export
KRDetect.outliers.changepoint <- function(x,
                                          perform.smoothing = TRUE,
                                          perform.cp.analysis = TRUE,
                                          bandwidth.type = "local",
                                          bandwidth.value = NULL,
                                          kernel.order = 2,
                                          cp.analysis.type = "parametric",
                                          pen.value = "5*log(n)",
                                          alpha.edivisive = 0.3,
                                          min.segment.length = 30,
                                          segment.length.for.merge = 15,
                                          method = "auto",
                                          prefer.grubbs = TRUE,
                                          alpha.default = NULL,
                                          L.default = NULL) {
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

  if (!cp.analysis.type %in% c("parametric", "nonparametric")) {
    stop("Invalid changepoint analysis method")
  }

  if (!method %in% c("auto", "grubbs.test", "normal.distribution", "chebyshev.inequality")) {
    stop("Invalid method")
  }

  # parameter settings
  normality.residuals = FALSE
  normality.residuals.transformed = FALSE
  normality.test.residuals.results = NULL
  normality.test.residuals.transformed.results = NULL
  data.input = data.frame(x)
  data.input$index = c(1:dim(data.input)[1])
  data = na.omit(data.input)

  if (dim(data)[1] < 4) {
    stop("Not enough data")
  }

  # data smoothing
  data$smoothed = NA

  if (perform.smoothing) {
    data$smoothed = smoothing(data$index, data$x, bandwidth.type, bandwidth.value, kernel.order)$data.smoothed
    data$residuals = data$x - data$smoothed
  } else {
    data$residuals = data$x
  }

  # changepoint analysis
  if (perform.cp.analysis) {
    data$cp.segment = changepoint(data$residuals, cp.analysis.type, pen.value, alpha.edivisive)$cp.segment

    if (max(data$cp.segment) > 1) {
      data$cp.segment = segment.length.control(data$index, data$residuals, data$cp.segment, min.segment.length, segment.length.for.merge)
    }
  } else {
    data$cp.segment = 1
    segments = 1
  }

  for (i in 1:max(data$cp.segment)) {
    if (length(data$x[(data$cp.segment == i) & (data$residuals != 0)]) < 10) {
      stop("Not enough nonzero residuals on homogeneous segments")
    }
  }

  # test of residual normality
  normality.test.residuals.results = data.frame(segment = NA, crit.left = NA, crit.right = NA, test.stat = NA, normality = NA)

  for (i in 1:max(data$cp.segment)) {
    temp = mc.test(data$residuals[(data$cp.segment == i)])

    if ((temp$test.stat < temp$crit[1]) | (temp$test.stat > temp$crit[2])) {
      normality = "rejected"
    } else {
      normality = "not rejected"
    }

    normality.test.residuals.results = rbind(normality.test.residuals.results, c(i, temp$crit[1], temp$crit[2], temp$test.stat, normality))
  }

  normality.test.residuals.results = normality.test.residuals.results[-1, ]

  if (length(which(normality.test.residuals.results$normality == "not rejected")) == dim(normality.test.residuals.results)[1]) {
    normality.residuals = TRUE
  }

  # Box Cox transform
  if (!normality.residuals) {
    data$residuals.transformed = NA

    for (i in 1:max(data$cp.segment)) {
      data$residuals.transformed[(data$cp.segment == i)] = boxcoxTransform(data$residuals[(data$cp.segment == i)])$x.transformed
    }
  }

  # test of transformed residuals normality
  if (!normality.residuals) {
    normality.test.residuals.transformed.results = data.frame(segment = NA, crit.left = NA, crit.right = NA, test.stat = NA, normality = NA)

    for (i in 1:max(data$cp.segment)) {
      temp = mc.test(data$residuals.transformed[(data$cp.segment == i)])

      if ((temp$test.stat < temp$crit[1]) | (temp$test.stat > temp$crit[2])) {
        normality = "rejected"
      } else {
        normality = "not rejected"
      }

      normality.test.residuals.transformed.results = rbind(normality.test.residuals.transformed.results, c(i, temp$crit[1], temp$crit[2], temp$test.stat, normality))
    }

    normality.test.residuals.transformed.results = normality.test.residuals.transformed.results[-1, ]
    if (length(which(normality.test.residuals.transformed.results$normality == "not rejected")) == dim(normality.test.residuals.transformed.results)[1]) {
      normality.residuals.transformed = TRUE
    }
  }


  # outlier detection using Grubbs test
  if ((method == "auto" & (normality.residuals | normality.residuals.transformed) & prefer.grubbs) | method == "grubbs.test") {

    detection.method = "Grubbs test"
    if (normality.residuals) {
      data$outlier = grubbs.detect(data$residuals, data$cp.segment)
    } else{
      data$outlier = grubbs.detect(data$residuals.transformed, data$cp.segment)
    }
  }

  # outlier detection using quantiles of normal distribution
  if ((method == "auto" & (normality.residuals | normality.residuals.transformed) & !prefer.grubbs) | method == "normal.distribution") {

    detection.method = "normal distribution"
    if (normality.residuals) {

      temp = normal.distr.quantiles.detect(data$residuals, data$cp.segment, alpha.default)
      data$outlier = temp$outlier
      alpha = temp$alpha
    } else {
      temp = normal.distr.quantiles.detect(data$residuals.transformed, data$cp.segment, alpha.default)
      data$outlier = temp$outlier
      alpha = temp$alpha
    }
  } else {
    alpha = alpha.default
  }

  # outlier detection using Chebyshev inequality
  if ((method == "auto" & (!normality.residuals & !normality.residuals.transformed)) | method == "chebyshev.inequality") {
    detection.method = "Chebyshev inequality"
    temp = chebyshev.inequality.detect(data$residuals, data$cp.segment, L.default)
    data$outlier = temp$outlier
    L = temp$L

  } else {
    L = L.default
  }

  # results
  if (normality.residuals) {
    normality.residuals.transformed = NULL
  }

  data.output = merge(data.input, data, by = c("index", "x"), all.x = TRUE)
  data.output = data.output[order(data.output$index),]
  normality.results = list(normality.residuals = normality.residuals,
                           normality.residuals.transformed = normality.residuals.transformed,
                           normality.test.residuals.results = normality.test.residuals.results,
                           normality.test.residuals.transformed.results = normality.test.residuals.transformed.results)

  result = list(method.type = "changepoint analysis",
                x = data.output$x,
                index = data.output$index,
                smoothed = data.output$smoothed,
                changepoints = data.output$cp.segment,
                normality.results = normality.results,
                detection.method = detection.method,
                alpha = alpha,
                L = L,
                outlier = data.output$outlier)

  class(result) = "KRDetect"

  return(result)
}
