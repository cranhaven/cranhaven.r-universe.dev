tnl <- function(n., m., l) {
  if (any(c(n., m.) < (2 * l + 1))) {
    stop(paste("n,m must be > 2l", "\n", ""))
  }
  if (n. == m.) {
    m <- m.
    n <- n.
  } else {
    m <- max(n., m.)
    n <- min(n., m.)
  }
  j1 <- NULL
  j1[1] <- 0
  for (i in 2:n) {
    j1[i] <- floor(m / n * (i - 1))
  }
  j2 <- NULL
  for (i in 1:(n - 1)) {
    j2[i] <- ceiling(m / n * (i + 1))
  }
  j2[n] <- (m + 1)

  x <- NULL
  for (j in 0:n) {
    x <- rbind(x, t(partitions::compositions(j, m)))
  }
  ss <- 0
  prob <- NULL
  for (v in 1:n) {
    ss <- ss + 1
    nn <- nrow(x)
    count <- 0
    for (kk in 1:nn) {
      zz2 <- NULL
      M <- x[kk, ]
      for (i in 1:l) {
        if (sum(M[1:(j2[i + (l - 1)])]) >= i) zz2[i] <- 1 else zz2[i] <- 0
      }
      for (i in (l + 1):(n - l)) {
        if (sum(M[1:(j1[i - (l - 1)])]) < i & sum(M[1:(j2[i + (l - 1)])]) >= i)
          {
          zz2[i] <- 1
        } else {
          zz2[i] <- 0
        }
      }
      for (i in (n - l + 1):n) {
        if (sum(M[1:(j1[i - (l - 1)])]) < i) zz2[i] <- 1 else zz2[i] <- 0
      }
      if (sum(zz2) == v) count <- count + 1
    }
    prob[ss] <- count
  }
  prob <- prob / choose((m + n), n)
  res <- NULL
  for (t in 1:n) {
    res[t] <- sum(prob[1:t])
  }
  result <- list(method = "exact", pmf = prob, cdf = res)
  return(result)
}

stest <- function(a., b., l) {
  if (any(c(length(a.), length(b.)) < (2 * l + 1))) {
    stop(paste("n,m must be > 2l", "\n", ""))
  }
  if (length(a.) <= length(b.)) {
    a <- sort(a.)
    b <- sort(b.)
    n <- length(a)
    m <- length(b)
  }
  if (length(a.) > length(b.)) {
    b <- sort(a.)
    a <- sort(b.)
    n <- length(a)
    m <- length(b)
  }

  j1 <- NULL
  j1[1] <- 0
  for (i in 2:n) {
    j1[i] <- floor(m / n * (i - 1))
  }
  j2 <- NULL
  for (i in 1:(n - 1)) {
    j2[i] <- ceiling(m / n * (i + 1))
  }
  j2[n] <- (m + 1)

  t <- 0
  for (i in 1:l) {
    if (a[i] < b[j2[i + (l - 1)]]) t <- t + 1
  }
  for (i in (l + 1):(n - l)) {
    if (a[i] >= b[j1[i - (l - 1)]] & a[i] < b[j2[i + (l - 1)]]) t <- t + 1
  }
  for (i in (n - l + 1):n) {
    if (a[i] >= b[j1[i - (l - 1)]]) t <- t + 1
  }
  return(t)
}

tnl.sim <- function(n., m., l, trial = 100000) {
  x <- y <- NULL
  statistic <- NULL
  for (i in 1:trial) {
    x <- stats::rnorm(n.)
    y <- stats::rnorm(m.)
    statistic[i] <- stest(x, y, l)
  }
  statistict <- (plyr::count(statistic) / trial)$freq
  statistict <- c(rep(0, (l - 1)), statistict)
  res <- NULL
  for (t in seq_along(statistict)) {
    res[t] <- sum(statistict[1:t])
  }
  result <- list(method = "Monte Carlo simulation", pmf = statistict, cdf = res)
  return(result)
}

a.ki <- function(n, m, k, i) {
  (choose((i + k - 1), (i - 1)) *
    choose(((m + n) - i - k), (n - i))) /
    choose((m + n), n)
}


#' Non-parametric tests for the two-sample problem based
#' on order statistics and power comparisons
#' @export
#' @rdname tnl.test
#' @param x the first (non-empty) numeric vector of data values.
#' @param y the second (non-empty) numeric vector of data values.
#' @param l class parameter of \eqn{T_n^{(\ell)}}.
#' @param exact the method that will be used.
#' "NULL" or a logical indicating whether an exact should be computed.
#' See 'Details' for the meaning of NULL.
#' @description  \code{\link{tnl.test}} performs a nonparametric test for
#'            two sample test on vectors of data.
#' @return \code{\link{tnl.test}} returns a list with the following components
#'    \describe{
#'      \item{\code{statistic:}}{the value of the test statistic.}
#'      \item{\code{p.value:}}{the p-value of the test.}}
#' @details A non-parametric two-sample test is performed for testing null
#'  hypothesis \eqn{H_0:F=G} against the alternative
#'  hypothesis \eqn{H_1:F\not= G}.
#'  The assumptions of the \eqn{T_n^{(\ell)}} test are that both
#'  samples should come from a continuous distribution and the samples
#'  should have the same sample size.
#'
#'  Missing values are silently omitted from \eqn{x} and \eqn{y}.
#'
#'  Exact and simulated p-values are available for the \eqn{T_n^{(\ell)}} test.
#'  If exact ="NULL" (the default) the p-value is computed based
#'  on exact distribution when the sample size is less than 11.
#'  Otherwise, p-value is computed based on a Monte Carlo simulation.
#'  If exact ="TRUE", an exact p-value is computed. If exact="FALSE"
#'  , a Monte Carlo simulation is performed to compute the p-value.
#'  It is recommended to calculate the p-value by a Monte Carlo simulation
#'  (use exact="FALSE"), as it takes too long to calculate the exact
#'  p-value when the sample size is greater than 10.
#'
#'  The probability mass function (pmf), cumulative density function (cdf)
#'  and quantile function of \eqn{T_n^{(\ell)}}
#'  are also available in this package, and the above-mentioned conditions
#'  about exact ="NULL", exact ="TRUE" and exact="FALSE" is also valid
#'  for these functions.
#'
#'  Exact distribution of \eqn{T_n^{(\ell)}}
#'  test is also computed under Lehman alternative.
#'
#'  Random number generator of \eqn{T_n^{(\ell)}}
#'  test statistic are provided under null hypothesis in the library.
#' @references Karakaya, K., Sert, S., Abusaif, I., Kuş, C., Ng, H. K. T.,
#'  & Nagaraja, H. N. (2023).
#'  A Class of Non-parametric Tests for the Two-Sample Problem based on
#'  Order Statistics and Power Comparisons. Submitted paper.
#'
#' Aliev, F., Özbek, L., Kaya, M. F., Kuş, C., Ng, H. K. T.,
#' & Nagaraja, H. N. (2022). A nonparametric test for the two-sample problem
#'  based on order statistics.
#'  Communications in Statistics-Theory and Methods, 1-25.
#' @examples
#' require(stats)
#' x <- rnorm(7, 2, 0.5)
#' y <- rnorm(5, 0, 1)
#' tnl.test(x, y, l = 2)
tnl.test <- function(x, y, l, exact = "NULL") {
  if (any(is.na(x)) | any(is.na(y))) {
    warning(
      "Since the data should not contain missing values,
we exclude the missing values from the data"
    )
  }
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  n <- length(x)
  m <- length(y)
  stat <- stest(x, y, l)

  if (exact == "TRUE") {
    p.value <- tnl(n, m, l)$cdf[stat]
  }
  if (exact == "FALSE") {
    p.value <- tnl.sim(n, m, l)$cdf[stat]
  }
  if (exact == "NULL" & n <= 10 & m <= 10) {
    p.value <- tnl(n, m, l)$cdf[stat]
  }
  if (exact == "NULL" & n > 10 | m > 10) {
    p.value <- tnl.sim(n, m, l)$cdf[stat]
  }
  result <- list(statistic = stat, p.value = p.value)
  return(result)
}


#' Distribution function of \eqn{T_n^{(\ell)}} against the specified quantiles
#' @export
#' @rdname tnl.test
#' @param k,q vector of quantiles.
#' @param n,m samples size.
#' @param l class parameter of \eqn{T_n^{(\ell)}}
#' @param exact the method that will be used. "NULL" or a logical indicating
#'         whether an exact should be computed.
#'         See 'Details' for the meaning of NULL.
#' @param trial number of trials for simulation.
#' @description  \code{\link{ptnl}} gives the distribution function of
#' \eqn{T_n^{(\ell)}} against the specified quantiles.
#' @return \code{\link{ptnl}} returns a list with the following components
#'    \describe{
#'      \item{\code{method}:}{The method that was used (exact or simulation).
#'                  See 'Details'.}
#'      \item{\code{cdf}:}{distribution function of \eqn{T_n^{(\ell)}}
#'                  against the specified quantiles.}
#'    }
#' @examples
#' ptnl(q = c(2, 5), n = 6, m = 5, l = 2, trial = 100000)
ptnl <- function(q, n, m, l, exact = "NULL", trial = 100000) {
  if (exact == "TRUE") {
    ptnl <- tnl(n, m, l)$cdf
    method <- "exact"
  }
  if (exact == "FALSE") {
    ptnl <- tnl.sim(n, m, l, trial)$cdf
    method <- "Monte Carlo simulation"
  }
  if (exact == "NULL" & n <= 10 & m <= 10) {
    ptnl <- tnl(n, m, l)$cdf
    method <- "exact"
  }
  if (exact == "NULL" & n > 10 | m > 10) {
    ptnl <- tnl.sim(n, m, l, trial)$cdf
    method <- "Monte Carlo simulation"
  }
  cdfq <- NULL
  for (i in seq_along(q)) {
    if (q[i] < l) {
      cdfq[i] <- 0
    }
    if (q[i] > min(n, m)) {
      cdfq[i] <- 1
    }
    if (q[i] >= l & q[i] <= min(n, m)) {
      cdfq[i] <- ptnl[q[i]]
    }
  }
  result <- list(method = method, cdf = cdfq)
  return(result)
}

#' Density of \eqn{T_n^{(\ell)}} against the specified quantiles
#' @export
#' @rdname tnl.test
#' @param k,q vector of quantiles.
#' @param n,m samples size.
#' @param l class parameter of \eqn{T_n^{(\ell)}}
#' @param trial number of trials for simulation.
#' @param exact the method that will be used.
#' "NULL" or a logical indicating whether
#' an exact should be computed. See 'Details' for the meaning of NULL.
#' @description  \code{\link{dtnl}} gives the density of \eqn{T_n^{(\ell)}}
#' against the specified quantiles.
#' @return \code{\link{dtnl}} returns a list with the following components
#'    \describe{
#'      \item{\code{method}:}{The method that was used (exact or simulation).
#'      See 'Details'.}
#'      \item{\code{pmf}:}{density of \eqn{T_n^{(\ell)}}
#'       against the specified quantiles.}
#'    }
#' @examples
#' dtnl(k = c(1, 3, 6), n = 7, m = 5, l = 2)
dtnl <- function(k, n, m, l, exact = "NULL", trial = 100000) {
  if (exact == "TRUE") {
    dtnl <- tnl(n, m, l)$pmf
    method <- "exact"
  }
  if (exact == "FALSE") {
    dtnl <- tnl.sim(n, m, l, trial)$pmf
    method <- "Monte Carlo simulation"
  }
  if (exact == "NULL" & n <= 10 & m <= 10) {
    dtnl <- tnl(n, m, l)$pmf
    method <- "exact"
  }
  if (exact == "NULL" & n > 10 | m > 10) {
    dtnl <- tnl.sim(n, m, l, trial)$pmf
    method <- "Monte Carlo simulation"
  }
  pmfk <- NULL
  for (i in seq_along(k)) {
    if (k[i] < l | k[i] > min(n, m)) {
      pmfk[i] <- 0
    } else {
      pmfk[i] <- dtnl[k[i]]
    }
  }
  result <- list(method = method, pmf = pmfk)
  return(result)
}

#' Quantile function of \eqn{T_n^{(\ell)}}
#' @export
#' @rdname tnl.test
#' @param p vector of probabilities.
#' @param n,m samples size.
#' @param l class parameter of \eqn{T_n^{(\ell)}}.
#' @param exact the method that will be used. "NULL" or a logical
#' indicating whether an exact should be computed.
#' See 'Details' for the meaning of NULL.
#' @param trial number of trials for simulation.
#' @description  \code{\link{qtnl}} gives the quantile function of
#' \eqn{T_n^{(\ell)}} against the specified probabilities.
#' @return \code{\link{qtnl}} returns a list with the following components
#'    \describe{
#'      \item{\code{method}:}{The method that was used (exact or simulation).
#'                             See 'Details'.}
#'      \item{\code{quantile}:}{quantile function against the specified
#'                                probabilities.}
#'    }
#' @examples
#' qtnl(p = c(.3, .9), n = 4, m = 5, l = 1)
qtnl <- function(p, n, m, l, exact = "NULL", trial = 100000) {
  if (any(p < 0) | any(p > 1)) {
    stop(paste("p must be between 0 and 1", "\n", ""))
  }
  if (exact == "TRUE") {
    cdf <- tnl(n, m, l)$cdf
    method <- "exact"
  }
  if (exact == "FALSE") {
    cdf <- tnl.sim(n, m, l, trial)$cdf
    method <- "Monte Carlo simulation"
  }
  if (exact == "NULL" & n <= 10 & m <= 10) {
    cdf <- tnl(n, m, l)$cdf
    method <- "exact"
  }
  if (exact == "NULL" & n > 10 | m > 10) {
    cdf <- tnl.sim(n, m, l, trial)$cdf
    method <- "Monte Carlo simulation"
  }

  q <- NULL
  for (j in seq_along(p)) {
    for (i in seq_along(cdf)) {
      if (cdf[i] > p[j]) {
        break
      }
    }
    q[j] <- i
  }
  result <- list(method = method, quantile = q)
  return(result)
}

#' Random generation for the \eqn{T_n^{(\ell)}}
#' @export
#' @rdname tnl.test
#' @param N number of observations. If length(N) > 1, the length is taken
#'               to be the number required.
#' @param n,m samples size.
#' @param l class parameter of \eqn{T_n^{(\ell)}}
#' @description \code{\link{rtnl}} generates random values from
#'      \eqn{T_n^{(\ell)}}.
#' @return \code{\link{rtnl}} return *N* of the generated random values.
#' @examples
#' rtnl(N = 20, n = 7, m = 10, l = 1)
rtnl <- function(N, n, m, l) {
  x <- y <- NULL
  statistic <- NULL
  for (i in 1:N) {
    x <- stats::rnorm(n)
    y <- stats::rnorm(m)
    statistic[i] <- stest(x, y, l)
  }
  return(statistic)
}

#' Function that calculates moments
#' @export
#' @rdname tnl.test
#' @param l class parameter of \eqn{T_n^{(\ell)}}
#' @param n.,m. samples size.
#' @description [tnl_mean()] gives an expression for
#' \eqn{E(T_n^{(\ell)})} under \eqn{H_0:F=G}.
#' @return [tnl_mean()] return the mean of \eqn{T_n^{(\ell)}}.
#' @examples
#' require(base)
#' tnl_mean(n. = 11, m. = 8, l = 1)
tnl_mean <- function(n., m., l) {
  if (any(c(n., m.) < (2 * l + 1))) {
    warning("n,m must be > 2l")
  }
  if (n. == m.) {
    m <- m.
    n <- n.
  } else {
    m <- max(n., m.)
    n <- min(n., m.)
  }
  j1 <- NULL
  j1[1] <- 0
  for (i in 2:n) {
    j1[i] <- floor(m / n * (i - 1))
  }
  j2 <- NULL
  for (i in 1:(n - 1)) {
    j2[i] <- ceiling(m / n * (i + 1))
  }
  j2[n] <- (m + 1)
  pi <- NULL
  for (i in 1:n) {
    p <- 0
    for (k in j1[max(1, i - (l - 1))]:(j2[min(n, i + (l - 1))] - 1)) {
      p <- p + a.ki(n, m, k, i)
    }
    pi[i] <- p
  }
  M <- sum(pi)
  return(M)
}

#' The distribution function of\eqn{T_n^{(\ell)}} under Lehmann alternatives.
#' @export
#' @rdname tnl.test
#' @param l class parameter of \eqn{T_n^{(\ell)}}
#' @param n.,m. samples size.
#' @param gamma parameter of Lehmann alternative
#' @description \code{\link{ptnl.lehmann}}  gives the  distribution function of
#' \eqn{T_n^{(\ell)}} under Lehmann alternatives.
#' @return \code{\link{ptnl.lehmann}} return vector of the distribution under
#' Lehmann alternatives against the specified gamma.
#' @examples
#' ptnl.lehmann(q = 3, n. = 5, m. = 7, l = 2, gamma = 1.2)
ptnl.lehmann <- function(q, n., m., l, gamma) {
  if (any(c(n., m.) < (2 * l + 1))) {
    stop(paste("n,m must be > 2l", "\n", ""))
  }
  if (n. == m.) {
    m <- m.
    n <- n.
  } else {
    m <- max(n., m.)
    n <- min(n., m.)
  }
  j1 <- NULL
  j1[1] <- 0
  for (i in 2:n) {
    j1[i] <- floor(m / n * (i - 1))
  }
  j2 <- NULL
  for (i in 1:(n - 1)) {
    j2[i] <- ceiling(m / n * (i + 1))
  }
  j2[n] <- (m + 1)

  x <- NULL
  for (j in 0:n) {
    x <- rbind(x, t(partitions::compositions(j, m)))
  }
  ss <- 0
  prob <- lehmann <- NULL
  for (v in 1:n) {
    ss <- ss + 1
    nn <- nrow(x)
    count <- 0
    for (kk in 1:nn) {
      zz2 <- NULL
      M <- x[kk, ]
      for (i in 1:l) {
        if (sum(M[1:(j2[i + (l - 1)])]) >= i) zz2[i] <- 1 else zz2[i] <- 0
      }
      for (i in (l + 1):(n - l)) {
        if (sum(M[1:(j1[i - (l - 1)])]) < i & sum(M[1:(j2[i + (l - 1)])]) >= i)
          {
          zz2[i] <- 1
        } else {
          zz2[i] <- 0
        }
      }
      for (i in (n - l + 1):n) {
        if (sum(M[1:(j1[i - (l - 1)])]) < i) zz2[i] <- 1 else zz2[i] <- 0
      }
      if (sum(zz2) == v) {
        plam <- 1
        for (jj in 1:(m - 1)) {
          plam <- plam * gamma(sum(M[1:jj]) + jj * gamma) /
            gamma(sum(M[1:(jj + 1)])
            + jj * gamma + 1)
        }
        plam <- plam * gamma(sum(M) + m * gamma) / gamma(n + m * gamma + 1)
        const <- (factorial(m) * factorial(n) * (gamma^m)) / factorial(M[1])
        lehm <- plam * const
        count <- count + lehm
      }
    }
    lehmann[v] <- count
    res <- NULL
    for (t in seq_along(lehmann)) {
      res[t] <- sum(lehmann[1:t])
    }
  }
  ptnllehq <- NULL
  for (i in seq_along(q)) {
    if (q[i] < 1) {
      ptnllehq[i] <- 0
    }
    if (q[i] >= 1 & q[i] <= n) {
      ptnllehq[i] <- res[q[i]]
    }
    if (q[i] > n) {
      ptnllehq[i] <- 1
    }
  }
  return(ptnllehq)
}


#' The density of \eqn{T_n^{(\ell)}} under Lehmann alternatives.
#' @export
#' @rdname tnl.test
#' @param l class parameter of \eqn{T_n^{(\ell)}}
#' @param n.,m. samples size.
#' @param gamma parameter of Lehmann alternative
#' @description \code{\link{dtnl.lehmann}}  gives the density of
#' \eqn{T_n^{(\ell)}} under Lehmann alternatives.
#' @return \code{\link{dtnl.lehmann}} return vector of the density under Lehmann
#' alternatives against the specified gamma.
#' @examples
#' dtnl.lehmann(k = 3, n. = 6, m. = 5, l = 2, gamma = 0.8)
dtnl.lehmann <- function(k, n., m., l, gamma) {
  if (any(c(n., m.) < (2 * l + 1))) {
    stop(paste("n,m must be > 2l", "\n", ""))
  }
  if (n. == m.) {
    m <- m.
    n <- n.
  } else {
    m <- max(n., m.)
    n <- min(n., m.)
  }
  j1 <- NULL
  j1[1] <- 0
  for (i in 2:n) {
    j1[i] <- floor(m / n * (i - 1))
  }
  j2 <- NULL
  for (i in 1:(n - 1)) {
    j2[i] <- ceiling(m / n * (i + 1))
  }
  j2[n] <- (m + 1)

  x <- NULL
  for (j in 0:n) {
    x <- rbind(x, t(partitions::compositions(j, m)))
  }
  ss <- 0
  prob <- lehmann <- NULL
  for (v in 1:n) {
    ss <- ss + 1
    nn <- nrow(x)
    count <- 0
    for (kk in 1:nn) {
      zz2 <- NULL
      M <- x[kk, ]
      for (i in 1:l) {
        if (sum(M[1:(j2[i + (l - 1)])]) >= i) zz2[i] <- 1 else zz2[i] <- 0
      }
      for (i in (l + 1):(n - l)) {
        if (sum(M[1:(j1[i - (l - 1)])]) < i & sum(M[1:(j2[i + (l - 1)])]) >= i)
          {
          zz2[i] <- 1
        } else {
          zz2[i] <- 0
        }
      }
      for (i in (n - l + 1):n) {
        if (sum(M[1:(j1[i - (l - 1)])]) < i) zz2[i] <- 1 else zz2[i] <- 0
      }
      if (sum(zz2) == v) {
        plam <- 1
        for (jj in 1:(m - 1)) {
          plam <- plam * gamma(sum(M[1:jj]) + jj * gamma) /
            gamma(sum(M[1:(jj + 1)])
            + jj * gamma + 1)
        }
        plam <- plam * gamma(sum(M) + m * gamma) / gamma(n + m * gamma + 1)
        const <- (factorial(m) * factorial(n) * (gamma^m)) / factorial(M[1])
        lehm <- plam * const
        count <- count + lehm
      }
    }
    lehmann[v] <- count
  }

  dtnllehk <- NULL
  for (i in seq_along(k)) {
    if (k[i] < 1 | k[i] > n) {
      dtnllehk[i] <- 0
    }
    if (k[i] >= 1 & k[i] <= n) {
      dtnllehk[i] <- lehmann[k[i]]
    }
  }
  return(dtnllehk)
}


#' quantile function of \eqn{T_n^{(\ell)}} under Lehmann alternatives.
#' @export
#' @rdname tnl.test
#' @param p vector of probabilities.
#' @param n.,m. samples size.
#' @param l class parameter of \eqn{T_n^{(\ell)}}.
#' @param gamma parameter of Lehmann alternative.
#' @description  \code{\link{qtnl.lehmann}} gives the quantile function of
#' \eqn{T_n^{(\ell)}} against the specified probabilities under
#' Lehmann alternatives.
#' @return \code{\link{qtnl.lehmann}} returns a quantile function
#' against the specified probabilities under Lehmann alternatives.
#'
#' @examples
#' qtnl.lehmann(p = c(.1, .5, .9), n. = 7, m. = 5, l = 1, gamma = 0.5)
qtnl.lehmann <- function(p, n., m., l, gamma) {
  if (any(c(n., m.) < (2 * l + 1))) {
    stop(paste("n,m must be > 2l", "\n", ""))
  }
  if (any(p < 0) | any(p > 1)) {
    stop(paste("p must be between 0 and 1", "\n", ""))
  }
  if (n. == m.) {
    m <- m.
    n <- n.
  } else {
    m <- max(n., m.)
    n <- min(n., m.)
  }
  j1 <- NULL
  j1[1] <- 0
  for (i in 2:n) {
    j1[i] <- floor(m / n * (i - 1))
  }
  j2 <- NULL
  for (i in 1:(n - 1)) {
    j2[i] <- ceiling(m / n * (i + 1))
  }
  j2[n] <- (m + 1)

  x <- NULL
  for (j in 0:n) {
    x <- rbind(x, t(partitions::compositions(j, m)))
  }
  ss <- 0
  prob <- lehmann <- NULL
  for (v in 1:n) {
    ss <- ss + 1
    nn <- nrow(x)
    count <- 0
    for (kk in 1:nn) {
      zz2 <- NULL
      M <- x[kk, ]
      for (i in 1:l) {
        if (sum(M[1:(j2[i + (l - 1)])]) >= i) zz2[i] <- 1 else zz2[i] <- 0
      }
      for (i in (l + 1):(n - l)) {
        if (sum(M[1:(j1[i - (l - 1)])]) < i & sum(M[1:(j2[i + (l - 1)])]) >= i)
          {
          zz2[i] <- 1
        } else {
          zz2[i] <- 0
        }
      }
      for (i in (n - l + 1):n) {
        if (sum(M[1:(j1[i - (l - 1)])]) < i) zz2[i] <- 1 else zz2[i] <- 0
      }
      if (sum(zz2) == v) {
        plam <- 1
        for (jj in 1:(m - 1)) {
          plam <- plam * gamma(sum(M[1:jj]) + jj * gamma) /
            gamma(sum(M[1:(jj + 1)])
            + jj * gamma + 1)
        }
        plam <- plam * gamma(sum(M) + m * gamma) / gamma(n + m * gamma + 1)
        const <- (factorial(m) * factorial(n) * (gamma^m)) / factorial(M[1])
        lehm <- plam * const
        count <- count + lehm
      }
    }
    lehmann[v] <- count
    res <- NULL
    for (t in 1:n) {
      res[t] <- sum(lehmann[1:t])
    }
  }
  q <- NULL
  for (j in seq_along(p)) {
    for (i in 1:n) {
      if (res[i] > p[j]) {
        break
      }
    }
    q[j] <- i
  }
  return(q)
}


#' Random generation for the \eqn{T_n^{(\ell)}} under Lehmann alternatives.
#' @export
#' @rdname tnl.test
#' @param N number of observations. If length(N) > 1, the length is taken
#'               to be the number required.
#' @param n.,m. samples size.
#' @param l class parameter of \eqn{T_n^{(\ell)}}.
#' @param gamma parameter of Lehmann alternative.
#' @description \code{\link{rtnl.lehmann}} generates random values from
#'    \eqn{T_n^{(\ell)}} under Lehmann alternatives.
#' @return \code{\link{rtnl.lehmann}} return *N* of the generated random values
#' under Lehmann alternatives.
#' @examples
#' rtnl.lehmann(N = 15, n = 7,m=7, l = 2, gamma = 0.5)
rtnl.lehmann <- function(N, n., m., l, gamma) {
  if (any(c(n., m.) < (2 * l + 1))) {
    stop(paste("n,m must be > 2l", "\n", ""))
  }
  if (n. == m.) {
    m <- m.
    n <- n.
  } else {
    m <- max(n., m.)
    n <- min(n., m.)
  }
  x <- y <- NULL
  statistic <- NULL
  for (i in 1:N) {
    x <- (stats::runif(m))^(1 / gamma)
    y <- stats::runif(n)
    statistic[i] <- stest(x, y, l)
  }
  return(statistic)
}
