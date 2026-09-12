#' Reduce a data set to representatives of bins and their frequencies
#'
#' @description Reduce a data set to a named list of 'values' and 'freq',
#' which are representatives of bins and their frequencies, respectively.
#' @param data A numeric vector of a data set.
#' @param bins A positive integer to represent the number of bins.
#' @return A named list of `values` and `freq` whose length is `bins`.
#' @examples
#' rlst <- databinning(mix2gauss$n200)
#' @seealso [base::rle()]
#' @export
databinning <- function(data, bins = 40) {
    data <- sort(data)
    nlen <- length(data)
    min0 <- data[1]
    max0 <- data[nlen]

    pts <- rep(0, bins)
    freq <- rep(0, bins)

    # pts are centered in the bins
    n <- bins
    binwidth <- (max0 - min0) / n
    start0 <- min0
    end0 <- max0
    breaks <- seq(start0, end0, length.out = bins + 1)
    start1 <- min0 + binwidth * 0.5

    for (i in 1:(bins - 1)) {
        n1 <- length(which(breaks[i] <= data & data < breaks[i + 1]))
        pts[i] <- start1 + (i - 1) * binwidth
        freq[i] <- n1
    }
    n1 <- length(which(breaks[bins] <= data & data <= breaks[bins + 1]))
    pts[bins] <- start1 + (bins - 1) * binwidth
    freq[bins] <- n1


    return(list(values = pts, freq = freq))
}


#' Incomplete Gamma Function
#'
#' @description Evaluate an incomplete gamma function:
#' \deqn{\gamma(a, x) = \int_{0}^{x} t^{a-1} e^{-t}dt,}
#' using SLATEC `dgami` in \url{https://netlib.org/slatec/}.
#' When (\eqn{x > 0} and \eqn{a \ge 0}) or (\eqn{x \ge 0} and \eqn{a > 0}),
#' compute the result, otherwise the value is `NaN`.
#' @param a A positive numeric vector.
#' @param x A nonnegative numeric vector with same length as `a`.
#' length.
#' @return A vector of values of an incomplete gamma function.
#' @seealso [igammac()]
#' @examples
#' igamma(1, 1)
#' @export
igamma <- function(a, x) .Call(rigamma_, a, x)


#' Complementary Incomplete Gamma Function
#'
#' @description Evaluate an complementary incomplete gamma function:
#' \deqn{\gamma^{*}(a, x) = \int_{x}^{\infty} t^{a-1} e^{-t}dt,}
#' using SLATEC `dgamic` in \url{https://netlib.org/slatec/}.
#' When (\eqn{x > 0} and \eqn{a \ge 0}) or (\eqn{x \ge 0} and \eqn{a > 0}),
#' compute the result, otherwise the value is `NaN`.
#' @param a A numeric vector.
#' @param x A nonnegative numeric vector with same length as `a`.
#' @return A vector of values of a complementary incomplete gamma function.
#' @seealso [igamma()]
#' @examples
#' igammac(1, 1)
#' @export
igammac <- function(a, x) .Call(ricgamma_, a, x)


#' Generate mixed Gaussian random numbers
#'
#' @description Generate mix gaussian random numbers whose density is:
#' \deqn{\frac{0.3}{\sqrt{2 \pi 0.5^2}}
#' \exp\left(\frac{(x+1)^2}{2 \cdot 0.5^2}\right) +
#' \frac{0.7}{\sqrt{2 \pi 0.5^2}}
#' \exp\left(\frac{(x-1)^2}{2 \cdot 0.5^2}\right)}.
#' @param n The number of random numbers.
#' @param seed A seed for random number generator.
#' @return A numeric vector of random numbers whose a density is described
#' in Description.
#' @seealso [mix2gauss_fun()]
#' @export
mix2gauss_gen <- function(n = 100, seed = NULL) {
    if (!is.null(seed)) {
        set.seed(seed)
    }
    v1 <- stats::rnorm(n, -1, 0.5)
    v2 <- stats::rnorm(n, 1, 0.5)
    u <- stats::runif(n)
    idx <- u < 0.3
    v <- rep(0.0, n)
    v[idx] <- v1[idx]
    v[!idx] <- v2[!idx]
    return(v)
}


#' A density function of mixed Gaussian distributions
#'
#' @description A density function of mixed Gaussian distributions
#' whose density is:
#' \deqn{\frac{0.3}{\sqrt{2 \pi 0.5^2}}
#' \exp\left(\frac{(x+1)^2}{2 \cdot 0.5^2}\right) +
#' \frac{0.7}{\sqrt{2 \pi 0.5^2}}
#' \exp\left(\frac{(x-1)^2}{2 \cdot 0.5^2}\right)}.
#' @param x A numeric vector for arguments of a density function.
#' @return A numeric vector of probabilities for a given argument `x`.
#' @seealso [mix2gauss_gen()]
#' @export
mix2gauss_fun <- function(x) {
    z <- 0.3 * stats::dnorm(x, -1, 0.5) + 0.7 * stats::dnorm(x, 1, 0.5)
    return(z)
}


#' Generate Mixed Gaussian Random Numbers
#'
#' @description A random number generator whose density function is
#' proportional to:
#' \deqn{\exp(\frac{x^2}{2}) + 5\exp(\frac{(x-1)^2}{0.2}) +
#' 3\exp(\frac{(x-1)^2}{0.5}).}
#' @param n The number of random numbers.
#' @param seed A seed for random number generator.
#' @return A numeric vector of probabilities for a given argument `x`.
#' @seealso [mix3gauss_fun()]
#' @export
mix3gauss_gen <- function(n = 100, seed = NULL) {
    if (!is.null(seed)) {
        set.seed(seed)
    }
    v0 <- stats::rnorm(n)
    v1 <- stats::rnorm(n, 1, sqrt(0.1))
    v2 <- stats::rnorm(n, 1, 0.5)
    u <- stats::runif(n)
    t <- 1 + 5 * sqrt(0.1) + 6
    idx0 <- u < 1 / t
    idx1 <- 1 / t <= u & u < (1 + 5 * sqrt(0.1)) / t
    idx2 <- u >= (1 + 5 * sqrt(0.1)) / t
    v <- rep(0.0, n)
    v[idx0] <- v0[idx0]
    v[idx1] <- v1[idx1]
    v[idx2] <- v2[idx2]
    return(v)
}


#' A density function of mixed gaussian distribution
#'
#' @description A density function proportional to:
#' \deqn{\exp(\frac{x^2}{2}) + 5\exp(\frac{(x-1)^2}{0.2}) +
#' 3\exp(\frac{(x-1)^2}{0.5}).}
#' @param x A numeric vector for arguments of a density function.
#' @return A numeric vector of probabilities for a given argument `x`.
#' @seealso [mix3gauss_gen()]
#' @export
mix3gauss_fun <- function(x) {
    t <- 1 + 5 * sqrt(0.1) + 6
    z <- (stats::dnorm(x) + 5 * sqrt(0.1) * stats::dnorm(x, 1, sqrt(0.1)) +
        6 * stats::dnorm(x, 1, 0.5)) / t
}


#' Generate random numbers of Mixed Exponential and Gamma Distributions
#'
#' @description Generate random numbers whose density function:
#' \deqn{0.2(2 e^{-2x}) + 0.8 \frac{x^3}{3!}e^{-x}.}
#' @param n The number of random numbers.
#' @param seed A seed for random number generator.
#' @return A numeric vector of probabilities for a given argument `x`.
#' @seealso [mixexpgamma_fun()]
#' @export
mixexpgamma_gen <- function(n = 100, seed = NULL) {
    if (!is.null(seed)) {
        set.seed(seed)
    }
    v1 <- stats::rexp(n, 2)
    v2 <- stats::rgamma(n, 4, 1)
    u <- stats::runif(n)
    idx <- u < 0.2
    v <- rep(0.0, n)
    v[idx] <- v1[idx]
    v[!idx] <- v2[!idx]
    return(v)
}


#' A density function of Mixed Exponential and Gamma Distributions
#' 
#' @description A density function of
#' \deqn{0.2(2 e^{-2x}) + 0.8 \frac{x^3}{3!}e^{-x}.}
#' @param x A numeric vector for arguments of a density function.
#' @return A numeric vector of probabilities for a given argument `x`.
#' @seealso [mixexpgamma_gen()]
#' @export
mixexpgamma_fun <- function(x) {
    z <- 0.2 * stats::dexp(x, 2) + 0.8 * stats::dgamma(x, 4, 1)
    return(z)
}