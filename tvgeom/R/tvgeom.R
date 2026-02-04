#' The Time-Varying (Right-Truncated) Geometric Distribution
#'
#' Density (\code{dtvgeom}), distribution function (\code{qtvgeom}), quantile
#' function (\code{ptvgeom}), and random number generation (\code{rtvgeom} and
#' \code{rttvgeom} for sampling from the \emph{full} and \emph{truncated}
#' distribution, respectively) for the time-varying, right-truncated geometric
#' distribution with parameter \code{prob}.
#'
#' The time-varying geometric distribution describes the number of independent
#' Bernoulli trials needed to obtain one success. The probability of success,
#' \code{prob}, may vary for each trial. It has mass \deqn{p(x) = prob[x] *
#' prod(1 - prob[1:(x-1)])} with support \eqn{x = 1, 2, ..., n + 1}, 
#' where \code{n} equals then length of \code{prob}. For \eqn{i} in
#' \eqn{prob, 0 \le i \le 1}. The \code{n+1} case represents the case that the event did not
#' happen in the first n trials.
#'
#' @param x,q vector of quantiles representing the trial at which the first
#' success occurred.
#' @param p vector of probabilities at which to evaluate the quantile function.
#' @param n number of observations to sample.
#' @param prob vector of the probability of success for each trial.
#' @param lower lower value (exclusive) at which to truncate the distribution
#' for random number generation. Defaults to \code{0}, in which case the
#' distribution is not left-truncated.
#' @param upper upper value (inclusive) at which to truncate the distribution
#' for random number generation. Defaults to \code{length(prob)}, in which case
#' the distribution is not right-truncated.
#' @param log,log.p logical; if \code{TRUE}, probabilities, p, are given as
#' \code{log(p)}. Defaults to \code{FALSE}.
#' @param lower.tail logical; if \code{FALSE}, \code{ptvgeom} returns
#' @return dtvgeom gives the probability mass, qtvgeom gives the 
#' quantile functions, ptvgeom gives the distribution function, rtvgeom
#' generates random numbers, and rttvgeom gives random numbers from the 
#' distribution truncated at bounds provided by the user.
#' \eqn{P(X > x)} instead of \eqn{P(X \le x)}. Defaults to \code{TRUE}.
#' @name tvgeom
#' @examples
#' # What's the probability that a given number of trials, n, are needed to get
#' # one success if `prob` = `p0`, as defined below...?
#' p0 <- .15 # the probability of success
#' 
#' # Axis labels (for plotting purposes, below).
#' x_lab <- "Number of trials, n"
#' y_lab <- sprintf("P(success at trial n | prob = %s)", p0)
#' 
#' # Scenario 1: the probability of success is constant and we invoke functions
#' # from base R's implementation of the geometric distribution.
#' y1 <- rgeom(1e3, p0) + 1 # '+1' b/c dgeom parameterizes in terms of failures
#' x1 <- seq_len(max(y1))
#' z1 <- dgeom(x1 - 1, p0)
#' plot(table(y1) / 1e3,
#'   xlab = x_lab, ylab = y_lab, col = "#00000020",
#'   bty = "n", ylim = c(0, p0)
#' )
#' lines(x1, z1, type = "l")
#' 
#' # Scenario 2: the probability of success is constant, but we use tvgeom's
#' # implementation of the time-varying geometric distribution. For the purposes
#' # of this demonstration, the length of vector `prob` (`n_p0`) is chosen to be
#' # arbitrarily large *relative* to the distribution of n above (`y1`) to
#' # ensure we don't accidentally create any censored observations!
#' n_p0 <- max(y1) * 5
#' p0_vec <- rep(p0, n_p0)
#' y2 <- rtvgeom(1e3, p0_vec)
#' x2 <- seq_len(max(max(y1), max(y2)))
#' z2 <- dtvgeom(x2, p0_vec) # dtvgeom is parameterized in terms of successes
#' points(x2[x2 <= max(y1)], z2[x2 <= max(y1)],
#'   col = "red", xlim = c(1, max(y1))
#' )
#' 
#' # Scenario 3: the probability of success for each process varies over time
#' # (e.g., chances increase linearly by `rate` for each subsequent trial until
#' # chances saturate at `prob` = 1).
#' rate <- 1.5
#' prob_tv <- numeric(n_p0)
#' for (i in 1:length(p0_vec)) {
#'   prob_tv[i] <- ifelse(i == 1, p0_vec[i], rate * prob_tv[i - 1])
#' }
#' prob_tv[prob_tv > 1] <- 1
#' y3 <- rtvgeom(1e3, prob_tv)
#' x3 <- seq_len(max(y3))
#' z3 <- dtvgeom(x3, prob_tv)
#' plot(table(y3) / 1e3,
#'   xlab = x_lab, col = "#00000020", bty = "n",
#'   ylim = c(0, max(z3)),
#'   ylab = sprintf("P(success at trial n | prob = %s)", "`prob_tv`")
#' )
#' lines(x3, z3, type = "l")
#' @rdname tvgeom
#' @export
dtvgeom <- function(x, prob, log = FALSE) {
  if (min(prob) < 0 | max(prob) > 1) {
    stop("prob contains an element greater than 1 or less than 0")
  }
  prob <- c(prob, 1)
  prob_c <- 1 - prob # the complement of vector "prob"
  multiplicand <- as.numeric(x == 1)
  cond <- x != 0 & x <= length(prob)
  multiplicand[cond == TRUE & x != 1] <- cumprod(prob_c)[x[cond == TRUE] - 1]
  out <- rep(0, length(x))
  out[x != 0 & x <= length(prob)] <-
    prob[x[cond == TRUE]] * multiplicand[cond == TRUE]

  if (isTRUE(log)) {
    return(log(out))
  }
  out
}


#' @rdname tvgeom
#' @export
ptvgeom <- function(q, prob, lower.tail = TRUE, log.p = FALSE) {
  if (min(prob) < 0 | max(prob) > 1) {
    stop("prob contains an element greater than 1 or less than 0")
  }

  out <- sapply(q, FUN = function(x) {
    range <- 1:x
    if (x <= 0) {
      range <- 0
    }
    if (x > (length(prob) + 1)) {
      range <- 1:(length(prob) + 1)
    }
    sum(sapply(range, FUN = dtvgeom, prob = prob)) # nolint
  })

  if (isFALSE(lower.tail)) {
    out <- 1 - out
    out[out <= 0] <- 0
  }
  if (isTRUE(log.p)) {
    return(log(out))
  }
  out
}

#' @rdname tvgeom
#' @export
qtvgeom <- function(p, prob, lower.tail = TRUE, log.p = FALSE) {
  if (isTRUE(log.p)) {
    p <- exp(p)
  }
  if (min(prob) < 0 | max(prob) > 1) {
    stop("prob contains an element greater than 1 or less than 0")
  }
  if (min(p) < 0 | max(p) > 1) {
    stop("p (on the probability scale) contains an element greater than 1 or
         less than 0")
  }

  if (isFALSE(lower.tail)) {
    p <- 1 - p
  }
  sapply(p, FUN = function(x) {
    # Calculate the cumulative distribution function (CDF) values.
    cdf <- cumsum(sapply(seq_along(prob), dtvgeom, prob = prob)) # nolint

    # Identify and return x value (vector position in the CDF) that corresponds
    # to the cumulative probability p.
    Position(
      f = function(y) {
        y >= x
      },
      cdf
    )
  })
}

#' @rdname tvgeom
#' @export
rtvgeom <- function(n, prob) {
  if (min(prob) < 0 | max(prob) > 1) {
    stop("prob contains an element greater than 1 or less than 0")
  }
  if (isFALSE(n %% 1 == 0) | n < 1) {
    stop("n must be an integer greater than 0")
  }
  x <- seq_along(c(prob, 1))
  sample(x, size = n, prob = dtvgeom(x, prob), replace = TRUE) # nolint
}

#' @rdname tvgeom
#' @export
rttvgeom <- function(n, prob, lower = 0, upper = length(prob) + 1) {
  if (min(prob) < 0 | max(prob) > 1) {
    stop("prob contains an element greater than 1 or less than 0")
  }

  if (isFALSE(n %% 1 == 0) | n < 1) {
    stop("n must be an integer greater than 0")
  }
  if (lower < 0 | lower > (length(prob) + 1)) {
    stop("lower must be equal to 0 or contained in the support of the random
         variable: (1, length(prob)+1)")
  }
  if (upper < 1 | upper > (length(prob) + 1)) {
    stop("upper must be contained in the support of the random variable:
         (1, length(prob)+1)")
  }
  x <- (lower + 1):upper
  pdf <- dtvgeom(x, prob = prob) # nolint
  npdf <- pdf / sum(pdf)
  sample(x, size = n, prob = npdf, replace = TRUE)
}
