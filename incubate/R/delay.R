# mkuhn, 2021-04-06
# delay distribution functions

#' Delayed Exponential Distribution
#'
#' @description
#' Density, distribution function, quantile function, random generation and restricted mean survival time function for the delayed exponential distribution.
#' There is an initial delay phase (parameter `delay1`) where no events occur. After that, `rate1` applies.
#' Optionally, a second phase is possible where the hazard rate might change (parameters `delay2` and `rate2`).
#'
#' @details
#' If only a single initial delay phase is there, the numerical arguments other than `n` are recycled to the length of the result (as with the exponential distribution in `stats`).
#' With two phases, the arguments are **not** recycled. Only the first element of delays and rates are used as it otherwise becomes ambiguous which delay and rate parameter apply for observations in different phases.
#' Generally, only the first elements of the logical arguments are used.
#'
#' When `cens=` is specified greater than 0, the actual number of censored observations is random. On average, it equals the expected number of censored observations as given by `cens`.
#'
#' @param x A numeric vector of values for which to get the density.
#' @param q A numeric vector of quantile values.
#' @param t A numeric vector of times that restrict the mean survival. Default is `+Inf`, i.e., the unrestricted mean survival time.
#' @param p A numeric vector of probabilities.
#' @param n integer. Number of random observations requested.
#' @param delay1 numeric. The first delay, must be non-negative.
#' @param rate1 numeric. The event rate, must be non-negative.
#' @param delay numeric. Alias for first delay.
#' @param rate numeric. Alias for first rate.
#' @param delay2 numeric. The second delay, must be non-negative.
#' @param rate2 numeric. The second event rate, must be non-negative.
#' @param log logical. Return value on log-scale?
#' @param lower.tail logical. Give cumulative probability of lower tail?
#' @param log.p logical. P-value on log-scale?
#' @param cens numeric. In [0, 1). Expected proportion of random right-censored observations.
#' @returns Functions pertaining to the delayed exponential distribution:
#' * `dexp_delayed` gives the density
#' * `pexp_delayed` gives the vector of cumulative probabilities or the gradient matrix (nbr parameters x quantile times)
#' * `qexp_delayed` gives the quantile function
#' * `rexp_delayed` generates a pseudo-random sample
#' * `mexp_delayed` gives the restricted mean survival time
#'
#' The length of the result is determined by `n` for `rexp_delayed`, and is the maximum of the lengths of the numerical arguments for the other functions,
#' R's recycling rules apply when only single initial delay phase is used.
#' @seealso [stats::Exponential]
#' @keywords distribution
#' @name DelayedExponential
NULL

#' @rdname DelayedExponential
#' @export
dexp_delayed <- function(
  x,
  delay1 = 0,
  rate1 = 1,
  delay2 = NULL,
  rate2 = NULL,
  delay = delay1,
  rate = rate1,
  log = FALSE
) {
  stopifnot(length(log) >= 1L, is.logical(log))
  log <- log[[1L]] # only first value of log is used
  if (!missing(delay)) {
    if (missing(delay1)) {
      delay1 <- delay
    } else {
      warning("Argument delay= is ignored as delay1= is given!", call. = FALSE)
    }
  }
  if (!missing(rate)) {
    if (missing(rate1)) {
      rate1 <- rate
    } else {
      warning("Argument rate= is ignored as rate1= is given!", call. = FALSE)
    }
  }

  stopifnot(all(
    !is.null(delay1),
    !is.null(rate1),
    is.finite(delay1),
    is.finite(rate1)
  ))

  # check for easy case: only a single phase
  if (is.null(delay2)) {
    if (!is.null(rate2)) {
      warning(
        "Argument rate2= is ignored, as argument delay2= is not set.",
        call. = FALSE
      )
    }
    return(stats::dexp(x = x - delay1, rate = rate1, log = log))
  }

  # two phases
  # take only first value of parameter arguments!
  if (
    length(delay1) > 1L ||
      length(rate1) > 1L ||
      length(delay2) > 1L ||
      length(rate2) > 1L
  ) {
    warning(
      "In two-phase setting we do not recycle parameters. Only the 1st value of the parameter arguments is used!",
      call. = FALSE
    )
    # first phase
    delay1 <- delay1[[1L]]
    rate1 <- rate1[[1L]]
    dvals <- stats::dexp(x = x - delay1, rate = rate1, log = log)
    delay2 <- delay2[[1L]]
    rate2 <- rate2[[1L]]
  }
  # we need both delay2 AND rate2
  stopifnot(is.finite(delay2), is.finite(rate2))

  # check delay constraint
  if (delay1 >= delay2) {
    stop(
      "First delay phase must antedate the second delay phase!",
      call. = FALSE
    )
  }

  # second phase
  # update density for observations that lie in the 2nd phase
  phase2Ind <- which(x >= delay2)
  if (length(phase2Ind)) {
    dvals[phase2Ind] <- stats::dexp(
      x = x[phase2Ind] - delay2,
      rate = rate2,
      log = log
    )
    dvals[phase2Ind] <- if (log) {
      dvals[phase2Ind] - rate1 * (delay2 - delay1)
    } else {
      dvals[phase2Ind] * exp(-rate1 * (delay2 - delay1))
    }
  }

  dvals
}

#' @rdname DelayedExponential
#' @param grad logical. Should the gradient be calculated at the given quantile values (and not the cumulative probabilities)?
#' @return Vector of cumulative probabilities or gradient matrix (nbr parameter x quantile times)
#' @export
pexp_delayed <- function(
  q,
  delay1 = 0,
  rate1 = 1,
  delay2 = NULL,
  rate2 = NULL,
  delay = delay1,
  rate = rate1,
  lower.tail = TRUE,
  log.p = FALSE,
  grad = FALSE
) {
  if (!missing(delay)) {
    if (missing(delay1)) {
      delay1 <- delay
    } else {
      warning("Argument delay= is ignored as delay1= is given!", call. = FALSE)
    }
  }
  if (!missing(rate)) {
    if (missing(rate1)) {
      rate1 <- rate
    } else {
      warning("Argument rate= is ignored as rate1= is given!", call. = FALSE)
    }
  }
  log.p <- isTRUE(log.p)
  lower.tail <- isTRUE(lower.tail)
  grad <- isTRUE(grad)

  stopifnot(all(is.finite(delay1), is.finite(rate1)))

  # check for easy case when only a single delay is specified
  if (is.null(delay2)) {
    if (!is.null(rate2)) {
      warning(
        "Argument rate2= is ignored, as argument delay2= is not set.",
        call. = FALSE
      )
    }
    return(
      if (grad) {
        if (log.p) {
          warning(
            "Argument log.p=TRUE is ignored here for gradient.",
            call. = FALSE
          )
        }
        local({
          expTerm <- ifelse(
            q >= delay1,
            yes = exp(-rate1 * (q - delay1)),
            no = 0
          )

          # return matrix of partial derivatives, 1st row: for delay1, 2nd row: for rate1
          rbind(delay1 = -rate1 * expTerm, rate1 = (q - delay1) * expTerm) *
            if (lower.tail) 1 else -1
        })
      } else {
        stats::pexp(
          q = q - delay1,
          rate = rate1,
          lower.tail = lower.tail,
          log.p = log.p
        )
      }
    )
  }

  # two phases
  if (
    length(delay1) > 1L ||
      length(rate1) > 1L ||
      length(delay2) > 1L ||
      length(rate2) > 1L
  ) {
    warning(
      "In two-phase setting we do not recycle parameters. Only the 1st value of the parameter arguments is used!",
      call. = FALSE
    )

    delay1 <- delay1[[1L]]
    rate1 <- rate1[[1L]]
    delay2 <- delay2[[1L]]
    rate2 <- rate2[[1L]]
  }
  # we need both delay2 AND rate2
  if (!is.finite(delay2) || !is.finite(rate2)) {
    stop("2nd delay and rate parameters must be finite!", call. = FALSE)
  }

  # check delay constraint
  if (delay1 >= delay2) {
    stop(
      "First delay phase must antedate the second delay phase!",
      call. = FALSE
    )
  }

  if (grad) {
    stop("Gradient not supported for two-phase setting.", call. = FALSE)
  }

  # first phase
  pvals <- stats::pexp(
    q = q - delay1,
    rate = rate1,
    lower.tail = lower.tail,
    log.p = log.p
  )
  # check if we have observations in 2nd phase
  phase2Ind <- which(q > delay2)
  if (length(phase2Ind)) {
    pvals[phase2Ind] <- stats::pexp(
      q = q[phase2Ind] - delay2 + rate1 / rate2 * (delay2 - delay1),
      rate = rate2,
      lower.tail = lower.tail,
      log.p = log.p
    )
  }

  pvals
}

#' @rdname DelayedExponential
#' @export
qexp_delayed <- function(
  p,
  delay1 = 0,
  rate1 = 1,
  delay2 = NULL,
  rate2 = NULL,
  delay = delay1,
  rate = rate1,
  lower.tail = TRUE,
  log.p = FALSE
) {
  #lower.tail = TRUE, log.p = FALSE
  if (!missing(delay)) {
    if (missing(delay1)) {
      delay1 <- delay
    } else {
      warning("Argument delay= is ignored as delay1= is given!", call. = FALSE)
    }
  }
  if (!missing(rate)) {
    if (missing(rate1)) {
      rate1 <- rate
    } else {
      warning("Argument rate= is ignored as rate1= is given!", call. = FALSE)
    }
  }

  stopifnot(all(is.finite(delay1), is.finite(rate1)))

  # check for easy case: only a single delay phase
  if (is.null(delay2)) {
    if (!is.null(rate2)) {
      warning(
        "Argument rate2= is ignored, as argument delay2= is not set.",
        call. = FALSE
      )
    }
    return(
      delay1 +
        stats::qexp(p = p, rate = rate1, lower.tail = lower.tail, log.p = log.p)
    ) #lower.tail = lower.tail, log.p = log.p
  }

  # two phases
  if (
    length(delay1) > 1L ||
      length(rate1) > 1L ||
      length(delay2) > 1L ||
      length(rate2) > 1L
  ) {
    warning(
      "In two-phase setting we do not recycle parameters. Only the 1st value of the parameter arguments is used!",
      call. = FALSE
    )
  }

  delay1 <- delay1[[1L]]
  rate1 <- rate1[[1L]]
  delay2 <- delay2[[1L]]
  rate2 <- rate2[[1L]]
  # we need both delay2 AND rate2
  stopifnot(is.finite(delay2), is.finite(rate2))
  # check delay constraint
  if (delay1 >= delay2) {
    stop(
      "First delay phase must antedate the second delay phase!",
      call. = FALSE
    )
  }

  # first phase
  qvals <- delay1 +
    stats::qexp(p = p, rate = rate1, lower.tail = lower.tail, log.p = log.p)

  # second phase
  # check if we have observations in 2nd phase
  # transform p-values to canonical meaning (lower.tail=T, log.p=F)
  if (!lower.tail) {
    p <- 1L - p
  }
  if (log.p) {
    p <- exp(p)
  }
  phase2Ind <- which(
    p >=
      pexp_delayed(
        q = delay2,
        delay1 = delay1,
        rate1 = rate1,
        lower.tail = TRUE,
        log.p = FALSE
      )
  )
  if (length(phase2Ind)) {
    qvals[phase2Ind] <- delay2 -
      rate1 / rate2 * (delay2 - delay1) -
      log(1 - p[phase2Ind]) / rate2
  } #stats::qexp(p = p[phase2Ind], rate = rate2, lower.tail = TRUE, log.p = FALSE)

  qvals
}

#' @rdname DelayedExponential
#' @export
rexp_delayed <- function(
  n,
  delay1 = 0,
  rate1 = 1,
  delay2 = NULL,
  rate2 = NULL,
  delay = delay1,
  rate = rate1,
  cens = 0
) {
  if (!missing(delay)) {
    if (missing(delay1)) {
      delay1 <- delay
    } else {
      warning("Argument delay= is ignored as delay1= is given!", call. = FALSE)
    }
  }
  if (!missing(rate)) {
    if (missing(rate1)) {
      rate1 <- rate
    } else {
      warning("Argument rate= is ignored as rate1= is given!", call. = FALSE)
    }
  }

  stopifnot(all(is.finite(delay1), is.finite(rate1)))

  stopifnot(is.numeric(n))
  n <- if (length(n) > 1) {
    length(n)
  } else {
    trunc(n)
  }
  if (n == 0) {
    return(numeric(0L))
  }
  stopifnot(length(n) == 1, n > 0)

  if (!is.numeric(cens) || length(cens) > 1L) {
    stop(
      "cens= is expected proportion of right censored observations!",
      call. = FALSE
    )
  }

  if (!is.finite(cens) || cens >= 1 || cens < 0) {
    stop("cens= argument is invalid!", call. = FALSE)
  }

  # single phase
  # check for easy case: only a single delay
  if (is.null(delay2)) {
    if (!is.null(rate2)) {
      warning(
        "Argument rate2= is ignored, as argument delay2= is not set.",
        call. = FALSE
      )
    }

    evTime <- delay1 + stats::rexp(n = n, rate = rate1)
    if (near(cens, 0)) {
      return(evTime)
    } else {
      # independent uniform censoring process U(delay1, Z) where Z is chosen
      #+as to give expected proportion of right-censoring
      censTime <- delay1 +
        stats::runif(
          n = n,
          max = (1 / cens + lambertW0_cpp(-exp(-1 / cens) / cens)) / rate1
        )
      censIdx <- which(censTime < evTime)

      # result: element-wise minimum of both processes
      res <- evTime
      evStatus <- rep_len(1, length.out = n)

      # avoid having too many censorings by chance
      if (length(censIdx) > 0) {
        # cap number of censorings at expected number of censorings (rounding to integer)
        #+but I don't like it because I want to trust the process
        # maxNbrCens <- if (cens == 1) {
        #   n
        # } else {
        #   min(n - 1, round(n * cens, digits = 0))
        # }
        # maxNbrCens <- max(1, maxNbrCens)
        # maxNbrCens <- min(maxNbrCens, length(censIdx))
        # censIdx <- censIdx[seq_len(maxNbrCens)]

        res[censIdx] <- censTime[censIdx]
        evStatus[censIdx] <- 0
      } #fi

      return(Surv(res, event = evStatus, type = "right"))
    }
  }

  # two phases
  stopifnot(!is.null(delay2))

  if (
    length(delay1) > 1L ||
      length(rate1) > 1L ||
      length(delay2) > 1L ||
      length(rate2) > 1L
  ) {
    warning(
      "In two-phase setting we do not recycle parameters. Only the 1st value of the parameter arguments is used!",
      call. = FALSE
    )
  }
  delay1 <- delay1[[1L]]
  rate1 <- rate1[[1L]]
  # we need both delay2 AND rate2
  delay2 <- delay2[[1L]]
  rate2 <- rate2[[1L]]

  # check delay constraint
  if (delay1 >= delay2) {
    stop(
      "First delay phase must antedate the second delay phase!",
      call. = FALSE
    )
  }

  if (is.null(rate2)) {
    stop(
      "Argument rate2= is null but a finite numeric argument is needed!",
      call. = FALSE
    )
  }

  if (!is.finite(delay2) || !is.finite(rate2)) {
    stop(
      "Please provide finite numeric arguments for delay2= and rate2=!",
      call. = FALSE
    )
  }

  if (is.list(cens) || !near(cens, 0)) {
    stop(
      "Censoring is not supported for two-phase exponential with delay.",
      call. = FALSE
    )
  }

  # check if rate changes noticeably
  if (isTRUE(near(rate1, rate2))) {
    return(delay1 + stats::rexp(n = n, rate = rate1))
  }

  # use inverse CDF-method
  qexp_delayed(
    p = stats::runif(n = n),
    delay1 = delay1,
    rate1 = rate1,
    delay2 = delay2,
    rate2 = rate2
  )
}


#' @rdname DelayedExponential
#' @export
mexp_delayed <- function(
  t = +Inf,
  delay1 = 0,
  rate1 = 1,
  delay2 = NULL,
  rate2 = NULL,
  delay = delay1,
  rate = rate1
) {
  if (!missing(delay)) {
    if (missing(delay1)) {
      delay1 <- delay
    } else {
      warning("Argument delay= is ignored as delay1= is given!", call. = FALSE)
    }
  }
  if (!missing(rate)) {
    if (missing(rate1)) {
      rate1 <- rate
    } else {
      warning("Argument rate= is ignored as rate1= is given!", call. = FALSE)
    }
  }

  stopifnot(all(is.finite(delay1), is.finite(rate1)))

  # single phase
  # calculate for single phase delayed exponential
  if (is.null(delay2)) {
    if (!is.null(rate2)) {
      warning(
        "Argument rate2= is ignored, as argument delay2= is not set.",
        call. = FALSE
      )
    }
    return(
      pmin.int(t, delay1) +
        pexp_delayed(q = t, delay1 = delay1, rate1 = rate1) / rate1
    )
  }

  # two phases
  if (
    length(delay1) > 1L ||
      length(rate1) > 1L ||
      length(delay2) > 1L ||
      length(rate2) > 1L
  ) {
    warning(
      "In two-phase setting we do not recycle parameters. Only the 1st value of the parameter arguments is used!",
      call. = FALSE
    )
  }
  delay1 <- delay1[[1L]]
  rate1 <- rate1[[1L]]
  # we need both delay2 AND rate2
  delay2 <- delay2[[1L]]
  rate2 <- rate2[[1L]]
  stopifnot(is.finite(delay2), is.finite(rate2))

  # check delay constraint
  if (delay1 >= delay2) {
    stop(
      "First delay phase must antedate the second delay phase!",
      call. = FALSE
    )
  }

  pmin.int(t, delay1) +
    pexp_delayed(q = pmin.int(t, delay2), delay1 = delay1, rate1 = rate1) /
      rate1 +
    pexp_delayed(
      q = delay2,
      delay1 = delay1,
      rate1 = rate1,
      lower.tail = FALSE
    ) *
      pexp_delayed(q = t, delay1 = delay2, rate1 = rate2) /
      rate2
}


#' Delayed Weibull Distribution
#'
#' @description
#' Density, distribution function, quantile function and random generation for the delayed Weibull distribution.
#' Besides the additional parameter `delay`, the other two Weibull-parameters are in principle retained as in R's stats-package:
#' * `shape`
#' * `scale` (as inverse of rate)
#'
#' @details
#' Additional arguments are forwarded via `...` to the underlying functions of
#' the exponential distribution in the stats-package.
#'
#' The numerical arguments other than `n` are recycled to the length of the
#' result. Only the first elements of the logical arguments are used.
#'
#' @param x A numeric vector of values for which to get the density.
#' @param q A numeric vector of quantile values.
#' @param t A numeric vector of times that restrict the mean survival. Default
#'   is `+Inf`, i.e., the unrestricted mean survival time.
#' @param p A numeric vector of probabilities.
#' @param n integer. Number of random observations requested.
#' @param delay1 numeric. The first delay, must be non-negative.
#' @param shape1 numeric. First shape parameter, must be positive.
#' @param scale1 numeric. First scale parameter (inverse of rate), must be
#'   positive.
#' @param delay numeric. Alias for first delay.
#' @param shape numeric. Alias for first shape.
#' @param scale numeric. Alias for first scale.
#' @param delay2 numeric. The second delay, must be non-negative.
#' @param shape2 numeric. The second shape parameter, must be non-negative.
#' @param scale2 numeric. The second scale parameter (inverse of rate), must be
#'   positive.
#' @param log logical. Return value on log-scale?
#' @param lower.tail logical. Give cumulative probability of lower tail?
#' @param log.p logical. P-value on log-scale?
#' @param cens numeric. Proportion of random right-censored observations. For
#'   small values of shape1, on average fewer censorings are achieved. There is still a bug in the CDF for uniform censoring ansatz!
#' @return Functions pertaining to the delayed Weibull distribution:
#' * `dweib_delayed` gives the density
#' * `pweib_delayed` gives the vector of cumulative probabilities or the gradient matrix (nbr parameters x quantile times)
#' * `qweib_delayed` gives the quantile function
#' * `rweib_delayed` generates a pseudo-random sample
#' * `mweib_delayed` gives the restricted mean survival time
#'
#' The length of the result is determined by `n` for `rweib_delayed`, and is the maximum of the lengths of the numerical arguments for the other functions, R's recycling rules apply.
#' @keywords distribution
#' @name DelayedWeibull
NULL

#' @rdname DelayedWeibull
#' @export
dweib_delayed <- function(
  x,
  delay1,
  shape1,
  scale1 = 1,
  delay2 = NULL,
  shape2 = NULL,
  scale2 = 1,
  delay = delay1,
  shape = shape1,
  scale = scale1,
  log = FALSE
) {
  stopifnot(length(log) >= 1L, is.logical(log))
  log <- log[[1L]] # only first value of log is used

  if (!missing(delay)) {
    if (missing(delay1)) {
      delay1 <- delay
    } else {
      warning("Argument delay= is ignored as delay1= is given!", call. = FALSE)
    }
  }
  if (!missing(shape)) {
    if (missing(shape1)) {
      shape1 <- shape
    } else {
      warning("Argument shape= is ignored as shape1= is given!", call. = FALSE)
    }
  }
  if (!missing(scale)) {
    if (missing(scale1)) {
      scale1 <- scale
    } else {
      warning("Argument scale= is ignored as scale1= is given!", call. = FALSE)
    }
  }

  stopifnot(all(is.finite(delay1), is.finite(shape1), is.finite(scale1)))

  # check for easy case: only a single phase
  if (is.null(delay2)) {
    if (!is.null(shape2) || !missing(scale2)) {
      warning(
        "Arguments shape2= and/or scale2= are ignored, as argument delay2= is not set.",
        call. = FALSE
      )
    }
    return(stats::dweibull(
      x = x - delay1,
      shape = shape1,
      scale = scale1,
      log = log
    ))
  }

  # two phases
  # take only first value of parameter arguments!
  if (
    length(delay1) > 1L ||
      length(shape1) > 1L ||
      length(scale1) > 1L ||
      length(delay2) > 1L ||
      length(shape2) > 1L ||
      length(scale2) > 1L
  ) {
    warning(
      "In two-phase setting we do not recycle parameters. Only the 1st value of the parameter arguments is used!",
      call. = FALSE
    )
  }

  delay1 <- delay1[[1L]]
  shape1 <- shape1[[1L]]
  scale1 <- scale1[[1L]]
  # we need both delay2 AND shape2 AND scale2
  delay2 <- delay2[[1L]]
  shape2 <- shape2[[1L]]
  scale2 <- scale2[[1L]]
  stopifnot(is.finite(delay2), is.finite(shape2), is.finite(scale2))

  # check delay constraint
  if (delay1 >= delay2) {
    stop(
      "First delay phase must antedate the second delay phase!",
      call. = FALSE
    )
  }

  # first phase densities
  dvals <- stats::dweibull(
    x = x - delay1,
    shape = shape1,
    scale = scale1,
    log = log
  )
  # second phase
  # update density for observations that lie in the 2nd phase
  phase2Ind <- which(x >= delay2)
  if (length(phase2Ind)) {
    dvals[phase2Ind] <- stats::dweibull(
      x = x[phase2Ind] - delay2,
      shape = shape2,
      scale = scale2,
      log = log
    )
    dvals[phase2Ind] <- if (log) {
      dvals[phase2Ind] - ((delay2 - delay1) / scale1)^shape1
    } else {
      dvals[phase2Ind] * exp(-((delay2 - delay1) / scale1)^shape1)
    }
  }

  dvals
}

#' @rdname DelayedWeibull
#' @param grad logical. Should the gradient be calculated at the given quantile values (and not the cumulative probabilities)?
#' @export
pweib_delayed <- function(
  q,
  delay1,
  shape1,
  scale1 = 1,
  delay2 = NULL,
  shape2 = NULL,
  scale2 = 1,
  delay = delay1,
  shape = shape1,
  scale = scale1,
  lower.tail = TRUE,
  log.p = FALSE,
  grad = FALSE
) {
  if (!missing(delay)) {
    if (missing(delay1)) {
      delay1 <- delay
    } else {
      warning("Argument delay= is ignored as delay1= is given!", call. = FALSE)
    }
  }
  if (!missing(shape)) {
    if (missing(shape1)) {
      shape1 <- shape
    } else {
      warning("Argument shape= is ignored as shape1= is given!", call. = FALSE)
    }
  }
  if (!missing(scale)) {
    if (missing(scale1)) {
      scale1 <- scale
    } else {
      warning("Argument scale= is ignored as scale1= is given!", call. = FALSE)
    }
  }

  lower.tail <- isTRUE(lower.tail[1L])
  log.p <- isTRUE(log.p[1L])
  grad <- isTRUE(grad[1L])

  #cat("delay1: ", delay1, "\tshape1: ", shape1, "\tscale1: ", scale1, "\n") ##DEBUG
  if (
    any(is.null(delay1), is.null(shape1), is.null(scale1)) ||
      !all(is.finite(delay1), is.finite(shape1), is.finite(scale1))
  ) {
    stop(
      "All arguments for delay1=, shape1= and scale1= must be finite!",
      call. = FALSE
    )
  }

  # check for easy case when only a single delay is given
  if (is.null(delay2)) {
    if (!is.null(shape2) || !missing(scale2)) {
      warning(
        "Arguments shape2= and/or scale2= are ignored, as argument delay2= is not set.",
        call. = FALSE
      )
    }

    return(
      if (grad) {
        if (log.p) {
          warning(
            "Argument 'log.p=TRUE' is ignored here for gradient.",
            call. = FALSE
          )
        }

        local({
          qValidInd <- -which(q <= delay1 | shape1 <= 0 | scale1 <= 0) #negative ind of invalid
          if (length(qValidInd) == 0L) {
            qValidInd <- TRUE
          } # all are valid

          q_std <- (q[qValidInd] - delay1) / scale1
          expTerm <- exp(-q_std^shape1)

          pd_delay1 <- pd_shape1 <- pd_scale1 <- numeric(length = length(q))
          pd_delay1[qValidInd] <- -shape1 /
            scale1 *
            q_std^(shape1 - 1) *
            expTerm
          pd_shape1[qValidInd] <- log(q_std) * q_std^shape1 * expTerm
          pd_scale1[qValidInd] <- -shape1 / scale1 * q_std^shape1 * expTerm

          rbind(delay1 = pd_delay1, shape1 = pd_shape1, scale1 = pd_scale1) *
            if (lower.tail) 1 else -1
        })
      } else {
        stats::pweibull(
          q = q - delay1,
          shape = shape1,
          scale = scale1,
          lower.tail = lower.tail,
          log.p = log.p
        )
      }
    )
  }

  # two phases
  if (grad) {
    stop("gradient is not implemented for two phases!", call. = FALSE)
  }

  if (
    length(delay1) > 1L ||
      length(shape1) > 1L ||
      length(scale1) > 1L ||
      length(delay2) > 1L ||
      length(shape2) > 1L ||
      length(scale2) > 1L
  ) {
    warning(
      "In two-phase setting we do not recycle parameters. Only the 1st value of the parameter arguments is used!",
      call. = FALSE
    )
  }

  delay1 <- delay1[[1L]]
  shape1 <- shape1[[1L]]
  scale1 <- scale1[[1L]]
  delay2 <- delay2[[1L]]
  shape2 <- shape2[[1L]]
  scale2 <- scale2[[1L]]
  # we need both delay2 AND shape2 AND scale2
  stopifnot(is.finite(delay2), is.finite(shape2), is.finite(scale2))

  # first phase
  pvals <- stats::pweibull(
    q = q - delay1,
    shape = shape1,
    scale = scale1,
    lower.tail = lower.tail,
    log.p = log.p
  )

  # check delay constraint
  if (delay1 >= delay2) {
    stop(
      "First delay phase must antedate the second delay phase!",
      call. = FALSE
    )
  }

  # check if we have observations in 2nd phase
  phase2Ind <- which(q > delay2)
  if (length(phase2Ind)) {
    # probability values to exceed time q
    pvals[phase2Ind] <- exp(
      -((delay2 - delay1) / scale1)^shape1 -
        ((q[phase2Ind] - delay2) / scale2)^shape2
    )
    if (lower.tail) {
      pvals[phase2Ind] <- 1L - pvals[phase2Ind]
    }
    if (log.p) pvals[phase2Ind] <- log(pvals[phase2Ind])
  }

  pvals
}

#' @rdname DelayedWeibull
#' @export
qweib_delayed <- function(
  p,
  delay1,
  shape1,
  scale1 = 1,
  delay2 = NULL,
  shape2 = NULL,
  scale2 = 1,
  delay = delay1,
  shape = shape1,
  scale = scale1,
  lower.tail = TRUE,
  log.p = FALSE
) {
  stopifnot(
    is.logical(lower.tail),
    length(lower.tail) >= 1L,
    is.logical(log.p),
    length(log.p) >= 1L
  )
  lower.tail <- isTRUE(lower.tail[[1L]])
  log.p <- isTRUE(log.p[[1L]])

  if (!missing(delay)) {
    if (missing(delay1)) {
      delay1 <- delay
    } else {
      warning("Argument delay= is ignored as delay1= is given!", call. = FALSE)
    }
  }
  if (!missing(shape)) {
    if (missing(shape1)) {
      shape1 <- shape
    } else {
      warning("Argument shape= is ignored as shape1= is given!", call. = FALSE)
    }
  }
  if (!missing(scale)) {
    if (missing(scale1)) {
      scale1 <- scale
    } else {
      warning("Argument scale= is ignored as scale1= is given!", call. = FALSE)
    }
  }

  stopifnot(all(is.finite(delay1), is.finite(shape1), is.finite(scale1)))

  # check for easy case: only a single delay phase
  if (is.null(delay2)) {
    if (!is.null(shape2) || !missing(scale2)) {
      warning(
        "Arguments shape2= and/or scale2= are ignored, as argument delay2= is not set.",
        call. = FALSE
      )
    }
    return(
      delay1 +
        stats::qweibull(
          p = p,
          shape = shape1,
          scale = scale1,
          lower.tail = lower.tail,
          log.p = log.p
        )
    )
  }

  # two phases
  if (
    length(delay1) > 1L ||
      length(shape1) > 1L ||
      length(scale1) > 1L ||
      length(delay2) > 1L ||
      length(shape2) > 1L ||
      length(scale2) > 1L
  ) {
    warning(
      "In two-phase setting we do not recycle parameters. Only the 1st value of the parameter arguments is used!",
      call. = FALSE
    )
  }

  delay1 <- delay1[[1L]]
  shape1 <- shape1[[1L]]
  scale1 <- scale1[[1L]]
  # we need both delay2 AND shape2 AND scale2
  delay2 <- delay2[[1L]]
  shape2 <- shape2[[1L]]
  scale2 <- scale2[[1L]]
  stopifnot(is.finite(delay2), is.finite(shape2), is.finite(scale2))

  # first phase
  qvals <- delay1 +
    stats::qweibull(
      p = p,
      shape = shape1,
      scale = scale1,
      lower.tail = lower.tail,
      log.p = log.p
    )

  # check delay constraint
  if (delay1 >= delay2) {
    stop(
      "First delay phase must antedate the second delay phase!",
      call. = FALSE
    )
  }

  # second phase
  # check if we have observations in 2nd phase
  # transform p-values to canonical meaning (lower.tail=T, log.p=F)
  if (!lower.tail) {
    p <- 1L - p
  }
  if (log.p) {
    p <- exp(p)
  }
  phase2Ind <- which(
    p >=
      pweib_delayed(
        q = delay2,
        delay1 = delay1,
        shape1 = shape1,
        scale1 = scale1,
        lower.tail = TRUE,
        log.p = FALSE
      )
  )
  if (length(phase2Ind)) {
    qvals[phase2Ind] <- delay2 +
      scale2 *
        (-log(1L - p[phase2Ind]) - ((delay2 - delay1) / scale1)^shape1)^(1 /
          shape2)
  }

  qvals
}

#' @rdname DelayedWeibull
#' @export
rweib_delayed <- function(
  n,
  delay1,
  shape1,
  scale1 = 1,
  delay2 = NULL,
  shape2 = NULL,
  scale2 = 1,
  delay = delay1,
  shape = shape1,
  scale = scale1,
  cens = 0
) {
  if (!missing(delay)) {
    if (missing(delay1)) {
      delay1 <- delay
    } else {
      warning("Argument delay= is ignored as delay1= is given!", call. = FALSE)
    }
  }
  if (!missing(shape)) {
    if (missing(shape1)) {
      shape1 <- shape
    } else {
      warning("Argument shape= is ignored as shape1= is given!", call. = FALSE)
    }
  }
  if (!missing(scale)) {
    if (missing(scale1)) {
      scale1 <- scale
    } else {
      warning("Argument scale= is ignored as scale1= is given!", call. = FALSE)
    }
  }

  stopifnot(all(is.finite(delay1), is.finite(shape1), is.finite(scale1)))

  stopifnot(is.numeric(n))
  n <- if (length(n) > 1) {
    length(n)
  } else {
    trunc(n)
  }
  if (n == 0) {
    return(numeric(0L))
  }
  stopifnot(length(n) == 1, n > 0)

  if (!is.numeric(cens) || length(cens) > 1L) {
    stop(
      "cens= is expected proportion of right censored observations!",
      call. = FALSE
    )
  }

  if (!is.finite(cens) || cens >= 1 || cens < 0) {
    stop("cens= argument invalid!", call. = FALSE)
  }

  # single phase
  # check for easy case: only a single delay phase
  if (is.null(delay2)) {
    if (!is.null(shape2) || !missing(scale2)) {
      warning(
        "Arguments shape2= and/or scale2= are ignored, as argument delay2= is not set.",
        call. = FALSE
      )
    }

    evTime <- delay1 + stats::rweibull(n = n, shape = shape1, scale = scale1)
    if (near(cens, 0)) {
      return(evTime)
    } else {
      # independent uniform censoring process U(delay1, Z)
      #+where Z is chosen as to give expected proportion of right-censoring
      # with shape1 minute the upper bound of the uniform support explodes and hence few censorings
      #+correct upward for small shape1 parameter helps to prop up censoring level in these cases
      censTime <- local({
        # find root to get the upper bound of the uniform support
        # r is defined as ((Z - delay1) / scale1)^shape1
        r <- stats::uniroot(
          f = function(.x) rootF_cens_unif_weib_cpp(.x, shape1, cens),
          lower = 1e-7,
          upper = 13,
          extendInt = "downX",
          tol = .00015 #set a fixed tolerance (platform independent)
        )$root
        stats::runif(
          n = n,
          min = delay1,
          max = delay1 + scale1 * r^(1 / shape1)
        )
      })
      censIdx <- which(censTime < evTime)

      # result: element-wise minimum of both processes
      res <- evTime
      evStatus <- rep_len(1, length.out = n)

      # avoid having too many censorings by chance
      if (length(censIdx) > 0) {
        # cap number of censorings at expected number of censorings (rounding to integer)
        #+but I don't like it because I want to trust the process
        # maxNbrCens <- if (cens == 1) {
        #   n
        # } else {
        #   min(n - 1, round(n * cens, digits = 0))
        # }
        # maxNbrCens <- max(1, maxNbrCens)
        # maxNbrCens <- min(maxNbrCens, length(censIdx))
        # censIdx <- censIdx[seq_len(maxNbrCens)]

        #res <- pmin.int(evTime, censTime)
        res[censIdx] <- censTime[censIdx]
        evStatus[censIdx] <- 0
      } #fi

      return(Surv(res, event = evStatus, type = "right"))
    }
  }

  # two phases
  if (
    length(delay1) > 1L ||
      length(shape1) > 1L ||
      length(scale1) > 1L ||
      length(delay2) > 1L ||
      length(shape2) > 1L ||
      length(scale2) > 1L
  ) {
    warning(
      "In two-phase setting, we do not recycle parameters. Only the 1st value of the parameter arguments is used!",
      call. = FALSE
    )
  }

  delay1 <- delay1[[1L]]
  shape1 <- shape1[[1L]]
  scale1 <- scale1[[1L]]
  # we need both delay2 AND shape2 AND scale2
  delay2 <- delay2[[1L]]
  shape2 <- shape2[[1L]]
  scale2 <- scale2[[1L]]
  stopifnot(is.finite(delay2), is.finite(shape2), is.finite(scale2))

  # check delay constraint
  if (delay1 >= delay2) {
    stop(
      "First delay phase must antedate the second delay phase!",
      call. = FALSE
    )
  }

  # use inverse CDF-method
  qweib_delayed(
    p = stats::runif(n = n, min = 0L, max = 1L),
    delay1 = delay1,
    shape1 = shape1,
    scale1 = scale1,
    delay2 = delay2,
    shape2 = shape2,
    scale2 = scale2
  )
}

#' @rdname DelayedWeibull
#' @export
mweib_delayed <- function(
  t = +Inf,
  delay1,
  shape1,
  scale1 = 1,
  delay2 = NULL,
  shape2 = NULL,
  scale2 = 1,
  delay = delay1,
  shape = shape1,
  scale = scale1
) {
  if (!missing(delay)) {
    if (missing(delay1)) {
      delay1 <- delay
    } else {
      warning("Argument delay= is ignored as delay1= is given!", call. = FALSE)
    }
  }
  if (!missing(shape)) {
    if (missing(shape1)) {
      shape1 <- shape
    } else {
      warning("Argument shape= is ignored as shape1= is given!", call. = FALSE)
    }
  }
  if (!missing(scale)) {
    if (missing(scale1)) {
      scale1 <- scale
    } else {
      warning("Argument scale= is ignored as scale1= is given!", call. = FALSE)
    }
  }

  stopifnot(is.numeric(t))
  stopifnot(all(is.finite(delay1), is.finite(shape1), is.finite(scale1)))

  # prepare return value
  mvals <- t
  is.na(mvals) <- is.na(t) # propagate NAs

  # check for easy case: only a single delay
  if (is.null(delay2)) {
    # single phase
    if (!is.null(shape2) || !missing(scale2)) {
      warning(
        "Arguments shape2= and/or scale2= are ignored, as argument delay2= is not set.",
        call. = FALSE
      )
    }

    afterInd <- which(t > delay1)
    # make use of lower incomplete gamma function which is calculated as gamma * pgamma
    mvals[afterInd] <- delay1 +
      scale1 /
        shape1 *
        gamma(1 / shape1) *
        stats::pgamma(
          q = ((t[afterInd] - delay1) / scale1)^shape1,
          shape = 1 / shape1
        )
  } else {
    # two phases
    if (
      length(delay1) > 1L ||
        length(shape1) > 1L ||
        length(scale1) > 1L ||
        length(delay2) > 1L ||
        length(shape2) > 1L ||
        length(scale2) > 1L
    ) {
      warning(
        "In two-phase setting, we do not recycle parameters. Only the 1st value of the parameter arguments is used!",
        call. = FALSE
      )
    }

    delay1 <- delay1[[1L]]
    shape1 <- shape1[[1L]]
    scale1 <- scale1[[1L]]
    # we need both delay2 AND shape2 AND scale2
    delay2 <- delay2[[1L]]
    shape2 <- shape2[[1L]]
    scale2 <- scale2[[1L]]
    stopifnot(is.finite(delay2), is.finite(shape2), is.finite(scale2))

    # check delay constraint
    if (delay1 >= delay2) {
      stop(
        "First delay phase must antedate the second delay phase!",
        call. = FALSE
      )
    }

    phase1Ind <- which(delay1 < t & t <= delay2)
    phase2Ind <- which(t > delay2)
    if (length(phase1Ind)) {
      mvals[phase1Ind] <- delay1 +
        scale1 /
          shape1 *
          gamma(1 / shape1) *
          stats::pgamma(
            q = ((t[phase1Ind] - delay1) / scale1)^shape1,
            shape = 1 / shape1
          )
    }
    if (length(phase2Ind)) {
      mvals[phase2Ind] <- delay1 +
        scale1 /
          shape1 *
          gamma(1 / shape1) *
          stats::pgamma(
            q = ((delay2 - delay1) / scale1)^shape1,
            shape = 1 / shape1
          ) +
        pweib_delayed(
          q = delay2,
          delay1 = delay1,
          shape1 = shape1,
          scale1 = scale1,
          lower.tail = FALSE
        ) *
          scale2 /
          shape2 *
          gamma(1 / shape2) *
          stats::pgamma(
            q = ((t[phase2Ind] - delay2) / scale2)^shape2,
            shape = 1 / shape2
          )
    }
  } # 2phase

  mvals
}


# Maybe in the future? Who needs this?
#
# #' Delayed Folded Normal Distribution
# #'
# #' @description
# #' Density, distribution function, quantile function, random generation and restricted mean survival time function for the delayed folded normal distribution.
# #' There is an initial delay phase (parameter `delay`) where no events occur. Beyond that delay, a folded normal distribution applies.
# #'
# #' @details
# #' The numerical arguments other than `n` are recycled to the length of the result (as with the normal distribution in `stats`).
# #' Generally, only the first elements of the logical arguments are used.
# #'
# #' @param x A numeric vector of values for which to get the density.
# #' @param q A numeric vector of quantile values.
# #' @param t A numeric vector of times that restrict the mean survival. Default is `+Inf`, i.e., the unrestricted mean survival time.
# #' @param p A numeric vector of probabilities.
# #' @param n integer. Number of random observations requested.
# #' @param delay numeric. The delay, must be non-negative.
# #' @param mean numeric. The expectation of the underlying normal distribution.
# #' @param sd numeric. The standard deviation of the underlying normal distribution.
# #' @param cens numeric. Expected proportion of random right-censored observations. XXX not implemented yet.
# #' @return Functions pertaining to the delayed folded normal distribution:
# #' * `dfnorm_delayed` gives the density
# #' * `pfnorm_delayed` gives the vector of cumulative probabilities or the gradient matrix (nbr parameters x quantile times)
# #' * `qfnorm_delayed` gives the quantile function
# #' * `rfnorm_delayed` generates a pseudo-random sample
# #' * `mfnorm_delayed` gives the restricted mean survival time
# #'
# #' The length of the result is determined by `n` for `rfnorm_delayed`, and is the maximum of the lengths of the numerical arguments for the other functions,
# #' R's recycling rules apply when only single initial delay phase is used.
# #' @seealso [stats::Normal]
# #' @keywords distribution
# #' @name DelayedFoldedNormal
# NULL
#
# #' @rdname DelayedFoldedNormal
# #' @export
# dfnorm_delayed <- function(x, delay = 0, mean = 1, sd = 1, log = FALSE) {
#   stopifnot(length(log) >= 1L, is.logical(log))
#   log <- log[[1L]] # only first value of log is used
#
#   dVals <- numeric(length(x))
#   xShift <- x - delay1
#   xInd <- which(xShift >= 0)
#   if (log) dVals[-xInd] <- -Inf
#
#   dVals[xInd] <- stats::dnorm(x = xShift[xInd], mean = mean, sd = sd, log = log) + stats::dnorm(x = xShift[xInd], mean = -mean, sd = sd, log = log)
#   dVals
# }

#' Builds the distribution object
#'
#' This object contains all relevant informations about the chosen distribution.
#' @param distribution character(1). Which distribution?
#' @return distribution object. currently, it is simply a list.
#' @export
buildDist <- function(distribution) {
  stopifnot(distribution %in% c("exponential", "weibull", "normal"))

  # return:
  list(
    dist = distribution,
    dist_name = switch(
      distribution,
      normal = "Normal distribution", #(with parameters expectation and std. deviation)
      exponential = "Delayed exponential distribution", #2-parameter
      weibull = "Delayed Weibull distribution", #3-parameter
      "unknown distribution"
    ),
    # some properties
    hasDelay = distribution != "normal",
    hasShape = distribution == "weibull",
    negAllowed = distribution == "normal",
    twoPhaseAllowed = distribution != "normal",

    cdf = switch(
      distribution,
      normal = stats::pnorm,
      exponential = pexp_delayed,
      weibull = pweib_delayed,
      stop(glue("Unknown distribution {distribution}."), call. = FALSE)
    ),

    pdf = switch(
      distribution,
      normal = stats::dnorm,
      exponential = dexp_delayed,
      weibull = dweib_delayed,
      stop(glue("Unknown distribution {distribution}."), call. = FALSE)
    ),

    random = switch(
      distribution,
      normal = stats::rnorm,
      exponential = rexp_delayed,
      weibull = rweib_delayed,
      stop(glue("Unknown distribution {distribution}."), call. = FALSE)
    ),

    param = function(
      twoPhase = FALSE,
      twoGroup = FALSE,
      bind = NULL,
      profiled = FALSE,
      transformed = FALSE
    ) {
      pars <- switch(
        distribution,
        normal = c("mean", "sd"),
        exponential = c("delay1", "rate1", "delay2", "rate2")[seq_len(
          2L * (1L + twoPhase)
        )],
        weibull = c(
          "delay1",
          "shape1",
          "scale1",
          "delay2",
          "shape2",
          "scale2"
        )[seq_len(3L * (1L + twoPhase))],
        stop(glue("Unknown distribution {distribution}."), call. = FALSE)
      )

      if (profiled) {
        # drop parameters that are profiled out
        # profiled-setting effects outcome for both transformed=TRUE but also on original scale (transformed=FALSE)
        # even though profiling is directly relevant only within optimization.
        switch(
          distribution,
          exponential = {
            if (!"rate1" %in% bind || length(bind) == length(pars)) {
              pars <- setdiff(pars, "rate1") #XXX think about profiling and twoPhase! (and twoGroup?!)
            }
          },
          weibull = {
            # drop parameters that are profiled out
            if (!"scale1" %in% bind || length(bind) == length(pars)) {
              pars <- setdiff(pars, "scale1") #XXX think about profiling and twoPhase! (and twoGroup?!)
            }
          },
          normal = {
            warning(
              "Profiling not supported for normal distribution.",
              call. = FALSE
            )
          },
          stop(glue("Unknown distribution {distribution}."), call. = FALSE)
        )
      } #fi

      if (transformed) {
        pars <- paste0(pars, "_tr")
        if (!is.null(bind) && any(nzchar(bind))) bind <- paste0(bind, "_tr")
      }

      if (twoGroup) {
        bind <- intersect(pars, bind) #intersect: enforces original order from pars
        pars_gr <- setdiff(pars, bind)
        # bind parameters first
        pars <- c(
          bind,
          paste(
            rep.int(pars_gr, times = 2L),
            rep(c("x", "y"), each = length(pars_gr)),
            sep = "."
          )
        )
      }

      pars
    } #fn param
  )
}

#' Get delay distribution function
#' @param distribution character(1). Which distribution?
#' @param type character(1). type of function, cdf: cumulative distribution function, density or random function
#' @param twoPhase logical(1). For `type='param'`, do we model two phases?
#' @param twoGroup logical(1). For `type='param'`, do we have two groups?
#' @param bind character. For `type='param'`, names of parameters that are bind between the two groups.
#' @param profiled logical(1). For `type='param'`, do we request profiling?
#' @param transformed logical(1). For `type='param'`, do we need parameter names transformed (as used inside the optimization function?)
#' @return selected distribution function or parameter names
#' @include delay_estimation.R
getDist <- function(
  distribution = c("exponential", "weibull", "normal"),
  type = c("cdf", "prob", "density", "random", "param"),
  twoPhase = FALSE,
  twoGroup = FALSE,
  bind = NULL,
  profiled = FALSE,
  transformed = FALSE
) {
  distribution <- match.arg(distribution)
  type <- match.arg(type)

  switch(
    distribution,
    normal = {
      stopifnot(!twoPhase)
      stopifnot(!profiled)

      switch(
        type,
        # cumulative distribution function
        prob = ,
        cdf = stats::pnorm,
        # density function
        density = stats::dnorm,
        random = stats::rnorm,
        param = {
          pars <- c("mean", "sd")

          if (transformed) {
            pars <- paste0(pars, "_tr")
            if (!is.null(bind) && any(nzchar(bind))) bind <- paste0(bind, "_tr")
          }

          if (twoGroup) {
            bind <- intersect(pars, bind) #intersect: enforces original order from pars
            pars_gr <- setdiff(pars, bind)
            # bind parameters first
            c(
              bind,
              paste(
                rep.int(pars_gr, times = 2L),
                rep(c("x", "y"), each = length(pars_gr)),
                sep = "."
              )
            )
          } else {
            pars
          }
        },
        stop("Unknown attribute of exponential distribution.", call. = FALSE)
      )
    },
    exponential = {
      switch(
        type,
        # cumulative distribution function
        prob = ,
        cdf = pexp_delayed,
        # density function
        density = dexp_delayed,
        # random draw function
        random = rexp_delayed,
        param = {
          pars <- c("delay1", "rate1", "delay2", "rate2")[seq_len(
            2L * (1L + twoPhase)
          )]

          # drop parameters that are profiled out
          # profiled-setting effects outcome for both transformed=TRUE but also on original scale (transformed=FALSE)
          # even though profiling is directly relevant only within optimization.
          if (
            profiled && (!"rate1" %in% bind || length(bind) == length(pars))
          ) {
            pars <- setdiff(pars, "rate1") #XXX think about profiling and twoPhase! (and twoGroup?!)
          }

          if (transformed) {
            pars <- paste0(pars, "_tr")
            if (!is.null(bind) && any(nzchar(bind))) bind <- paste0(bind, "_tr")
          }

          if (twoGroup) {
            bind <- intersect(pars, bind) #intersect: enforces original order from pars
            pars_gr <- setdiff(pars, bind)
            # bind parameters first
            c(
              bind,
              paste(
                rep.int(pars_gr, times = 2L),
                rep(c("x", "y"), each = length(pars_gr)),
                sep = "."
              )
            )
          } else {
            pars
          }
        },
        stop("Unknown attribute of exponential distribution.", call. = FALSE)
      )
    },
    weibull = {
      switch(
        type,
        # cumulative distribution function
        prob = ,
        cdf = pweib_delayed,
        # density function
        density = dweib_delayed,
        # random draw function
        random = rweib_delayed,
        param = {
          pars <- c(
            "delay1",
            "shape1",
            "scale1",
            "delay2",
            "shape2",
            "scale2"
          )[seq_len(3L * (1L + twoPhase))]

          # drop parameters that are profiled out
          if (
            profiled && (!"scale1" %in% bind || length(bind) == length(pars))
          ) {
            pars <- setdiff(pars, "scale1") #XXX think about profiling and twoPhase! (and twoGroup?!)
          }

          if (transformed) {
            pars <- paste0(pars, "_tr")
            if (!is.null(bind) && any(nzchar(bind))) bind <- paste0(bind, "_tr")
          }

          if (twoGroup) {
            bind <- intersect(pars, bind) #intersect: enforces original order from pars
            pars_gr <- setdiff(pars, bind)
            # bind parameters first
            c(
              bind,
              paste(
                rep.int(pars_gr, times = 2L),
                rep(c("x", "y"), each = length(pars_gr)),
                sep = "."
              )
            )
          } else {
            pars
          }
        },
        stop("Unknown attribute of Weibull distribution.", call. = FALSE)
      )
    },
    stop(glue("Unknown distribution {distribution}."), call. = FALSE)
  )
}


#' Get the standard deviation of the fitted delay model fit
#'
#' It's similar in notion to `variance` from package `distribution3`.
#'
#' For two group models you need to specify the group.
#' @param object a fitted `incubate_fit` object
#' @param group the group, "x" or "y"
#' @param type what variance to calculate? Variance of the distribution or variance of the minimum
#' @returns predicted variance of distribution or of minimum for the specified group
getVariance <- function(
  object,
  group = "x",
  type = c("distribution", "minimum")
) {
  stopifnot(`expect an incubate fit!` = inherits(object, "incubate_fit"))
  stopifnot(`only single phase fits currently supported!` = !object$twoPhase)
  stopifnot(`provide group x or y!` = group %in% c("x", "y"))
  type <- match.arg(type)
  stopifnot(is.character(type), length(type) == 1L)

  twoGroup <- isTRUE(object$twoGroup)
  coefGr <- coef.incubate_fit(object, group = group, transformed = FALSE)

  varV <- switch(
    type,
    distribution = {
      # variance of the underlying estimated distribution
      switch(
        object$distO$dist,
        weibull = {
          # calculate var from the parameters
          shape1 <- coefGr[["shape1"]]

          coefGr[["scale1"]]^2 *
            (gamma(1 + 2 / shape1) - gamma(1 + 1 / shape1)^2)
        },
        exponential = {
          # for exponential distribution, the scale parameter is the SD
          1 / coefGr[["rate1"]]^2
        },
        normal = {
          # for normal distribution, the scale parameter is the SD
          coefGr[["sd"]]^2
        },
        stop(
          "getVariance: this distribution is not supported currently!",
          call. = FALSE
        )
      )
    },
    minimum = {
      # estimate variance for min-observation
      # F_{X_{(r)}}(x)=\sum _{j=r}^{n}{\binom {n}{j}}\left[F_{X}(x)\right]^{j}\left[1-F_{X}(x)\right]^{n-j}
      # For a non-negative random variable, there's an elegant formula (Tonelli)
      # E[X] = ∫₀^∞ [1 − F(x)] dx
      # For the second moment:
      # E[X^2] = 2 ∫₀^∞ x·[1 − F(x)] dx
      groupIdx <- 1L + (twoGroup && group == 'y')
      survF <- purrr::partial(
        .f = getDist(
          object$distO$dist,
          type = "cdf",
          twoPhase = object$twoPhase
        ),
        !!!c(coef(object), list(lower.tail = FALSE))
      )
      survFMin <- function(.x) survF(q = .x)^object$nobs[[groupIdx]]
      2 *
        stats::integrate(
          f = function(.x) .x * survFMin(.x),
          lower = 0,
          upper = +Inf
        )$value -
        stats::integrate(f = survFMin, lower = 0, upper = +Inf)$value^2
    },
    stop("Unknown type of variance requested!", call. = FALSE)
  )

  stopifnot(is.finite(varV), varV >= 0)
  varV
} #fn getVariance
