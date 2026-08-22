#' Simulate data from a cosinor model
#'
#' This function simulates data from a cosinor model with a single covariate,
#' where the time scale is month, and optionally
#' allows for single covariate effects on the mean,
#' amplitude, and acrophase.
#'
#' @param n The sample size. An \code{integer} greater than 0.
#' @param mesor A \code{numeric}. The MESOR (midline estimating statistic of
#' rhythm) for \code{group = 0}. The MESOR is independent of the cosinor
#' components, so only one value is allowed even if there are multiple
#' components in the data being simulated.
#' @param amp A \code{numeric}. The amplitude value (for \code{group = 0} if
#' grouped data are being simulated (\code{beta.group = TRUE})). If simulating
#' data with multiple components, specify a vector with values for each
#' component. E.g: \code{amp = c(5, 10)}.
#' @param acro A \code{numeric}. The acrophase value in radians
#' (for \code{group = 0} if grouped data are being simulated
#' (\code{beta.group = TRUE})). If simulating data with multiple components,
#' specify a vector with values for each component. E.g: \code{acr = c(0, pi)}
#' for two components.
#' @param period The period of the rhythm data (for \code{group = 0} if
#' grouped data are being simulated (\code{beta.group = TRUE})).
#' If simulating data with multiple components, specify a vector with values
#' for each component. E.g: \code{period = c(12, 6)} for two components.
#' @param n_components The number of components in the model.
#' This must match the length of the inputs for \code{amp} and \code{acro}.
#' @param beta.group A \code{logical}. If \code{TRUE} a second group of data
#' will be simulated and included in the returned data set. If \code{FALSE},
#' \code{beta.acro}, \code{beta.mesor}, and \code{beta.amp} arguments will be
#' ignored.
#' @param beta.mesor A \code{numeric}. The MESOR value term for \code{group = 1}
#' @param beta.amp  A \code{numeric}. The amplitude value for \code{group = 1}.
#' If simulating data with multiple components, specify a vector with values for
#' each component. E.g: \code{amp = c(2, 8)}.
#' @param beta.acro A \code{numeric}. The acrophase value in radians
#' (for \code{group = 1}. If simulating data with multiple components,
#' specify a vector with values for each component. E.g: \code{acr = c(2, 5)}
#' for two components.
#' @param family A \code{character}. The family (see \code{?family}) of the
#' simulated dataset. Can handle values in \code{c("poisson", "binomial",
#' "gamma", "gaussian")}.
#' @param n_period A \code{numeric}. The number of cycles of the rhythm to be
#' simulated.
#' @param ... Extra arguments, including \code{alpha} that controls the
#' \code{shape} argument when sampling from a gamma distribution
#' (when \code{family = "gamma"}; default is 1), and \code{sd}
#' (standard deviation) which is used when sampling from a normal distribution
#' (when \code{family = "gaussian"}; default is 1). To specify these parameters
#' for the beta (treatment) group, use \code{beta.alpha} and \code{beta.sd}
#'
#' @return Returns simulated data in a \code{data.frame}.
#'
#' @examples
#' simulate_cosinor(
#'   n = 100,
#'   mesor = 1,
#'   amp = 1,
#'   acro = 1,
#'   period = 24,
#'   family = "gaussian"
#' )
#' @srrstats {G5.1}
#'
#' @export
simulate_cosinor <- function(n,
                             mesor,
                             amp,
                             acro,
                             period = 24,
                             n_components,
                             beta.group = FALSE,
                             beta.mesor,
                             beta.amp,
                             beta.acro,
                             n_period = 1,
                             family = c(
                               "gaussian",
                               "poisson",
                               "binomial",
                               "gamma"
                             ),
                             ...) {
  # attempt to infer n_components if missing
  if (missing(n_components)) {
    if (length(amp) == length(acro)) {
      n_components <- length(amp)
    }
  }

  assertthat::assert_that(
    assertthat::is.flag(beta.group),
    msg = "beta.group argument must be logical"
  )

  if (!beta.group & !missing(beta.mesor) &
    !missing(beta.amp) & !missing(beta.acro)) {
    beta.group <- TRUE
    message(paste(
      "all betas were present but beta.group was FALSE.",
      "beta.group has been changed to be TRUE."
    ))
  }

  family <- match.arg(family)

  .validate_simulate_cosinor_inputs(
    n,
    mesor,
    amp,
    acro,
    period,
    n_components,
    beta.group,
    beta.mesor,
    beta.amp,
    beta.acro,
    n_period
  )

  # generate a time vector
  ttt <- stats::runif(n, min = 0, n_period * max(period))

  # create dataset for only one group if beta.group = FALSE
  if (!beta.group) {
    if (!"sd" %in% names(list(...))) {
      sd_val <- 1
    } else {
      sd_val <- list(...)$sd
    }
    if (!"alpha" %in% names(list(...))) {
      alpha_val <- 1
    } else {
      alpha <- list(...)$alpha
    }
    df <- .get_dataset(
      family = family,
      amp = amp,
      acro = acro,
      ttt = ttt,
      mesor = mesor,
      n_components = n_components,
      period = period,
      sd_val = sd_val,
      alpha_val = alpha_val
    )
    # label the group
    df$group <- 0
  }

  # create dataset for two groups if beta.group = TRUE
  if (beta.group) {
    if (!"sd" %in% names(list(...))) {
      sd_val <- 1
    } else {
      sd_val <- list(...)$sd
    }
    if (!"alpha" %in% names(list(...))) {
      alpha_val <- 1
    } else {
      alpha_val <- list(...)$alpha
    }

    data_A <- .get_dataset(
      family = family,
      amp = amp,
      acro = acro,
      ttt = ttt,
      mesor = mesor,
      n_components = n_components,
      period = period,
      sd_val = sd_val,
      alpha_val = alpha_val
    )

    if (!"beta.sd" %in% names(list(...))) {
      beta.sd_val <- 1
    } else {
      beta.sd_val <- list(...)$beta.sd
    }
    if (!"beta.alpha" %in% names(list(...))) {
      beta.alpha_val <- 1
    } else {
      beta.alpha_val <- list(...)$beta.alpha
    }


    data_B <- .get_dataset(
      family = family,
      amp = beta.amp,
      acro = beta.acro,
      ttt = ttt,
      mesor = beta.mesor,
      n_components = n_components,
      period = period,
      sd_val = beta.sd_val,
      alpha_val = beta.alpha_val
    )
    data_A$group <- 0
    data_B$group <- 1
    df <- rbind(data_A, data_B)
  }

  return(df)
}

#' Validate args passed to \code{simulate_cosinor()}.
#'
#' @param n Arg from \code{simulate_cosinor()}.
#' @param mesor Arg from \code{simulate_cosinor()}.
#' @param amp Arg from \code{simulate_cosinor()}.
#' @param acro Arg from \code{simulate_cosinor()}.
#' @param period Arg from \code{simulate_cosinor()}.
#' @param n_components Arg from \code{simulate_cosinor()}.
#' @param beta.group Arg from \code{simulate_cosinor()}.
#' @param beta.mesor Arg from \code{simulate_cosinor()}.
#' @param beta.amp Arg from \code{simulate_cosinor()}.
#' @param beta.acro Arg from \code{simulate_cosinor()}.
#' @param n_period Arg from \code{simulate_cosinor()}.
#'
#' @return \code{NULL}
#'
#' @noRd
.validate_simulate_cosinor_inputs <- function(n,
                                              mesor,
                                              amp,
                                              acro,
                                              period,
                                              n_components,
                                              beta.group,
                                              beta.mesor,
                                              beta.amp,
                                              beta.acro,
                                              n_period) {
  # validating inputs
  assertthat::assert_that(
    assertthat::is.count(n),
    msg = "n must be an integer greater than 0"
  )

  assertthat::assert_that(
    assertthat::is.count(n_components),
    msg = "n_components must be an integer greater than 0"
  )

  assertthat::assert_that(
    is.numeric(mesor) & length(mesor) == 1,
    msg = "mesor must a single number"
  )

  assertthat::assert_that(
    is.numeric(amp) & length(amp) == n_components,
    msg = paste(
      "amp must be a vector containing numbers, with length",
      "equal to n_components"
    )
  )

  assertthat::assert_that(
    is.numeric(acro) & length(acro) == n_components,
    msg = paste(
      "acro must be a vector containing numbers, with",
      "length equal to n_components"
    )
  )

  assertthat::assert_that(
    is.numeric(period) & length(period) == n_components,
    msg = paste(
      "period must be a vector containing numbers, with",
      "length equal to n_components"
    )
  )

  if (beta.group) {
    assertthat::assert_that(
      is.numeric(beta.mesor) & length(beta.mesor) == 1,
      msg = "beta.mesor must be a single number"
    )

    assertthat::assert_that(
      is.numeric(beta.amp) & length(beta.amp) == n_components,
      msg = paste(
        "beta.amp must be a vector containing numbers,",
        "with length equal to n_components"
      )
    )

    assertthat::assert_that(
      is.numeric(beta.acro) & length(beta.acro) == n_components,
      msg = paste(
        "beta.acro must be a vector containing numbers,",
        "with length equal to n_components"
      )
    )
  }
}


#' Simulate a dataset from a cosinor model.
#'
#' @param family Arg from \code{simulate_cosinor()}.
#' @param amp Arg from \code{simulate_cosinor()}.
#' @param acro Arg from \code{simulate_cosinor()}.
#' @param ttt Vector of time for which to sample points.
#' @param mesor Arg from \code{simulate_cosinor()}.
#' @param n_components Arg from \code{simulate_cosinor()}.
#' @param period Arg from \code{simulate_cosinor()}.
#' @param sd Standard deviation (used when \code{family = "gaussian")}.
#' @param alpha Used when sampling from gamma distribution.
#' @param ... Optional, unused args.
#'
#' @return A \code{data.frame}.
#' @noRd
.get_dataset <- function(family,
                         amp,
                         acro,
                         ttt,
                         mesor,
                         n_components,
                         period,
                         sd_val,
                         alpha_val) {
  d_params <- .get_params(
    amp = amp,
    acro = acro,
    n_components = n_components,
    ttt = ttt,
    period = period
  )

  if (family == "gaussian") {
    d_params$param <- mesor + d_params$param
    d_params$Y <- stats::rnorm(
      n = length(ttt),
      mean = d_params$param,
      sd = sd_val
    )
  }
  if (family == "poisson") {
    d_params$param <- exp(mesor + d_params$param)
    d_params$Y <- stats::rpois(
      n = length(ttt),
      lambda = d_params$param
    )
  }
  if (family == "binomial") {
    d_params$param <- exp(
      mesor + d_params$param
    ) / (1 + exp(mesor + d_params$param))
    d_params$Y <- stats::rbinom(
      n = length(ttt),
      size = 1,
      prob = d_params$param
    )
  }
  if (family == "gamma") {
    d_params$param <- alpha_val / exp(mesor + d_params$param)
    d_params$Y <- stats::rgamma(
      n = length(ttt),
      shape = alpha_val,
      rate = d_params$param
    )
  }

  with(d_params, data.frame(Y, times = ttt))
}

#' Get cosinor parameters given amplitude and acrophase inputs.
#'
#' @param amp Arg from \code{simulate_cosinor()}.
#' @param acro Arg from \code{simulate_cosinor()}.
#' @param n_components Arg from \code{simulate_cosinor()}.
#' @param ttt vector of time for which to sample points.
#' @param period Arg from \code{simulate_cosinor()}.
#'
#' @return A \code{data.frame}
#' @noRd
.get_params <- function(amp, acro, n_components, ttt, period) {
  param <- 0
  for (i in seq_len(n_components)) {
    B <- amp[i] * cos(acro[i])
    G <- amp[i] * sin(acro[i])
    rrr <- cos(2 * pi * (ttt) / period[i])
    sss <- sin(2 * pi * (ttt) / period[i])
    param <- param + B * rrr + G * sss
  }
  data.frame(ttt, param)
}
