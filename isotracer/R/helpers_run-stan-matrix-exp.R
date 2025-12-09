### * None of the functions in this file is exported

### * matrix_exp_stan()

#' Run a stan model from a network model, using matrix exponential to solve ODEs.
#'
#' Incorporate a loglik trace: https://mc-stan.org/loo/reference/extract_log_lik.html
#'
#' @param nm A \code{networkModel} object.
#' @param iter A positive integer specifying the number of iterations for each
#'     chain (including warmup). The default is 2000.
#' @param chains A positive integer specifying the number of Markov chains.
#'     The default is 4.
#' @param cores Number of cores to use for parallel run. Default is
#'     \code{NULL}, which means to use the value stored in
#'     \code{options()[["mc.cores"]]} (or 1 if this value is not set).
#' @param stanfit If TRUE, returns a `stanfit` object instead of the more
#'     classical `mcmc.list` object.
#' @param use_fixed_values Boolean, if TRUE any parameter value set with
#'     \code{set_params()} will be taken as fixed during the MCMC run. Default
#'     is FALSE.
#' @param vb Boolean, if TRUE will use \code{rstan::vb} for a quick approximate
#'   sampling of the posterior. Important note from \code{?rstan::vb}:
#'   "This is still considered an experimental feature.  We recommend calling
#'   \code{stan} or \code{sampling} for final inferences and only using ‘vb’ to
#'   get a rough idea of the parameter distributions."
#' @param ... Passed to \code{rstan::sampling}.
#'
#' @keywords internal
#' @noRd

matrix_exp_stan <- function(nm, iter = 2000, chains = 4, cores = NULL,
                            stanfit = FALSE, use_fixed_values = FALSE,
                            vb = FALSE, ...) {
  # Detect cores
  cores <- get_n_cores(cores = cores)
  # Convert network model to stan data
  stan.data <- prep_stan_data_expm(nm, use_fixed_values = use_fixed_values)
  # Fit the model
  stan.data[["ode_method"]] <- 1 # For matrix exponential
  # Approximate sampling (vb = TRUE)
  if (vb) {
    if (stanfit) {
      stop("vb not implemented for stanfit = TRUE")
    }
    fits <- lapply(seq_len(chains), function(i) {
      fit <- rstan::vb(stanmodels[["networkModel"]],
                       data = stan.data,
                       iter = iter,
                       pars = c("nonConstantParams", "log_lik",
                                "rawUniformParams", "rawHcauchyParams",
                                "rawBetaParams", "rawTrNormParams",
                                "rawExponentialParams", "rawGammaParams"), ...)
      stopifnot(!"isotracer_stan_data" %in% names(attributes(fit)))
      attr(fit, "isotracer_stan_data") <- stan.data
      fit
    })
    fits <- lapply(fits, stanfit_to_named_mcmclist)
    fits <- lapply(fits, function(i) i[[1]])
    return(coda::as.mcmc.list(fits))
  }
  # Accurate sampling (vb = FALSE)
  fit <- rstan::sampling(stanmodels[["networkModel"]],
                         data = stan.data,
                         iter = iter,
                         chains = chains,
                         cores = cores,
                         pars = c("nonConstantParams", "log_lik",
                                  "rawUniformParams", "rawHcauchyParams",
                                  "rawBetaParams", "rawTrNormParams",
                                  "rawExponentialParams", "rawGammaParams"), ...)
  stopifnot(!"isotracer_stan_data" %in% names(attributes(fit)))
  attr(fit, "isotracer_stan_data") <- stan.data
  # Return
  if (stanfit) {
    return(fit)
  } else {
    return(stanfit_to_named_mcmclist(stanfit = fit))
  }
}

### * prep_stan_data_expm()

#' Prepare stan data from a network model
#'
#' @param nm A \code{networkModel} object.
#' @param use_fixed_values Boolean, if TRUE any parameter value set with
#'     \code{set_params()} will be taken as fixed during the MCMC run. Default
#'     is FALSE.
#' 
#' @keywords internal
#' @noRd

prep_stan_data_expm <- function(nm, use_fixed_values = FALSE) {
    d <- list()
    params_nm <- params(nm, simplify = TRUE)
    priors_nm <- priors(nm, fix_set_params = use_fixed_values,
                        quiet = TRUE)
    priors_nm <- priors_nm[match(params_nm, priors_nm[["in_model"]]), ]
    stopifnot(all(params_nm == priors_nm[["in_model"]]))
    # For now the stan model is only implemented for network models with the
    # same number of compartments on each row (a more general case where rows
    # can have different numbers of compartments is easily converted to this
    # case, by adding compartments without connections to fill the topology in
    # each row).
    stopifnot(length(unique(sapply(comps(nm), length))) == 1)
    # Counts
    d[["nComps"]] <- length(comps(nm)[[1]])
    d[["nGroups"]] <- nrow(nm)
    d[["nParams"]] <- length(params_nm)
    # Encode priors
    d <- c(d, encode_priors(params_nm, priors_nm))
    # Encode distribution families (for proportions and sizes)
    d <- c(d, encode_distrib_families(nm))
    # Encode initial conditions
    d <- c(d, encode_init(nm))
    # Encode steady state compartments
    d <- c(d, encode_steady(nm))
    # Encode split compartments
    d <- c(d, encode_split(nm, params_nm))
    # Encode decay rate for radioactive tracers
    lambda_decay <- attr(nm, "lambda_hl")
    if (is.null(lambda_decay)) {
        lambda_decay <- 0
    }
    d[["lambda_decay"]] <- lambda_decay
    # Encode intervals in-between events
    d <- c(d, encode_intervals(nm))
    # Encode pulse events
    d <- c(d, encode_pulse_events(nm))
    # Encode unique obs times
    d <- c(d, encode_unique_obs_times(nm))
    # Encode individual obs
    d <- c(d, encode_individual_obs(nm, params_nm))
    # Encode uptake rates (upsilons)
    d <- c(d, encode_upsilons(nm, params_nm))
    # Encode losses (lambdas)
    d <- c(d, encode_lambdas(nm, params_nm))
    # Add encoding for Euler (so that the Stan model does not crash)
    d[["allParams"]] <- params_nm
    euler <- encode_time_schemes(nm)
    stopifnot(!any(names(euler) %in% names(d)))
    d <- c(d, euler)
    return(d)
}

### * encode_intervals()

#' Encode time intervals between events
#'
#' @param nm A \code{networkModel} object.
#'
#' @keywords internal
#' @noRd

encode_intervals <- function(nm) {
    o <- list()
    nGroups <- nrow(nm)
    timelines <- lapply(seq_len(nrow(nm)), function(i) {
        nm_row_get_timeline(nm[i, ])
    })
    nTimeIntervals <- sapply(timelines, '[[', "nIntervals")
    o[["maxNtimeIntervals"]] <- max(nTimeIntervals)
    o[["nTimeIntervals"]] <- setNames(c(nTimeIntervals, 0), # Padded
                                      nm = c(paste0("grp", seq_len(nrow(nm))), "padding"))
    durations <- lapply(timelines, function(x) {
        x[["intervalEnds"]] - x[["intervalStarts"]]
    })
    o[["intervalsLengths"]] <- array(0, dim = c(max(nTimeIntervals), nGroups),
                                     dimnames = list(seq_len(max(nTimeIntervals)),
                                                     paste0("grp", seq_len(nGroups))))
    for (g in seq_len(nGroups)) {
        o[["intervalsLengths"]][1:(o[["nTimeIntervals"]][g]), g] <- durations[[g]]
    }
    o
}

### ** nm_row_get_timeline()

#' @examples
#' nm_row_get_timeline(aquarium_mod[1, ])
#' nm_row_get_timeline(trini_mod[1, ])
#' @keywords internal
#' @noRd
nm_row_get_timeline <- function(nm_row) {
    events <- nm_row_get_events(nm_row)
    obs <- nm_row_get_obs(nm_row)
    o <- c(events, obs)
    o[["maxTime"]] <- max(c(o[["eventTimes"]], o[["obsTimes"]]))
    o[["nIntervals"]] <- o[["nEventTimes"]] + 1
    o[["intervalStarts"]] <- c(0, o[["eventTimes"]])
    o[["intervalEnds"]] <- c(o[["eventTimes"]], o[["maxTime"]])
    o[["intervalStartsWithAnEvent"]] <- o[["intervalStarts"]] %in% o[["eventTimes"]]
    o[["obsIntervalIndex"]] <- sapply(o[["obsTimes"]], function(i) {
        w <- which(o[["intervalStarts"]] <= i)
        if (length(w) > 0) { return(max(w)) }
        return(o[["nIntervals"]])
    })
    o[["elapsedTimeSinceEvent"]] <- o[["obsTimes"]] - o[["intervalStarts"]][o[["obsIntervalIndex"]]]
    return(o)
}

### ** plot_nm_row_timeline()

#' @param nm_row_timeline Output from \code{nm_row_get_timeline}.
#' @return None, called for side-effect of drawing a timeline plot where
#'     boundaries are light gray, events are red, and observation times are
#'     blue.
#' @examples
#' plot_nm_row_timeline(nm_row_get_timeline(aquarium_mod[1, ]))
#' plot_nm_row_timeline(nm_row_get_timeline(trini_mod[1, ]))
#' @keywords internal
#' @noRd
plot_nm_row_timeline <- function(nm_row_timeline) {
    z <- nm_row_timeline
    plot(0, type = "n", xlim = c(0, z[["maxTime"]]), ylim = c(0, 1),
         xlab = "Time", axes = FALSE, ylab = "")
    graphics::axis(1)
    graphics::lines(c(0, 0), c(0, 1), col = "lightgray")
    graphics::lines(rep(z[["maxTime"]], 2), c(0, 1), col = "lightgray")
    for (i in seq_along(z[["eventTimes"]])) {
        graphics::lines(rep(z[["eventTimes"]], 2), c(0, 1), col = "red", lty = 2, lwd = 2)
    }
    graphics::points(z[["obsTimes"]], y = rep(0.5, length(z[["obsTimes"]])),
           pch = 21, col = "blue", bg = "blue")
}

### ** nm_row_get_events()

nm_row_get_events <- function(nm_row) {
    stopifnot(nrow(nm_row) == 1)
    o <- list()
    if (!"events" %in% colnames(nm_row)) {
        o[["eventTimes"]] <- numeric()
    } else {
        o[["eventTimes"]] <- sort(unique(nm_row[["events"]][[1]][["time"]]))
    }
    o[["nEventTimes"]] <- length(o[["eventTimes"]])
    o <- o[c("nEventTimes", "eventTimes")]
    return(o)
}

### ** nm_row_get_obs()

nm_row_get_obs <- function(nm_row) {
    stopifnot(nrow(nm_row) == 1)
    stopifnot("observations" %in% colnames(nm_row))
    o <- list()
    o[["obsTimes"]] <- sort(unique(nm_row[["observations"]][[1]][["time"]]))
    o[["nObsTimes"]] <- length(o[["obsTimes"]])
    o <- o[c("nObsTimes", "obsTimes")]
    return(o)
}

### * encode_pulse_events()

#' Encode pulse events (for Stan model with matrix exponential)
#'
#' @param nm A \code{networkModel} object.
#'
#' @keywords internal
#' @noRd

encode_pulse_events <- function(nm) {
    o <- list()
    if (!"events" %in% names(nm)) {
        nm[["events"]] <- rep(list(NULL), nrow(nm))
    }
    comps <- comps(nm)[[1]]
    nGroups <- nrow(nm)
    timelines <- lapply(seq_len(nrow(nm)), function(i) {
        nm_row_get_timeline(nm[i, ])
    })
    nPulseEvents <- sapply(seq_len(nrow(nm)), function(i) {
        n <- nrow(nm[i, ][["events"]][[1]])
        ifelse(is.null(n), 0, n)
    })
    o[["maxNpulseEvents"]] <- max(nPulseEvents)
    o[["nPulseEvents"]] <- setNames(c(nPulseEvents, 0), # Padded
                                    nm = c(paste0("grp", seq_len(nrow(nm))), "padding"))
    interval_indices <- list()
    comp_indices <- list()
    marked <- list()
    unmarked <- list()
    for (g in seq_len(nGroups)) {
        x <- nm[["events"]][[g]]
        if (!is.null(x)) {
            x <- x[order(x[["time"]]), ]
            stopifnot(all(x$event == "pulse"))
            interval_indices[[g]] <- match(x[["time"]], timelines[[g]][["intervalStarts"]])
            comp_indices[[g]] <- match(x[["compartment"]], comps)
            marked[[g]] <- sapply(x$characteristics, '[[', "marked")
            unmarked[[g]] <- sapply(x$characteristics, '[[', "unmarked")
        }
    }
    o[["pulseEventsIndices"]] <- array(0, dim = c(max(nPulseEvents), 2, nGroups),
                                       dimnames = list(seq_len(max(nPulseEvents)),
                                                       c("interval", "comp"),
                                                       paste0("grp", seq_len(nGroups))))
    o[["pulseEventsQuantities"]] <- array(0, dim = c(max(nPulseEvents), 2, nGroups),
                                          dimnames = list(seq_len(max(nPulseEvents)),
                                                          c("unmarked", "marked"),
                                                          paste0("grp", seq_len(nGroups))))
    for (g in seq_len(nGroups)) {
        if (!is.null(nm[["events"]][[g]])) {
            o[["pulseEventsIndices"]][1:(o[["nPulseEvents"]][g]), 1, g] <- interval_indices[[g]]
            o[["pulseEventsIndices"]][1:(o[["nPulseEvents"]][g]), 2, g] <- comp_indices[[g]]
            o[["pulseEventsQuantities"]][1:(o[["nPulseEvents"]][g]), 1, g] <- unmarked[[g]]
            o[["pulseEventsQuantities"]][1:(o[["nPulseEvents"]][g]), 2, g] <- marked[[g]]
        }
    }
    o
}

### * encode_unique_obs_times()

#' Encode unique observation times (for Stan model with matrix exponential)
#'
#' @param nm A \code{networkModel} object.
#'
#' @keywords internal
#' @noRd

encode_unique_obs_times <- function(nm) {
    o <- list()
    comps <- comps(nm)[[1]]
    nGroups <- nrow(nm)
    timelines <- lapply(seq_len(nrow(nm)), function(i) {
        nm_row_get_timeline(nm[i, ])
    })
    nObsTimes <- sapply(timelines, '[[', "nObsTimes")
    o[["maxNobsTimes"]] <- max(nObsTimes)
    o[["nObsTimes"]] <- setNames(c(nObsTimes, 0), # Padded
                                 nm = c(paste0("grp", seq_len(nrow(nm))), "padding"))
    o[["elapsedTimeSinceEvent"]] <- array(0, dim = c(nGroups, max(nObsTimes)),
                                           dimnames = list(paste0("grp", seq_len(nGroups)),
                                                           seq_len(max(nObsTimes))))
    o[["obsIntervalsIndices"]] <- array(0, dim = c(nGroups, max(nObsTimes)),
                                        dimnames = list(paste0("grp", seq_len(nGroups)),
                                                        seq_len(max(nObsTimes))))
    for (g in seq_len(nGroups)) {
        o[["elapsedTimeSinceEvent"]][g, 1:(o[["nObsTimes"]][g])] <- timelines[[g]][["elapsedTimeSinceEvent"]]
        o[["obsIntervalsIndices"]][g, 1:(o[["nObsTimes"]][g])] <- timelines[[g]][["obsIntervalIndex"]]
    }
    o
}

### * encode_individual_obs()

#' Encode individual observations (for Stan model with matrix exponential)
#'
#' @param nm A \code{networkModel} object.
#' @param allParams Parameters of the network model.
#'
#' @keywords internal
#' @noRd

encode_individual_obs <- function(nm, allParams) {
    o <- list()
    comps <- comps(nm)[[1]]
    nGroups <- nrow(nm)
    zeta_by_comp <- attr(nm, "size_zeta_per_compartment")
    if (is.null(zeta_by_comp)) {
        zeta_by_comp<- FALSE
    }
    timelines <- lapply(seq_len(nrow(nm)), function(i) {
        nm_row_get_timeline(nm[i, ])
    })
    # TODO Add filtering to keep only compartments present in topo
    # TODO Handle gracefully the case without observations
    # Get sizes
    sizes <- purrr::map(nm$observations, function(x) {
        na.omit(x[, c("compartment", "size", "time")])
    })
    # Get proportions
    props <- purrr::map(nm$observations, function(x) {
        na.omit(x[, c("compartment", "proportion", "time")])
    })
    # Convert original time into indices of unique observation times
    for (g in seq_len(nGroups)) {
        sizes[[g]][["timepoint"]] <- match(sizes[[g]][["time"]], timelines[[g]][["obsTimes"]])
        props[[g]][["timepoint"]] <- match(props[[g]][["time"]], timelines[[g]][["obsTimes"]])
    }
    # Encode compartments
    for (g in seq_len(nGroups)) {
        sizes[[g]][["compartment"]] <- match(sizes[[g]][["compartment"]], comps)
        props[[g]][["compartment"]] <- match(props[[g]][["compartment"]], comps)
    }
    # Encode sizes and props
    o[["nSizesObs"]] <- c(purrr::map_dbl(sizes, nrow), 0) # Padded
    names(o[["nSizesObs"]]) <- c(paste0("grp", seq_len(nrow(nm))), "padding")
    o[["nPropsObs"]] <- c(purrr::map_dbl(props, nrow), 0) # Padded
    names(o[["nPropsObs"]]) <- c(paste0("grp", seq_len(nrow(nm))), "padding")
    o[["maxNsizesObs"]] <- max(o[["nSizesObs"]])
    o[["maxNpropsObs"]] <- max(o[["nPropsObs"]])
    # Prepare containers
    o[["sizesObsIndices"]] <- array(0, dim = c(o[["maxNsizesObs"]], 3, nGroups),
                                    dimnames = list(seq_len(o[["maxNsizesObs"]]),
                                                    c("comp", "timepoint", "zeta"),
                                                    paste0("grp", seq_len(nGroups))))
    o[["sizesObs"]] <- array(0, dim = c(o[["maxNsizesObs"]], nGroups),
                             dimnames = list(seq_len(o[["maxNsizesObs"]]),
                                             paste0("grp", seq_len(nGroups))))
    o[["propsObsIndices"]] <- array(0, dim = c(o[["maxNpropsObs"]], 3, nGroups),
                                    dimnames = list(seq_len(o[["maxNpropsObs"]]),
                                                    c("comp", "timepoint", "eta"),
                                                    paste0("grp", seq_len(nGroups))))
    o[["propsObs"]] <- array(0, dim = c(o[["maxNpropsObs"]], nGroups),
                             dimnames = list(seq_len(o[["maxNpropsObs"]]),
                                             paste0("grp", seq_len(nGroups))))
    # Fill containers
    for (g in seq_len(nGroups)) {
        if (o[["nSizesObs"]][g] > 0) {
            o[["sizesObsIndices"]][1:o[["nSizesObs"]][g], 1:2, g] <- as.matrix(sizes[[g]][, c("compartment", "timepoint")])
            o[["sizesObs"]][1:o[["nSizesObs"]][g], g] <- as.matrix(sizes[[g]][, c("size")])
            if (!zeta_by_comp) {
                zeta_global <- nm[["parameters"]][[g]]$in_model[nm[["parameters"]][[g]]$in_replicate == "zeta"]
                zeta_index <- match(zeta_global, allParams)
                o[["sizesObsIndices"]][1:o[["nSizesObs"]][g], 3, g] <- zeta_index
            } else {
                comps <- colnames(nm$topology[[g]])
                comps <- setNames(seq_along(comps), nm = comps)
                zeta_replicate <- paste0("zeta_", names(comps)[sizes[[g]][["compartment"]]])
                zeta_global <- nm[["parameters"]][[g]]$in_model[match(zeta_replicate, nm[["parameters"]][[g]]$in_replicate)]
                zeta_index <- match(zeta_global, allParams)
                o[["sizesObsIndices"]][1:o[["nSizesObs"]][g], 3, g] <- zeta_index
            }
        }
    }
    for (g in seq_len(nGroups)) {
        if (o[["nPropsObs"]][g] > 0) {
            o[["propsObsIndices"]][1:o[["nPropsObs"]][g], 1:2, g] <- as.matrix(props[[g]][, c("compartment", "timepoint")])
            o[["propsObs"]][1:o[["nPropsObs"]][g], g] <- as.matrix(props[[g]][, c("proportion")])
            eta_global <- nm[["parameters"]][[g]]$in_model[nm[["parameters"]][[g]]$in_replicate == "eta"]
            eta_index <- match(eta_global, allParams)
            o[["propsObsIndices"]][1:o[["nPropsObs"]][g], 3, g] <- eta_index
        }
    }
    # Return
    return(o)
}
