### * None of the functions in this file is exported

### * mugen_stan()

#' Run a stan model from a network model (temporary name)
#'
#' Incorporate a loglik trace: https://mc-stan.org/loo/reference/extract_log_lik.html
#'
#' @param nm A \code{networkModel} object.
#' @param iter A positive integer specifying the number of iterations for each
#'     chain (including warmup). The default is 2000.
#' @param chains A positive integer specifying the number of Markov chains.
#'     The default is 4.
#' @param euler_control An optional list containing extra parameters for the
#'     Euler method. The list elements can be \code{"dt"} or
#'     \code{"grid_size"}, which are respectively the time step size for
#'     trajectory calculations (\code{"dt"}) or the number of points for the
#'     calculation (\code{"grid_size"}). If none is provided, a default grid
#'     size of 256 steps is used.
#' @param cores Number of cores to use for parallel run. Default is
#'     \code{NULL}, which means to use the value stored in
#'     \code{options()[["mc.cores"]]} (or 1 if this value is not set).
#' @param stanfit If TRUE, returns a `stanfit` object instead of the more
#'     classical `mcmc.list` object.
#' @param use_fixed_values Boolean, if TRUE any parameter value set with
#'     \code{set_params()} will be taken as fixed during the MCMC run. Default
#'     is FALSE.
#' @param ... Passed to \code{rstan::sampling}.
#'
#' @keywords internal
#' @noRd

mugen_stan <- function(nm, iter = 2000, chains = 4, euler_control = list(),
                       cores = NULL, stanfit = FALSE,
                       use_fixed_values = FALSE, ...) {
    # Detect cores
    cores <- get_n_cores(cores = cores)
    # Convert network model to stan data
    stan.data <- prep_stan_data_euler(nm, dt = euler_control[["dt"]],
                                      grid_size = euler_control[["grid_size"]],
                                      use_fixed_values = use_fixed_values)
    # Fit the model
    stan.data[["ode_method"]] <- 2 # For Euler scheme
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

### * prep_stan_data_euler()

#' Prepare stan data from a network model
#'
#' @param nm A \code{networkModel} object.
#' @param dt,grid_size Either the time step size for trajectory calculations
#'     (\code{dt}) or the number of points for the calculation
#'     (\code{grid_size}) can be provided. If none is provided, then a default
#'     grid size of 256 steps is used.
#' @param use_fixed_values Boolean, if TRUE any parameter value set with
#'     \code{set_params()} will be taken as fixed during the MCMC run. Default
#'     is FALSE.
#' 
#' @keywords internal
#' @noRd

prep_stan_data_euler <- function(nm, dt = NULL, grid_size = NULL,
                                 use_fixed_values = FALSE) {
    d <- list()
    end <- NULL
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
    # Encode steady state compartments
    dSteady <- encode_steady(nm)
    d <- c(d, dSteady)
    # Encode split compartments
    dSplit <- encode_split(nm, params_nm)
    d <- c(d, dSplit)
    # Encode initial conditions
    dInit <- encode_init(nm)
    d <- c(d, dInit)
    # Encode events
    dEvents <- encode_events(nm, dt = dt, grid_size = grid_size, end = end)
    d <- c(d, dEvents)
    # Encode observations (including eta and zeta parameter indices)
    dObs <- encode_obs(nm, params_nm, dt = dt, grid_size = grid_size, end = end)
    d <- c(d, dObs)
    # Encode parameter priors
    dPriors <- encode_priors(params_nm, priors_nm)
    d <- c(d, dPriors)
    # Encode time schemes
    dTimeSchemes <- encode_time_schemes(nm, dt = dt, grid_size = grid_size,
                                        end = end)
    d <- c(d, dTimeSchemes)
    # Encode uptake rates (upsilons)
    dUpsilons <- encode_upsilons(nm, params_nm)
    d <- c(d, dUpsilons)
    # Encode losses (lambdas)
    dLambdas <- encode_lambdas(nm, params_nm)
    d <- c(d, dLambdas)
    # Encode decay rate for radioactive tracers
    lambda_decay <- attr(nm, "lambda_hl")
    if (is.null(lambda_decay)) {
        lambda_decay <- 0
    }
    d[["lambda_decay"]] <- lambda_decay
    # Encode distribution families (for proportions and sizes)
    d <- c(d, encode_distrib_families(nm))
    # Add encoding for matrix exponential (so that the Stan model does not crash)
    d[["allParams"]] <- params_nm
    matrix_exp <- encode_intervals(nm)
    matrix_exp <- c(matrix_exp, encode_unique_obs_times(nm))
    stopifnot(!any(names(matrix_exp) %in% names(d)))
    d <- c(d, matrix_exp)
    return(d)
}

### * encode_events()

#' Encode events (for Stan model using Euler integration)
#'
#' For now, only pulse events are encoded.
#'
#' @param nm A \code{networkModel} object.
#' @param dt,grid_size Either the time step size for trajectory calculations
#'     (\code{dt}) or the number of points for the calculation
#'     (\code{grid_size}) can be provided. If none is provided, then a default
#'     grid size of 256 steps is used.
#' @param end Time value for end point. If not provided, the last observation
#'     or event is used.
#'
#' @examples
#' encode_events <- isotracer:::encode_events
#' encode_events(aquarium_mod)
#' encode_events(trini_mod)
#' 
#' @keywords internal
#' @noRd

encode_events <- function(nm, dt = NULL, grid_size = NULL, end = NULL) {
    d <- nm_get_time_schemes(nm, dt = dt, grid_size = grid_size, end = end)
    nGroups <- nrow(nm)
    o <- list()
    events <- nm[["events"]]
    stopifnot(all(dplyr::bind_rows(events)[["event"]] == "pulse"))
    # Encode compartments and timepoints
    for (i in seq_len(nrow(nm))) {
        comps <- colnames(nm$topology[[i]])
        comps <- setNames(seq_along(comps), nm = comps)
        if (!is.null(events[[i]])) {
            events[[i]]$compartment <- comps[events[[i]]$compartment]
            events[[i]]$timepoints <- match(events[[i]]$time, d$timepoints[[i]])
        }
    }
    # Encode the number of events
    if (length(events) == 0) {
        nEvents <- rep(0, nrow(nm))
    } else {
        nEvents <- sapply(events, function(x) {
            if (is.null(x)) return(0)
            return(nrow(x))
        })
    }
    o[["maxNpulseEvents"]] <- max(nEvents)
    o[["nPulseEvents"]] <- setNames(c(nEvents, 0), # Padded
                                    nm = c(paste0("grp", seq_len(nrow(nm))), "padding"))
    # Encode pulses
    o[["pulseEventsIndices"]] <- array(0, dim = c(max(nEvents), 2, nGroups),
                                       dimnames = list(seq_len(max(nEvents)),
                                                       c("timepoint", "comp"),
                                                       paste0("grp", seq_len(nGroups))))
    o[["pulseEventsQuantities"]] <- array(0, dim = c(max(nEvents), 2, nGroups),
                                          dimnames = list(seq_len(max(nEvents)),
                                                          c("unmarked", "marked"),
                                                          paste0("grp", seq_len(nGroups))))
    for (i in seq_len(nGroups)) {
        if (!is.null(events[[i]])) {
            o[["pulseEventsIndices"]][1:nEvents[i], 1:2, i] <-
                as.matrix(events[[i]][, c("timepoints", "compartment")])
            chars <- lapply(events[[i]]$characteristics, tibble::as_tibble)
            chars <- dplyr::bind_rows(chars)[, c("unmarked", "marked")]
            o[["pulseEventsQuantities"]][1:nEvents[i], 1:2, i] <-
                as.matrix(chars)
        }
    }
    # Return
    return(o)
}

### ** nm_get_time_schemes()

#' Build the time schemes for numerical solving of the system of differential equations
#'
#' This function processes each row of a networkModel. It always assumes that
#' the starting time point is t=0.
#'
#' @param nm A networkModel
#' @param dt,grid_size Either the time step size for trajectory calculations
#'     (\code{dt}) or the number of points for the calculation
#'     (\code{grid_size}) can be provided. If none is provided, then a default
#'     grid size of 256 steps is used.
#' @param end Time value for end point. If not provided, the last observation
#'     or event is used.
#' @param at Optional, vector of time values at which the trajectory must be
#'     evaluated
#'
#' @keywords internal
#' @noRd

nm_get_time_schemes <- function(nm, dt = NULL, grid_size = NULL, end = NULL,
                                at = NULL) {
    # Get the time schemes for each row of nm
    ts <- lapply(seq_len(nrow(nm)), function(i) {
        z <- nm_row_get_time_scheme(nm[i, ], dt = dt, grid_size = grid_size,
                                    end = end, at = at)
        z <- tibble::as_tibble(lapply(z, list))
    })
    ts <- dplyr::bind_rows(ts)
    return(ts)    
}

### ** nm_row_get_time_scheme()

#' Build the time scheme for numerical solving of the system of differential equations
#'
#' This function is applied to one row of a networkModel. It always assumes
#' that the starting time point is t=0.
#'
#' @param nm_row A row from a \code{networkModel} object.
#' @param dt,grid_size Either the time step size for trajectory calculations
#'     (\code{dt}) or the number of points for the calculation
#'     (\code{grid_size}) can be provided. If none is provided, then a default
#'     grid size of 256 steps is used.
#' @param end Time value for end point. If not provided, the last observation
#'     or event is used.
#' @param at Optional, vector of time values at which the trajectory must be
#'     evaluated
#'
#' @examples
#' isotracer:::nm_row_get_time_scheme(aquarium_mod)
#' 
#' @keywords internal
#' @noRd

nm_row_get_time_scheme <- function(nm_row, dt = NULL, grid_size = NULL, end = NULL,
                                   at = NULL) {
    nmRow <- nm_row
    stopifnot(nrow(nmRow) == 1)
    # Parse dt and gridsize
    if (!(is.null(dt) | is.null(grid_size))) {
        stop("Only \"dt\" or \"grid_size\" can be specified, not both.")
    }
    if (is.null(dt) & is.null(grid_size)) {
        grid_size <- 256
    }
    # Get observation times
    observations <- nmRow[["observations"]][[1]]
    obsTimes <- observations[["time"]]
    obsTimes <- sort(unique(obsTimes))
    # Get events time
    eventTimes <- c()
    if (!is.null(nmRow[["events"]][[1]])) {
        eventTimes <- unique(nmRow[["events"]][[1]]$time)
    }
    # Get "at" times
    atTimes <- c()
    if (!is.null(at)) {
        atTimes <- unique(at)
    }
    # Get end time
    if (is.null(end)) {
        maxTime <- max(c(obsTimes, eventTimes, atTimes))
    } else {
        maxTime <- end
    }
    # Calculate dt
    if (is.null(dt)) {
        dt <- maxTime / grid_size
    }
    # Generate a time scheme
    timepoints <- c(seq(0, maxTime, by = dt), obsTimes, eventTimes, atTimes)
    timepoints <- sort(unique(timepoints))
    timepoints <- timepoints[timepoints <= maxTime]
    # (if end = NULL, timepoints is a timeline with timesteps at most dt wide
    # and containing all the observations sampling times and the event times,
    # even if not a multiple of dt.)
    # (else, timepoints is truncated at end.)
    # Annotate the observations tibble with the timepoints ids
    observations[["timepoint"]] <- match(observations[["time"]], timepoints)
    if (is.null(end)) {
        stopifnot(!any(is.na(observations[["timepoint"]])))
    }
    # Get unique dts
    dts <- timepoints_to_dt(timepoints)
    # Return
    return(list(timepoints = timepoints,
                unique_dt = dts[["unique_dt"]],
                dt_i = dts[["dt_i"]],
                observations = observations))
}

### ** timepoints_to_dt()

#' Prepare the set of unique dt from a vector of timepoints
#'
#' Useful to identify how many different transfer matrices have to be calculated.
#'
#' @param timepoints Numeric vector of timepoints, sorted.
#'
#' @keywords internal
#' @noRd

timepoints_to_dt <- function(timepoints) {
    dts <- diff(timepoints)
    uniqueDts <- sort(unique(dts))
    dt_i <- match(dts, uniqueDts)
    stopifnot(!any(is.na(dt_i)))
    return(list(unique_dt = uniqueDts,
                dt_i = dt_i))
}

### * encode_obs()

#' Encode observations for a network model
#'
#' @param nm A \code{networkModel} object.
#' @param allParams Parameters of the network model.
#' @param dt,grid_size Either the time step size for trajectory calculations
#'     (\code{dt}) or the number of points for the calculation
#'     (\code{grid_size}) can be provided. If none is provided, then a default
#'     grid size of 256 steps is used.
#' @param end Time value for end point. If not provided, the last observation
#'     or event is used.
#' 
#' @return NULL if the observations column only contain NULLs.
#'
#' @importFrom stats na.omit
#' 
#' @keywords internal
#' @noRd

encode_obs <- function(nm, allParams, dt = NULL, grid_size = NULL, end = NULL) {
    d <- nm_get_time_schemes(nm, dt = dt, grid_size = grid_size, end = end)
    nGroups <- nrow(nm)
    zeta_by_comp <- attr(nm, "size_zeta_per_compartment")
    if (is.null(zeta_by_comp)) {
        zeta_by_comp<- FALSE
    }
    # TODO Add filtering to keep only compartments present in topo
    # TODO Handle gracefully the case without observations
    if (all(sapply(nm$observations, is.null))) {
        return(NULL)
    }
    # Get sizes
    sizes <- purrr::map(d$observations, function(x) {
        na.omit(x[, c("compartment", "size", "timepoint")])
    })
    # Get proportions
    props <- purrr::map(d$observations, function(x) {
        na.omit(x[, c("compartment", "proportion", "timepoint")])
    })
    # Encode compartments
    for (i in seq_len(nrow(nm))) {
        comps <- colnames(nm$topology[[i]])
        comps <- setNames(seq_along(comps), nm = comps)
        sizes[[i]]$compartment <- comps[sizes[[i]]$compartment]
        props[[i]]$compartment <- comps[props[[i]]$compartment]
    }
    # Encode sizes and props
    o <- list()
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

### * encode_time_schemes()

#' Encode the time schemes for stan data
#'
#' @param nm A \code{networkModel} object.
#' @param dt,grid_size Either the time step size for trajectory calculations
#'     (\code{dt}) or the number of points for the calculation
#'     (\code{grid_size}) can be provided. If none is provided, then a default
#'     grid size of 256 steps is used.
#' @param end Time value for end point. If not provided, the last observation
#'     or event is used.
#'
#' @keywords internal
#' @noRd

encode_time_schemes <- function(nm, dt = NULL, grid_size = NULL, end = NULL) {
    # (timestep and dt are used interchangeably)
    # Get the time schemes
    ts <- nm_get_time_schemes(nm, dt = dt, grid_size = grid_size, end = end)
    # Build the arrays
    nGroups <- nrow(nm)
    n_unique_dts <- purrr::map_dbl(ts$unique_dt, length)
    maxN_unique_dts <- max(n_unique_dts)
    n_timesteps <- purrr::map_dbl(ts$dt_i, length)
    maxN_timesteps <- max(n_timesteps)
    unique_dts <- array(0, dim = c(maxN_unique_dts, nGroups),
                        dimnames = list(c(1:maxN_unique_dts),
                                        paste0("grp", 1:nGroups)))
    timesteps <- array(0, c(maxN_timesteps, nGroups),
                       dimnames = list(c(1:maxN_timesteps),
                                        paste0("grp", 1:nGroups)))
    # Fill the arrays
    for (i in seq_len(nrow(nm))) {
        unique_dts[1:n_unique_dts[i], i] <- ts[["unique_dt"]][[i]]
        timesteps[1:n_timesteps[i],  i] <- ts[["dt_i"]][[i]]
    }
    # Prepare data
    d <- list()
    d[["nTimesteps"]] <- setNames(c(n_timesteps, 0), # Padded
                                  nm = c(paste0("grp", 1:nGroups), "padding"))
    d[["nUniqueDts"]] <- setNames(c(n_unique_dts, 0), # Padded
                                  nm = c(paste0("grp", 1:nGroups), "padding"))
    d[["timesteps"]] <- timesteps
    d[["unique_dts"]] <- unique_dts
    d[["maxNtimesteps"]] <- max(d[["nTimesteps"]])
    d[["maxNuniqueDts"]] <- max(d[["nUniqueDts"]])
    return(d)
}
