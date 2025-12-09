### * All functions in this file are exported

### * project()

#' Calculate the trajectories of a network model
#'
#' @param nm A \code{networkModel} object.
#' @param dt,grid_size Either the time step size for trajectory calculations
#'   (\code{dt}) or the number of points for the calculation (\code{grid_size})
#'   can be provided. If none is provided, then a default grid size of 256 steps
#'   is used.
#' @param at Optional, vector of time values at which the trajectory must be
#'   evaluated.
#' @param end Time value for end point. If not provided, the last observation or
#'   event is used.
#' @param flows Return flow values? The default is "no" and no flows are
#'   calculated. Other values are "total" (total flows summed up from beginning
#'   to end timepoint), "average" (average flows per time unit, equal to total
#'   flows divided by the projection duration), and "per_dt" (detailled flow
#'   values are returned for each interval dt of the projection).
#' @param cached_ts,cached_ee Used for optimization by other functions, not for
#'   use by the package user.
#' @param ignore_pulses Default to FALSE (i.e. apply pulses when projecting the
#'   network system). It is set to TRUE when calculating steady-state flows.
#'
#' @return A network model object with a \code{"trajectory"} column.
#' 
#' @examples
#' m <- aquarium_mod
#' m <- set_params(m, sample_params(m))
#' z <- project(m)
#' z <- project(m, flows = "per_dt")
#' z <- project(m, flows = "total")
#' z <- project(m, flows = "average")
#' 
#' @export

project <- function(nm, dt = NULL, grid_size = NULL, at = NULL, end = NULL,
                    flows = "no", cached_ts = NULL, cached_ee = NULL,
                    ignore_pulses = FALSE) {
    `!!` <- rlang::`!!`
    if (!flows %in% c("no", "total", "average", "per_dt")) {
        stop("\"flows\" must be on of \"no\", \"total\", \"average\", \"per_dt\".")
    }
    if (flows == "no") {
        get_flows <- FALSE
    } else {
        get_flows <- TRUE
    }
    flow_option <- flows
    # Check that all parameters have values assigned
    params <- dplyr::bind_rows(nm$parameters)
    stopifnot(!any(is.na(params$value)))
    # Project row by row
    trajectories <- list()
    flows <- list()
    for (i in seq_len(nrow(nm))) {
        z  <- project_row(nm[i, ], dt = dt, grid_size = grid_size,
                          at = at, end = end, flows = get_flows,
                          cached_ts = cached_ts[[i]], cached_ee = cached_ee[[i]],
                          lambda_decay = attr(nm, "lambda_hl"),
                          ignore_pulses = ignore_pulses)
        trajectories[[i]] <- z[, c("timepoints", "unmarked", "marked", "sizes", "proportions")]
        if (get_flows) {
            flows[[i]] <- z[, c("timepoints", "dt", "flows")]
        }
    }
    nm$trajectory <- trajectories
    if (get_flows) {
        if (flow_option == "per_dt") {
            nm$flows <- flows
        } else if (flow_option == "total") {
            for (i in seq_along(flows)) {
                z <- flows[[i]][["flows"]][[1]]
                if (is.na(z[[length(z)]] )) {
                    z <- z[1:(length(z)-1)]
                }
                z <- dplyr::bind_rows(z)
                z <- dplyr::group_by(z, `!!`(rlang::sym("from")),
                                     `!!`(rlang::sym("to")))
                z <- dplyr::summarize(z, total_flow = sum(`!!`(rlang::sym("flow"))))
                flows[[i]] <- z
            }
            nm$flows <- flows
        } else if (flow_option == "average") {
            for (i in seq_along(flows)) {
                z <- flows[[i]][["flows"]][[1]]
                if (is.na(z[[length(z)]] )) {
                    z <- z[1:(length(z)-1)]
                }
                z <- dplyr::bind_rows(z)
                z <- dplyr::group_by(z, `!!`(rlang::sym("from")),
                                     `!!`(rlang::sym("to")))
                z <- dplyr::summarize(z, total_flow = sum(`!!`(rlang::sym("flow"))))
                duration <- diff(range(flows[[i]][["timepoints"]][[1]]))
                z$average_flow <- z$total_flow / duration
                z$total_flow <- NULL
                flows[[i]] <- z
            }
            nm$flows <- flows
        } else {
            stop("Value of \"flows\" not allowed: ", flow_option)
        }
    }
    return(nm)
}

### * sample_from()

#' Generate samples from a network model
#'
#' @param nm A \code{networkModel} object.
#' @param at Vector of time values at which the samples should be taken.
#' @param dt,grid_size Time step size or grid points, respectively.
#' @param end Final timepoint used in the projections.
#' @param error.draws Integer, number of draws from the error distribution for
#'     each sample (default: 1).
#' @param cached_ts,cached_ee Used for optimization by other functions, not for
#'     use by the package user.
#'
#' @return A tibble containing the generated samples.
#' 
#' @importFrom stats rnorm
#' @importFrom stats rgamma
#' @importFrom stats rbeta
#'
#' @examples
#' library(magrittr)
#' mod <- new_networkModel() %>%
#'    set_topo("NH4 -> algae -> daphnia -> NH4")
#' inits <- tibble::tribble(
#'      ~comps, ~sizes, ~props, ~treatment,
#'       "NH4",    0.2,    0.8,    "light",
#'     "algae",      1,  0.004,    "light",
#'   "daphnia",      2,  0.004,    "light",
#'       "NH4",    0.5,    0.8,     "dark",
#'     "algae",    1.2,  0.004,     "dark",
#'   "daphnia",    1.3,  0.004,     "dark")
#' mod <- set_init(mod, inits, comp = "comps", size = "sizes",
#'                 prop = "props", group_by = "treatment")
#' mod <- add_covariates(mod, upsilon_NH4_to_algae ~ treatment)
#' mod <- mod %>%
#'   set_params(c("eta" = 0.2, "lambda_algae" = 0, "lambda_daphnia" = 0,
#'                "lambda_NH4" = 0, "upsilon_NH4_to_algae|light" = 0.3,
#'                "upsilon_NH4_to_algae|dark" = 0.1,
#'                "upsilon_algae_to_daphnia" = 0.13,
#'                "upsilon_daphnia_to_NH4" = 0.045, "zeta" = 0.1))
#' spl <- mod %>% sample_from(at = 1:10)
#' spl
#'
#' @export

sample_from <- function(nm, at, dt = NULL, grid_size = NULL, end = NULL, error.draws = 1,
                        cached_ts = NULL, cached_ee = NULL) {
    obs <- list()
    prop_family <- attr(nm, "prop_family")
    size_family <- attr(nm, "size_family")
    zeta_by_comp <- attr(nm, "size_zeta_per_compartment")
    if (is.null(zeta_by_comp)) {
        zeta_by_comp <- FALSE
    }
    # Project nm
    nm <- project(nm, at = at, dt = dt, grid_size = grid_size, end = end,
                  cached_ts = cached_ts, cached_ee = cached_ee)
    # Extract samples
    for (i in seq_len(nrow(nm))) {
        z <- nm$trajectory[[i]]
        params <- nm$parameters[[i]]
        indices <- sort(unique(match(at, z$timepoints[[1]])))
        sizes <- tibble::as_tibble(z$unmarked[[1]] + z$marked[[1]])[indices, ]
        props <- tibble::as_tibble(z$proportions[[1]])[indices, ]
        stopifnot(!any(c("time", "comp", "size", "prop") %in% names(sizes)))
        sizes$time <- z$timepoints[[1]][indices]
        props$time <- z$timepoints[[1]][indices]
        sizes <- data.table::melt(data.table::as.data.table(sizes),
                                  id.vars = "time",
                                  variable.name = "comp", value.name = "meanSize",
                                  variable.factor = FALSE)
        props <- data.table::melt(data.table::as.data.table(props),
                                  id.vars = "time",
                                  variable.name = "comp", value.name = "meanProp",
                                  variable.factor = FALSE)
        sizes <- dplyr::bind_rows(rep(list(tibble::as_tibble(sizes)), error.draws))
        props <- dplyr::bind_rows(rep(list(tibble::as_tibble(props)), error.draws))
        # Add zeta values to sizes tibble
        if (!zeta_by_comp) {
            sizes$zeta <- params$value[params$in_replicate == "zeta"]
        } else {
            zeta_params <- paste0("zeta_", sizes[["comp"]])
            sizes$zeta <- params$value[match(zeta_params, params$in_replicate)]
        }
        # Apply size "noise"
        if (size_family == "normal_cv") {
            sizes$size <- rnorm(nrow(sizes),
                                mean = sizes$meanSize,
                                sd = sizes$meanSize * sizes$zeta)
            negSizes <- which(sizes$size < 0)
            while (length(negSizes) > 0) {
                sizes$size[negSizes] <- rnorm(length(negSizes),
                                              sizes$meanSize[negSizes],
                                              sd = sizes$meanSize[negSizes] * sizes$zeta[negSizes])
                negSizes <- which(sizes$size < 0)
            }
        } else if (size_family == "normal_sd") {
            sizes$size <- rnorm(nrow(sizes),
                                mean = sizes$meanSize,
                                sd = sizes$zeta)
            negSizes <- which(sizes$size < 0)
            while (length(negSizes) > 0) {
                sizes$size[negSizes] <- rnorm(length(negSizes),
                                              sizes$meanSize[negSizes],
                                              sd = sizes$zeta)
                negSizes <- which(sizes$size < 0)
            }
        }
        sizes$zeta <- NULL
        # Apply prop "noise"
        if (prop_family == "gamma_cv") {
            shapes <- rep(params$value[params$in_replicate == "eta"], nrow(props))^(-2)
            rates <- shapes  / props$meanProp
            props$prop <- rgamma(nrow(props), shape = shapes, rate = rates)
        } else if (prop_family == "normal_cv") {
            means <- props$meanProp
            sds <- means * params$value[params$in_replicate == "eta"]
            props$prop <- rnorm(nrow(props), mean = means, sd = sds)
            negProps <- which(props$prop < 0)
            while (length(negProps) > 0) {
                props$prop[negProps] <- rnorm(length(negProps),
                                              means[negProps],
                                              sd = sds[negProps])
                negProps <- which(props$prop < 0)
            }
        } else if (prop_family == "normal_sd") {
            means <- props$meanProp
            sds <- rep(params$value[params$in_replicate == "eta"], nrow(props))
            props$prop <- rnorm(nrow(props), mean = means, sd = sds)
            negProps <- which(props$prop < 0)
            while (length(negProps) > 0) {
                props$prop[negProps] <- rnorm(length(negProps),
                                              means[negProps],
                                              sd = sds[negProps])
                negProps <- which(props$prop < 0)
            }
        } else if (prop_family == "beta_phi") {
            phis <- rep(params$value[params$in_replicate == "eta"], nrow(props))
            alphas <- props$meanProp * phis
            betas <- phis * (1 - props$meanProp)
            props$prop <- rbeta(nrow(props), shape1 = alphas, shape2 = betas)
        } else {
            stop("Unknown distribution family:", prop_family)
        }
        stopifnot(all(sizes[["time"]] == props[["time"]]))
        stopifnot(all(sizes[["comp"]] == props[["comp"]]))
        obs[[i]] <- dplyr::bind_cols(sizes[, c("time", "comp", "size")],
                                     props[, c("prop")])
    }
    if (is.null(groups(nm))) {
        stopifnot(nrow(nm) == 1)
        return(obs[[1]])
    }
    groups <- groups(nm)
    out <- dplyr::bind_cols(tibble::tibble(obs), groups)
    out <- tidyr::unnest(out, cols = "obs")
    return(out)
}

### * calculate_steady_state()

#' Calculate steady-state compartment sizes for a network
#'
#' This is an experimental function. It attempts to calculate steady-state
#' compartment sizes using the set parameter values and the initial compartment
#' sizes. Use it with caution!
#' 
#' Note about how steady state sizes for split compartments are calculated: the
#' steady size of the active portion is calculated divide it is divided by the
#' active fraction (portion.act parameter) to get the total size including the
#' refractory portion. In this case we get a "steady-state" refractory portion,
#' consistent with steady state size of active fraction and with portion.act
#' parameter.
#' 
#' @param nm A network model, with set parameter values.
#'
#' @return A tibble containing steady-state compartment sizes.
#'
#' @examples
#' m <- aquarium_mod
#' m <- set_prior(m, constant_p(0), "lambda")
#' m <- set_params(m, sample_params(m))
#' proj <- project(m, end = 40)
#' plot(proj)
#'
#' z <- calculate_steady_state(m)
#' z
#' z$stable_sizes
#'
#' @export

calculate_steady_state <- function(nm) {
    out <- lapply(seq_len(nrow(nm)), function(i) {
        tibble::tibble(stable_sizes = list(calculate_steady_state_one_row(nm[i, ])))
    })
    out <- dplyr::bind_rows(out)
    nm[["stable_sizes"]] <- out[["stable_sizes"]]
    return(nm)
}
