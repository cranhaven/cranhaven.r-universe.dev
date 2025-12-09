### * None of the functions in this file is exported

### * project_row()

### ** Doc

#' Calculate the trajectories for one row
#'
#' This is the workhorse function doing all the hard-work of projecting
#' trajectories through numerical integration. All other functions involving
#' any trajectory projection (except the Stan model itself) should rely on this
#' function.
#' 
#' @param nm_row One row of a \code{networkModel} object.
#' @param dt,grid_size Either the time step size for trajectory calculations
#'     (\code{dt}) or the number of points for the calculation
#'     (\code{grid_size}) can be provided. If none is provided, then a default
#'     grid size of 256 steps is used.
#' @param at Optional, vector of time values at which the trajectory must be
#'     evaluated.
#' @param end Time value for end point. If not provided, the last observation
#'     or event is used.
#' @param flows Boolean, return flows in the output?
#' @param cached_ts,cached_ee Used for optimization by other functions, not for
#'     use by the package user.
#' @param lambda_decay If non-NULL, used as the decay rate for marked tracer
#'     (used to model radioactive tracers).
#'
#' @return A tibble with one row. If flows are calculated, the returned flow
#'     values are flows during each dt intervals, not instantaneous flows. Note
#'     that in the case of a radioactive tracer, the returned flows do not
#'     incorporate the "loss flow" due to radioactive decay (so if one is to do
#'     some material accounting based on the flow values, some material will
#'     "leak" and appear not to be accounted for and actually corresponds to
#'     the tracer lost by radioactive decay).
#' 
#' @examples
#' project_row <- isotracer:::project_row
#' 
#' m <- aquarium_mod
#' m <- set_params(m, sample_params(m))
#' z <- project_row(m[1,])
#' z <- project_row(m[1,], flows = TRUE)
#'
#' # Calculate total flows over the projected time window
#' f <- z$flows[[1]]
#' f <- f[1:(length(f)-1)] # Drop last entry (which is NA)
#' f <- dplyr::bind_rows(f)
#' f <- dplyr::group_by(f, from, to)
#' f <- dplyr::summarize(f, total_flow = sum(flow))
#' duration <- diff(range(z$timepoints[[1]]))
#' f$mean_instantaneous_flow <- f$total_flow / duration
#' 
#' @keywords internal
#' @noRd

### ** Code

project_row <- function(nm_row, dt = NULL, grid_size = NULL, at = NULL, end = NULL,
                        flows = FALSE, cached_ts = NULL, cached_ee = NULL,
                        lambda_decay = NULL, ignore_pulses = FALSE) {
    ### * Preprocessing
    if (is.null(lambda_decay)) {
        lambda_decay <- 0
    }
    get_flows <- flows
    nmRow <- nm_row
    nComps <- ncol(nmRow$topology[[1]])
    comps <- colnames(nmRow$topology[[1]])
    params <- nmRow$parameters[[1]]
    params <- setNames(params$value, nm = params$in_replicate)
    ss <- attr(nmRow$topology[[1]], "steadyState")
    ss <- match(ss, comps)
    sp <- attr(nmRow$topology[[1]], "split")
    sp <- match(sp, comps)
    if (is.null(end)) {
        if (is.null(at)) {
            end <- max(nmRow$observations[[1]][["time"]])
            if (!is.null(nmRow[["events"]][[1]])) {
                end <- max(c(end, nmRow$events[[1]][["time"]]))
            }
        } else {
            end <- max(at)
        }
    }
    if (get_flows) {
        flows <- list()
        flow_template <- data.frame(from = rep(comps, each = nComps),
                                    to = rep(comps, nComps),
                                    flow = NA,
                                    factor = 1,
                                    in_topo = as.vector(nmRow$topology[[1]]),
                                    stringsAsFactors = FALSE)
        flow_template[["in_topo"]][flow_template[["from"]] == flow_template[["to"]]] <- 1
        # factor to adjust sign of comps_to_NA flows
        flow_template[["factor"]][flow_template[["from"]] == flow_template[["to"]]] <- -1
        flow_template[["to"]][flow_template[["from"]] == flow_template[["to"]]] <- NA
        flow_template_keep <- flow_template$in_topo > 0
        flow_template$in_topo <- NULL
        flow_template_to_fill <- flow_template[flow_template_keep, ]
        flow_template_to_fill[["factor"]] <- NULL
        flow_template_adjust <- flow_template[["factor"]][flow_template_keep]
    }
    ### * Get time scheme
    if (is.null(cached_ts)) {
        ts <- nm_row_get_time_scheme(nm_row = nmRow, dt = dt, grid_size = grid_size,
                                     end = end, at = at)
    }  else {
        ts <- cached_ts
    }
    timepoints <- ts$timepoints
    timesteps <- ts$dt_i
    dts <- ts$unique_dt
    ### * Get events
    if (is.null(cached_ee)) {
        events <- encode_events(nmRow, end = end, dt = dt, grid_size = grid_size)
    } else {
        events <- cached_ee
    }
    nPulses <- events[["maxNpulseEvents"]]
    pulseEvents <- rbind(events[["pulseEventsIndices"]][,,1],
                         c(0, 0)) # Padding to keep matrix when one event only
    pulseQuantities <- rbind(events[["pulseEventsQuantities"]][,,1],
                             c(0, 0))
    ### * Build transition matrices
    transitions <- list()
    transitionsDecay <- list() # This one incorporates the effect of lambda_decay
    for (i in seq_along(dts)) {
        transitions[[i]] <- build_transition_matrix(nmRow$topology[[1]],
                                                    nmRow$parameters[[1]],
                                                    dts[i])
        transitionsDecay[[i]] <- (transitions[[i]] -
                                  diag(nComps) * lambda_decay * dts[i])
    }
    if (get_flows) {
        transfer_mat <- list()
        for (i in seq_along(dts)) {
            transfer_mat[[i]] <- build_transfer_matrix(nmRow$topology[[1]],
                                                       nmRow$parameters[[1]],
                                                       dts[i])
        }
    }
    ### * Initialize event index
    pulseIndex <- 1
    ### * Initialize unmarked and marked quantities
    unmarked <- matrix(NA, ncol = nComps, nrow = length(timepoints))
    marked <- matrix(NA, ncol = nComps, nrow = length(timepoints))
    init <- nmRow$initial[[1]]
    init <- init[match(comps, init$compartment), ]
    stopifnot(all(comps == init$compartment))
    ## Update split compartments
    if (length(sp) > 0) {
        initRefr <- list()
        for (j in sp) {
            portion <- as.vector(params[paste0("portion.act_", comps[j])])
            initRefr[[j]] <- c("unmarked" = init[["size"]][j] * (1 - portion) * (1 - init[["proportion"]][j]),
                               "marked" = init[["size"]][j] * (1 - portion) * init[["proportion"]][j])
            init[["size"]][j] <- init[["size"]][j] * portion
        }
    }
    init$unmarked <- init$size * (1 - init$proportion)
    init$marked <- init$size * init$proportion
    unmarked[1, ] <- init$unmarked[match(comps, init$compartment)]
    marked[1, ] <- init$marked[match(comps, init$compartment)]
    ## Apply pulses
    if (nPulses > 0 & (!ignore_pulses)) {
        if (pulseIndex <= nPulses) {
            while(pulseIndex <= nPulses && pulseEvents[pulseIndex, 1] == 1) {
                unmarked[1, pulseEvents[pulseIndex, 2]] <- unmarked[1, pulseEvents[pulseIndex, 2]] + pulseQuantities[pulseIndex, 1]
                marked[1, pulseEvents[pulseIndex, 2]] <- marked[1, pulseEvents[pulseIndex, 2]] + pulseQuantities[pulseIndex, 2]
                pulseIndex <- pulseIndex + 1
            }
        }
    }
    ### * Loop over dt
    for (t in seq_along(timesteps)) {
        # Apply the transfer matrix
        unmarked[t+1, ] <- transitions[[timesteps[t]]] %*% unmarked[t, ]
        marked[t+1, ] <- transitionsDecay[[timesteps[t]]] %*% marked[t, ]
        # Gather flows
        if (get_flows) {
            flows_t <- transfer_mat[[timesteps[t]]]
            sizes_t <- unmarked[t, ] + marked[t, ]
            for (j in seq_along(comps)) {
                flows_t[, j] <- flows_t[, j] * sizes_t[j]
            }
            flows_to_store <- as.vector(flows_t)[flow_template_keep]
            flows[[t]] <- flow_template_to_fill
            flows[[t]]$flow <- flows_to_store * flow_template_adjust # Adjust sign of comp_to_NA flows
        }
        # Reset steady-state compartments
        for (j in ss) {
            unmarked[t+1,j] <- unmarked[t,j]
            marked[t+1,j] <- marked[t,j]
        }
        # Apply pulse events
        if (nPulses > 0 & (!ignore_pulses)) {
            if (pulseIndex <= nPulses) {
                while(pulseIndex <= nPulses && pulseEvents[pulseIndex, 1] == t+1) {
                    unmarked[t+1, pulseEvents[pulseIndex, 2]] <- unmarked[t+1, pulseEvents[pulseIndex, 2]] + pulseQuantities[pulseIndex, 1]
                    marked[t+1, pulseEvents[pulseIndex, 2]] <- marked[t+1, pulseEvents[pulseIndex, 2]] + pulseQuantities[pulseIndex, 2]
                    pulseIndex <- pulseIndex + 1
                }
            }
        }
    } # End of loop

    ### * Clean-up and return
    colnames(unmarked) <- comps
    colnames(marked) <- comps
    if (length(sp) > 0) {
        for (j in sp) {
            unmarked[, j] <- unmarked[, j] + initRefr[[j]]["unmarked"]
            marked[, j] <- marked[, j] + initRefr[[j]]["marked"]
        }
    }
    sizes <- unmarked + marked
    proportions <- marked / sizes
    colnames(proportions) <- comps
    o <- tibble::tibble(timepoints = list(timepoints),
                        unmarked = list(unmarked),
                        marked = list(marked),
                        sizes = list(sizes),
                        proportions = list(proportions))
    if (get_flows) {
        o$dt <- list(c(diff(o[["timepoints"]][[1]]), NA))
        o$flows <- list(c(flows, NA))
    }
    return(o)
}

### * build_transition_matrix()

#' Build the transition matrix for a topology
#'
#' @param topo A network topology.
#' @param params A tibble with "in_replicate" and "value" columns giving the
#'     parameter values.
#' @param dt Numerical, time step value.
#' @param apply_steady_state Boolean, if TRUE set values for steady state
#'     compartments to the appropriate 1.
#'
#' @keywords internal
#' @noRd

build_transition_matrix <- function(topo, params, dt,
                                    apply_steady_state = FALSE) {
    nComps <- ncol(topo)
    comps <- colnames(topo)
    params <- setNames(params$value, nm = params$in_replicate)
    m <- matrix(0, ncol = nComps, nrow= nComps)
    for (i in seq_len(nComps)) {
        # upsilons
        for (j in seq_len(nComps)) {
            if (topo[i,j] == 1) {
                p <- paste("upsilon", comps[j], "to", comps[i], sep = "_")
                m[i,j] <- params[p]
                m[j,j] <- m[j,j] - params[p]
            }
        }
        # lambdas
        p <- paste("lambda", comps[i], sep = "_")
        m[i,i] <- m[i,i] - params[p]
    }
    # Apply dt
    m <- diag(nComps) + m * dt
    colnames(m) <- comps
    rownames(m) <- comps
    # Apply steady states
    if (apply_steady_state) {
        ss_comps <- attr(topo, "steadyState")
        if (length(ss_comps) > 0) {
            for (comp in ss_comps) {
                i <- which(comps == comp)
                m[i, ] <- 0
                m[i, i] <- 1
            }
        }
    }
    return(m)
}

### * build_transfer_matrix()

#' Build the transfer matrix for a topology
#'
#' A transfer matrix is similar to a transition matrix, except that the
#' diagonal doesn't contain the numerical values corresponding to what remains
#' after transfer. The transfer matrix describes the flows rather than the
#' compartment states after transition.
#'
#' Note that this function does not take into account radioactive decay when
#' calculating how much material is lost (lambda rates) from a
#' compartment. This is equivalent to consider that the flows are calculated
#' for a situation where all isotopes are stable.
#'
#' @param topo A network topology.
#' @param params A tibble with "in_replicate" and "value" columns giving the
#'     parameter values.
#' @param dt Numerical, time step value.
#'
#' @keywords internal
#' @noRd

build_transfer_matrix <- function(topo, params, dt) {
    nComps <- ncol(topo)
    comps <- colnames(topo)
    params <- setNames(params$value, nm = params$in_replicate)
    m <- matrix(0, ncol = nComps, nrow= nComps)
    for (i in seq_len(nComps)) {
        # upsilons
        for (j in seq_len(nComps)) {
            if (topo[i,j] == 1) {
                p <- paste("upsilon", comps[j], "to", comps[i], sep = "_")
                m[i,j] <- params[p]
            }
        }
        # lambdas
        p <- paste("lambda", comps[i], sep = "_")
        if (m[i,i] != 0) {
            stop("A compartment has a flow to itself (which is not permitted in the model).")
        }
        m[i,i] <- m[i,i] - params[p]
    }
    # Apply dt
    m <- m * dt
    colnames(m) <- comps
    rownames(m) <- comps
    return(m)
}

### * calc_instantaneous_flow()

#' Calculate instantaneous flow given a topology, sizes and parameter values
#'
#' Note that this function applies portions of active compartments
#' instantaneously, i.e. it assumes that for a split compartment the size and
#' active portion values are correct. This means that this function should not
#' be used to calculate flows in a network which contains split compartments
#' and which is not at equilibrium over than at t0, since in such a network the
#' estimated parameters give the active portion values at t0 (i.e. for the
#' initial conditions).
#' 
#' @param topo Network topology.
#' @param sizes Compartement sizes (tibble).
#' @param parameters Parameter values (tibble). A named vector containing
#'     parameter values is also accepted.
#'
#' @return A tibble with instantaneous flows.
#'
#' @examples
#' calc_instantaneous_flow <- isotracer:::calc_instantaneous_flow
#' 
#' # Small network
#' m <- aquarium_mod
#' p <- sample_params(m)
#' m <- set_params(m, p)
#' flows <- calc_instantaneous_flow(m$topology[[1]], m$initial[[1]],
#'                                  m$parameters[[1]])
#' 
#' # Large network
#' m <- trini_mod
#' p <- sample_params(m)
#' m <- set_params(m, p)
#' flows <- calc_instantaneous_flow(m$topology[[1]], m$initial[[1]],
#'                                  m$parameters[[1]])
#' 
#' @keywords internal
#' @noRd

calc_instantaneous_flow <- function(topo, sizes, parameters) {
    # Convert parameters to a tibble if needed
    if (!is(parameters, "tbl")) {
        if (!(is.vector(parameters) & !is.null(attr(parameters, "names")))) {
            stop("\"parameters\" must be a tibble or a named vector.")
        }
        parameters <- tibble::tibble(value = parameters,
                                     in_replicate = names(parameters))
    }
    # Process input
    n_comps <- ncol(topo)
    comps <- colnames(topo)
    comp_indices <- setNames(seq_len(n_comps), nm = comps)
    params <- parameters
    params <- setNames(params$value, nm = params$in_replicate)
    ss <- attr(topo, "steadyState")
    ss <- match(ss, comps)
    sp <- attr(topo, "split")
    sp <- match(sp, comps)
    # Build transfer matrix
    # (could use the build_transfer_matrix() function for consistency)
    tm <- matrix(0, ncol = n_comps, nrow= n_comps)
    for (i in seq_len(n_comps)) {
        # upsilons
        for (j in seq_len(n_comps)) {
            if (topo[i,j] == 1) {
                p <- paste("upsilon", comps[j], "to", comps[i], sep = "_")
                tm[i,j] <- params[p]
            }
        }
        # lambdas
        p <- paste("lambda", comps[i], sep = "_")
        if (tm[i,i] != 0) {
            stop("A compartment has a flow to itself (which is not permitted in the model).")
        }
        tm[i,i] <- tm[i,i] - params[p]
    }
    # Prepare sizes
    sizes <- sizes[match(comps, sizes$compartment), ]
    stopifnot(all(comps == sizes$compartment))
    ## Update split compartments
    if (length(sp) > 0) {
        sizeRefr <- list()
        for (j in sp) {
            portion <- as.vector(params[paste0("portion.act_", comps[j])])
            sizeRefr[[j]] <- sizes[["size"]][j] * (1 - portion)
            sizes[["size"]][j] <- sizes[["size"]][j] * portion
        }
    }
    # Calculate flows
    flows_mat <- tm
    stopifnot(ncol(flows_mat) == n_comps)
    for (j in seq_len(n_comps)) {
        flows_mat[, j] <- flows_mat[, j] * sizes[["size"]][j]
    }
    # Put flows into a tibble format
    flows <- tibble::tibble(from = rep(comps, each = n_comps),
                            to = rep(comps, n_comps),
                            instantaneous_flow = as.vector(flows_mat))
    ## Check that the matrix major order was correct
    # (maybe we can get rid of this checking step?)
    check_mat <- matrix(flows[["instantaneous_flow"]], ncol = n_comps, byrow = FALSE)
    delta <- max(abs(check_mat - flows_mat))
    if (delta > 1e-15) {
        stop("Issue with matrix major order when formatting flows.")
    }
    # Post-processing
    flows$in_topo <- as.vector(topo)
    flows[["in_topo"]][flows[["from"]] == flows[["to"]]] <- 1
    flows[["to"]][flows[["from"]] == flows[["to"]]] <- NA
    flows <- flows[flows$in_topo > 0, ]
    flows$in_topo <- NULL
    # Return
    return(flows)
}

### * gather_flows()

#' Helper function to calculate flows while calculating trajectories
#'
#' This function does not take into account any refractory portions, and
#' assumes that what is passed to it concerns only active portions.
#'
#' This function assumes that topo, sizes and transfer_mat are all ordered in
#' the same way (i.e. compartments match across arguments, and topo has the
#' same, ordered colnames and rownames).
#' 
#' @param topo A network topology.
#' @param sizes A vector containing compartment sizes.
#' @param transfer_mat A transfer matrix.
#'
#' @return A well-formatted tibble.
#' 
#' @keywords internal
#' @noRd

gather_flows <- function(topo, sizes, transfer_mat) {
    comps <- colnames(topo)
    n_comps <- ncol(topo)
    flows <- transfer_mat
    for (i in seq_len(ncol(topo))) {
        flows[, i] <- flows[, i] * sizes[i]
    }
    flows <- tibble::tibble(from = rep(comps, each = n_comps),
                            to = rep(comps, n_comps),
                            flow = as.vector(flows))
    flows$in_topo <- as.vector(topo)
    flows[["in_topo"]][flows[["from"]] == flows[["to"]]] <- 1
    flows[["to"]][flows[["from"]] == flows[["to"]]] <- NA
    flows <- flows[flows$in_topo > 0, ]
    flows$in_topo <- NULL
    return(flows)
}

### * potential_steady_state()

#' Simple test to check if a network potentially admis a steady state
#'
#' This is an imperfect test, but it will reject the two most obvious cases
#' where no steady state is (probably) possible: (1) if all lambdas are zero
#' but one source is in steady state (material accumulates in the network) and
#' (2) at least one lambda is non-null, but there are no steady state sources
#' (material leaks from the network without being replaced).
#'
#' Note that this is a bit rough, since some exceptions can exists: for example
#' if independent sub-networks exist in the network.
#'
#' @param x A one-row network object.
#'
#' @return Boolean.
#'
#' @examples
#' potential_steady_state <- isotracer:::potential_steady_state
#' 
#' m <- aquarium_mod
#' m <- set_prior(m, constant_p(0), "lambda")
#' m <- set_params(m, sample_params(m))
#' potential_steady_state(m)
#'
#' m <- set_prior(m, normal_p(0, 3), "lambda_NH4")
#' m <- set_params(m, sample_params(m))
#' potential_steady_state(m)
#'
#' m <- trini_mod[1, ]
#' m <- set_params(m, sample_params(m))
#' potential_steady_state(m)
#' 
#' @keywords internal
#' @noRd

potential_steady_state <- function(x) {
    if (nrow(x) != 1) {
        stop("\"x\" must have exactly one row.")
    }
    topo <- topo(x, simplify = TRUE)
    params <- x$parameters[[1]]
    lambdas <- params$value[grepl("^lambda_", params$in_replicate)]
    has_steady_state <- length(attr(topo, "steadyState")) > 0
    has_non_zero_lambda <- any(lambdas != 0)
    if (has_steady_state & ! has_non_zero_lambda) {
        return(FALSE)
    }
    if (! has_steady_state & has_non_zero_lambda) {
        return(FALSE)
    }
    return(TRUE)
}

### * calculate_steady_state_one_row()

#' Calculate steady-state compartment sizes for a one-row network
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
#' @param nm_row A one-row network model, with set parameter values.
#'
#' @return A named vector containing the steady state compartment sizes.
#'
#' @examples
#' calculate_steady_state_one_row <- isotracer:::calculate_steady_state_one_row
#' 
#' # Simple model, no split compartments
#' m <- aquarium_mod
#' m <- set_prior(m, constant_p(0), "lambda")
#' set.seed(4)
#' m <- set_params(m, sample_params(m))
#' proj <- project(m, end = 40)
#' plot(proj)
#' calculate_steady_state_one_row(m)
#'
#' # With split compartments
#' m <- trini_mod[1, ]
#' set.seed(4)
#' m <- set_params(m, sample_params(m))
#' proj <- project(m, end = 40)
#' plot(proj)
#' calculate_steady_state_one_row(m)
#' 
#' @keywords internal
#' @noRd

calculate_steady_state_one_row <- function(nm_row) {
    stopifnot(nrow(nm_row) == 1)
    topo <- topo(nm_row, simplify = TRUE)
    if (!potential_steady_state(nm_row)) {
        stop("The network model row seems not to admit a steady state.\n",
             "(i.e. potential_steady_state(...) returned FALSE).")
    }
    # Go on
    has_ss_comps <- length(attr(topo, "steadyState")) != 0
    has_split_comps <- length(attr(topo, "split")) != 0
    params <- nm_row$parameters[[1]]
    transition_mat <- build_transition_matrix(topo = topo, params = params, dt = 1,
                                              apply_steady_state = TRUE)
    decomp <- eigen(transition_mat)
    values <- decomp$values
    vectors <- decomp$vectors
    # Process split compartments
    if (has_split_comps) {
        sp_comps <- attr(topo, "split")
        inits <- nm_row$initial[[1]]
        original_inits <- inits
        original_inits$refr_size <- 0
        original_inits$p_act <- 1
        for (i in seq_len(nrow(inits))) {
            if (inits$compartment[i] %in% sp_comps) {
                p_act <- params$value[params$in_replicate == paste0("portion.act_", inits$compartment[i])]
                original_inits$refr_size[i] <- original_inits$size[i] * (1 - p_act)
                original_inits$p_act[i] <- p_act
                inits$size[i] <- inits$size[i] * p_act
            }
        }
        nm_row$initial[[1]] <- inits
    }
    # Determine eigen elements of interest
    if (!has_ss_comps) {
        accepted <- which(Im(values) == 0)
        if (length(accepted) > 1) {
            accepted <- which(Im(values) == 0 &
                              apply(sign(Re(vectors)), 2, function(x) length(unique(x))) == 1)
        }
        if (length(accepted) == 0) {
            stop("No real eigenvalue for the transition matrix.")
        }
    } else {
        accepted <- which(values == 1)
        if (length(accepted) == 0) {
            stop("1 is not an eigenvalue of the transition matrix.")
        }
        # Check consistency with ss comps
        ss_comps <- attr(topo, "steadyState")
        ss_comps_i <- sapply(ss_comps, function(x) which(colnames(topo) == x))
        for (i in accepted) {
            v <- vectors[, i]
            values_v <- v[ss_comps_i]
            if (!sum(values_v != 0) > 0) {
                stop("Eigenvector incompatible with steady state compartments.")
            }
            if (!sum(values_v != 0) == 1) {
                stop("Steady state compartments incompatible with current implementation.")
            }
        }
    }
    # Calculate steady state sizes
    values <- values[accepted]
    vectors <- vectors[, accepted, drop = FALSE]
    comps <- colnames(topo)
    inits <- nm_row$initial[[1]]
    stopifnot(setequal(comps, inits$compartment))
    if (!has_ss_comps) {
        total_size <- sum(inits$size)
        if (length(accepted) > 1) {
            stop("Unexpected eigenvalue decomposition: more than one real eigen value found.")
        }
        stable_sizes <- vectors[, 1]
        stopifnot(all(stable_sizes == Re(stable_sizes)))
        stable_sizes <- Re(stable_sizes)
        names(stable_sizes) <- colnames(transition_mat)
        adjust <- total_size / sum(stable_sizes)
        stable_sizes <- adjust * stable_sizes
    } else {
        ss_comps <- attr(topo, "steadyState")
        ss_comps_i <- sapply(ss_comps, function(x) which(colnames(topo) == x))
        for (v in seq_len(ncol(vectors))) {
            focal_ss <- which(vectors[, v][ss_comps_i] > 0)
            if (!length(focal_ss) == 1) {
                stop("More than one steady state compartment participating in an eigenvector.")
            }
            focal_init <- inits$size[inits$compartment == colnames(topo)[ss_comps_i[focal_ss]]]
            focal_index <- ss_comps_i[focal_ss]
            adjust_factor <- focal_init / vectors[, v][focal_index]
            vectors[, v] <- vectors[, v] * adjust_factor
        }
        stable_sizes <- apply(vectors, 1, sum)
        names(stable_sizes) <- colnames(transition_mat)
        for (i in ss_comps) {
            if (abs(stable_sizes[i] - inits$size[inits$compartment == i]) > .Machine$double.eps * 1000) {
                stop("Incompatible initial and steady state size for a steady state compartment.\n",
                     "(size difference is ", abs(stable_sizes[i] - inits$size[inits$compartment == i]), ").")
            }
        }
    }
    # Process split compartments
    if (has_split_comps) {
      p_act <- setNames(original_inits$p_act,
                        nm = original_inits$compartment)
      stable_sizes <- stable_sizes / p_act[names(stable_sizes)]
      ## refr_sizes <- setNames(original_inits$refr_size,
      ##                        nm = original_inits$compartment)
      ## stable_sizes <- stable_sizes + refr_sizes[names(stable_sizes)]
    }
    # Return
    return(stable_sizes)
}
