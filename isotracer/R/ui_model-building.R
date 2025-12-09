### * All functions in this file are exported

### * new_networkModel()

#' Create an empty network model
#'
#' The first step in building a network model is to create a new, empty
#' \code{networkModel} object. This model can then be completed using functions
#' such as \code{set_topo()}, \code{set_init()}, etc...
#'
#' @param quiet Boolean, if \code{FALSE} print a message indicating which
#'     distribution family is used for proportions.
#'
#' @return An empty \code{networkModel} object. It is basically a zero-row
#'     tibble with the appropriate columns.
#' 
#' @examples
#' m <- new_networkModel()
#' m
#' class(m)
#'
#' @export

new_networkModel <- function(quiet = FALSE) {
    verbose <- !quiet
    x <- tibble::tibble(topology = list(),
                        initial = list(),
                        observations = list())
    attr(x, "prop_family") <- "gamma_cv"
    if (verbose) {
        message("Using default distribution family for proportions (\"gamma_cv\").")
        describe_z_eta("eta", attr(x, "prop_family"))
    }
    attr(x, "size_family") <- "normal_cv"
    if (verbose) {
        message("Using default distribution family for sizes (\"normal_cv\").")
        describe_z_eta("zeta", attr(x, "size_family"))
    }
    attr(x, "size_zeta_per_compartment") <- FALSE
    x <- structure(x, class = c("networkModel", class(x)))
    return(x)
}

### * set_prop_family()

#' Set the distribution family for observed proportions
#'
#' @param nm A \code{networkModel} object (output from
#'     \code{\link{new_networkModel}}).
#' @param family Allowed values are "gamma_cv", "beta_phi", "normal_cv", and
#'     "normal_sd".
#' @param quiet Boolean, if \code{FALSE} print a message indicating which
#'     distribution family is used for proportions.
#'
#' @return A \code{networkModel} object.
#'
#' @examples
#' library(magrittr)
#' 
#' m <- new_networkModel() %>%
#'   set_topo(links = "NH4, NO3 -> epi -> pseph, tricor")
#' m <- m %>% set_prop_family("beta_phi")
#' m
#' attr(m, "prop_family")
#'
#' @export

set_prop_family <- function(nm, family, quiet = FALSE) {
    verbose <- !quiet
    known_families <- c("gamma_cv" = 1, "normal_cv" = 2, "normal_sd" = 3,
                        "beta_phi" = 4)
    if (!family %in% names(known_families)) {
        stop("Unknown distribution family for proportions. Got value: \"",
             family, "\"\n",
             "Allowed values are: ", paste0(sapply(names(known_families), function(x) paste0("\"", x, "\"")),
                                            collapse = ", "))
    }
    attr(nm, "prop_family") <- family
    if (verbose) {
        message("Using distribution family for proportions: \"", family, "\".",
                sep = "")
        describe_z_eta("eta", family)
    }
    return(nm)
}

### * set_size_family()

#' Set the distribution family for observed sizes
#'
#' @param nm A \code{networkModel} object (output from
#'     \code{\link{new_networkModel}}).
#' @param family Allowed values are "normal_cv" and "normal_sd".
#' @param by_compartment Boolean, if TRUE then zeta is compartment-specific.
#' @param quiet Boolean, if \code{FALSE} print a message indicating which
#'     distribution family is used for proportions.
#' @param quiet_reset Boolean, write a message when model parameters (and
#'     covariates and priors) are reset?
#'
#' @return A \code{networkModel} object.
#'
#' @examples
#' library(magrittr)
#' 
#' m <- new_networkModel() %>%
#'   set_topo(links = "NH4, NO3 -> epi -> pseph, tricor")
#' m <- m %>% set_size_family("normal_sd")
#' m
#' attr(m, "size_family")
#'
#' m <- m %>% set_size_family(by_compartment = TRUE)
#' attr(m, "size_zeta_per_compartment")
#' 
#' @export

set_size_family <- function(nm, family, by_compartment, quiet = FALSE,
                            quiet_reset = FALSE) {
    verbose <- !quiet
    verbose_reset <- !quiet_reset
    if (!missing(family)) {
        known_families <- c("normal_cv" = 1, "normal_sd" = 2)
        if (!family %in% names(known_families)) {
            stop("Unknown distribution family for proportions. Got value: \"",
                 family, "\"\n",
                 "Allowed values are: ", paste0(sapply(names(known_families), function(x) paste0("\"", x, "\"")),
                                                collapse = ", "))
        }
        attr(nm, "size_family") <- family
        if (verbose) {
            message("Using distribution family for sizes: \"", family, "\".",
                    sep = "")
            describe_z_eta("zeta", family)
        }
    }
    if (!missing(by_compartment)) {
        stopifnot(by_compartment %in% c(FALSE, TRUE))
        attr(nm, "size_zeta_per_compartment") <- by_compartment
        if(verbose) {
            message("Compartment-specific zeta set to ", by_compartment, ".",
                    sep = "")
        }
        if (!is.null(nm[["parameters"]])) {
            # nm already had parameters
            # Reset to update the zeta parameters
            nm <- add_param_mapping(nm)
            if (verbose_reset) {
                message("Parameters priors and covariates were reset because `by_compartment` was specified.")
                message("(This means that no priors are set and no covariates are used in the returned networkModel.)")
            }
        }
    }
    return(nm)
}

### * set_topo()

### ** Doc

#' Set the topology in a network model.
#'
#' @param nm A \code{networkModel} object (output from
#'     \code{\link{new_networkModel}}).
#' @param ... One or more strings describing the links defining the network
#'     topology. Optionally, links can be given as a data frame. See the
#'     examples for more details about acceptable input formats.
#' @param from Optional, string containing the column name for sources if
#'     links are provided as a data frame.
#' @param to Optional, string containing the column name for destinations if
#'     links are provided as a data frame.
#'
#' @return A \code{networkModel} object.
#'
#' @examples
#' # A single string can describe several links in one go.
#' m <- new_networkModel() %>%
#'   set_topo("NH4, NO3 -> epi -> pseph, tricor")
#' m
#' topo(m)
#'
#' # Several strings can be given as distinct arguments.
#' m2 <- new_networkModel() %>%
#'   set_topo("NH4, NO3 -> epi -> pseph, tricor",
#'            "NH4 -> FBOM, CBOM", "CBOM <- NO3")
#' m2
#' topo(m2)
#'
#' # Multiple strings can be also be combined into a single argument with `c()`.
#' links <- c("NH4, NO3 -> epi -> pseph, tricor", "NH4 -> FBOM, CBOM",
#'            "CBOM <- NO3")
#' m3 <- new_networkModel() %>%
#'   set_topo(links)
#' m3
#' topo(m3)
#'
#' # A data frame can be used to specify the links.
#' links <- data.frame(source = c("NH4", "NO3", "epi"),
#'                     consumer = c("epi", "epi", "petro"))
#' links
#' m4 <- new_networkModel() %>%
#'   set_topo(links, from = "source", to = "consumer")
#' m4
#' m4$topology[[1]]
#' 
#' @export

### ** Code

set_topo <- function(nm, ..., from = NULL, to = NULL) {
    # Parse the ... argument
    args <- list(...)
    if (length(args) > 1) {
        if (!all(sapply(args, is.character))) {
            stop("Only strings can be used when providing multiple link arguments.\n",
                 "(Maybe you provided both string(s) and data frame(s)?)")
        }
        links <- unlist(args)
    } else {
        links <- args[[1]]
    }
    # Build the topology object
    topology <- make_topology(links = links, from = from, to = to)
    # If the network model is empty, create a row of NULLs
    message_reset <- TRUE
    if (nrow(nm) == 0) {
        nm[1, 1] <- NA
        message_reset <- FALSE
    }
    # Assign all the cells in the "topology" column
    for (i in seq_len(nrow(nm))) {
        nm$topology[[i]] <- topology
    }
    # Add parameter mapping
    nm <- add_param_mapping(nm)
    if (message_reset) {
        msg <- c("`set_topo()` was called on a non-empty networkModel object.",
                 "As a result, the parameter mapping and the priors of the model were reset.")
        message(paste0(msg, collapse = " "))
    }
    return(nm)
}

### * set_steady()

#' Flag some network compartments as being in a steady state
#'
#' @param nm A \code{networkModel} object.
#' @param comps Vector of strings, names of the compartments to set steady.
#' @param which Vector of integers giving the nm rows to update. Default is to
#'     update all rows.
#'
#' @return A \code{networkModel} object.
#'
#' @examples
#' library(magrittr)
#' x <- new_networkModel() %>%
#'    set_topo("NH4 -> algae -> daphnia") %>%
#'    set_steady("NH4")
#' topo(x)
#'
#' @export

set_steady <- function(nm, comps = NULL, which = NULL) {
    if (is.null(which)) {
        which <- seq_len(nrow(nm))
    }
    stopifnot(all(which %in% seq_len(nrow(nm))))
    # Update row by row
    for (i in which) {
        topo <- nm[["topology"]][[i]]
        steady <- attr(topo, "steadyState")
        for (comp in comps) {
            stopifnot(comp %in% colnames(topo))
            steady <- c(steady, comp)
        }
        attr(nm[["topology"]][[i]], "steadyState") <- steady
    }
    # Return
    return(nm)
}

### * set_split()

#' Flag some network compartments as being split compartments
#'
#' This function automatically adds a default prior (uniform on [0,1]) for the
#' active portion of split compartments.
#' 
#' @param nm A \code{networkModel} object.
#' @param comps Vector of strings, the names of the compartments to set split.
#' @param which Vector of integers giving the nm rows to update. Default is to
#'     update all rows.
#'
#' @return A \code{networkModel} object.
#'
#' @examples
#' library(magrittr)
#' x <- new_networkModel() %>%
#'    set_topo("NH4 -> algae -> daphnia") %>%
#'    set_split("algae")
#' topo(x)
#'
#' @export

set_split <- function(nm, comps = NULL, which = NULL) {
    if (is.null(which)) {
        which <- seq_len(nrow(nm))
    }
    stopifnot(all(which %in% seq_len(nrow(nm))))
    # Update row by row
    for (i in which) {
        topo <- nm[["topology"]][[i]]
        split <- attr(topo, "split")
        for (comp in comps) {
            stopifnot(comp %in% colnames(topo))
            split <- c(split, comp)
        }
        attr(nm[["topology"]][[i]], "split") <- split
        # Add the corresponding parameters
        params <- nm[["parameters"]][[i]]
        for (comp in comps) {
            paramName <- paste0("portion.act_", comp)
            params <- tibble::add_row(params, in_replicate = paramName,
                                      in_model = paramName)
        }
        nm[["parameters"]][[i]] <- params
    }
    # Update priors
    for (comp in comps) {
        paramName <- paste0("portion.act_", comp)
        attr(nm, "priors") <- tibble::add_row(attr(nm, "priors"),
                                              in_model = paramName,
                                              prior = list(uniform_p(0, 1)))
    }
    return(nm)
}

### * set_half_life()

#' Set the half-life for radioactive tracers
#'
#' Indicating a non-zero value for half-life will add a decay to the marked
#' portion of the tracer element. The decay constant is calculated from the
#' half-life value as:
#'
#' lambda_decay = log(2) / half_life
#'
#' Note that for correct calculations the half-life value should be given in
#' the same time unit (e.g. hour, day) that the time unit used for
#' observations.
#' 
#' @param nm A \code{networkModel} object.
#' @param hl Half-life value, in the same time unit as the observations are (or
#'     will be) given. Setting half-life to zero is equivalent to using a
#'     stable isotope (no decay used in the model).
#' @param quiet Boolean for verbosity.
#'
#' @return A \code{networkModel} object.
#'
#' @examples
#' library(magrittr)
#' x <- new_networkModel() %>%
#'     set_topo("32P -> root -> leaf") %>%
#'     set_half_life(hl = 14.268)
#' x
#'
#' @export

set_half_life <- function(nm, hl, quiet = FALSE) {
    verbose <- !quiet
    if (hl == 0) {
        if (verbose) {
            message("Half-life set to zero; the model will assume stable isotopes.")
        }
        attr(nm, "lambda_hl") <- NULL
    } else {
        lambda_hl <- log(2) / hl
        if (verbose) {
            message("Half-life set to ", hl, " time units.\n",
                    "(equivalent to a decay rate of ", signif(lambda_hl, 3), " per time unit).")
        }
        attr(nm, "lambda_hl") <- lambda_hl
    }
    return(nm)
}

### * add_pulse_event()

#' Register a pulse event on one of the compartment of a topology
#'
#' When applied to a steady-state compartment, this is equivalent to changing
#' the steady state. Negative values are allowed, so one can add a "pulse" to a
#' steady-state compartment and then later add a similar but negative "pulse"
#' to simulate a drip in a stream for example.
#'
#' @param nm A \code{networkModel} object.
#' @param time Numeric, time at which the pulse is happening.
#' @param comp One compartment name only.
#' @param unmarked Numeric, quantity of unmarked marker added.
#' @param marked Numeric, quantity of marked marker added.
#' @param which Vector of integers giving the nm rows to update. Default is to
#'     update all rows.
#' @param pulses Optionally, a tibble containing the pulse information in
#'     columns. If provided, `comp`, `time`, `unmarked` and `marked` must be
#'     strings giving the corresponding column names.
#'
#' @return A \code{networkModel} object.
#'
#' @examples
#' m <- trini_mod
#' m$events <- NULL
#' pulses <- tibble::tribble(
#'    ~ stream,    ~ transect, ~ comp, ~ time, ~ qty_14N, ~ qty_15N,
#'        "UL",  "transect.1",  "NH4",     11,         0,  -0.00569,
#'        "UL",  "transect.2",  "NH4",     11,         0,  -0.00264,
#'        "UL",  "transect.3",  "NH4",     11,         0, -0.000726,
#'        "UL",  "transect.1",  "NO3",     11,         0,  -0.00851,
#'        "UL",  "transect.2",  "NO3",     11,         0,  -0.01118,
#'        "UL",  "transect.3",  "NO3",     11,         0,  -0.01244,
#'    )
#' m <- add_pulse_event(m, pulses = pulses, comp = "comp", time = "time",
#'                      unmarked = "qty_14N", marked = "qty_15N")
#' m
#'  
#' 
#' @export

add_pulse_event <- function(nm, time, comp = NULL, unmarked, marked,
                            which = NULL, pulses) {
    if (!missing(pulses)) {
        # Table syntax
        if (is.null(groups(nm))) {
            # No groups
            for (i in seq_len(nrow(pulses))) {
                args <- list(nm = nm,
                             time = pulses[[time]][i],
                             comp = pulses[[comp]][i],
                             unmarked = pulses[[unmarked]][i],
                             marked = pulses[[marked]][i])
                nm <- do.call(add_pulse_event, args)
            }
            return(nm)
        } else {
            # Groups
            grps <- groups(nm)
            gp_vars <- colnames(grps)
            if (!all(gp_vars %in% colnames(pulses))) {
                gp_vars_str <- paste0("\"", gp_vars, "\"")
                stop("The `pulses` argument must have columns specifying the group variables of the network model.\n",
                     "  (Model grouped by: ", paste(gp_vars_str, collapse = ", "), ")\n",
                     "  (Missing column(s) in `pulses` table: ", paste(gp_vars_str[!gp_vars %in% colnames(pulses)], collapse = ", "), ")\n")
            }
            pulses_labels <- apply(as.matrix(pulses[, gp_vars]), 1, paste, collapse = " ")
            for (i in seq_len(nrow(pulses))) {
                grps <- groups(nm)
                grps_labels <- apply(as.matrix(grps), 1, paste, collapse = " ")
                row_i <- which(grps_labels == pulses_labels[i])
                stopifnot(length(row_i) == 1)
                args <- list(nm = nm,
                             time = pulses[[time]][i],
                             comp = pulses[[comp]][i],
                             unmarked = pulses[[unmarked]][i],
                             marked = pulses[[marked]][i],
                             which = row_i)
                nm <- do.call(add_pulse_event, args)
            }
            return(nm)
        }
    }
    # Single pulse
    if (is.null(which)) {
        which <- seq_len(nrow(nm))
    }
    stopifnot(all(which %in% seq_len(nrow(nm))))
    # Create an empty "events" column if needed
    if (!"events" %in% colnames(nm)) {
        nm[["events"]] <- rep(list(NULL), nrow(nm))
    }
    # Update row by row
    for (i in which) {
        topo <- nm[["topology"]][[i]]
        stopifnot(comp %in% colnames(topo))
        this_event <- tibble::tibble(event = "pulse",
                         time = time,
                         compartment = comp,
                         characteristics = list(list(unmarked = unmarked,
                                                marked = marked)))
        nm[["events"]][[i]] <- dplyr::bind_rows(nm[["events"]][[i]],
                                                this_event)
        nm[["events"]][[i]] <- nm[["events"]][[i]][order(nm[["events"]][[i]][["time"]],
                                                         nm[["events"]][[i]][["compartment"]]), ]
    }
    # Return
    return(nm)
}

### * set_init()

#' Set initial conditions in a network model
#'
#' @param nm A \code{networkModel} object (e.g. output from
#'     \code{\link{new_networkModel}})
#' @param data A tibble containing the initial conditions
#' @param comp String, name of the \code{data} column with the compartment names
#' @param size String, name of the \code{data} column with the compartment sizes
#' @param prop String, name of the \code{data} column with the compartment
#'     proportions of marked tracer
#' @param group_by Optional vector of string giving the names of the columns to
#'     use for grouping the data into replicates
#'
#' @return A \code{networkModel} object.
#'
#' @examples
#' # Using the topology from the Trinidad case study
#' m <- new_networkModel() %>%
#'   set_topo("NH4, NO3 -> epi, FBOM", "epi -> petro, pseph",
#'            "FBOM -> tricor", "petro, tricor -> arg")
#'
#' # Taking initial condtions from the 'lalaja' dataset at t=0
#' inits <- lalaja[lalaja[["time.days"]] == 0, ]
#' inits
#' m <- set_init(m, inits, comp = "compartment", size = "mgN.per.m2",
#'               prop = "prop15N", group_by = "transect")
#' m
#' 
#' @export

set_init <- function(nm, data, comp, size, prop, group_by = NULL) {
    # Trim extra compartments
    topo_comps <- unique(unlist(comps(nm)))
    init_comps <- unique(data[[comp]])
    extra_comps <- init_comps[!init_comps %in% topo_comps]
    if (length(extra_comps) > 0) {
        message(paste0(strwrap("Some compartments are not present in the network topology and are removed from the initial data:"),
                       collapse = "\n"), "\n",
                "  - ", paste0(extra_comps, collapse = "\n  - "), "\n")
        data <- data[data[[comp]] %in% topo_comps, ]
    }
    # Build init
    grouped_init <- make_init(data = data, comp = comp,
                              size = size, prop = prop,
                              group_by = group_by)
    out <- merge_nm_by_group(nm = nm, tib = grouped_init,
                             destination = "initial",
                             tib_name = "initial conditions")
    attr(out, "prop_family") <- attr(nm, "prop_family")
    attr(out, "default_columns") <- list(comp = comp,
                                         size = size,
                                         prop = prop,
                                         group_by = group_by)
    # Check that each compartment gets exactly one initial condition
    for (i in seq_len(nrow(out))) {
        z <- out$initial[[i]]
        if (nrow(z) != nrow(na.omit(z))) {
            stop("Some NAs are present in the initial conditions.")
        }
        if (!all(table(z$compartment) == 1)) {
            stop("Some compartments are present twice in the initial conditions data.")
        }
        if (!all(topo_comps %in% z[["compartment"]])) {
            stop("Some compartments are present in the topology but do not have initial conditions.")
        }
    }
    return(out)
}

### * set_obs()

#' Set observations in a network model
#'
#' @param nm A \code{networkModel} object (e.g. output from
#'     \code{\link{new_networkModel}})
#' @param data A tibble containing the observations. If NULL, remove
#'     observations from the model.
#' @param comp String, name of the \code{data} column with the compartment
#'     names
#' @param size String, name of the \code{data} column with the compartment
#'     sizes
#' @param prop String, name of the \code{data} column with the compartment
#'     proportions of heavy tracer
#' @param time String, name of the \code{data} column with the sampling times
#' @param group_by Optional vector of string giving the names of the columns to
#'     use for grouping the data into replicates
#'
#' @return A \code{networkModel} object.
#'
#' @examples
#' # Using the topology from the Trinidad case study
#' m <- new_networkModel() %>%
#'   set_topo("NH4, NO3 -> epi, FBOM", "epi -> petro, pseph",
#'            "FBOM -> tricor", "petro, tricor -> arg")
#'
#' # Taking initial condtions from the 'lalaja' dataset at t=0
#' inits <- lalaja[lalaja[["time.days"]] == 0, ]
#' inits
#' m <- set_init(m, inits, comp = "compartment", size = "mgN.per.m2",
#'               prop = "prop15N", group_by = "transect")
#' m
#'
#' # Taking observations from 'lalaja'
#' m <- set_obs(m, lalaja[lalaja[["time.days"]] > 0, ], time = "time.days")
#' m
#' plot(m)
#' 
#' @export

set_obs <- function(nm, data, comp, size, prop, time, group_by) {
    if (is.null(data)) {
        nm[["observations"]] <- rep(list(NULL), nrow(nm))
        return(nm)
    }
    # Get default columns
    default_columns <- attr(nm, "default_columns")
    columns_from_attr <- c()
    if (missing(comp)) {
        comp <- default_columns[["comp"]]
        if (!is.null(comp)) {
            columns_from_attr <- c(columns_from_attr, "comp")
        }
    }
    if (missing(size)) {
        size <- default_columns[["size"]]
        if (!is.null(size)) {
            columns_from_attr <- c(columns_from_attr, "size")
        }
    }
    if (missing(prop)) {
        prop <- default_columns[["prop"]]
        if (!is.null(prop)) {
            columns_from_attr <- c(columns_from_attr, "prop")
        }
    }
    if (missing(group_by)) {
        group_by <- default_columns[["group_by"]]
        if (!is.null(group_by)) {
            columns_from_attr <- c(columns_from_attr, "group_by")
        }
    }
    if (length(columns_from_attr) > 0) {
        msg <- "Using the same columns by default as the ones used with `set_init()`:\n"
        for (i in columns_from_attr) {
            msg <- c(msg, paste0("  ", i, " = \"", default_columns[[i]], "\"\n"))
        }
        msg <- paste0(msg, collapse = "")
        message(msg)
    }
    # Trim extra compartments
    topo_comps <- unique(unlist(comps(nm)))
    obs_comps <- unique(data[[comp]])
    extra_comps <- obs_comps[!obs_comps %in% topo_comps]
    if (length(extra_comps) > 0) {
        message(paste0(strwrap("Some compartments are not present in the network topology and are removed from the observed data:"),
                       collapse = "\n"), "\n",
                "  - ", paste0(extra_comps, collapse = "\n  - "), "\n")
        data <- data[data[[comp]] %in% topo_comps, ]
    }
    # Build observations
    grouped_obs <- make_obs(data = data, comp = comp, size = size, prop = prop,
                            time = time, group_by = group_by)
    out <- merge_nm_by_group(nm = nm, tib = grouped_obs,
                             destination = "observations",
                             tib_name = "observations")
    default_columns[["time"]] <- time
    attr(out, "default_columns") <- default_columns
    attr(out, "prop_family") <- attr(nm, "prop_family")
    return(out)
}

### * set_params()

#' Set the parameters in a network model
#'
#' @param nm A \code{networkModel} object.
#' @param params A named vector or a tibble with columns c("parameter",
#'     "value") containing the (global) parameter values.
#' @param force Boolean, if FALSE will not overwrite already set parameters.
#' @param quick Boolean, if TRUE take some shortcuts for faster parameter
#'     settings when called by another function. This should usually be left to
#'     the default (FALSE) by a regular package user.
#'
#' @return A \code{networkModel} object.
#'
#' @examples
#' m <- aquarium_mod
#' p <- sample_params(m)
#' m2 <- set_params(m, p)
#' m2$parameters
#' 
#' @export

set_params <- function(nm, params, force = TRUE, quick = FALSE) {
    if (is.vector(params)) {
        params <- data.frame(value = params, parameter = names(params),
                             stringsAsFactors = FALSE)
    }
    prev_params <- attr(nm, "parameterValues")
    if (!is.null(prev_params) & !force) {
        kept_indices <- !(prev_params$parameter %in% params$parameter)
        kept_params <- prev_params[kept_indices, ]
        new_params <- dplyr::bind_rows(kept_params, params)
    } else {
        new_params <- params
    }
    if (!quick) {
        new_params <- new_params[order(new_params[["parameter"]]), ]
        attr(nm, "parameterValues") <- tibble::as_tibble(new_params)
    } else {
        attr(nm, "parameterValues") <- new_params
    }
    for (i in seq_len(nrow(nm))) {
        nm[["parameters"]][[i]]$value <- new_params[["value"]][match(nm[["parameters"]][[i]]$in_model,
                                                                     new_params[["parameter"]])]
    }
    return(nm)
}

### * set_prior() | set_priors()

#' Set prior(s) for a network model
#'
#' @param x A \code{networkModel} object.
#' @param prior A prior built with e.g. uniform_p() or hcauchy_p(). Call
#'     \code{available_priors()} to see a table of implemented
#'     priors. Alternatively, if \code{prior} is a tibble, the function will
#'     try to use it to set parameter priors. The format of such an argument is
#'     the same as the format of the output of the getter function
#'     \code{priors()} (see examples). Note that if `prior` is given as a
#'     tibble, all other arguments (except `x`) are disregarded.
#' @param param String, target parameter or regexp to target several
#'     parameters. Default is the empty string \code{""}, which will match all
#'     parameters.
#' @param use_regexp Boolean, if \code{TRUE} (the default) then \code{param} is
#'     used as a regular expression to match one or several parameter names.
#' @param quiet Boolean, if \code{FALSE} print a message indicating which
#'     parameters had their prior modified.
#'
#' @return A \code{networkModel} object.
#'
#' @examples
#' # Copy `aquarium_mod`
#' m <- aquarium_mod
#' priors(m)
#'
#' # Modify the priors of `m`
#' m <- set_priors(m, exponential_p(0.5), "lambda")
#' priors(m)
#'
#' # Re-apply priors from the original `aquarium_mod`
#' prev_priors <- priors(aquarium_mod)
#' prev_priors
#' m <- set_priors(m, prev_priors)
#' priors(m)
#' 
#' @export

set_prior <- function(x, prior, param = "", use_regexp = TRUE, quiet = FALSE) {
    verbose <- !quiet
    params <- params(x, simplify = TRUE)
    priors <- priors(x)
    stopifnot(setequal(params, priors[["in_model"]]))
    params <- priors[["in_model"]]
    # If `prior` is a tibble
    if (is(prior, "tbl_df")) {
        if (!valid_prior_tbl(prior)) {
            stop("`prior` is not a valid prior tibble.")
        }
        prior <- prior[, c("in_model", "prior")]
        extra_params <- which(!prior[["in_model"]] %in% params)
        if (length(extra_params) > 0) {
            message("Some parameter names in `prior` are not used in the network model and will be ignored.\n",
                    paste0(paste0("\"", prior[["in_model"]][extra_params], "\""), collapse = ", "))
        }
        prior <- prior[which(prior[["in_model"]] %in% params), ]
        param_indices <- match(prior[["in_model"]], params)
        for (i in seq_along(param_indices)) {
            priors[["prior"]][[param_indices[i]]] <- prior[["prior"]][[i]]
        }
        if (verbose) {
            message("Prior modified for parameter(s): \n  - ",
                    paste0(prior[["in_model"]], collapse = "\n  - "))
        }
        attr(x, "priors") <- priors
        return(x)
    }
    # If `prior` is not a tibble
    if (!is(prior, "prior")) {
        stop("`prior` must be a valid prior. See `available_priors()`.")
    }
    if (use_regexp) {
        param_indices <- which(grepl(pattern = param, x = params))
    } else {
        param_indices <- which(params == param)
    }
    if (length(param_indices) == 0) {
        stop("No matching parameter found for: ", param, ".\n",
             "Available parameters are: \n- ",
             paste0(params, collapse = "\n- "))
    }
    for (i in param_indices) {
        priors[["prior"]][[i]] <- prior
    }
    if (verbose) {
        message("Prior modified for parameter(s): \n  - ",
                paste0(params[param_indices], collapse = "\n  - "))
    }
    attr(x, "priors") <- priors
    return(x)
}

#' @rdname set_prior
#' @export

set_priors <- set_prior
    
### * add_covariates()

#' Add fixed effects of one or several covariates to some parameters.
#'
#' Note that new global parameters are not given any default prior.
#'
#' @param nm A \code{networkModel} object.
#' @param ... One or several formulas defining the covariates.
#' @param use_regexpr Boolean, use regular expression to match the parameters
#'     affected by the formulas?
#' 
#' @return A \code{networkModel} object.
#'
#' @examples
#' # Using a subset of the topology from the Trinidad case study
#' m <- new_networkModel() %>%
#'   set_topo("NH4, NO3 -> epi, FBOM", "epi -> petro, pseph")
#'
#' # Taking initial condtions from the 'lalaja' dataset at t=0
#' # Grouping by transect id
#' inits <- lalaja[lalaja[["time.days"]] == 0, ]
#' inits
#' m <- set_init(m, inits, comp = "compartment", size = "mgN.per.m2",
#'               prop = "prop15N", group_by = "transect")
#' m
#'
#' # Default model
#' params(m, simplify = TRUE)
#'
#' # Adding an effect of the "transect" covariate on some parameters
#' m <- add_covariates(m, upsilon_epi_to_pseph ~ transect)
#' params(m, simplify = TRUE)
#'
#' @export

add_covariates <- function(nm, ..., use_regexpr = TRUE) {
    formula <- list(...)
    # Apply formula
    for (eachFormula in formula) {
        nm <- refresh_param_mapping(nm, eachFormula, use_regexpr = use_regexpr)
    }
    # Refresh priors
    current_priors <- priors(nm)
    global_params <- params(nm, simplify = TRUE)
    default_priors <- tibble::tibble(in_model = global_params)
    default_priors$prior <- rep(list(NULL), nrow(default_priors))
    kept_priors <- current_priors[current_priors$in_model %in% global_params, ]
    new_priors <- default_priors[!default_priors$in_model %in%
                                 current_priors$in_model, ]
    priors <- dplyr::bind_rows(kept_priors, new_priors)
    priors <- priors[order(priors$in_model), ]
    # Return
    attr(nm, "priors") <- priors
    return(nm)
}

### * print.networkModel()

#' Print method for \code{networkModel} objects
#'
#' @param x A \code{networkModel} object.
#' @param ... Passsed to the next method.
#'
#' @return Called for the side effect of printing a network model object.
#' 
#' @method print networkModel
#'
#' @export
#' 

print.networkModel <- function(x, ...) {
    NextMethod()
    lambda_hl <- attr(x, "lambda_hl")
    if (!is.null(lambda_hl)) {
        hl <- signif(log(2) / lambda_hl, 5)
        cat("# Half-life of marked tracer:", hl, "time units.\n")
    }
}
