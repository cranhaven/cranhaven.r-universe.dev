### * TODO

# Clean-up this file

### * All functions in this file are exported

### * predict.networkModel()

#' Add a column with predictions from a fit
#'
#' @param object Network model
#' @param fit Model fit (mcmc.list object)
#' @param draws Integer, number of draws from the posteriors
#' @param error.draws Integer, number of draws from the error distribution, for
#'     a given posterior draw.
#' @param probs Credible interval (default 0.95).
#' @param cores Number of cores to use for parallel calculations. Default is
#'     \code{NULL}, which means to use the value stored in
#'     \code{options()[["mc.cores"]]} (or 1 if this value is not set).
#' @param dt,grid_size Time step size or grid points, respectively.
#' @param at Timepoints at which the predictions should be returned.
#' @param end Final timepoint used in the projections.
#' @param ... Not used.
#'
#' @return A network model object with an added column \code{"prediction"}.
#' 
#' @importFrom stats quantile
#' 
#' @export

predict.networkModel <- function(object, fit, draws = NULL, error.draws = 5,
                                 probs = 0.95, cores = NULL,
                                 dt = NULL, grid_size = NULL, at = NULL, end = NULL,
                                 ...) {
    `!!` <- rlang::`!!`
    # Process `cores` argument
    cores <- get_n_cores(cores = cores)
    # Process nm
    nm <- object
    if (!"events" %in% colnames(nm)) {
        nm[["events"]] <- rep(list(NULL), nrow(nm))
    }
    if (!"group" %in% colnames(nm)) {
        nm[["group"]] <- rep(list(NULL), nrow(nm))
    }
    # Process projection arguments (always caching)
    arg_end <- end
    cache <- list()
    rows_time_schemes <- list()
    rows_encoded_events <- list()
    nmRow_ends <- list()
    nmRow_ats <- list()
    for (i in seq_len(nrow(nm))) {
        nmRow <- nm[i, ]
        end <- arg_end
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
        if (is.null(at)) {
            at <- seq(0, end, length.out = 65)
        }
        nmRow_ends[[i]] <- end
        nmRow_ats[[i]] <- at
        rows_time_schemes[[i]] <- nm_row_get_time_scheme(nm_row = nmRow, dt = dt,
                                                         grid_size = grid_size,
                                                         end = end, at = at)
        rows_encoded_events[[i]] <- encode_events(nmRow, dt = dt, grid_size = grid_size,
                                                  end = end)
    }
    cache[["rows_time_schemes"]] <- rows_time_schemes
    cache[["rows_encoded_events"]] <- rows_encoded_events
    cache[["nmRow_ends"]] <- nmRow_ends
    cache[["nmRow_ats"]] <- nmRow_ats
    end <- arg_end
    # See https://github.com/HenrikBengtsson/future/issues/263#issuecomment-445047269
    # for a reason not to manipulate future::plan() in the package code.
    if (is.null(draws)) {
        draws <- nrow(fit[[1]])
    }
    # Get parameter samples
    samples <- tidy_mcmc_list(fit)
    if (draws > nrow(samples)) {
        warning("Number of draws greater (", draws,
                ") than number of samples (", nrow(samples) ,").\n",
                "Setting number of draws to ", nrow(samples), ".")
        draws <- nrow(samples)
    }
    samples <- samples[sample(seq_len(nrow(samples)), draws), ]
    # Project rows for each set of parameter values
    nm$prediction <- purrr::map(seq_len(nrow(nm)), function(k) {
        nmRow <- nm[k, ]
        projections <- parallel::mclapply(seq_len(nrow(samples)), function(i) {
            nmRow <- set_params(nmRow, samples$mcmc.parameters[[i]], force = TRUE, quick = TRUE)
            pred <- sample_from(nmRow, at = cache[["nmRow_ats"]][[k]],
                                dt = dt, grid_size = grid_size, end = end, error.draws = error.draws,
                                cached_ts = list(cache[["rows_time_schemes"]][[k]]),
                                cached_ee = list(cache[["rows_encoded_events"]][[k]]))
            return(pred)
        }, mc.cores = cores)
        # Concatenate data
        pred <- dplyr::bind_rows(projections)
        pred <- tidyr::nest(dplyr::group_by(pred,
                                            `!!`(rlang::sym("time")),
                                            `!!`(rlang::sym("comp"))))
        pred$sizes <- purrr::map(pred$data, function(x) {
            z <- quantile(x$size, probs = c((1-probs)/2, 1 -(1-probs)/2))
            m <- mean(x$size)
            out <- c(z[1], m, z[2])
            names(out) <- c("low", "mean", "high")
            return(out)
        })
        pred$props <- purrr::map(pred$data, function(x) {
            if (length(na.omit(x$prop)) == 0) { # All NAs can happen when the size is zero for example
                return(c(low = NA, mean = NA, high = NA))
            }
            z <- quantile(x$prop, probs = c((1-probs)/2, 1 -(1-probs)/2))
            m <- c(mean = mean(x$prop))
            out <- c(z[1], m, z[2])
            names(out) <- c("low", "mean", "high")
            return(out)
        })
        sizes <- do.call(dplyr::bind_rows, pred$sizes)
        names(sizes) <- c("size_low", "size_mean", "size_high")
        props <- do.call(dplyr::bind_rows, pred$props)
        names(props) <- c("prop_low", "prop_mean", "prop_high")
        pred <- dplyr::bind_cols(pred[, c("time", "comp")], sizes, props)
        pred[["compartment"]] <- pred[["comp"]]
        pred[["comp"]] <- NULL
        return(pred)
    })
    # Return
    return(nm)
}

### * posterior_predict.networkModelStanfit()

### ** Generic

#' Draw from the posterior predictive distribution of the model outcome
#'
#' @param object Model from which posterior predictions can be made.
#' @param ... Passed to the appropriate method.
#' 
#' @return Usually methods will implement a \code{draw} parameter, and the
#'     returned object is a "draw" by N matrix where N is the number of data
#'     points predicted per draw.
#' 
#' @export

posterior_predict <- function(object, ...) {
    UseMethod("posterior_predict")
}

### ** Method

#' Draw from the posterior predictive distribution of the model outcome
#'
#' @method posterior_predict networkModelStanfit
#'
#' @param object A networkModelStanfit object.
#' @param draw Integer, number of draws to perform from the posterior. Default
#'     is 100.
#' @param newdata Should be the model used to fit the networkStanfit object.
#' @param cores Number of cores to use for parallel calculations. Default is
#'     \code{NULL}, which means to use the value stored in
#'     \code{options()[["mc.cores"]]} (or 1 if this value is not set).
#' @param ... Not used for now.
#' 
#' @return A "draw" by N matrix where N is the number of data points predicted
#'     per draw.
#' 
#' @export

posterior_predict.networkModelStanfit <- function(object, newdata, draw = NULL,
                                                  cores = NULL, ...) {
    `!!` <- rlang::`!!`
    # Process `cores` argument
    cores <- get_n_cores(cores = cores)
    nm <- newdata
    fit <- object
    if (is.null(draw)) {
        draws <- min(100, nrow(fit[[1]]))
    }
    # Get parameter samples
    samples <- tidy_mcmc_list(fit)
    if (draws > nrow(samples)) {
        warning("Number of draws greater (", draws,
                ") than number of samples (", nrow(samples) ,").\n",
                "Setting number of draws to ", nrow(samples), ".")
        draws <- nrow(samples)
    }
    samples <- samples[sample(seq_len(nrow(samples)), draws), ]
    # Project rows for each set of parameter values
    nm$prediction <- purrr::map(seq_len(nrow(nm)), function(k) {
        nmRow <- nm[k, ]
        at <- unique(nmRow[["observations"]][[1]][["time"]])
        projections <- parallel::mclapply(seq_len(nrow(samples)), function(i) {
            nmRow <- set_params(nmRow, samples$mcmc.parameters[[i]],
                                force = TRUE)
            nmRow <- project(nmRow, at = at)
            pred <- sample_from(nmRow, at, error.draws = 1)
            pred$sample_id <- i
            return(pred)
        }, mc.cores = cores)
        # Concatenate data
        pred <- dplyr::bind_rows(projections)
        pred$group <- nmRow$group
        pred$group_str <- group2string(nmRow$group[[1]])
        return(pred)
    })
    # Return
    return(nm)
}

### * tidy_data()

#' Extract data from a networkModel object into a tidy tibble.
#'
#' @param x A networkModel object.
#'
#' @return A tibble (note: row ordering is not the same as in the input).
#'
#' @examples
#' tidy_data(aquarium_mod)
#' tidy_data(trini_mod)
#'
#' @export

tidy_data <- function(x) {
    # Remove "networkModel" class to avoid dplyr using groups.networkModel method
    stopifnot(class(x)[1] == "networkModel")
    stopifnot(length(class(x)) > 1)
    class(x) <- class(x)[2:length(class(x))]
    if (!"group" %in% colnames(x)) {
        x[["group"]] <- rep(list(NULL), nrow(x))
    }
    group_mapping <- x[["group"]]
    names(group_mapping) <- sapply(group_mapping, group2string)
    x[["group"]] <- names(group_mapping)
    obs <- tidyr::unnest(x[, c("observations", "group")], cols = "observations")
    tidy_obs <- tidyr::pivot_longer(obs, cols = c("size", "proportion"),
                                    names_to = "type", values_to = "value")
    tidy_obs <- tidy_obs[!is.na(tidy_obs[["value"]]), ]
    tidy_obs$group_str <- tidy_obs$group
    tidy_obs[["group"]] <- group_mapping[tidy_obs$group_str]
    tidy_obs <- tidy_obs[order(tidy_obs$compartment,
                               tidy_obs$time,
                               tidy_obs$group_str,
                               tidy_obs$type), ]
    tidy_obs <- tidy_obs[, c("compartment", "time", "group_str", "group",
                             "type", "value")]
    return(tidy_obs)
}

### * tidy_posterior_predict()

#' Draw from the posterior predictive distribution of the model outcome
#'
#' @param object A networkModelStanfit object.
#' @param draw Integer, number of draws to sample from the posterior. Default
#'     is 100.
#' @param newdata The original model used to fit the networkStanfit object.
#' @param cores Number of cores to use for parallel calculations. Default is
#'     \code{NULL}, which means to use the value stored in
#'     \code{options()[["mc.cores"]]} (or 1 if this value is not set).
#' @param ... Not used for now.
#' 
#' @return A tidy table.
#'
#' @export

tidy_posterior_predict <- function(object, newdata, draw = NULL, cores = NULL,
                                   ...) {
    `!!` <- rlang::`!!`
    # Process `cores` argument
    cores <- get_n_cores(cores = cores)
    # Process nm
    nm <- newdata
    if (!"group" %in% colnames(nm)) {
        nm[["group"]] <- rep(list(NULL), nrow(nm))
    }
    fit <- object
    if (is.null(draw)) {
        draws <- min(100, nrow(fit[[1]]))
    } else {
        draws <- draw
    }
    # Get parameter samples
    samples <- tidy_mcmc_list(fit)
    if (draws > nrow(samples)) {
        warning("Number of draws greater (", draws,
                ") than number of samples (", nrow(samples) ,").\n",
                "Setting number of draws to ", nrow(samples), ".")
        draws <- nrow(samples)
    }
    samples <- samples[sample(seq_len(nrow(samples)), draws), ]
    # Project rows for each set of parameter values
    nm$prediction <- purrr::map(seq_len(nrow(nm)), function(k) {
        nmRow <- nm[k, ]
        at <- unique(nmRow[["observations"]][[1]][["time"]])
        projections <- parallel::mclapply(seq_len(nrow(samples)), function(i) {
            nmRow <- set_params(nmRow, samples$mcmc.parameters[[i]],
                                force = TRUE)
            nmRow <- project(nmRow, at = at)
            pred <- sample_from(nmRow, at, error.draws = 1)
            pred$random_sample_id <- i
            return(pred)
        }, mc.cores = cores)
        # Concatenate data
        pred <- dplyr::bind_rows(projections)
        pred$group <- nmRow$group
        pred$group_str <- group2string(nmRow$group[[1]])
        return(pred)
    })
    # Return
    z <- dplyr::bind_rows(nm$prediction)
    z$compartment <- z$comp
    z$comp <- NULL
    z <- tidyr::pivot_longer(z, cols = c("size", "prop"), names_to = "type",
                             values_to = "value")
    z <- z[, c("compartment", "time", "group_str", "group",
               "type", "value", "random_sample_id")]
    z <- z[order(z$compartment, z$time, z$group_str, z$random_sample_id, z$type), ]
    z[["type"]][z[["type"]] == "prop"] <- "proportion"
    return(z)
}

### * tidy_dpp()

#' Prepare tidy data and posterior predictions
#'
#' This function prepares both tidy data from a model and tidy posterior
#' predictions from a model fit. Having those two tibbles prepared at the same
#' time allows to merge them to ensure that observed data, predicted data and
#' original variables other than observations are all in sync when using y and
#' y_rep objects for bayesplot functions.
#'
#' @param model A networkModel object.
#' @param fit A networkModelStanfit object.
#' @param draw Integer, number of draws to sample from the posterior.
#' @param cores Number of cores to use for parallel calculations. Default is
#'     \code{NULL}, which means to use the value stored in
#'     \code{options()[["mc.cores"]]} (or 1 if this value is not set).
#'
#' @return A list with y, y_rep and vars.
#'
#' @export

tidy_dpp <- function(model, fit, draw = NULL, cores = NULL) {
    `!!` <- rlang::`!!`
    td <- tidy_data(model)
    tpp <- tidy_posterior_predict(fit, model, draw = draw, cores = cores)
    tpp$group <- NULL
    template <- unique(td[, c("compartment", "time", "group_str", "type")])
    tpp <- dplyr::left_join(template, tpp, by = c("compartment", "time",
                                                  "group_str", "type"))
    # Arrange td and tpp similarly
    tpp <- dplyr::group_by(tpp,
                           `!!`(rlang::sym("compartment")),
                           `!!`(rlang::sym("time")),
                           `!!`(rlang::sym("group_str")),
                           `!!`(rlang::sym("type")))
    tpp <- tidyr::nest(tpp)
    all <- dplyr::left_join(td, tpp, by = c("compartment", "time", "group_str", "type"))
    # Extract y and y_rep
    y <- all$value
    y_rep <- lapply(all$data, function(x) {
        x <- x[["value"]][order(x[["random_sample_id"]])]
        x
    })
    y_rep <- do.call(cbind, y_rep)
    # Extract vars
    vars <- all[, c("compartment", "time", "type", "group")]
    groups <- vars[["group"]]
    groups <- do.call(rbind, groups)
    if (is.null(groups)) {
        vars[["group."]] <- rep(list(NULL), nrow(vars))
        vars[["group"]] <- NULL
    } else {
        groups_cols <- paste0("group.", colnames(groups))
        if (any(groups_cols %in% colnames(vars))) {
            stop("One group column name already used (one of ",
                 groups_cols, ")", "\n",
                 "Group column names cannot be: ", colnames(vars), "\n")
        }
        for (i in seq_along(groups_cols)) {
            vars[[groups_cols[i]]] <- groups[, colnames(groups)[i]]
        }
        vars[["groups"]] <- NULL
    }
    # Return
    out <- list(y = y,
                y_rep = y_rep,
                vars = vars)
    return(structure(out, class = c("ppcNetworkModel", class(out))))
}

### * filter.ppcNetworkModel() (generic and method)

# Precious help from:
# https://r.789695.n4.nabble.com/R-CMD-check-warning-with-S3-method-td4692255.html
# https://github.com/wch/s3methodtest/blob/master/R/test.r
# to understand how to provide a filter method without loading dplyr by default.

#' Filter (alias for filter function from dplyr)
#'
#' @param .data Data to filter.
#' @param ... Passed to dplyr::filter.
#' @param preserve Ignored.
#'
#' @return See the returned value for dplyr::filter.
#' 
#' @name filter
#' @importFrom dplyr filter
#' @export filter
#'
NULL

#' Filter method for output of tidy_data_and_posterior_predict()
#'
#' @param .data A ppcNetworkModel object.
#' @param ... Passed to dplyr::filter.
#' @param .preserve Ignored.
#'
#' @return A pccNetworkModel object filtered appropriately based on the
#'     [["vars"]] tibble.
#'
#' @importFrom dplyr filter
#' @method filter ppcNetworkModel
#' @export
#' 

filter.ppcNetworkModel <- function(.data, ..., .preserve = FALSE) {
    out <- .data
    out[["vars"]][["row_id"]] <- seq_len(nrow(out[["vars"]]))
    out[["vars"]] <- dplyr::filter(out[["vars"]], ...)
    out[["y"]] <- out[["y"]][out[["vars"]][["row_id"]]]
    out[["y_rep"]] <- out[["y_rep"]][, out[["vars"]][["row_id"]]]
    out[["vars"]][["row_id"]] <- NULL
    return(out)
}

### * tidy_trajectories()

#' Build a tidy table with the trajectories for each iteration
#'
#' If neither \code{n_per_chain} and \code{n} are provided, all iterations are
#' used.
#'
#' Warning: This function is still maturing and its interface and output might
#' change in the future.
#' 
#' @param nm A \code{networkModel} object.
#' @param mcmc The corresponding output from \code{run_mcmc}.
#' @param n_per_chain Integer, number of iterations randomly drawn per
#'     chain. Note that iterations are in sync across chains (in practice,
#'     random iterations are chosen, and then parameter values extracted for
#'     those same iterations from all chains).
#' @param n Integer, number of iterations randomly drawn from \code{mcmc}. Note
#'     that iterations are *not* drawn in sync across chains in this case (use
#'     \code{n_per_chain} if you need to have the same iterations taken across
#'     all chains).
#' @param n_grid Size of the time grid used to calculate trajectories
#' @param cores Number of cores to use for parallel calculations. Default is
#'     \code{NULL}, which means to use the value stored in
#'     \code{options()[["mc.cores"]]} (or 1 if this value is not set).
#' @param dt,grid_size Time step size or grid points, respectively.
#' @param at Timepoints at which the predictions should be returned.
#' @param end Final timepoint used in the projections.
#' @param use_cache Boolean, use cache for faster calculations?
#'
#' @return A tidy table containing the mcmc iterations (chain, iteration,
#'     parameters), the grouping variables from the network model and the
#'     trajectories.
#'
#' @examples
#' tt <- tidy_trajectories(aquarium_mod, aquarium_run, n = 10, cores = 2)
#' tt
#' 
#' @export

tidy_trajectories <- function(nm, mcmc, n_per_chain = NULL, n = NULL, n_grid = 64,
                              dt = NULL, grid_size = NULL, at = NULL, end = NULL,
                              use_cache = TRUE, cores = NULL) {
    cores <- get_n_cores(cores = cores)
    to <- tidy_mcmc_list(mcmc)
    arg_end <- end
    if (use_cache) {
        cache <- list()
        rows_time_schemes <- list()
        rows_encoded_events <- list()
        for (i in seq_len(nrow(nm))) {
            nmRow <- nm[i, ]
            end <- arg_end
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
            rows_time_schemes[[i]] <- nm_row_get_time_scheme(nm_row = nmRow, dt = dt,
                                                             grid_size = grid_size,
                                                             end = end, at = at)
            rows_encoded_events[[i]] <- encode_events(nmRow, end = end, dt = dt,
                                                      grid_size = grid_size)
        }
        cache[["rows_time_schemes"]] <- rows_time_schemes
        cache[["rows_encoded_events"]] <- rows_encoded_events
    } else {
        cache <- NULL
    }
    end <- arg_end
    if (!is.null(n_per_chain)) {
        my_iters <- sample(unique(to$mcmc.iteration), size = n_per_chain)
        to <- to[to$mcmc.iteration %in% my_iters, ]
    }
    if (!is.null(n)) {
        if (!is.null(n_per_chain)) {
            warning("Both \"n_per_chain\" and \"n\" are provided. Using \"n_per_chain\".")
        } else {
            to <- to[sample(1:nrow(to), size = n), ]
        }
    }
    # Run networks for each chain and each iteration
    trajectories <- purrr::map(seq_len(nrow(nm)), function(k) {
        nmRow <- nm[k, ]
        projections <- parallel::mclapply(seq_len(nrow(to)), function(i) {
            nmRow <- set_params(nmRow, to$mcmc.parameters[[i]], force = TRUE, quick = TRUE)
            nmRow <- project(nmRow, grid_size = n_grid, dt = dt, 
                             at = at, end = end, cached_ts = cache[["rows_time_schemes"]][k],
                             cached_ee = cache[["rows_encoded_events"]][k])
            return(nmRow$trajectory[[1]])
        }, mc.cores = cores)
        out <- to
        out$trajectories <- projections
        return(out)
    })
    # Unnest
    nm$trajectories <- trajectories
    if (is.null(groups(nm))) {
        my_out <- tidyr::unnest(nm[, "trajectories"], "trajectories")
    } else {
        # Careful unnesting to keep correct groups when there is more than
        # one grouping variable
        groups <- nm[["group"]]
        nm[["group"]] <- seq_len(nrow(nm))
        out <- tidyr::unnest(nm[, c("group", "trajectories")], "trajectories")
        group_list <- groups[out[["group"]]]
        out[["group"]] <- group_list
        my_out <- out
    }
    # Return
    return(my_out)
}

### * tidy_flows()

#' Build a tidy table with the flows for each iteration
#'
#' If neither \code{n_per_chain} and \code{n} are provided, all iterations are
#' used.
#'
#' Warning: This function is still maturing and its interface and output might
#' change in the future.
#'
#' Note about how steady state sizes for split compartments are calculated: the
#' steady size of the active portion is calculated divide it is divided by the
#' active fraction (portion.act parameter) to get the total size including the
#' refractory portion. In this case we get a "steady-state" refractory portion,
#' consistent with steady state size of active fraction and with portion.act
#' parameter.
#' 
#' @param nm A \code{networkModel} object.
#' @param mcmc The corresponding output from \code{run_mcmc}.
#' @param n_per_chain Integer, number of iterations randomly drawn per
#'   chain. Note that iterations are in sync across chains (in practice, random
#'   iterations are chosen, and then parameter values extracted for those same
#'   iterations from all chains).
#' @param n Integer, number of iterations randomly drawn from \code{mcmc}. Note
#'   that iterations are *not* drawn in sync across chains in this case (use
#'   \code{n_per_chain} if you need to have the same iterations taken across all
#'   chains).
#' @param n_grid Size of the time grid used to calculate trajectories
#' @param steady_state Boolean (default: FALSE). If TRUE, then steady state
#'   compartment sizes are calculated for each iteration and steady state flows
#'   are calculated from those compartment sizes. Note that any pulse that
#'   might be specified in the input model \code{nm} is ignored in this case.
#' @param cores Number of cores to use for parallel calculations. Default is
#'   \code{NULL}, which means to use the value stored in
#'   \code{options()[["mc.cores"]]} (or 1 if this value is not set).
#' @param dt,grid_size Time step size or grid points, respectively.
#' @param at Timepoints at which the predictions should be returned.
#' @param end Final timepoint used in the projections.
#' @param use_cache Boolean, use cache for faster calculations?
#'
#' @return A tidy table containing the mcmc iterations (chain, iteration,
#'     parameters), the grouping variables from the network model and the
#'     flows. The returned flow values are the average flow per unit of time
#'     over the trajectory calculations (or steady state flows if
#'     \code{steady_state} is TRUE).
#' 
#' @examples
#' tf <- tidy_flows(aquarium_mod, aquarium_run, n_per_chain = 25, cores = 2)
#' tf
#' tfmcmc <- as.mcmc.list(tf)
#' plot(tfmcmc)
#' 
#' @export

tidy_flows <- function(nm, mcmc, n_per_chain = NULL, n = NULL, n_grid = 64,
                       steady_state = FALSE,
                       dt = NULL, grid_size = NULL, at = NULL, end = NULL,
                       use_cache = TRUE, cores = NULL) {
    cores <- get_n_cores(cores = cores)
    to <- tidy_mcmc_list(mcmc)
    arg_end <- end
    if (use_cache) {
        cache <- list()
        rows_time_schemes <- list()
        rows_encoded_events <- list()
        for (i in seq_len(nrow(nm))) {
            nmRow <- nm[i, ]
            end <- arg_end
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
            rows_time_schemes[[i]] <- nm_row_get_time_scheme(nm_row = nmRow, dt = dt,
                                                             grid_size = grid_size,
                                                             end = end, at = at)
            rows_encoded_events[[i]] <- encode_events(nmRow, end = end, dt = dt,
                                                      grid_size = grid_size)
        }
        cache[["rows_time_schemes"]] <- rows_time_schemes
        cache[["rows_encoded_events"]] <- rows_encoded_events
    } else {
        cache <- NULL
    }
    end <- arg_end
    if (!is.null(n_per_chain)) {
        my_iters <- sample(unique(to$mcmc.iteration), size = n_per_chain)
        to <- to[to$mcmc.iteration %in% my_iters, ]
    }
    if (!is.null(n)) {
        if (!is.null(n_per_chain)) {
            warning("Both \"n_per_chain\" and \"n\" are provided. Using \"n_per_chain\".")
        } else {
            to <- to[sample(1:nrow(to), size = n), ]
        }
    }
    # Run networks for each chain and each iteration
    if (!steady_state) {
        flows <- purrr::map(seq_len(nrow(nm)), function(k) {
            nmRow <- nm[k, ]
            flows <- parallel::mclapply(seq_len(nrow(to)), function(i) {
                nmRow <- set_params(nmRow, to$mcmc.parameters[[i]], force = TRUE, quick = TRUE)
                nmRow <- project(nmRow, flows = "average", grid_size = n_grid, dt = dt, 
                                 at = at, end = end, cached_ts = cache[["rows_time_schemes"]][k],
                                 cached_ee = cache[["rows_encoded_events"]][k])
                return(nmRow$flows[[1]])
            }, mc.cores = cores)
            out <- to
            out$flows <- flows
            return(out)
        })
    } else {
      flows <- purrr::map(seq_len(nrow(nm)), function(k) {
            nmRow <- nm[k, ]
            flows <- parallel::mclapply(seq_len(nrow(to)), function(i) {
                nmRow_to <- set_params(nmRow, to$mcmc.parameters[[i]], force = TRUE, quick = TRUE)
                ss_sizes <- calculate_steady_state_one_row(nmRow_to)
                ss_inits <- tibble::tibble(compartment = names(ss_sizes),
                                           size = ss_sizes,
                                           proportion = 0)
                nmRow_to$initial <- list(ss_inits)
                nmRow_to <- project(nmRow_to, grid_size = 3, flows = "average",
                                    ignore_pulses = TRUE)
                return(nmRow_to$flows[[1]])
            }, mc.cores = cores)
            out <- to
            out$flows <- flows
            return(out)
        })
    }
    # Unnest
    nm$flows <- flows
    if (is.null(groups(nm))) {
        my_out <- tidyr::unnest(nm[, "flows"], "flows")
    } else {
        # Careful unnesting to keep correct groups when there is more than
        # one grouping variable
        groups <- nm[["group"]]
        nm[["group"]] <- seq_len(nrow(nm))
        out <- tidyr::unnest(nm[, c("group", "flows")], "flows")
        group_list <- groups[out[["group"]]]
        out[["group"]] <- group_list
        my_out <- out
    }
    # Add a "tidy_flows" class
    return(structure(my_out, class = c("tidy_flows", class(my_out))))
}

### * as.mcmc.list.tidy_flows() method

#' Convert a \code{tidy_flows} object to an \code{mcmc.list}
#'
#' @param x A tidy flow object, as returned by \code{\link{tidy_flows}}. Note
#'     that all chains must have the same iterations extracted (i.e. you must
#'     use \code{n_per_chain} when calling \code{\link{tidy_flows}}).
#' @param ... Not used for now.
#'
#' @return A \code{mcmc.list} object, with ordered iterations.
#'
#' @method as.mcmc.list tidy_flows
#' 
#' @export

as.mcmc.list.tidy_flows <- function(x, ...) {
    `!!` <- rlang::`!!`
    if (!length(unique(table(x$mcmc.iteration))) == 1 |
        all(table(x$mcmc.iteration) == 1)) {
        stop("Not all chains have the same iterations in \"x\". Make sure you use \"n_per_chain\" (with a value > 1) and not \"n\" when calling \"tidy_flows()\".")
    }
    groups <- x[["group"]]
    if (!is.null(groups)) {
        groups <- sapply(groups, function(z) paste0(z, collapse = ","))
    }
    # Convert flow tibbles to named vectors
    for (i in seq_len(nrow(x))) {
        f <- x$flows[[i]]
        grp_suffix <- ""
        if (!is.null(groups)) {
            grp_suffix <- paste0("|", groups[i])
        }
        params <- paste0(f$from, "_to_", f$to, grp_suffix)
        f <- setNames(f$average_flow, nm = params)
        x$flows[[i]] <- f
    }
    # Pool parameter values per iteration per chain
    x <- dplyr::group_by(x[, c("mcmc.chain", "mcmc.iteration", "flows")],
                         `!!`(rlang::sym("mcmc.chain")),
                         `!!`(rlang::sym("mcmc.iteration")))
    x <- tidyr::nest(x)
    x$data <- lapply(x$data, function(z) unlist(z$flows))
    x <- dplyr::ungroup(x)
    x <- tidyr::nest(dplyr::group_by(x, `!!`(rlang::sym("mcmc.chain"))))
    x$data <- lapply(x$data, function(z) {
        z <- z[order(z$mcmc.iteration), ]
        params <- do.call(rbind, z$data)
        coda::as.mcmc(params)
    })
    # Return
    out <- coda::as.mcmc.list(x$data)
    return(structure(out, class = c("tidy_flows_mcmc.list", class(out))))
}


### * sample_params()

#' Sample parameter values from priors
#'
#' @param nm A \code{networkModel} object.
#'
#' @return A named vector containing parameter values.
#'
#' @examples
#' library(magrittr)
#' 
#' p <- sample_params(aquarium_mod)
#' p
#' 
#' proj <- aquarium_mod %>% set_params(p) %>% project(end = 10)
#' plot(proj)
#' 
#' @export

sample_params <- function(nm) {
    p <- priors(nm)
    p$value <- sapply(p$prior, function(x) sample_from_prior(x, n = 1))
    return(tibble::deframe(p[, c("in_model", "value")]))
}

### * tidy_steady_states()

#' Build a tidy table with the calculated steady states for each iteration
#'
#' If neither \code{n_per_chain} and \code{n} are provided, all iterations are
#' used.
#'
#' Note about how steady state sizes for split compartments are calculated: the
#' steady size of the active portion is calculated divide it is divided by the
#' active fraction (portion.act parameter) to get the total size including the
#' refractory portion. In this case we get a "steady-state" refractory portion,
#' consistent with steady state size of active fraction and with portion.act
#' parameter.
#' 
#' @param nm A \code{networkModel} object.
#' @param mcmc The corresponding output from \code{run_mcmc}.
#' @param n_per_chain Integer, number of iterations randomly drawn per
#'     chain. Note that iterations are in sync across chains (in practice,
#'     random iterations are chosen, and then parameter values extracted for
#'     those same iterations from all chains).
#' @param n Integer, number of iterations randomly drawn from \code{mcmc}. Note
#'     that iterations are *not* drawn in sync across chains in this case (use
#'     \code{n_per_chain} if you need to have the same iterations taken across
#'     all chains).
#'
#' @return A tidy table containing the mcmc iterations (chain, iteration,
#'     parameters), the grouping variables from the network model and the
#'     steady state sizes.
#' 
#' @export

tidy_steady_states <- function(nm, mcmc, n_per_chain = NULL, n = NULL) {
    to <- tidy_mcmc_list(mcmc)
    if (!is.null(n_per_chain)) {
        my_iters <- sample(unique(to$mcmc.iteration), size = n_per_chain)
        to <- to[to$mcmc.iteration %in% my_iters, ]
    }
    if (!is.null(n)) {
        if (!is.null(n_per_chain)) {
            warning("Both \"n_per_chain\" and \"n\" are provided. Using \"n_per_chain\".")
        } else {
            to <- to[sample(1:nrow(to), size = n), ]
        }
    }
    # Calculate steady states for each chain and each iteration
    ss_sizes <- purrr::map(seq_len(nrow(nm)), function(k) {
        nmRow <- nm[k, ]
        sizes <- list()
        for (i in seq_len(nrow(to))) {
            nmRow <- set_params(nmRow, to$mcmc.parameters[[i]], force = TRUE)
            sizes[[i]] <- calculate_steady_state_one_row(nmRow)
        }
        out <- to
        out$stable_sizes <- sizes
        return(out)
    })
    # Unnest
    nm$stable_sizes <- ss_sizes
    if (is.null(groups(nm))) {
        my_out <- tidyr::unnest(nm[, "stable_sizes"], "stable_sizes")
    } else {
        # Careful unnesting to keep correct groups when there is more than
        # one grouping variable
        groups <- nm[["group"]]
        nm[["group"]] <- seq_len(nrow(nm))
        out <- tidyr::unnest(nm[, c("group", "stable_sizes")], "stable_sizes")
        group_list <- groups[out[["group"]]]
        out[["group"]] <- group_list
        my_out <- out
    }
    # Add a "tidy_steady_states" class
    return(structure(my_out, class = c("tidy_steady_states", class(my_out))))
}

### * as.mcmc.list.tidy_steady_states() method

#' Convert a \code{tidy_steady_states} object to an \code{mcmc.list}
#'
#' @param x A tidy steady states object, as returned by
#'     \code{\link{tidy_steady_states}}. Note that all chains must have the
#'     same iterations extracted (i.e. you must use \code{n_per_chain} when
#'     calling \code{\link{tidy_flows}}).
#' @param ... Not used for now.
#'
#' @return A \code{mcmc.list} object, with ordered iterations.
#'
#' @method as.mcmc.list tidy_steady_states
#' 
#' @export

as.mcmc.list.tidy_steady_states <- function(x, ...) {
    `!!` <- rlang::`!!`
    if (!length(unique(table(x$mcmc.iteration))) == 1 |
        all(table(x$mcmc.iteration) == 1)) {
        stop("Not all chains have the same iterations in \"x\". Make sure you use \"n_per_chain\" (with a value > 1) and not \"n\" when calling \"tidy_flows()\".")
    }
    groups <- x[["group"]]
    if (!is.null(groups)) {
        groups <- sapply(groups, function(z) paste0(z, collapse = ","))
    }
    # Convert steady state names to incorporate group name
    for (i in seq_len(nrow(x))) {
        f <- x$stable_sizes[[i]]
        grp_suffix <- ""
        if (!is.null(groups)) {
            grp_suffix <- paste0("|", groups[i])
        }
        params <- paste0(names(f), grp_suffix)
        f <- setNames(f, nm = params)
        x$stable_sizes[[i]] <- f
    }
    # Pool parameter values per iteration per chain
    x <- dplyr::group_by(x[, c("mcmc.chain", "mcmc.iteration", "stable_sizes")],
                         `!!`(rlang::sym("mcmc.chain")),
                         `!!`(rlang::sym("mcmc.iteration")))
    x <- tidyr::nest(x)
    x$data <- lapply(x$data, function(z) unlist(z$stable_sizes))
    x <- dplyr::ungroup(x)
    x <- tidyr::nest(dplyr::group_by(x, `!!`(rlang::sym("mcmc.chain"))))
    x$data <- lapply(x$data, function(z) {
        z <- z[order(z$mcmc.iteration), ]
        params <- do.call(rbind, z$data)
        coda::as.mcmc(params)
    })
    # Return
    out <- coda::as.mcmc.list(x$data)
    return(structure(out, class = c("tidy_flows_mcmc.list", class(out))))
}

### * tidy_mcmc()

#' Extract a tidy output from an mcmc.list
#'
#' @param x An mcmc.list object
#' @param spread Boolean, spread the parameters into separate columns?
#' @param include_constant Boolean, include constant parameters as proper
#'     parameter traces?
#'
#' @return A tidy table containing one iteration per row
#'
#' @examples
#' fit <- lapply(1:4, function(i) {
#'   z <- matrix(rnorm(200), ncol = 2)
#'   colnames(z) <- c("alpha", "beta")
#'   coda::as.mcmc(z)
#' })
#' fit <- coda::as.mcmc.list(fit)
#' tidy_mcmc(fit)
#' tidy_mcmc(fit, spread = TRUE)
#'
#' @export

tidy_mcmc <- function(x, spread = FALSE, include_constant = TRUE) {
    z <- x
    n_iters <- nrow(z[[1]])
    constant_params <- attr(z, "constant_params")
    # Add traces for constant params (if needed)
    if (!is.null(constant_params) & include_constant) {
        constant_template <- matrix(NA, ncol = length(constant_params),
                                    nrow = n_iters)
        for (i in seq_along(constant_params)) {
            constant_template[, i] <- constant_params[i]
        }
        colnames(constant_template) <- names(constant_params)
        for (i in seq_along(z)) {
            zi <- coda::as.mcmc(cbind(z[[i]], constant_template))
            attr(zi, "mcpar") <- attr(z[[i]], "mcpar")
            z[[i]] <- zi
        }
    }
    paramNames <- colnames(z[[1]])
    # If spread = FALSE
    if (!spread) {
        tables <- lapply(seq_along(z), function(i) {
            my_chain <- as.matrix(z[[i]])
            tibble::tibble(mcmc.chain = i,
                           mcmc.iteration = seq_len(nrow(my_chain)),
                           mcmc.parameters = lapply(seq_len(nrow(my_chain)),
                                                    function(k) {
                                                        setNames(my_chain[k,], paramNames)
                                                    })
                           )
        })
        out <- dplyr::bind_rows(tables)
        return(out)
    }
    # If spread = TRUE
    mcmc_parameters <- lapply(seq_along(z), function(i) {
        my_chain <- tibble::as_tibble(as.matrix(z[[i]]))
        mcmc_pars <- tibble::tibble(mcmc.chain = i,
                                    mcmc.iteration = seq_len(nrow(my_chain)))
        dplyr::bind_cols(mcmc_pars, my_chain)
    })
    out <- dplyr::bind_rows(mcmc_parameters)
    return(out)
}

