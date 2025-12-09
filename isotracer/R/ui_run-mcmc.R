### * All functions in this file are exported

### * run_mcmc()

### ** Doc

#' Run a MCMC sampler on a network model using Stan
#'
#' @param model A \code{networkModel}.
#' @param iter A positive integer specifying the number of iterations for each
#'   chain (including warmup). The default is 2000.
#' @param chains A positive integer specifying the number of Markov chains.
#'   The default is 4.
#' @param method A character string indicating the method to use to solve ODE
#'   in the Stan model; available methods are "matrix_exp" and "euler". The
#'   default is "matrix_exp", which uses matrix exponential and is reasonably
#'   fast for small networks. For large networks, the "euler" method can be
#'   used. It implements a simple forward Euler method to solve the ODE and can
#'   be faster than the matrix exponential approach, but extra caution must be
#'   taken to check for numerical accuracy (e.g. testing different \code{dt}
#'   time step values, ensuring that the product between \code{dt} and the
#'   largest transfer rates expected from the priors is always very small
#'   compared to 1).
#' @param euler_control An optional list containing extra parameters when using
#'   \code{method = "euler"}. Allowed list elements are \code{"dt"} and
#'   \code{"grid_size"}, which are respectively the time step size for
#'   trajectory calculations (\code{"dt"}) or the number of points for the
#'   calculation (\code{"grid_size"}). Only one of "dt" or "grid_size" can be
#'   specified, not both. If none is provided, a default grid size of 256 steps
#'   is used.
#' @param cores Number of cores to use for parallel use. Default is
#'   \code{NULL}, which means to use the value stored in
#'   \code{options()[["mc.cores"]]} (or 1 if this value is not set).
#' @param stanfit If TRUE, returns a `stanfit` object instead of the more
#'   classical `mcmc.list` object. Note that when an `mcmc.list` object is
#'   returned, the original `stanfit` object is still accessible as an
#'   attribute of that object (see Examples).
#' @param vb Boolean, if TRUE will use \code{rstan::vb} for a quick approximate
#'   sampling of the posterior. Important note from \code{?rstan::vb}:
#'   "This is still considered an experimental feature.  We recommend calling
#'   \code{stan} or \code{sampling} for final inferences and only using ‘vb’ to
#'   get a rough idea of the parameter distributions."
#' @param ... Arguments passed to `rstan::sampling` (e.g. iter, chains).
#'
#' @return An object of class `stanfit` returned by `rstan::sampling` if
#'   \code{stanfit = TRUE}, otherwise the result of converting this
#'   \code{stanfit} object with \code{stanfit_to_named_mcmclist} (i.e. an object
#'   of class \code{networkModelStanfit} and \code{mcmc.list}, which still
#'   carries the original `stanfit` object stored as an attribute).
#'
#' @examples
#' aquarium_mod
#' \dontrun{
#'   # The 'aquarium_run' object is shipped with the package, so you don't
#'   # actually need to run the line below to obtain it
#'   aquarium_run <- run_mcmc(aquarium_mod)
#' 
#'   plot(aquarium_run)
#'   summary(aquarium_run)
#'
#'   # The original stanfit object returned by Stan
#'   sfit <- attr(aquarium_run, "stanfit")
#'   sfit
#'
#'   # The stanfit object can be used for diagnostics, LOO cross-validation, etc.
#'   rstan::loo(sfit)
#' }
#' 
#' @export

### ** Code

run_mcmc <- function(model, iter = 2000, chains = 4, method = "matrix_exp",
                     euler_control = list(),
                     cores = NULL, stanfit = FALSE, vb = FALSE, ...) {
    stopifnot(method %in% c("matrix_exp", "euler"))
    if (method != "euler" & length(euler_control) != 0) {
        stop("The `euler_control` parameter is not empty, but `method` is not set to \"euler\".")
    }
    if (method == "euler") {
      if (vb) {
        stop("vb not implemented for euler method")
      }
        fit <- mugen_stan(nm = model, iter = iter, chains = chains,
                          euler_control = euler_control,
                          cores = cores, stanfit = stanfit, ...)
    }
    if (method == "matrix_exp") {
        fit <- matrix_exp_stan(nm = model, iter = iter, chains = chains,
                               cores = cores, stanfit = stanfit,
                               vb = vb, ...)
    }
    return(fit)
}

### * stanfit_to_named_mcmclist()

#' Convert a Stanfit object to a nicely named mcmc.list object
#'
#' When running \code{run_mcmc} with \code{stanfit = FALSE} (typically for
#' debugging purposes), the parameters in the returned \code{stanfit} object
#' are named using a base label and an indexing system. This function provides
#' a way to convert this \code{stanfit} object into a more conventional
#' \code{mcmc.list} object where parameters are named according to their role
#' in the original network model used when running \code{run_mcmc}.
#' 
#' @param stanfit A stanfit object returned by \code{rstan::sampling}.
#' 
#' @return An \code{mcmc.list} object. It also has the original stanfit object
#'   stored as an attribute \code{"stanfit"}.
#'
#' @export

stanfit_to_named_mcmclist <- function(stanfit) {
    stan_data <- attr(stanfit, "isotracer_stan_data")
    fit <- stanfit
    # Get mcpars
    start <- fit@sim[["warmup"]] + 1
    end <- fit@sim[["iter"]]
    thin <- fit@sim[["thin"]]
    n_kept <- fit@sim[["n_save"]] - fit@sim[["warmup2"]]
    mcpars <- c(start, end, thin)
    # Prepare the mcmc.list object
    out <- rstan::As.mcmc.list(fit)
    for (i in seq_along(out)) {
        stopifnot(nrow(out[[i]]) == n_kept)
    }
    attr(out, "mcpar") <- mcpars
    rawNames <- coda::varnames(out)
    nonConstantParamNames <- rawNames[grepl("^nonConstantParams[\\[\\.]",
                                            rawNames)]
    loglikNames <- rawNames[grepl("^log_lik[\\[\\.]", rawNames)]
    ll <- out[, loglikNames]
    out <- out[, nonConstantParamNames]
    coda::varnames(out) <- stan_data[["allParams"]][stan_data[["mappingParamPriorType"]] != 0]
    llTrace <- lapply(ll, function(x) {
        out <- coda::as.mcmc(apply(as.matrix(x), 1, sum))
        attr(out, "mcpar") <- attr(x, "mcpar")
        return(out)
    })
    llTrace <- coda::as.mcmc.list(llTrace)
    attr(out, "loglik") <- llTrace
    attr(out, "mcpar") <- mcpars
    # Add values of constant parameters (if any)
    n_constant_params <- sum(stan_data[["mappingParamPriorType"]] == 0)
    if (n_constant_params > 0) {
        constant_params <- stan_data[["allParams"]][stan_data[["mappingParamPriorType"]] == 0]
        constant_values <- stan_data[["constantParams"]][stan_data[["mappingParamPriorType"]] == 0]
        constant_params <- setNames(constant_values, nm = constant_params)
        attr(out, "constant_params") <- constant_params
    }
    # Return the mcmc.list object
    outClass <- c("networkModelStanfit", class(out))
    attr(out, "stanfit") <- stanfit
    return(structure(out, class = outClass))
}
