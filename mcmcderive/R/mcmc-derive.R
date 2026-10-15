#' MCMC Derive
#'
#' Generates an MCMC object with derived parameters from an MCMC object.
#'
#' It's important to note that parameters in the expression that also
#' occur in the original object are not included in the new object
#' unless `primary = TRUE` in which case they are simply copied from the
#' original object to the new one.
#' This applies even when the primary parameters are redefined in values.
#'
#' @param object An MCMC object.
#' @param expr A string of the R code defining the values of the
#' derived parameter(s) with respect to the parameters in object.
#' @param values A named list of additional R objects to evaluate in the R expression.
#' @param monitor A regular expression specifying the
#' derived parameter(s) in expr to monitor.
#' @param primary A flag specifying whether to include the original primary
#' parameters in the new MCMC object.
#' @param parallel A flag specifying whether to generate the derived parameters
#' for each chain in parallel.
#' @param silent A flag specifying whether to suppress messages and warnings.
#' @param ... Unused.
#' @return An MCMC object with the derived parameter(s).
#' @export
#' @examples
#' mcmcr::mcmcr_example
#'
#' expr <- "
#'  log(alpha2) <- alpha
#'  gamma <- sum(alpha) * sigma"
#'
#' mcmc_derive(mcmcr::mcmcr_example, expr, silent = TRUE)
mcmc_derive <- function(object, ...) {
  UseMethod("mcmc_derive")
}

#' @describeIn mcmc_derive Get derived parameters for an [nlist::nlist-object()]
#' @export
mcmc_derive.nlist <- function(object, expr, values = list(), monitor = ".*",
                              primary = FALSE, silent = getOption("mcmcderive.silent", FALSE), ...) {
  chk_unused(...)
  object <- as.mcmcr(object)
  object <- mcmc_derive(object,
    expr = expr, values = values,
    monitor = monitor, primary = primary, silent = silent
  )
  nlist::as_nlist(object)
}

#' @describeIn mcmc_derive Get derived parameters for an [nlist::nlists-object()]
#' @export
mcmc_derive.nlists <- function(object, expr, values = list(), monitor = ".*",
                               primary = FALSE, silent = getOption("mcmcderive.silent", FALSE), ...) {
  chk_unused(...)
  object <- as.mcmcr(object)
  object <- mcmc_derive(object,
    expr = expr, values = values,
    monitor = monitor, primary = primary, silent = silent
  )
  nlist::as_nlists(object)
}

#' @describeIn mcmc_derive Get derived parameters for an [coda::mcmc()] object
#' @export
mcmc_derive.mcmc <- function(object, expr, values = list(), monitor = ".*",
                             primary = FALSE, silent = getOption("mcmcderive.silent", FALSE), ...) {
  chk_unused(...)
  object <- as.mcmcr(object)
  object <- mcmc_derive(object,
    expr = expr, values = values,
    monitor = monitor, primary = primary, silent = silent
  )
  coda::as.mcmc(object)
}

#' @describeIn mcmc_derive Get derived parameters for an [coda::mcmc.list()] object
#' @export
mcmc_derive.mcmc.list <- function(object, expr, values = list(), monitor = ".*",
                                  primary = FALSE, parallel = FALSE,
                                  silent = getOption("mcmcderive.silent", FALSE), ...) {
  chk_unused(...)
  object <- as.mcmcr(object)
  object <- mcmc_derive(object,
    expr = expr, values = values,
    monitor = monitor, primary = primary,
    parallel = parallel, silent = silent
  )
  coda::as.mcmc.list(object)
}

#' @describeIn mcmc_derive Get derived parameters for an [mcmcr::mcmcr-object()]
#' @export
mcmc_derive.mcmcr <- function(object, expr, values = list(), monitor = ".*",
                              primary = FALSE, parallel = FALSE,
                              silent = getOption("mcmcderive.silent", FALSE), ...) {
  chk_string(expr)
  chk_list(values)
  if (length(values)) {
    chk_named(values)
    chk_unique(names(values))
  }
  chk_string(monitor)
  chk_flag(primary)
  chk_flag(parallel)
  chk_flag(silent)
  chk_unused(...)

  original <- object
  if (length(values)) {
    object <- drop_overridden_parameters(object, values, silent = silent)
    values <- drop_absent_values(values, expr, silent = silent)
  }
  object <- drop_absent_parameters(object, expr, silent = silent)
  values <- add_new_variables(values, object, expr, silent = silent)
  monitor <- monitor_variables(monitor, values)

  object <- split_apply_combine(object, expr, values, monitor, parallel)

  if (primary) {
    original <- subset(original, pars = setdiff(pars(original), pars(object)))
    object <- bind_parameters(object, original)
  }
  object
}

#' @describeIn mcmc_derive Get derived parameters for an [mcmcr::mcmcrs-object()]
#' @export
mcmc_derive.mcmcrs <- function(object, expr, values = list(), monitor = ".*",
                               primary = FALSE, parallel = FALSE, silent = getOption("mcmcderive.silent", FALSE), ...) {
  chk_unused(...)
  object <- lapply(object, mcmc_derive,
    expr = expr, values = values,
    monitor = monitor, primary = primary, parallel = parallel,
    silent = silent
  )
  as.mcmcrs(object)
}
