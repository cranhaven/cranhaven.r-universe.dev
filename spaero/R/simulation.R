#' Create surveillance data simulator.
#'
#' \code{create_simulator} creates a pomp object that will run
#' simulations of an SIR or SIS model according to Gillespie's direct
#' method and generate simulated observations of the process.
#'
#' See the vignette "Getting Started with spaero" for a description of
#' the model. The "params" argument must include all model
#' parameters. These will become the default parameters for the model
#' object. They can be overridden when the simulation is run via the
#' "params" argument of \code{pomp::simulate}. The case is the same
#' for the "times" argument. The "covar" argument should be a data
#' frame with a column named for each of the time-dependent parameters
#' and a column named time. This data frame describes the time series
#' of each of the time-dependent parameters. In the simulation,
#' interpolation based on this data frame determines the value of
#' these parameters at specific instants in time. The user must ensure
#' that these values result in the parameters remaining non-negative
#' for the course of the simulation.
#'
#' @return A pomp object with which simulations can be run via
#' \code{pomp::simulate}.
#' @param times A numeric vector of increasing times at which the
#' state of the simulation will be sampled.
#' @param t0 The time at which the simulation is started with state
#' variable set to the initial conditions specified via params.
#' @param process_model Character string giving the process
#' model. Allowed values are '"SIR"' and '"SIS"'.
#' @param transmission Character string describing the transmission
#' model. Allowed values are '"density-dependent"' and
#' '"frequency-dependent"'.
#' @param params A named numeric vector of parameter values and
#' initial conditions.
#' @param covar A data frame containing values of the time-dependent
#' components of the parameters.
#'
#' @seealso \code{\link[pomp]{pomp}} for documentation of pomp objects
#' @export
#' @examples
#'
#' foo <- create_simulator()
#' out <- pomp::simulate(foo, times = seq(0, 20, by = 1/26))
#' out <- as(out, "data.frame")
#' head(out)
#'
#' opar <- par(mfrow = c(2, 1))
#' plot((S/N)~time, data = out, type = "l")
#' plot(cases~time, data = out, type = "l")
#' par(opar)
#'
create_simulator <- function(times = seq(0, 9), t0 = min(times),
                             process_model = c("SIR", "SIS"),
                             transmission = c("density-dependent",
                                 "frequency-dependent"),
                             params = c(gamma = 24, mu = 1 / 70, d = 1 / 70,
                                 eta = 1e-5, beta_par = 1e-4, rho = 0.1,
                                 S_0 = 1, I_0 = 0, R_0 = 0, N_0 = 1e5, p = 0),
                             covar = data.frame(gamma_t = c(0, 0),
                                 mu_t = c(0, 0), d_t = c(0, 0), eta_t = c(0, 0),
                                 beta_par_t = c(0, 0), p_t = c(0, 0),
                                 time = c(0, 1e6))) {
  process_model <- match.arg(process_model)
  transmission <- match.arg(transmission)
  if (!requireNamespace("pomp", quietly = TRUE)) {
    stop(paste("The pomp package is needed for this function to work.",
               "Please install it."),
         call. = FALSE)
  }
  if (transmission == "density-dependent") {
    if (process_model == "SIR") {
      template <- dendep_sir_template
    } else {
      template <- dendep_sis_template
    }
  } else if (transmission == "frequency-dependent") {
    if (process_model == "SIR") {
      template <- freqdep_sir_template
    } else {
      template <- freqdep_sis_template
    }
  }

  rprocess <- do.call(pomp::gillespie_hl, template)
  covartab <- pomp::covariate_table(covar, times = "time")
  rinit <- paste("double m = N_0/(S_0 + I_0 + R_0); S = nearbyint(m*S_0);",
                 "I = nearbyint(m*I_0); R = nearbyint(m*R_0); N = N_0;",
                 "cases = 0;")
  rmeas <- "reports = rbinom(cases, rho);"
  pomp::simulate(times = times, t0 = t0, params = params,
                 paramnames = names(params), rprocess = rprocess,
                 rmeasure = pomp::Csnippet(rmeas),
                 rinit = pomp::Csnippet(rinit), covar = covartab,
                 statenames = c("S", "I", "R", "N", "cases"),
                 accumvars = "cases", obsnames = "reports")
}


dendep_sir_template <- list(
    birthS=list("rate = N_0 * (mu + mu_t) * (1 - (p + p_t));",
        c(S=1, I = 0, R = 0, N = 1, cases = 0)),
    infectS=list("rate = ((beta_par + beta_par_t) * I + (eta + eta_t)) * S;",
        c(S=-1, I=1, R = 0, N = 0, cases = 0)),
    recoverI=list("rate = (gamma + gamma_t) * I;",
        c(S = 0, I=-1, R=1, N = 0, cases = 1)),
    deathS=list("rate = (d + d_t) * S;",
        c(S=-1, N = -1, R = 0, I = 0, cases = 0)),
    deathI=list("rate = (d + d_t) * I;",
        c(I=-1, N = -1, S = 0, R = 0, cases = 0)),
    deathR=list("rate = (d + d_t) * R ;",
        c(R=-1, N = -1, S = 0, I = 0, cases = 0)),
    vaccinate=list("rate = N_0 * (mu + mu_t) * (p + p_t);",
        c(R=1, N = 1, S = 0, I = 0, cases = 0)))

dendep_sis_template <- dendep_sir_template
dendep_sis_template$recoverI[[2]] <- c(S = 1, I=-1, R=0, N = 0, cases = 1)

freqdep_sir_template <- dendep_sir_template
freqdep_sis_template <- dendep_sis_template
freqdep_sis_template$infectS[[1]] <- freqdep_sir_template$infectS[[1]] <-
  "rate = ((beta_par + beta_par_t) * I / N + (eta + eta_t)) * S;"
