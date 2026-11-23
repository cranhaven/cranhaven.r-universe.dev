# S4 class, constructors and methods for simulating survival times
# Call function from utils.R
# Last update on 082320

setClassUnion("numericORNULL", c("numeric", "NULL"))
setClassUnion("charORNULL", c("character", "NULL"))
setClassUnion("listORNULL", c("list", "NULL"))
`%notin%` <- Negate(`%in%`)

#' S4 Class for specifying parameters for enrollment time, drop-out pattern and analysis start time
#'
#' @keywords class
.clinClass = setClass(".clinClass", slots=list(gamma ="numeric", e_itv = "numericORNULL", # enroll
                                               CCOD ="character", CCOD_t = "numeric",
                                               etaC = "numeric", etaE = "numeric", d_itv = "numericORNULL"))


#' Specify parameters for enrollment time, drop-out pattern and analysis start time
#'
#' This function allows user to specify the enrollment and drop-out rate, and the type of clinical cut-off Date. Both enrollment times and drop-out times follow piece-wise exponential distribution.
#'
#' @param gamma A vector of rate of enrollment per unit of time
#' @param e_itv A vector of duration of time periods for recruitment with rates specified in \code{gamma}. Note that the length of \code{e_itv} should be same length as \code{gamma} or 1 less.
#' @param CCOD Type of analysis start time. Analysis starts at \code{CCOD_t} months after the first or last patient's enrollment if \code{CCOD = "fixed-first"} or \code{CCOD = "fixed-last"} respectively. Analysis starts when \code{CCOD_t} events have been observed if \code{CCOD = "event"}
#' @param CCOD_t Time difference between analysis start and first patient's enrollment if \code{CCOD = "fixed-first"}. Time difference between analysis start and last patient's enrollment if \code{CCOD = "fixed-last"}. Number of events observed when analysis starts if \code{CCOD = "event"}. Patients enrolled after the analysis start time are excluded from the analysis
#' @param etaC A vector for dropout rate per unit time for control arm
#' @param etaE A vector for dropout rate per unit time for experimental arm. If left \code{NULL}, it uses the same dropout rate as eta.
#' @param d_itv A vector of duration of time periods for dropping out the study with rates specified in \code{etaC} and \code{etaE}. Note that the length of \code{d_itv} should be same length as \code{etaC} or 1 less.
#' @return A \code{.clinClass} class containing information on enrollment time, drop-out pattern and analysis start time
#'
#'
#' @examples
#' # set the operational parameter values for the trial
#' # analysis starts at64 time units after first patient in
#' set_clin(gamma = 10, e_itv = 4, etaC = 0.003,  CCOD = "fixed-first", CCOD_t = 64)
#'
#' # analysis starts at 12 time units after last patient in
#' set_clin(gamma = 2, e_itv = 18, etaC = 0.005,  CCOD = "fixed-last", CCOD_t = 12)
#'
#'
#' @export
#' @keywords constructor
set_clin <- function(gamma, e_itv, CCOD, CCOD_t, etaC, etaE, d_itv) {
  if (missing(gamma)) stop("Please indicate the rate of enrollment per unit of time (gamma)")

  if (missing(e_itv)) e_itv = NULL
  if (length(e_itv) != length(gamma) & length(e_itv) != length(gamma) - 1) stop("The length of e_itv should be the same length as gamma or 1 less")

  if (missing(CCOD) || CCOD %notin% c("fixed-first", "fixed-last", "event")){
    stop("Please indicate type of clinical cut-off date (CCOD). Options include fixed-first, fixed-last, or event")
  }
  if (missing(CCOD_t)){
    if(CCOD == "fixed-first") stop("Please specify the time difference between analysis start and first patient's enrollment (CCOD_t).")
    else if (CCOD == "fixed-last") stop("Please specify the time difference between analysis start and first last's enrollment (CCOD_t)")
    else stop("Please specify the number of events observed when analysis starts (CCOD_t)")
  }

  if (missing(etaC)) stop("Please specify dropout rate per unit time for control arm (etaC).")
  if (missing(etaE) || length(etaC) != length(etaE)) {
    ps_message("Dropout rate per unit time for treatment arm (etaE) is not specified correcly. Disregard this warning if this is for external trial. Otherwise the same dropout rate as eta is used.")
    etaE = etaC
  }
  if(missing(d_itv)) d_itv = NULL
  if (length(d_itv) < length(etaC) - 1) stop("Please adjust duration of time periods for dropping out of the study (d_itv). The length of d_itv should be same length as etaC or 1 less.")
  new(".clinClass", gamma =  gamma, e_itv = e_itv, CCOD = CCOD,  CCOD_t =  CCOD_t, etaE = etaE, etaC = etaC, d_itv = d_itv)
}

#' S4 Class for setting parameters for time-to-events
#'
#' @keywords class
.eventClass = setClass(".eventClass", slots = list(
  event = "character",
  lambdaC = "numeric", shape = "numericORNULL", t_itv = "numericORNULL",
  beta = "numericORNULL",
  change = "listORNULL", keep = "charORNULL"
))



#' Set up time-to-events
#'
#' @description
#'
#' Defines the model formula and distribution to be used when simulating time-to-events. Please
#' see the user-guide for the model formulations
#'
#' @param event Distribution of time-to-events: \code{event = "pwexp"} for piece-wise exponential
#' distribution. \code{event = "weibull"} for Weibull distribution
#'
#' @param lambdaC Baseline hazard rate of internal control arm. Specify a vector for piece-wise
#' hazard with duration specified in \code{t_itv} if \code{event = "pwexp"}
#'
#' @param beta covariates' coefficients (i.e. log hazard ratios). Must be equal in length to the number of covariates
#' created by [simu_cov()] (or less if restricted by `keep`) plus the number of covariates
#' defined by `change`.
#'
#' @param shape the shape parameter of Weibull distribution if \code{event = "weibull"}. \code{NULL} if
#' \code{event = "pwexp"}
#'
#' @param t_itv a vector indicating interval lengths where the exponential rates provided in
#' \code{lambdaC} apply. Note that the length of \code{t_itv} is at least 1 less than that of
#' \code{lambdaC} and that the final value rate in \code{lambdaC} applies after time `sum(t_itv)`.
#' \code{NULL} if \code{event = "weibull"}
#'
#' @param change A list of additional derivered covariates
#' to be used in simulating time-to-events. See details
#'
#' @param keep A character vector specifying which of the original covariates (i.e. those not
#' derived via the `change` argument) should be included into the model to simulate time-to-events.
#' If left unspecified all covariates will be included.
#'
#' @return a \code{.eventClass} class containing time-to-events information
#'
#'
#' @details
#'
#' The `change` argument is used to specify additional derived covariates to be used when
#' simulating time-to-events. For example, let’s say have 3 covariates `cov1`, `cov2` & `cov3`
#' but that we also wish to include a new covariate that is an interaction
#' between `cov1` and `cov2` as well as another covariate that is equal to the sum of
#' `cov2` and `cov3`; we could implement this as follows:
#'
#' ```
#' set_event(
#'     event = "weibull",
#'     shape = 0.9,
#'     lambdaC = 0.0135,
#'     beta = c(5, 3, 1, 7, 9),
#'     change = list(
#'         c("cov1", "*", "cov2"),
#'         c("cov2", "+", "cov3")
#'     )
#' )
#' ```
#' Note that in the above example 5 values have been specified to beta,
#' 3 for the original three covariates
#' and 2 for the two additional derived covariates included via `change`.
#'
#' Variables derived via `change` are automatically included in the model regardless
#' of whether they are listed in `keep` or not. Likewise, these covariates are derived
#' separately and not via a standard R formula, that is to say including an interaction
#' term does not automatically include the individual fixed effects.
#'
#'
#' @examples
#' # time-to-event follows a Weibull distribution
#' set_event(event = "weibull", shape = 0.9, lambdaC = 0.0135)
#'
#' # time-to-event follows a piece-wise exponential distribution
#' set_event(event = "pwexp", t_itv = 1, lambdaC = c(0.1, 0.02))
#'
#'
#' @export
#' @keywords constructor
#' @return a \code{matrix} containing simulated time-to-events information

set_event <- function(event, lambdaC, beta, shape, t_itv, change, keep) {

  if (missing(event) || event %notin% c("weibull", "pwexp")) {
    stop(
      paste(
        "Distribution of time-to-events (event) is not correctly specify.",
        "Options include weibull, and pwexp"
      )
    )
  }

  if (missing(lambdaC)) {
    stop("Please provide the baseline hazard rate of internal control arm (lambdaC).")
  }

  if (missing(t_itv)) {
    t_itv <- NULL
  }

  if (missing(shape)) {
    shape <- NULL
  }

  if (event == "weibull" & is.null(shape)) {
    stop(
      paste(
        "Simulate time following weibull distribution.",
        "Please provide shape of the weibull distribution"
      )
    )
  }

  if (event == "pwexp" & (length(t_itv) < length(lambdaC) - 1)) {
    stop("Length of t_it should be at least 1 less than that of lambdaC")
  }

  if (missing(change)) {
    change <- NULL
  }

  if (missing(keep)) {
    keep <- NULL
  }

  if (missing(beta)) {
    beta <- NULL
  }

  new(
    ".eventClass",
    event = event,
    lambdaC = lambdaC,
    shape = shape,
    t_itv = t_itv,
    beta = beta,
    change = change,
    keep = keep
  )
}


#' Simulate survival times
#' @param dt a list of \code{matrix} generated in \code{\link{simu_cov}} containing simulated covariates information
#' @param eventObj an object of class \code{.eventClass} generated in \code{\link{set_event}} including event information
#' @param clinInt an object of class \code{.clinClass} generated in \code{\link{set_clin}} including internal trial information
#' @param clinExt an object of class \code{.clinClass} generated in \code{\link{set_clin}} including external trial information
#' @param seed the seed of R‘s random number generator. Default is the first element of .Random.seed
#'
#' @return a list of \code{matrix} containing simulated time-to-events information
#'
#' @keywords internal method
add_time=function(dt, eventObj, clinInt, clinExt, seed){
  if (missing(dt))  stop("Please provide dt")
  if (missing(eventObj))  stop("Please provide eventObj")
  if (missing(clinInt)) stop("Please provide clinInt.")
  if (missing(clinExt)) stop("Please provide clinExt.")
  if (missing(seed)){
    ps_message("Setting up survival time... Set seed to ",.Random.seed[1])
    seed = .Random.seed[1]
  } else set.seed(seed)

  # checl
  if (any(c("driftHR", "HR", "trt", "ext") %notin% colnames(dt), sum(!grepl("cov[1234567890]", colnames(dt)[colnames(dt) %notin% c("driftHR", "HR", "trt", "ext")])) > 0)) stop("Please make sure the trial data contains the correct variable names.")

  new_cov_name = NULL
  cov_name = colnames(dt)[colnames(dt) %notin% c("driftHR", "HR", "trt", "ext")]
  flog.debug(cat("[add_time] Number of original covariates", length(cov_name), "\n"))

  keep = eventObj@keep
  if (is.null(keep)) {
    keep = cov_name
    msg <- paste("All original covariates (if any):", paste(keep, collapse = " "), "are used for time-to-failure.")
    ps_message(msg)
  } else if (sum(grepl("none", keep)) > 0){
    keep = NULL
    ps_message("No original covariates are used for time-to-failure.")
  } else if (sum(keep %notin% cov_name) > 0) {
    stop("Please correctly specify the covariates to keep when simulating failure times.")
  }

  change = eventObj@change
  if (!is.null(change)){ # no change to covariates
    for (i in 1:length(change)){
      if (length(change[[i]]) != 3) {
        stop("Please correctly specify the operation for the #", i, " entry.")
      } else if (change[[i]][2] == "^") {
        s = (change[[i]][1] %notin% colnames(dt)[colnames(dt) %notin% c("driftHR", "HR", "trt", "ext")]) +
          is.na(as.numeric(change[[i]][3]))
        if (s > 0) stop("Please correctly specify the ^ operation for the #", i, " entry.")
      } else if (change[[i]][2] %in% c("+", "*")) {
        s = (change[[i]][1] %notin% colnames(dt)[colnames(dt) %notin% c("driftHR", "HR", "trt", "ext")]) +
          (change[[i]][3] %notin% colnames(dt)[colnames(dt) %notin% c("driftHR", "HR", "trt", "ext")])
        if (s > 0) stop("Please correctly specify the +/* operation for the #", i, " entry.")
      } else {
        stop("Please correctly specify the operation. The second entry should be ^, + or *.")
      }
      new_cov_name = c(new_cov_name, paste(change[[i]],collapse=""))
    }
  }

  flog.debug(cat("[add_time] Number of new covariates", length(new_cov_name), "\n"))
  flog.debug(cat("[add_time] Number of original covariates to keep", length(keep), "\n"))

  n_cov = length(new_cov_name) + length(keep)
  flog.debug(cat("[add_time] Number of covariates is (length of beta should be)", n_cov, "\n"))

  beta = eventObj@beta
  if (length(beta) %notin% c(1, n_cov)){
    ps_message("Coefficient for covariates (beta) is not recognized or correctly specified. Default value 1 is used for all covariates")
    beta = rep(1, n_cov)
  } else if (length(beta) == 1){
    ps_message("User provides one coefficient for covariate. This value is used for all covariates")
    beta = rep(beta, n_cov)
  }


  hr = unique(dt[, 'HR'])
  dr = unique(dt[, 'driftHR'])
  flog.debug(paste("[add_time] For this dataset, HR =", hr, "driftHR =", dr, "\n"))
  time_int = g_one_t(ext = 0, dt = dt,
                     gamma = clinInt@gamma, e_itv = clinInt@e_itv,
                     etaC = clinInt@etaC, etaE = clinInt@etaE, d_itv = clinInt@d_itv,
                     CCOD = clinInt@CCOD, CCOD_t = clinInt@CCOD_t,
                     event = eventObj@event, lambda = eventObj@lambdaC,
                     shape = eventObj@shape, t_itv = eventObj@t_itv,
                     HR = hr, beta = beta,
                     change = change, keep = keep)

  lambdaExt = eventObj@lambdaC * dr

  time_ext = g_one_t(ext = 1, dt = dt,
                     gamma = clinExt@gamma, e_itv = clinExt@e_itv,
                     etaC = clinExt@etaC, etaE = clinExt@etaE, d_itv = clinExt@d_itv,
                     CCOD = clinExt@CCOD, CCOD_t = clinExt@CCOD_t,
                     event = eventObj@event, lambda = lambdaExt,
                     shape = eventObj@shape, t_itv = eventObj@t_itv,
                     HR = hr, beta = beta,
                     change = change, keep = keep)

  rbind(time_int, time_ext)

}
