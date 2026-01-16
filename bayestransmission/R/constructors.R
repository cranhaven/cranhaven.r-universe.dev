#' Construct a parameter with a prior, weight and an update flag.
#'
#' @param init the initial value of the parameter.
#' @param weight the weight of the prior.
#' @param update a flag indicating if the parameter shouldbe updated in the MCMC.
#' @param prior mean value of the prior distribution, may be used with weight to fully determine prior parameters.
#'
#' @returns A list with the following elements:
#'  * `init` the initial value of the parameter.
#'  * `weight` the weight of the prior.
#'  * `update` a flag indicating if the parameter shouldbe updated in the MCMC.
#'  * `prior` mean value of the prior distribution, may be used with weight to fully determine prior parameters.
#'
#' @export
#'
#' @examples
#' # Fully specified parameter.
#' Param(init = 0, weight = 1, update = TRUE, prior = 0.5)
#' # Fixed parameter
#' # Weight = 0 implies update=FALSE and prior is ignored.
#' Param(0, 0)
#' # Update parameter that starts at zero.
#' Param(0, weight =1, update=TRUE)
#' # Parameters specified at zero implies fixed.
#' Param(0)
Param <- function(init, weight = if_else(init == 0, 0, 1), update = weight > 0, prior = init) {
  structure(
    list(
      init = init,
      update = update,
      prior = prior,
      weight = weight
    ),
    class = "Param"
  )
}
is_valid_param <- function(x) {
  assertthat::see_if(
    is.list(x),
    length(x) == 4,
    all(c("init", "weight", "update", "prior") %in% names(x)),
    is.numeric(x$init),
    is.numeric(x$weight),
    is.logical(x$update),
    is.numeric(x$prior)
  )
}

check_param <- function(param, name = deparse(substitute(param))) {
  if (rlang::is_scalar_double(param)) {
    param <- Param(param)
  } else {
    assertthat::assert_that(is_valid_param(param))
  }
  return(param)
}


#' Specify a random testing parameter with a rate.
#'
#' @param param Values for the positive rate of the test.
#' @param rate Values for the rate of the test.
#'
#' @returns A list of with param and rate.
#' @export
#'
#' @examples
#' ParamWRate(Param(0.5, 0), rate = Param(1, 0))
ParamWRate <- function(param = Param(), rate = Param()) {
  if (!inherits(param, "Param") || !inherits(rate, "Param")) {
    if (rlang::is_scalar_double(param)) {
      param <- Param(param)
    } else {
      stop("param and rate should be of class Param")
    }
    if (rlang::is_scalar_double(rate)) {
      rate <- Param(rate)
    } else {
      stop("param and rate should be of class Param")
    }
  }
  list(
    param = param,
    rate = rate
  )
}
check_paramwrate <- function(x, name = deparse(substitute(x))) {
  assertthat::assert_that(is.list(x),
              x %has_name% 'param',
              x %has_name% 'rate')
  x$param <- check_param(x$param, paste0(name, "$parameter"))
  x$rate <- check_param(x$rate, paste0(name, "$rate"))
  return(x)
}

#' InSitu Parameters
#'
#' @param probs The probability of the individual being in each state.
#' @param priors The prior probability of the individual being in each state.
#' @param doit A flag indicating if the rate(s) should be updated in the MCMC.
#' @param nstates The number of states (2 or 3). If NULL, inferred from probs length.
#'                For 2-state models, uses `c(uncolonized, latent=0, colonized)`.
#'                For 3-state models, uses `c(uncolonized, latent, colonized)`.
#'
#' @returns A list of parameters for in situ testing.
#' @export
#'
#' @examples
#' InsituParams()
#' InsituParams(nstates = 2)  # c(0.9, 0.0, 0.1)
#' InsituParams(nstates = 3)  # c(0.98, 0.01, 0.01)
InsituParams <- function(probs = NULL, priors = NULL, doit = NULL, nstates = NULL) {
  # Determine defaults based on nstates
  if (is.null(probs)) {
    if (is.null(nstates)) {
      # Default to 2-state model defaults (matches original C++ for 2-state)
      probs <- c(0.9, 0.0, 0.1)
    } else if (nstates == 2) {
      probs <- c(0.9, 0.0, 0.1)  # uncolonized, latent=0, colonized
    } else if (nstates == 3) {
      probs <- c(0.98, 0.01, 0.01)  # uncolonized, latent, colonized
    } else {
      stop("nstates must be 2 or 3")
    }
  }
  
  stopifnot(2 <= length(probs) && length(probs) <= 3)
  if (length(probs) == 2) probs <- c(probs, 0)
  
  if (is.null(doit)) {
    # For 2-state: update uncolonized and colonized, not latent
    # For 3-state: update all three
    if (is.null(nstates)) {
      nstates <- if (probs[2] == 0) 2 else 3
    }
    doit <- if (nstates == 2) c(TRUE, FALSE, TRUE) else c(TRUE, TRUE, TRUE)
  }
  
  if (is.null(priors)) {
    priors <- probs * doit + (1 - doit) * 1  # Use 1 for fixed parameters
  }
  
  list(
    probs = probs,
    priors = priors,
    doit = doit
  )
}


TestParams <- function(n, probs = c(0.5, 0.5, 0), priors = probs, doit = probs != 0) {
  stopifnot(2 <= length(probs) && length(probs) <= 3)
  if (length(probs) == 2) probs <- c(probs, 0)
  stopifnot(
    length(n) == length(probs),
    length(n) == length(priors),
    length(n) == length(doit)
  )

  list(
    probs = probs,
    priors = priors,
    doit = doit,
    n = n
  )
}

#' Random Testing Parameter Set
#'
#' @param uncolonized Testing when the individual is uncolonized.
#' @param colonized Testing when the individual is colonized.
#' @param latent Testing when the individual is latent.
#'
#' @returns list of parameters for random testing.
#' @export
#'
#' @examples
#' RandomTestParams()
RandomTestParams <- function(
    uncolonized = ParamWRate(Param(0.5, 0), Param(1, 0)),
    colonized = ParamWRate(Param(0.5, 0), Param(1, 0)),
    latent = ParamWRate(Param(0), Param(0))) {
  uncolonized <- check_paramwrate(uncolonized)
  colonized <- check_paramwrate(colonized)
  latent <- check_paramwrate(latent)

  list(
    uncolonized = uncolonized,
    colonized = colonized,
    latent = latent
  )
}
#' @describeIn RandomTestParams Clinical Test Parameters Alias
#' @export
ClinicalTestParams <- RandomTestParams


#' Out of Unit Infection Parameters
#'
#' @param acquisition Rate of acquisition of the disease moving into latent state.
#' @param clearance Rate of clearance of the disease moving into uncolonized state.
#' @param progression Rate of progression of the disease moving into colonized state.
#'
#' @returns A list of parameters for out of unit infection.
#' @export
#'
#' @examples
#' OutOfUnitInfectionParams()
OutOfUnitInfectionParams <- function(
    acquisition = Param(0.05),
    clearance = Param(0.01),
    progression = Param(0)) {
  acquisition <- check_param(acquisition)
  clearance <- check_param(clearance)
  progression <- check_param(progression)
  list(
    acquisition = acquisition,
    clearance = clearance,
    progression = progression
  )
}

#' Surveillance Test Parameters
#'
#' Specify the rates of positive tests for each state of the model.
#'
#' @param colonized Also known as the true positive rate for a two state model.
#' @param uncolonized Also known as the false positive rate for a two state model.
#' @param latent The rate of positive tests when the individual is in the (optional) latent state.
#'
#' @returns A list of parameters for surveillance testing.
#'
#' @export
#' @examples
#' SurveillanceTestParams()
SurveillanceTestParams <- function(
    colonized = Param(init = 0.8, weight = 1),       # High sensitivity for detecting colonized
    uncolonized = Param(init = 1e-10, weight = 0),   # Very low false positive rate
    latent = Param(init = 0.0, weight = 0)) {        # Not used in 2-state model
    colonized <- check_param(colonized)
    uncolonized <- check_param(uncolonized)
    latent <- check_param(latent)
  list(
    colonized = colonized,
    uncolonized = uncolonized,
    latent = latent
  )
}

#' Antibiotic Parameters
#'
#' @param onoff If Anti-biotic are used or not.
#' @param delay The delay in using antibiotics.
#' @param life The life of antibiotics.
#' @export
#'
#' @returns list of parameters for antibiotic effect
#'
#' @examples
#' AbxParams()
AbxParams <- function(
    onoff = FALSE,
    delay = 0.0,
    life = 1.0) {
  list(
    onoff = onoff,
    delay = delay,
    life = life
  )
}


#' Antibiotic Administration Rate Parameters
#'
#' @param uncolonized Rate of antibiotic administration when the individual is uncolonized.
#' @param colonized Rate of antibiotic administration when the individual is colonized.
#' @param latent Rate of antibiotic administration when the individual is latent.
#'
#' @returns list of parameters for antibiotic administration.
#' @export
#'
#' @examples
#' AbxRateParams()
AbxRateParams <- function(
    uncolonized = Param(1, 0),
    colonized = Param(1, 0),
    latent = Param(0)) {
  uncolonized <- check_param(uncolonized)
  colonized <- check_param(colonized)
  latent <- check_param(latent)
  list(
    uncolonized = uncolonized,
    colonized = colonized,
    latent = latent
  )
}

#' Log-Normal Acquisition Parameters
#'
#' Acquisition parameters for the log-normal model (LogNormalAbxICP).
#' This model has 8 acquisition parameters accessed by index in C++.
#' Note: When accessed via setupLogNormalICPAcquisition, parameters are set by index,
#' so this returns an unnamed list where position matters.
#'
#' @param time Time parameter (index 0)
#' @param constant Constant parameter (index 1)
#' @param log_tot_inpat Log total in-patients parameter (index 2)
#' @param log_col Log number colonized parameter (index 3)
#' @param col Number colonized parameter (index 4)
#' @param abx_col Number abx colonized parameter (index 5)
#' @param onabx Susceptible patient on Abx effect (index 6)
#' @param everabx Susceptible patient ever on Abx effect (index 7)
#'
#' @returns An unnamed list of 8 parameters in the correct order for LogNormalAbxICP.
#' @export
#'
#' @examples
#' LogNormalAcquisitionParams()
LogNormalAcquisitionParams <- function(
    time = Param(0, 0),
    constant = Param(0.001, 1),
    log_tot_inpat = Param(-1, 0),
    log_col = Param(1, 0),
    col = Param(0, 0),
    abx_col = Param(0, 0),
    onabx = Param(0, 0),
    everabx = Param(0, 0)) {
  # Return unnamed list - accessed by index in C++
  list(time, constant, log_tot_inpat, log_col, col, abx_col, onabx, everabx)
}

#' Linear Antibiotic Acquisition Parameters
#'
#' Acquisition parameters for LinearAbxModel and LinearAbxModel2.
#'
#' The model for this acquisition model is given by
#'
#' \deqn{
#' P(\mathrm{Acq(t)}) =
#'     \left[e^{\beta_\mathrm{time}(t-t_0)}\right]\\
#' \left\{e^{\beta_0}
#'     \left[
#'         \left(
#'           \frac{\beta_\mathrm{freq}}{P(t)}+(1 - e^{\beta_\mathrm{freq}})
#'         \right)
#'         e^{\beta_\mathrm{mass}}\left(
#'             (N_c(t) - N_{ca}(t)) + e^{\beta_\mathrm{col\_abx}}N_{ca}(t)
#'             \right)
#'         + 1 - e^{\beta_\mathrm{mass}}
#'         \right]
#'     \right\}\\
#'  \left[
#'      N_S(t) - N_E(t) +
#'      e^{\beta_\mathrm{suss\_ever}}
#'      \left(
#'        \left(
#'          E_i(t) - A_i(t)
#'        \right)
#'        + A_i(t)e^{\beta_\mathrm{suss\_abx}}
#'      \right)
#'  \right]
#' }{
#'  P(Acq(t)) = exp(beta_time*(t-t0)) * {
#'    exp(beta_0) * [
#'      (beta_freq/P(t)
#'    + (1-exp(beta_freq))) * exp(beta_mass) * ((N_c(t) - N_ca(t)) + exp(beta_col_abx)*N_ca(t)) + 1 - exp(beta_mass)
#'    ]
#'  }
#'  * [
#'    N_S(t) - N_E(t) + exp(beta_suss_ever)*((E_i(t)-A_i(t)) + A_i(t)*exp(beta_suss_abx))
#'  ]
#' }
#' where P(Acq(t)) is the acquisition probability at time t, with effects from time (beta_time), 
#' mass action (beta_mass), frequency dependence (beta_freq), 
#' colonized individuals on antibiotics (beta_col_abx), 
#' and susceptible individuals currently (beta_suss_abx) or ever (beta_suss_ever) on antibiotics.
#'
#' @param base The base rate of acquisition.
#' @param time The time effect on acquisition.
#' @param mass The mass action effect on acquisition.
#' @param freq The frequency effect on acquisition.
#' @param col_abx The effect for colonized on antibiotics.
#' @param suss_abx The effect on susceptible being currently on antibiotics.
#' @param suss_ever The effect on susceptible ever being on antibiotics.
#'
#' @returns A list of parameters for acquisition.
#' @export
#'
#' @examples
#' LinearAbxAcquisitionParams()
LinearAbxAcquisitionParams <- function(
    base = Param(0.001),
    time = Param(1, 0),
    mass = Param(1.0, 1),
    freq = Param(1.0, 1),
    col_abx = Param(1, 0),
    suss_abx = Param(1, 0),
    suss_ever = Param(1, 0)) {
  list(
    base = base,
    time = time,
    mass = mass,
    freq = freq,
    col_abx = col_abx,
    suss_abx = suss_abx,
    suss_ever = suss_ever
  )
}

#' Progression Parameters
#'
#' @param rate Base progression rate
#' @param abx  Effect of current antibiotics on progression
#' @param ever_abx Effect of ever having taken antibiotics on progression
#'
#' @returns A list of parameters for progression.
#' @export
#'
#' @examples
#' ProgressionParams()
ProgressionParams <- function(
    rate = Param(0.01),
    abx = Param(1, 0),
    ever_abx = Param(1, 0)) {
  list(
    rate = rate,
    abx = abx,
    ever_abx = ever_abx
  )
}

#' Clearance Parameters
#'
#' @param rate base rate of clearance
#' @param abx effect of antibiotics on clearance
#' @param ever_abx effect of ever having taken antibiotics on clearance
#'
#' @returns A list of parameters for clearance.
#' @export
#'
#' @examples
#' ClearanceParams()
ClearanceParams <- function(
    rate = Param(0.01),
    abx = Param(1, 0),
    ever_abx = Param(1, 0)) {
  list(
    rate = rate,
    abx = abx,
    ever_abx = ever_abx
  )
}


#' In Unit Parameters
#'
#' @param acquisition Acquisition, for rate of acquisition of the disease moving into 
#'                    colonized(2-State)/latent(3-state) state.
#' @param progression Progression from latent state to colonized state.
#' @param clearance Clearance from colonized state to uncolonized state.
#'
#' @returns A list of parameters for in unit infection.
#' @export
#' @examples
#' InUnitParams(
#'   acquisition = LinearAbxAcquisitionParams(),
#'   progression = ProgressionParams(),
#'   clearance = ClearanceParams()
#' )
InUnitParams <- function(
    acquisition = LinearAbxAcquisitionParams(),
    progression = ProgressionParams(),
    clearance = ClearanceParams()) {
  assertthat::assert_that(
    is.list(acquisition),
    is.list(progression),
    is.list(clearance)
  )
  list(
    acquisition = acquisition,
    progression = progression,
    clearance = clearance
  )
}

#' @describeIn InUnitParams In Unit Parameters with Antibiotics.
#' @export
#' @examples
#' ABXInUnitParams(
#'   acquisition = LinearAbxAcquisitionParams(),
#'   progression = ProgressionParams(),
#'   clearance = ClearanceParams()
#' )
ABXInUnitParams <- function(
    acquisition = LinearAbxAcquisitionParams(),
    progression = ProgressionParams(),
    clearance = ClearanceParams()) {
  InUnitParams(
    acquisition = acquisition,
    progression = progression,
    clearance = clearance
  )
}


#' Model Parameters for a Log Normal Model
#'
#' @param modname The name of the model used. Usually specified by specification functions.
#' @param nstates The number of states in the model.
#' @param nmetro The number of Metropolis-Hastings steps to take between outputs.
#' @param forward TODO
#' @param cheat TODO
#' @param Insitu In Situ Parameters
#' @param SurveillanceTest Surveillance Testing Parameters
#' @param ClinicalTest Clinical Testing Parameters
#' @param OutOfUnitInfection Out of Unit Infection Parameters
#' @param InUnit In Unit Parameters, should be a list of lists with parameters
#'               for the acquisition, progression and clearance of the disease.
#' @param Abx Antibiotic Parameters
#' @param AbxRate Antibiotic Rate Parameters
#'
#' @returns A list of parameters for the model.
#' @export
#'
#' @examples
#' LogNormalModelParams("LogNormalModel")
LogNormalModelParams <-
  function(modname,
           nstates = 2L,
           nmetro = 1L,
           forward = TRUE,
           cheat = FALSE,
           Insitu = NULL,
           SurveillanceTest = SurveillanceTestParams(),
           ClinicalTest = ClinicalTestParams(),
           OutOfUnitInfection = OutOfUnitInfectionParams(),
           InUnit = NULL,  # Default depends on modname
           Abx = AbxParams(),
           AbxRate = AbxRateParams()) {
    assertthat::assert_that(
      assertthat::is.string(modname),
      assertthat::is.count(nstates),
      assertthat::is.count(nmetro),
      assertthat::is.flag(forward),
      assertthat::is.flag(cheat)
    )

    # Create default Insitu params based on nstates if not provided
    if (is.null(Insitu)) {
      Insitu <- InsituParams(nstates = nstates)
    }
    
    # Create default InUnit params based on model type if not provided
    if (is.null(InUnit)) {
      if (modname == "LogNormalModel" || modname == "MixedModel") {
        # LogNormalModel and MixedModel use LogNormalAbxICP with 8 acquisition params
        InUnit <- InUnitParams(acquisition = LogNormalAcquisitionParams())
      } else {
        # LinearAbxModel, LinearAbxModel2, etc. use LinearAbxICP with 7 params
        InUnit <- InUnitParams(acquisition = LinearAbxAcquisitionParams())
      }
    }

    list(
      modname = modname,
      nstates = as.integer(nstates),
      nmetro = as.integer(nmetro),
      forward = as.logical(forward),
      cheat = cheat,
      Insitu = Insitu,
      SurveillanceTest = SurveillanceTest,
      ClinicalTest = ClinicalTest,
      OutCol = OutOfUnitInfection,
      InCol = InUnit,
      Abx = Abx,
      AbxRate = AbxRate
    )
  }


#' @describeIn LogNormalModelParams Linear Antibiotic Model Alias
#' @param ... Additional arguments passed to LogNormalModelParams
#' @export
LinearAbxModel <- function(
    ...,
    InUnit = ABXInUnitParams()) {  # Fixed: was ABXInUnitParameters()
  LogNormalModelParams("LinearAbxModel", ..., InUnit = InUnit)
}
