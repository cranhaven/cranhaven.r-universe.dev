# S4 class, constructors and methods for generating posterior samples from MCMC
# Call function from utils.R

setClassUnion("charORNULL", c("character", "NULL"))
setClassUnion("numericORNULL", c("numeric", "NULL"))
setClassUnion("matrixORNULL", c("matrix", "NULL"))
`%notin%` <- Negate(`%in%`)

#' S4 Class for specifying prior distributions and predictors for MCMC methods
#'
#' @keywords class
.priorClass <- setClass(
  ".priorClass",
  slots = list(
    pred = "character",
    prior = "charORNULL",
    r0 = "numericORNULL",
    alpha = "numericORNULL",
    sigma = "numericORNULL"
  )
)

#' Specify prior distributions and predictors for MCMC methods
#'
#' @param pred Predictors to include in the weibull distribution. No covariates except for
#' treatment indicator is included if \code{pred = NULL}. Only propensity score generated
#' using a logistic regression model on all covariates and treatment indicator are included
#' if \code{pred = ps}. All covariates and treatment indicator are included if \code{pred = all}
#'
#' @param prior Prior distribution for the precision parameter that controls the degree of
#' borrowing. Half-cauchy distribution if \code{prior = "cauchy"}. No external data is
#' included in the data if \code{prior = "no_ext"}. External control arm is assumed to
#' have the same baseline hazards as internal control arm if \code{prior = "full_ext"}.
#' Other options include "gamma" and "unif"
#'
#' @param r0 Initial values for the shape of the weibull distribution for time-to-events
#'
#' @param alpha Initial values for log of baseline hazard rate for external and internal control
#' arms. Length of \code{alpha} should be 1 if \code{prior = "full_ext"} or \code{prior =
#' "no_ext"}, and equal to 2 otherwise
#'
#' @param sigma Initial values for precision parameter if \code{prior = "cauchy"}.
#' If left \code{NULL}, default value 0.03 is used
#'
#' @return a \code{.priorClass} class containing survival data and prior information
#'
#' @examples
#' # hierachical Bayesian model with precision parameter follows a half-cauchy distribution
#' set_prior(pred = "none", prior = "cauchy", r0 = 1, alpha = c(0, 0), sigma = 0.03)
#'
#' # hierachical Bayesian model with precision parameter follows a gamma distribution
#' set_prior(pred = "none", prior = "gamma", r0 = 1, alpha = c(0, 0))
#'
#' # conventional Bayesian model to not borrow from external control arm
#' set_prior(pred = "none", prior = "no_ext", alpha = 0)
#'
#' # conventional Bayesian model to fully borrow from external control arm
#' set_prior(pred = "none", prior = "full_ext", alpha = 0)
#'
#'
#' @export
#' @keywords constructor
set_prior <- function(pred, prior, r0, alpha, sigma) {

  if(missing(prior) || prior %notin% c("gamma", "cauchy", "no_ext", "full_ext", "unif")) {
    stop("Prior distribution for the precision parameter (prior) is not correctly specified. Options include gamma, cauchy, unif, no_ext, full_ext.")
  } else if (prior == "no_ext" & sum(grepl("ps", pred)) > 0) {
    stop("User choose to not include external information. Adjusting for propensity score is not an option.")
  }

  if(missing(r0)) {
    r0 = 1
    ps_message('No initial values for the shape of the weibull distribution (r0) is detected. Default value 1 is used')
  }

  if(missing(alpha) || (prior %in% c("gamma", "cauchy", "unif") & length(alpha) != 2)) {
    alpha = c(0, 0)
    ps_message('Values for log of baseline hazard rate for external and internal control arms (alpha) is not correctly specified. Default value 0 is used.')
  }
  if (missing(alpha) || (prior %in% c("no_ext", "full_ext") & length(alpha) != 1)){
    alpha = 0
    ps_message('Values for log of baseline hazard rate for external and internal control arms (alpha) is not correctly specified. Default value 0 is used.')
  }
  if (missing(sigma)) sigma = NULL
  if (prior %in% c("cauchy", "unif") & length(sigma) != 1) {
    sigma = 0.03
    ps_message("Initial value for precision parameter (sigma) is missing or not correctly specified. Default value 0.03 is used.")
  }
  new(".priorClass", pred = pred, prior = prior, r0 = r0, alpha = alpha, sigma = sigma)
}



#' Concatenate multiple \code{.priorClasss} class
#'
#' @param x A \code{.priorClasss} class with prior distribution information generated in \code{\link{set_prior}}
#' @param ... A \code{.priorClasss} class with prior distribution information generated in \code{\link{set_prior}}
#' @return A vector of \code{.priorClasss} classes
#'
#'
#'
#'
#' @export
#' @keywords helper method
setMethod("c", signature(x = ".priorClass"), function(x, ...){
  elements = list(x, ...)
  priorClassList = list()
  for (i in 1:length(elements)){
    priorClassList[[i]] = new(".priorClass",
                              pred = slot(elements[[i]], "pred"), prior = slot(elements[[i]], "prior"),
                              r0 = slot(elements[[i]], "r0"), alpha = slot(elements[[i]], "alpha"),
                              sigma = slot(elements[[i]], "sigma"))
  }
  class(priorClassList) = ".priorClass"
  priorClassList
})


#' Generating posterior samples from MCMC
#' @param dt a list of \code{matrix} containing simulated time-to-events information
#' @param priorObj an object of class \code{.priorClass} generated in \code{\link{set_prior}}
#' @param n.chains number of parallel chains for the model
#' @param n.adapt number of iterations for adaptation
#' @param n.burn number of iterations discarded as burn-in
#' @param n.iter number of iterations to monitor
#' @param seed the seed of random number generator. Default is the first element of .Random.seed

#' @keywords internal method
#' @return A \code{list} containing hazard ratio and prior information
add_mcmc <- function(dt, priorObj, n.chains, n.adapt, n.burn,  n.iter, seed){

  if (missing(dt)) {
    stop("Please provide a dataset (dt).")
  } else {
    #extract covariate names in the dataset
    key_vars <- c("driftHR", "HR", "trt", "ext", "time", "cnsr", "wgt")
    cov_name <- colnames(dt)[colnames(dt) %notin% key_vars]

    if (sum(c("trt", "ext", "time", "cnsr") %notin% colnames(dt)) > 0) {
      stop("Please make sure the trial data contains at least trt, ext, time and cnsr.")
    } else if (sum(!grepl("cov[1234567890]", cov_name)) > 0) {
      stop("Please make sure the covariates in the trial have the right name.")
    }
  }
  flog.debug(cat("[add_mcmc] cov_name =", cov_name, "\n"))

  if (missing(seed)){
    ps_message("Setting up MCMC... Set seed to ",.Random.seed[1])
    seed = .Random.seed[1]
  } else set.seed(seed)

  n_mcmc <- valid_mcmc(n.chains, n.adapt, n.burn, n.iter)

  if (length(priorObj) == 1) priorObj = c(priorObj)

  lapply(seq(1, length(priorObj), by = 1), function(i){
    prior <- priorObj[[i]]@prior
    pred <- priorObj[[i]]@pred

    #---
    if (missing(pred)) {
      pred = "none"
      ps_message("Predictors to include in the weibull distribution (pred) is not provided. No predictor is used.")
    } else if (sum(pred %notin% c(cov_name, "none", "ps", "all")) > 0 ){
      stop("pred is not correctly specified. Options include none, ps, all and covariate names start with cov.")
    } else if (sum(grepl("none", pred)) > 0) {
      pred = "none"
      ps_message("Input none is found in pred. Any other input is disregarded.")
    } else if (sum(grepl("all", pred)) > 0) {
      pred = "all"
      ps_message("Input all is found in pred. Any other input is disregarded.")
    } else if (sum(grepl("ps", pred)) == 0) {
      ps_message("Selected covariate(s)", pred, "are used as predictors in the weibull distribution.\n")
    } else if (sum(grepl("ps", pred)) > 0 & length(pred) == 1){ # column wgt does not exist
      dt_ps <- c_ps(dt, cov_name)
      ps_message("Propensity score calculated using all coariates", cov_name, "is used as predictors in the weibull distribution.\n")
    } else if (sum(grepl("ps", pred)) > 0 & ("wgt" %in% colnames(dt))) { # column wgt already existed
      ps_message("Weight already provided in the data. It is used as predictor directly. \n")
    } else if (sum(grepl("ps", pred)) > 0 & length(pred) > 1){
      ps_message("Propensity score calculated based selected covariate(s)", pred[pred != "ps"],
                  "is used as predictors in the weibull distribution.\n")
    }

    flog.debug(cat(">>> prior =", prior, ", pred = ", pred, "\n"))

    mcmc_res <- r_post(
      dt = dt,
      prior = prior,
      pred = pred,
      r0 = priorObj[[i]]@r0,
      alpha = priorObj[[i]]@alpha,
      sigma = priorObj[[i]]@sigma,
      seed = seed,
      n.chains = n_mcmc[["n.chains"]],
      n.adapt = n_mcmc[["n.adapt"]],
      n.burn = n_mcmc[["n.burn"]],
      n.iter = n_mcmc[["n.iter"]]
    )

    mcmc_sum <- rej_est(mcmc_res)

    flog.debug(cat(">>> mcmc_sum =", mcmc_sum, "\n"))

    results <- list(
      "prior" = prior,
      "pred" = paste(pred, collapse = " "), # print to one row
      "mcmc.list" = mcmc_res,
      "summary" = mcmc_sum
    )

    if ("driftHR" %in% colnames(dt)) {
      results[["driftHR"]] <- unique(dt[, "driftHR"])
    }
    if ("HR" %in% colnames(dt)) {
      results[["HR"]] <- unique(dt[, "HR"])
    }
    return(results)
  })
}

#' validate the operational parameters for MCMC method
#' @param n.chains number of parallel chains for the model
#' @param n.adapt number of iterations for adaptation
#' @param n.burn number of iterations discarded as burn-in
#' @param n.iter number of iterations to monitor
#' @keywords internal method
#'
#' @return A \code{list} containing numbers of parallel chains and iterations
valid_mcmc <- function(n.chains, n.adapt, n.burn,  n.iter){
  if (missing(n.chains) || !is.numeric(n.chains)) {
    n.chains = 2
    ps_message("No number of parallel chains for the model is provided. Default value 2 is used")
  }
  if (missing(n.adapt) || !is.numeric(n.chains)) {
    n.adapt = 1000
    ps_message("No Number of iterations for adaptation (n.adapt) is provided. Default value 1000 is used")
  }
  if (missing(n.burn) || !is.numeric(n.burn)) {
    n.burn = 0
    ps_message("No mumber of iterations discarded as burn-in (n.burn) is provided. No iteration will be discarded.")
  }
  if (missing(n.iter) || !is.numeric(n.iter)) {
    n.iter = 10000
    ps_message("No number of iterations to monitor is provided. Default value 10000 is used")
  }
  return(list("n.chains" = n.chains, "n.adapt" = n.adapt, "n.burn" = n.burn, "n.iter" = n.iter))
}
