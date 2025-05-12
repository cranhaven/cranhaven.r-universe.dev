#' Generate simulation endpoint data for an arm in a trial.
#'
#' Parameters of marginal distribution of endpoints and their target correlation
#' are specified. This function returns endpoint data of one arm. To generate
#' data of multiple arms, one need to call this function for each of the arms.
#'
#' @param ... calls describing the marginal distributions of endpoints. They
#' can be data frames, e.g., `df`, or valid R expressions evaluated to be data
#' frames, e.g., `data.frame(x = runif(10), y = 1)`, `df %>% filter(x < 1)`,
#' `myFunctionReturnDataFrame()`, or simply any quantile functions (see the
#' `dist` argument of `simdata::simdesign_norta`).
#'
#' @param cor_matrix a target correlation matrix. See the `cor_target_final`
#' argument of `simdata::simdesign_norta`.
#'
#' @param arm_label a character to name the arm. When specified, a column `arm`
#' will be added to the end of the simulated data frame. NULL by default.
#'
#' @param trial_size size of an arm.
#'
#' @param var_prefix a character as the prefix of endpoints specified by common
#' distributions.
#'
#' @return A data frame of endpoints.
#'
#' @importFrom simdata simdesign_norta simulate_data
#'
#' @examplesIf interactive()
#'
#' library(dplyr)
#' data(df)
#' head(df)
#'
#' cmat <- matrix(.5, 10, 10)
#' diag(cmat) <- 1.
#'
#' ## the first argument should always be x if a user-defined quantile function
#' ## is supplied. This is required by the `simdata` package.
#' user_qfun <- function(x, shape1 = .6, shape2 = 1.2){
#'   qbeta(x, shape1 = shape1, shape2 = shape2)
#' }
#'
#' data <- simulateTrialData(
#'   df[, c('Gender', 'Age')] %>% dplyr::filter(Age < 50), # an expression with
#'                                                         # its value a data frame
#'   df %>% dplyr::select(-c(Gender, Age, Race)), # same as above
#'   norm(mean = 0.2,sd = 1.2), # distribution name. Note that it is not a valid
#'                              # R function but a symbol.
#'   binom(prob =.4, size=1  ), # spaces are ignore, order does not matter
#'   norm(mean =-0.4, sd=0.9),  # same distribution with different parameters
#'   user_qfun,                 # user-defined quantile
#'
#'   cor_matrix = cmat,
#'   trial_size = 1000,
#'   arm_label = 'placebo'
#' )
#'
#' ## for diagnosis
#' plot(data)
#' cor(data %>% dplyr::select(-matches('arm')))
#'
#' ## Note that user_qfun is simply the quantile function of a beta distribution,
#' ## so a preferred way to use easySimData is as follow
#' data <- simulateTrialData(
#'   df[, c('Gender', 'Age')] %>% dplyr::filter(Age < 50), # an expression with
#'                                                         # its value a data frame
#'   df %>% dplyr::select(-c(Gender, Age, Race)), # same as above
#'   norm(mean = 0.2,sd = 1.2), # distribution name. Note that it is not a valid
#'                              # R function but a symbol.
#'   binom(prob =.4, size=1  ), # spaces are ignored, order does not matter
#'   norm(mean =-0.4, sd=0.9),  # same distribution with different parameters
#'   beta(shape1 = .5, shape2 = 1.2), # distribution name
#'
#'   cor_matrix = cmat,
#'   trial_size = 1000,
#'   arm_label = 'placebo'
#' )
#'
#'
#' @export
simulateTrialData <- function(
    ...,
    cor_matrix,
    trial_size = 100,
    arm_label = NULL,
    var_prefix = 'easySimData'){

  config <- configureTrial(..., var_prefix = var_prefix)

  design <-
    simdesign_norta(
      cor_target_final = cor_matrix,
      dist = config
    )

  simulated_data <- simulate_data(design, n_obs = trial_size) %>% as.data.frame()
  colnames(simulated_data) <- attr(config, 'var_names')
  if(!is.null(arm_label)){
    simulated_data$arm <- arm_label
  }

  attr(simulated_data, 'call') <- match.call()
  attr(simulated_data, 'quantile_function') <- config

  class(simulated_data) <- c('easySimData', class(simulated_data))

  invisible(simulated_data)

}

