# Simulations of ideal data
# Arseniy Khvorov
# Created 2019/10/21
# Last edit 2019/10/21

#' Generate ideal data for the scaled logit model
#'
#' Allows variation of all parameters and the creation of an arbitrary number of
#' covariates.
#'
#' @param n Number of observations.
#' @param theta Baseline risk parameter on the logit scale.
#' @param beta_0 Intercept of the linear part.
#' @param covariate_list A list in the form of \code{name = list(gen_fun,
#'   true_par)} where \code{gen_fun} is a function that takes \code{n} as an
#'   argument and returns a vector of observations, \code{true_par} is the true
#'   parameter value of that covariate. See examples.
#' @param outcome_name Name to give to the outcome
#' @param seed Seed to set. If \code{NULL}, no seed will be set.
#' @param attach_true_vals,attach_seed Whether to attach additional attributes.
#'
#' @return A \code{\link[tibble]{tibble}}.
#'
#' @examples
#' # One titre
#' one_titre <- sclr_ideal_data(
#'   covariate_list = list(
#'     logHI = list(gen_fun = function(n) rnorm(n, 2, 2), true_par = 2)
#'   )
#' )
#' sclr(status ~ logHI, one_titre) # Verify
#'
#' # Two titres
#' two_titre <- sclr_ideal_data(
#'   covariate_list = list(
#'     logHI = list(gen_fun = function(n) rnorm(n, 2, 2), true_par = 2),
#'     logNI = list(gen_fun = function(n) rnorm(n, 2, 2), true_par = 1)
#'   )
#' )
#' sclr(status ~ logHI + logNI, two_titre) # Verify
#' 
#' @importFrom stats as.formula rbinom
#' 
#' @export
sclr_ideal_data <- function(n = 1000, theta = 0, beta_0 = -5,
                            covariate_list = list(
                              logHI = list(
                                gen_fun = function(n) rnorm(n, 2, 2),
                                true_par = 2
                              )
                            ),
                            outcome_name = "status",
                            seed = NULL,
                            attach_true_vals = FALSE,
                            attach_seed = FALSE) {
  if (!is.null(seed)) set.seed(seed)

  lambda <- invlogit(theta)
  
  dat <- tibble(.rows = n)

  # Generate covariates
  for (cov_name in names(covariate_list)) {
    if (!all(c("gen_fun", "true_par") %in% names(covariate_list[[cov_name]])))
      abort("entries in covariate_list must contain gen_fun and true_par")
    dat[[cov_name]] <- covariate_list[[cov_name]]$gen_fun(nrow(dat))
  }
  formula <- paste0("~", paste(names(covariate_list), collapse = "+"))
  model_matrix <- model.matrix(as.formula(formula), dat)

  # Generate the parameter matrix
  par_mat <- matrix(c(beta_0, rep(0, length(covariate_list))))
  rownames(par_mat) <- c("intercept", names(covariate_list))
  for (cov_name in names(covariate_list)) {
    par_mat[cov_name, ] <- covariate_list[[cov_name]]$true_par
  }

  # Generate status
  linear_part <- model_matrix %*% par_mat
  dat[[outcome_name]] <- rbinom(nrow(dat), 1, lambda / (1 + exp(linear_part)))

  # Attach additional attributes
  if (attach_true_vals) {
    true_vals <- tibble(
      term = c("theta", "beta_0", paste0("beta_", names(covariate_list))),
      true_value = c(theta, beta_0, rep(NA, length(covariate_list)))
    )
    for (cov_name in names(covariate_list)) {
      true_vals[true_vals$term == paste0("beta_", cov_name), "true_value"] <-
        covariate_list[[cov_name]]$true_par
    }
    attr(dat, "true_values") <- true_vals
  }
  if (attach_seed) attr(dat, "seed") <- seed

  dat
}
