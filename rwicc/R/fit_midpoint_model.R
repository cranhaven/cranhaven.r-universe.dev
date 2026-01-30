#' Fit model using midpoint imputation
#'
#' @param participant_level_data a data.frame or tibble with the following variables:
#' \itemize{
#' \item ID: participant ID
#' \item E: study enrollment date
#' \item L: date of last negative test for seroconversion
#' \item R: date of first positive test for seroconversion
#' \item Cohort` (optional): this variable can be used to stratify the modeling of
#' the seroconversion distribution.
#' }
#' @param obs_level_data a data.frame or tibble with the following variables:
#' \itemize{
#' \item ID: participant ID
#' \item O: biomarker sample collection dates
#' \item Y: MAA classifications (binary outcomes)
#' }
#' @param maxit maximum iterations, passed to \code{bigglm}
#' @param tolerance convergence criterion, passed to \code{bigglm}
#'
#' @return a vector of logistic regression coefficient estimates
#' @export
#' @examples
#' sim_data = simulate_interval_censoring(
#'   "theta" = c(0.986, -3.88),
#'   "study_cohort_size" = 4500,
#'   "preconversion_interval_length" = 365,
#'   "hazard_alpha" = 1,
#'   "hazard_beta" = 0.5)
#'
#' theta_est_midpoint = fit_midpoint_model(
#'   obs_level_data = sim_data$obs_data,
#'   participant_level_data = sim_data$pt_data
#' )
#'
#' @importFrom biglm bigglm
#' @importFrom dplyr mutate left_join select
#' @importFrom lubridate ddays
#' @importFrom stats binomial coef
fit_midpoint_model <- function(participant_level_data,
                               obs_level_data,
                               maxit = 1000,
                               tolerance = 1e-8) {

  # prevent notes about undefined variables due to dplyr syntax:
  L <- R <- ID <- S_midpoint <- O <- NULL

  # bigglm's default maxit = 8, which is not large enough to ensure convergence for this data.
  # I don't think any of the scenarios examined in our paper ever get close to 1000 iterations though;
  # this is effectively saying maxit = Inf.

  participant_level_data %<>%
    dplyr::mutate(S_midpoint = L + (R - L) / lubridate::ddays(2))

  obs_level_data %<>%
    dplyr::left_join(
      by = "ID",
      participant_level_data %>% dplyr::select(ID, S_midpoint)
    ) %>%
    dplyr::mutate(T_midpoint = (O - S_midpoint) / lubridate::ddays(365))

  phi_model_est_midpoint <-
    biglm::bigglm(
      epsilon = tolerance,
      maxit = maxit,
      quiet = TRUE,
      data = obs_level_data,
      family = stats::binomial(),
      Y ~ T_midpoint
    )

  coefs <- coef(phi_model_est_midpoint)

  return(coefs)
}
