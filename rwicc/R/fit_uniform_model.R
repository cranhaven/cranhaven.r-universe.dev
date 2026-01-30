#' Fit model using uniform imputation
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
#' @param n_imputations number of imputed data sets to create
#'
#' @return a vector of logistic regression coefficient estimates
#'
#' @export
#'
#' @examples
#' sim_data = simulate_interval_censoring(
#'   "theta" = c(0.986, -3.88),
#'   "study_cohort_size" = 4500,
#'   "preconversion_interval_length" = 365,
#'   "hazard_alpha" = 1,
#'   "hazard_beta" = 0.5)
#'
#' theta_est_midpoint = fit_uniform_model(
#'   obs_level_data = sim_data$obs_data,
#'   participant_level_data = sim_data$pt_data
#' )
#'
#' @importFrom biglm bigglm
#' @importFrom dplyr mutate n select any_of left_join
#' @importFrom lubridate ddays
#' @importFrom stats runif binomial coef
fit_uniform_model <- function(participant_level_data,
                              obs_level_data,
                              maxit = 1000,
                              tolerance = 1e-8,
                              n_imputations = 10) {
  L <- R <- ID <- S_imputed <- O <- NULL

  # create a matrix to store the results from each imputed data set
  imputed_coef_ests <- matrix(
    nrow = n_imputations,
    ncol = 2,
    dimnames = list(NULL, c("theta0", "theta1"))
  )

  for (i in 1:n_imputations)
  {
    participant_level_data %<>%
      dplyr::mutate(
        S_imputed =
          L +
            stats::runif(
              n = dplyr::n(),
              min = 0,
              max = (R - L) / lubridate::ddays(1)
            )
      )

    obs_level_data %<>%
      dplyr::select(-dplyr::any_of("S_imputed")) %>%
      dplyr::left_join(participant_level_data %>% dplyr::select(ID, S_imputed),
        by = "ID"
      ) %>%
      dplyr::mutate(T_imputed = (O - S_imputed) / lubridate::ddays(365))

    phi_model_est_imputed <-
      biglm::bigglm(
        epsilon = tolerance,
        maxit = maxit,
        quiet = TRUE,
        data = obs_level_data,
        family = stats::binomial(),
        Y ~ T_imputed
      )

    imputed_coef_ests[i, ] <- stats::coef(phi_model_est_imputed)
  }

  imputed_coef_ests_mean <- colMeans(imputed_coef_ests)
}
