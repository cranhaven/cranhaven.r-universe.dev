#' Simulate a dataset with interval-censored seroconversion dates
#'
#' \code{simulate_interval_censoring} generates a simulated data set from a
#' data-generating model based on the typical structure of a cohort study of HIV
#' biomarker progression, as described in Morrison et al (2021); \doi{10.1111/biom.13472}.
#'
#' @references
#'
#' Morrison, Laeyendecker, and Brookmeyer (2021).
#' "Regression with interval-censored covariates: Application to cross-sectional incidence estimation".
#' Biometrics. \doi{10.1111/biom.13472}.
#'
#' @param study_cohort_size the number of participants to simulate (N_0 in the
#'   paper)
#' @param hazard_alpha the hazard (instantaneous risk) of seroconversion at the
#'   start date of the cohort study for those participants at risk of
#'   seroconversion
#' @param hazard_beta the change in hazard per calendar year
#' @param preconversion_interval_length the number of days between tests for
#'   seroconversion
#' @param theta the parameters of a logistic model (with linear functional from)
#'   specifying the probability of MAA-positive biomarkers as a function of time
#'   since seroconversion
#' @param probability_of_ever_seroconverting the probability that each
#'   participant is at risk of HIV seroconversion
#' @param years_in_study the duration of follow-up for each participant
#' @param max_scheduling_offset the maximum divergence of pre-seroconversion
#'   followup visits from the prescribed schedule
#' @param days_from_study_start_to_recruitment_end the length of the recruitment
#'   period
#' @param study_start_date the date when the study starts recruitment ("d_0" in
#'   the main text). The value of this parameter does not affect the simulation
#'   results; it is only necessary as a reference point for generating E, L, R,
#'   O, and S.

#' @return A list containing the following two tibbles:
#'
#' \itemize{
#' \item \code{pt_data}: a tibble of participant-level information, with the following columns:
#' \itemize{
#' \item \code{ID}: participant ID
#' \item \code{E}: enrollment date
#' \item \code{L}: date of last HIV test prior to seroconversion
#' \item \code{R}: date of first HIV test after seroconversion
#' }
#' \item \code{obs_data}: a tibble of longitudinal observations with the following columns:
#' \itemize{
#' \item \code{ID}: participant ID
#' \item \code{O}: dates of biomarker sample collection
#' \item \code{Y}: MAA classifications of biomarker samples
#' }}

#' @export
#'
#' @examples
#' study_data <- simulate_interval_censoring()
#' participant_characteristics <- study_data$pt_data
#' longitudinal_observations <- study_data$obs_data
#' @importFrom dplyr tibble if_else filter group_by summarize mutate n select
#' @importFrom lubridate days ddays
#' @importFrom stats rbinom runif
#' @importFrom magrittr %<>% %>%
simulate_interval_censoring <- function(study_cohort_size = 4500,
                                        hazard_alpha = 1,
                                        hazard_beta = 0.5,
                                        preconversion_interval_length = 84,
                                        theta = c(0.986, -3.88),
                                        probability_of_ever_seroconverting = 0.05,
                                        years_in_study = 10,
                                        max_scheduling_offset = 7,
                                        days_from_study_start_to_recruitment_end = 365,
                                        study_start_date = lubridate::ymd("2001-01-01")) {

  # this bit of code just removes some notes produced by check(); see
  # https://r-pkgs.org/package-within.html?q=no%20visible%20binding#echo-a-working-package
  ID <- E <- L <- R <- O <- Y <- S <- `exit date` <- `elapsed time` <- `years from study start to seroconversion` <-
    `years from study start to sample date` <- NULL

  # define p(Y=1|T=t):
  phi <- build_phi_function_from_coefs(theta)

  # define the sequence of post-diagnosis biomarker collection dates:
  post_seroconversion_obs_dates <-
    c(
      seq(0, 12 * 7, by = 28),
      # 0, 4, 8, and 12 weeks post-diagnosis
      seq(12 * 14, 365 * 2, by = 12 * 7),
      # then at 12 week intervals until two years
      seq(365 * 2, years_in_study * 365, by = 365)
    ) # then at 1 year intervals until drop-out or study end.

  # generate the number of study cohort participants who are at risk of
  # infection; this step takes the place of assigning A_i to each cohort
  # participant in the main text (the final data set only includes participants
  # with A_i = 1):
  n_at_risk <- stats::rbinom(
    n = 1,
    size = study_cohort_size,
    prob = probability_of_ever_seroconverting
  )

  # generate E (enrollment date), F (exit date), S (seroconversion date):
  sim_participant_data <- dplyr::tibble(
    "ID" = 1:n_at_risk,
    "E" =
      study_start_date +
        lubridate::days(
          sample(
            x = 0:days_from_study_start_to_recruitment_end,
            size = n_at_risk,
            replace = TRUE
          )
        ),
    `exit date` = E + years_in_study * lubridate::days(365),
    "years from study start to seroconversion" =
      seroconversion_inverse_survival_function(
        u = stats::runif(n = n_at_risk),
        e = (E - study_start_date) / lubridate::ddays(365),
        hazard_alpha = hazard_alpha,
        hazard_beta = hazard_beta
      ),
    S = study_start_date + `years from study start to seroconversion` * 365
    # note that this variable is rounded down at the day level; to compute T
    # below we will use the non-rounded value from `years from study start to
    # seroconversion`
  )

  # generate L, R:
  for (i in rownames(sim_participant_data))
  {
    E <- sim_participant_data[i, "E", drop = TRUE]
    S <- sim_participant_data[i, "S", drop = TRUE]

    preconversion_obs_dates <-
      E +
      seq(0, years_in_study * 365, by = preconversion_interval_length)

    # generate small random offsets from schedule in followup dates; when one
    # checkup is later than planned, the next is scheduled relative to last
    # checkup, rather than reverting to schedule; hence offsets accumulate:
    offsets <- cumsum(sample(
      x = (-max_scheduling_offset):max_scheduling_offset,
      size = length(preconversion_obs_dates) - 1,
      replace = TRUE
    ))

    preconversion_obs_dates[-1] <-
      preconversion_obs_dates[-1] + offsets

    sim_participant_data[i, "L"] <-
      max(preconversion_obs_dates[preconversion_obs_dates <= S])

    sim_participant_data[i, "R"] <-
      dplyr::if_else(any(preconversion_obs_dates > S),
        min(preconversion_obs_dates[preconversion_obs_dates > S]),
        as.Date(NA)
      )
  }

  # remove participants who wouldn't be diagnosed until after their their
  # followup period ends:
  sim_participant_data %<>%
    dplyr::filter(R <= `exit date`)

  # # generate O, Y:
  sim_obs_data <-
    sim_participant_data %>%
    dplyr::group_by(ID, R, `exit date`, `years from study start to seroconversion`) %>%
    dplyr::summarize(
      .groups = "drop",
      "O" = R + post_seroconversion_obs_dates
    ) %>%
    # everyone gets 10 years of observation, total, no longer how long they took
    # to seroconvert:
    dplyr::filter(O <= `exit date`) %>%
    dplyr::mutate(
      # we use relative times in order to calculate phi(t) with t = the elapsed
      # time from the exact moment of seroconversion until the date of testing
      # (we assume that all testing occurs at midnight at the beginning of the
      # scheduled day):
      "years from study start to sample date" = (O - study_start_date) / lubridate::ddays(365),
      "elapsed time" =
        `years from study start to sample date` - `years from study start to seroconversion`,
      "Y" = as.numeric(stats::rbinom(
        size = 1,
        n = dplyr::n(),
        p = phi(`elapsed time`)
        # could switch to rbernoulli here, but doing so would change a few of
        # the generated values
      ))
    )

  # remove variables not needed for analysis:
  {
    sim_participant_data %<>% dplyr::select(ID, E, L, R)
    sim_obs_data %<>% dplyr::select(ID, O, Y)
  }

  return(list(
    obs_data = sim_obs_data,
    pt_data = sim_participant_data
  ))
}
