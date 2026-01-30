#' plot estimated and true CDFs for seroconversion date distribution
#'
#' @param true_hazard_alpha The data-generating hazard at the start of the study
#' @param true_hazard_beta The change in data-generating hazard per calendar year
#' @param omega.hat tibble of estimated discrete hazards
#'
#' @return a ggplot
#' @export
#' @examples
#' \dontrun{
#'
#' hazard_alpha = 1
#' hazard_beta = 0.5
#' study_data <- simulate_interval_censoring(
#'   "hazard_alpha" = hazard_alpha,
#'   "hazard_beta" = hazard_beta)
#'
#' # fit model:
#' EM_algorithm_outputs <- fit_joint_model(
#'   obs_level_data = study_data$obs_data,
#'   participant_level_data = study_data$pt_data
#' )
#' plot1 = plot_CDF(
#'   true_hazard_alpha = hazard_alpha,
#'   true_hazard_beta = hazard_beta,
#'   omega.hat = EM_algorithm_outputs$Omega)
#'
#' print(plot1)
#' }
#'
#' @importFrom dplyr mutate lag filter
#' @importFrom ggplot2 ggplot aes geom_step xlab ylab geom_function scale_colour_discrete scale_linetype_discrete theme element_text element_blank element_line margin
#' @importFrom lubridate ymd ddays
plot_CDF <- function(true_hazard_alpha,
                     true_hazard_beta,
                     omega.hat) {
  `P(S>s|S>=s,E=e)` <- `P(S>s|E=0)` <- `P(S>=s|E=0)` <- S <- NULL
  cum_haz_fn0 <- function(years_since_study_start) {
    true_hazard_alpha * years_since_study_start + true_hazard_beta / 2 * years_since_study_start^2
  }

  cum_haz_fn <- function(years_since_study_start,
                         `years from study start to enrollment`) {
    cum_haz_fn0(years_since_study_start) - cum_haz_fn0(`years from study start to enrollment`)
  }

  surv_fn <- function(years_since_study_start,
                      `years from study start to enrollment`) {
    exp(-cum_haz_fn(
      years_since_study_start,
      `years from study start to enrollment`
    ))
  }

  true_model_label <- "Data-generating model"
  est_model_label <- "Estimated model"

  lwd1 <- 1
  omega.hat %<>%
    dplyr::mutate(
      "P(S>s|E=0)" = cumprod(`P(S>s|S>=s,E=e)`),
      "P(S>=s|E=0)" = dplyr::lag(`P(S>s|E=0)`, default = 1)
    )

  plot1 <- ggplot2::ggplot(
    ggplot2::aes(
      y = 1 - `P(S>=s|E=0)`,
      x = (S - lubridate::ymd("2001-01-01")) / lubridate::ddays(365)
    ),
    data = omega.hat %>% dplyr::filter(S < max(S))
  ) +
    ggplot2::geom_step(
      direction = "hv",
      ggplot2::aes(
        col = est_model_label,
        linetype = est_model_label
      ),
      lwd = lwd1
    ) +
    ggplot2::xlab("Calendar time (years since start of study)") +
    ggplot2::ylab("Probability of seroconversion") +
    ggplot2::geom_function(
      fun = function(x) {
        1 - surv_fn(x, 0)
      },
      ggplot2::aes(
        col = true_model_label,
        linetype = true_model_label
      ),
      lwd = lwd1
    ) +
    ggplot2::scale_colour_discrete("") +
    ggplot2::scale_linetype_discrete("") +
    ggplot2::theme(
      # axis.title.x = ggplot2::element_text(size = 20),
      # axis.text.x = ggplot2::element_text(size = 14),
      # axis.title.y = ggplot2::element_text(size = 20),
      # axis.text.y = ggplot2::element_text(size = 14),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      legend.position = "top",
      legend.text =
        ggplot2::element_text(
          # size = 14,
          margin = ggplot2::margin(r = 10, unit = "pt")
        )
    )
}
