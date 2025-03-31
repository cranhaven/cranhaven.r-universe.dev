#' `graphCohort()` aids in the visualisation of cohorts timelines, useful to get a grip on intersections.
#'
#' @param subject_id Only one subject id per visualisation
#' @param cohorts List of cohorts
#'
#' @return A ggplot graph
#' @import dplyr
#' @import ggplot2
#' @importFrom tibble tibble
#'
#' @examples
#' hosptalised <- tibble::tibble(cohort_definition_id = 2,
#'                               subject_id = 1,
#'                               cohort_start_date = "2018-01-01",
#'                               cohort_end_date = "2018-01-10")
#'
#' icu_patients <- tibble::tibble(cohort_definition_id = 5,
#'                               subject_id = 1,
#'                               cohort_start_date = "2018-01-02",
#'                               cohort_end_date = "2018-01-04")
#'
#' drugs_treatment <- tibble::tibble(cohort_definition_id = 5,
#'                                   subject_id = 1,
#'                               cohort_start_date = "2018-01-07",
#'                               cohort_end_date = "2018-01-09")
#'
#' TestGenerator::graphCohort(subject_id = 1, cohorts = list("hosptalised" = hosptalised,
#'                                                       "icu_patients" = icu_patients,
#'                                                       "drugs_treatment" = drugs_treatment))
#' @export
graphCohort <- function(subject_id, cohorts = list()) {

  data <- compressCohorts(data = cohorts, id = subject_id)
  data %>%
    dplyr::mutate(record = as.character(row_number())) |>
    ggplot() +
    geom_segment(
      aes(
        x = cohort_start_date,
        y = cohort,
        xend = cohort_end_date,
        yend = cohort, col = cohort, fill = cohort
      ),
      size = 4.5, alpha = .5
    ) +
    geom_point(aes(x = cohort_start_date, y = cohort, color = cohort), size = 4) +
    geom_point(aes(x = cohort_end_date, y = cohort, color = cohort), size = 4) +
    ylab("") +
    xlab("") +
    theme(legend.position = "none") +
    ggtitle(glue::glue("subject {subject_id}"))

  # ggsave(here::here("results", glue::glue("subject{subject_id}.png")))
}

compressCohorts <- function(data, id) {
  result <- list()
  cohortName <- names(data)
  for (i in 1:length(data)) {
    cleanCohort <- data[[i]] %>%
      dplyr::filter(subject_id == id) %>%
      dplyr::mutate(cohort = cohortName[i])
    result[[cohortName[i]]] <- cleanCohort
  }
  result <- bind_rows(result)
  return(result)
}
