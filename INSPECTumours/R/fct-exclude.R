#' Filter rows to exclude from the analysis
#' @param df initial df
#' @param study_id_ex string: study id
#' @param animal_id_ex string: animal id
#' @param day_ex string: day
#' @param reason string: why it should be excluded
#' @importFrom dplyr %>% filter mutate
#' @importFrom rlang .data
#' @return dataframe with rows that meets exclusion criteria
exclude_data <- function(df, study_id_ex, animal_id_ex, day_ex, reason) {

  if (animal_id_ex == "All") {
    exc_data <- df %>%
      filter(.data$study == study_id_ex)
  }
  else if (day_ex == "All") {
    exc_data <- df %>%
      filter(.data$study == study_id_ex &
               .data$animal_id == animal_id_ex)
  } else {
    exc_data <- df %>%
      filter(.data$study == study_id_ex &
               .data$animal_id == animal_id_ex &
               .data$day == day_ex)
  }
  return(mutate(exc_data, reason = reason))
}

#' makes df with data to be excluded
#' @param df initial data frame
#' @param min_points minimum number of data points for one animal_id per study
#' @return df
#' @importFrom dplyr %>% group_by filter ungroup mutate n
#' @importFrom rlang .data
below_min_points <- function(df, min_points) {
  ex_df <- df %>%
    group_by(.data$study, .data$animal_id) %>%
    filter(n() < min_points) %>%
    ungroup() %>%
    mutate(reason = "few_data_points") %>%
    as.data.frame()
  return(ex_df)
}
