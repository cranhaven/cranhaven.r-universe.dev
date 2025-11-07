#' Summary Stats
#'
#' Return summary statistics
#'
#' @param obj object returned from  \code{apply_model}
#'
#' @returns list with bootstrap samples with a model fit for each sample,
#' original data, string representing the independent variables, the outcome
#' variable, an integer representing the maximum number of dimensions, a string
#' representing the order of which variables are compared. Also, a tibble
#' summarizing the parameter statistics quantiles.
#' @export
#'
#' @examples
#' summary_stats(obj3)
summary_stats <- function(obj){

  #initialize objects to NULL
  measure <- value <- n_indeps <- model <- auc <- time <- NULL

  #bootstrap longer format
  obj$bootstrap_data_longer <- obj$bootstrap_data %>%
    tidyr::pivot_longer(cols = c(dplyr::starts_with(obj$perf_measure),
                                 dplyr::starts_with("time_")),
                        names_to = c("measure", "model"),
                        names_sep = "_",
                        values_to = "value") %>%
    tidyr::pivot_wider(names_from = measure,
                       values_from = value)

  #summary statistics
  obj$summary_stats <- obj$bootstrap_data_longer %>%
      dplyr::group_by(n_indeps, model) %>%
      dplyr::summarise(perf_m = mean(auc),
                perf_q025 = stats::quantile(auc, 0.025),
                perf_q975 = stats::quantile(auc, 0.975),
                time_m = mean(time),
                time_q025 = stats::quantile(time, 0.025),
                time_q975 = stats::quantile(time, 0.975)) %>%
      dplyr::ungroup()

  #store data
  obj$bootstrap_data_longer <- NULL

  return(obj)

}
