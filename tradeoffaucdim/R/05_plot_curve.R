#' Plot curve
#'
#' Return plot features.
#'
#' @param obj object returned by \code{summary_statistics}
#'
#' @returns list with bootstrap samples with a model fit for each sample,
#' original data, string representing the independent variables, the outcome
#' variable, an integer representing the maximum number of dimensions, a string
#' representing the order of which variables are compared. Also, a tibble
#' summarizing the parameter statistics quantiles, and its graphical
#' representation.
#' @export
#'
#' @examples
#' plot_curve(obj4)
plot_curve <- function(obj){

  #initialize objects to NULL
  n_indeps <- perf_m <- model <- auc <- perf_q025 <-
    perf_q975 <- time_m <- time_q025 <-time_q975 <- value <- NULL

    #plot performance
    obj$plot_performance <- ggplot2::ggplot(data = obj$summary_stats )+
      ggplot2::geom_line(
        ggplot2::aes( x = n_indeps, y = perf_m, colour = model ))+
      ggplot2::geom_errorbar(ggplot2::aes( x = n_indeps, y = perf_m,
                         ymin = perf_q025, ymax= perf_q975,
                        colour = model),
                    width=.2, linetype = 1) +
      ggplot2::xlab("Dimensionality") +
      ggplot2::ylab(toupper(obj$perf_measure[1])) +
      ggplot2::labs(caption = paste0("Variable order: ",
                                     paste0(obj$ordered_indep_vars,
                                            collapse = ", ")))+
      ggplot2::theme_bw()

    #add plot histogram
    obj$plot_performance_hist <- ggplot2::ggplot(
      data = obj$bootstrap_data %>%
        dplyr::select(dplyr::starts_with(obj$perf_measure), "n_indeps") %>%
        tidyr::pivot_longer(cols = dplyr::starts_with(obj$perf_measure[1]),
                            names_sep = "_",
                            names_to = c("measure", "model"),
                            values_to = obj$perf_measure[1]) ,
      mapping = ggplot2::aes(x = auc, fill = model)
    )+
      ggplot2::geom_histogram() +
      ggplot2::facet_wrap(~n_indeps, scales = "free") +
      ggplot2::theme_bw()+
      ggplot2::xlab(toupper(obj$perf_measure[1])) +
      ggplot2::ylab("Count")

    #plot time
    obj$plot_time <- ggplot2::ggplot(data = obj$summary_stats )+
      ggplot2::geom_bar(ggplot2::aes( x = n_indeps, y = time_m, fill = model ),
                        stat = "identity", position =
                          ggplot2::position_dodge(0.9),
                        alpha = 0.5)+
      ggplot2::geom_errorbar(ggplot2::aes( x = n_indeps, y = time_m,
                                           ymin = time_q025, ymax= time_q975,
                                           colour = model),
                             position =  ggplot2::position_dodge(0.9),
                             width=.2, linetype = 1) +
      ggplot2::xlab("Dimensionality") +
      ggplot2::ylab("Time (in seconds)") +
      ggplot2::labs(caption = paste0("Variable order: ",
                                     paste0(obj$ordered_indep_vars, collapse = ", ")))+
      ggplot2::theme_bw()

    #plot time histogram
    obj$plot_time_hist <- ggplot2::ggplot(
      data = obj$bootstrap_data %>%
        dplyr::select(dplyr::starts_with("time"), "n_indeps") %>%
        tidyr::pivot_longer(cols = dplyr::starts_with("time"),
                            names_to = c("measure", "model"),
                            names_sep = "_",
                            values_to = "value") ,
      mapping = ggplot2::aes(x = value, fill = model)
    )+
      ggplot2::geom_histogram() +
      ggplot2::facet_wrap(~n_indeps, scales = "free") +
      ggplot2::theme_bw()+
      ggplot2::xlab("Time elapsed (in seconds)") +
      ggplot2::ylab("Count")

  return(obj)
}
