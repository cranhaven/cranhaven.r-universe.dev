#' Plot the change of a measure (or set of measures) over time where each measure is in a different column
#'
#' @param data A data frame or tibble
#' @param time_col Unquoted column name with time values to plot on the x axis
#' @param ... Unquoted column names of one or more measures to plot (up to 6 measures)
#' @return A ggplot plot object
#' @export
#' @examples
#' measure_change_over_time_wide(ggplot2::economics, date, pop, unemploy)
measure_change_over_time_wide <- function(data, time_col, ...) {
  tot <- NULL
  value <- series_type <- NULL
  tcol <- rlang::enquo(time_col)
  mcols <- rlang::enquos(...)
  if (length(mcols) > 6) {
    message("Sorry! This function supports a maximum of 6 measures")
    return(1)
  } else {
    d <- data %>%
      select(!!tcol, !!!mcols) %>%
      gather(!!!mcols, key = "series_type", value = "value")
    g <- ggplot(d, aes(!!tcol, value, linetype = series_type)) +
      geom_line()
  }
  g
}



#' Plot the change of a measure (or set of measures) over time where the data is in "long" format
#' That is, all measures are in one column with another column labeling each measure value
#'
#' @param data A data frame or tibble
#' @param time_col Unquoted column name with time values to plot on the x axis
#' @param measure_values Unquted column name of the column with the measure values to be plotted
#' @param measure_labels Unquoted column name containing the name of the measure in the corresponding measure_values (see below) row (up to 6 measures)
#' @param ... Unquoted names of measures to plot (up to 6 measures)
#' @return A ggplot plot object
#' @export
#' @examples
#' measure_change_over_time_long(ggplot2::economics_long, date, variable, value, pop, unemploy)

measure_change_over_time_long <- function(data, time_col, measure_labels, measure_values, ...) {
  tcol <- rlang::enquo(time_col)
  mlabs <- rlang::enquo(measure_labels)
  vals <- rlang::enquo(measure_values)
  to_plot <- rlang::ensyms(...)
  to_plot <- unlist(purrr::map(to_plot, toString), use.names = FALSE)
  data %>%
    filter(!!mlabs %in% to_plot) %>%
    ggplot(aes(!!tcol, !!vals, linetype = !!mlabs)) +
     geom_line()
}
