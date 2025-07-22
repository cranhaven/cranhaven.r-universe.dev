
#' Construct geom_weekly_timeslots function
#' @inheritParams ggplot2::layer
#' @param data (`SlotsDetails`) A \link{SlotsDetails} data table.
#' @param ... Other arguments passed on to [layer()]. These are
#'   often aesthetics, used to set an aesthetic to a fixed value, like
#'   `colour = "red"` or `size = 3`. They may also be parameters
#'   to the paired geom/stat.
#'
#' @return (`ggplot`) A ggplot object.
#' @export
#'
#' @examples
#' geom_weekly_timeslots(data = SlotsDetails())
#'
geom_weekly_timeslots <- function(mapping = NULL, data = NULL, ...) {
  .assert_SlotsDetails(data)

  null_ggplot <- ggplot2::ggplot() + ggplot2::geom_blank()
  if(nrow(data) == 0){return(null_ggplot)}

  (
    date_limits <- data
    |> dplyr::mutate(week_number = lubridate::ceiling_date(lubridate::as_date(end_datetime), unit = "1 week"))
    |> dplyr::filter(week_number == max(week_number))
    |> dplyr::summarise(start_date = lubridate::as_date(min(start_datetime)), end_date = lubridate::as_date(max(end_datetime)))
    |> dplyr::mutate(start_date = lubridate::floor_date(start_date, unit = "1 week"), end_date = lubridate::ceiling_date(end_date, unit = "1 week"))

  )

  (
    fig <- data
    |> dplyr::mutate(
      xmin = lubridate::as_date(start_datetime),
      xmax = lubridate::as_date(end_datetime) +1,
      ymin = format(start_datetime, format ="%H:%M"),
      ymax = format(end_datetime, format ="%H:%M")
    )
    |> ggplot2::ggplot(ggplot2::aes(xmin = xmin, xmax = xmax , ymin = ymin, ymax = ymax, label = title))
    + ggplot2::geom_rect(colour = "grey50")
    + ggplot2::scale_x_date(date_breaks = "1 day", labels = scales::label_date_short(), limits = c(date_limits$start_date, date_limits$end_date))
    + ggfittext::geom_fit_text(grow = TRUE)
  )

  return(fig)
}
