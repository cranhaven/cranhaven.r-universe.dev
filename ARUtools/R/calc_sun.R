#' Calculate time to sunrise/sunset
#'
#' Calculate the sunrise/sunset of each sound file for the day of, the day before
#' and the day after to get the nearest sunrise to the recording. Times are
#' calculated using the 'suncalc' package.
#'
#' @param aru_tz Character. Must be either "local" or a timezone listed in
#'   `OlsonNames()`. See Details.
#'
#' @details Timezones. To ensure that the sunrise/sunset times are calculated
#'   correctly relative to the time of the recording, we need to know the
#'   timezone of the date/time of the recording. If ARUs were calibrated with a
#'   specific timezone before going into the field, that can be specified by
#'   using, for example, `aru_tz = "America/Toronto"`. If on the other hand each
#'   ARU was calibrated to whichever timezone was local when it was deployed use
#'   `aru_tz = "local"`. The specific timezone will be calculated individually
#'   based on the longitude and latitude of each recording.
#'
#' @inheritParams common_docs
#'
#' @return Data frame with metadata and added timezone of recording time (`tz`),
#'  and time to sunrise/sunset (`t2sr`, `t2ss`).
#' @export
#'
#' @examples
#' s <- clean_site_index(example_sites_clean,
#'   name_date = c("date_time_start", "date_time_end")
#' )
#' m <- clean_metadata(project_files = example_files) |>
#'   add_sites(s)
#' calc_sun(m)
#'
calc_sun <- function(meta_sites, aru_tz = "local") {
  # Checks
  check_data(meta_sites, type = "meta_sites", ref = "add_sites()")
  check_tz(aru_tz)
  meta_sites <- check_UTC(meta_sites)

  # Make sure date time correct format
  meta_sites <- dplyr::mutate(
    meta_sites,
    date_time = lubridate::as_datetime(.data$date_time),
    date = lubridate::as_date(.data$date)
  )

  # If sf, convert to df
  crs <- sf::st_crs(meta_sites)
  m <- sf_to_df(meta_sites)

  if (aru_tz == "local") {
    # Get timezones from location if not set globally

    tz <- dplyr::select(m, "longitude", "latitude") |>
      dplyr::distinct() |>
      tidyr::drop_na() |>
      dplyr::mutate(
        tz = lutz::tz_lookup_coords(
          lon = .data$longitude,
          lat = .data$latitude,
          method = "accurate"
        )
      )

    m <- dplyr::left_join(m, tz, by = c("longitude", "latitude"))
  } else {
    m <- dplyr::mutate(m, tz = .env$aru_tz)
  }

  # Calculate sunrise/sunset
  ss <- dplyr::select(m, "date", "tz", "longitude", "latitude") |>
    dplyr::distinct() |>
    tidyr::drop_na() |>
    tidyr::nest(dates = -"tz") |>
    dplyr::mutate(times = purrr::map2(
      .data$dates, .data$tz, ~ calc_all_ss(.x, tz = .y)
    )) |>
    tidyr::unnest("times") |>
    dplyr::select(-"dates", -"date_before", -"date_after")

  m <- dplyr::left_join(m, ss, by = c("date", "tz", "longitude", "latitude"))

  # Calculate time to sunrise/sunset
  m <- calc_ss_diff(m)

  # Arrange
  # - Match order of starting data (meta_sites)
  m <- dplyr::arrange(m, match(.data$path, meta_sites$path))

  m |>
    df_to_sf(crs = crs) |> # If was sf, convert back
    dplyr::select(dplyr::all_of(names(meta_sites)), "tz", "t2sr", "t2ss") |>
    dplyr::relocate(dplyr::any_of("geometry"), .after = dplyr::last_col())
}

#' Calculate sunrise and sunset times for range of dates
#'
#' For day of, day before and day after.
#'
#' @param dates Data frame. Containing dates, longitude and latitude
#' @param tz Character. Timezone that the ARUs were set to (i.e. timezone the
#'   date/times would be in). Must be valide tz from OlsonNames().
#'
#' @noRd
calc_all_ss <- function(dates, tz) {
  ss_day_of <- calc_ss(dates, tz)

  ss_day_before <- dates |>
    dplyr::mutate(date = date - lubridate::days(1)) |>
    calc_ss(tz, suffix = "_before") |>
    dplyr::bind_cols(date = dates$date)

  ss_day_after <- dates |>
    dplyr::mutate(date = date + lubridate::days(1)) |>
    calc_ss(tz, suffix = "_after") |>
    dplyr::bind_cols(date = dates$date)

  ss_day_of |>
    dplyr::left_join(ss_day_before, by = c("date", "longitude", "latitude")) |>
    dplyr::left_join(ss_day_after, by = c("date", "longitude", "latitude"))
}


#' Calculate sunrise sunset
#'
#' A wrapper around suncalc::getSunlightTimes to ensure
#'    timezones and longitude/latitude are correct.
#'
#' Compared to https://gml.noaa.gov/grad/solcalc/ ==> Good
#'
#' @param .data data frame from prep_sunrise_sunset
#' @param var_day variable from .data to use as date to calculate sunrise
#'
#' @return Returns a data frame with sunrise and sunset
#'
#' @noRd
calc_ss <- function(dates, tz, suffix = "") {
  dplyr::rename(dates, "lon" = "longitude", "lat" = "latitude") |>
    suncalc::getSunlightTimes(
      date = NULL, lat = NULL, lon = NULL,
      keep = c("sunrise", "sunset"), tz = tz) |>
    dplyr::mutate(
      sunrise = lubridate::force_tz(.data$sunrise, "UTC"),
      sunset = lubridate::force_tz(.data$sunset, "UTC")
    ) |>
    dplyr::rename(
      "date{suffix}" := "date",
      "sunrise{suffix}" := "sunrise",
      "sunset{suffix}" := "sunset",
      "longitude" = "lon",
      "latitude" = "lat"
    ) |>
    dplyr::relocate("longitude", .before = "latitude") |>
    dplyr::as_tibble()
}

#' Convert sunrise/sunset times to time-to-sunrise/sunset
#'
#' Takes the difference between the time of the event and the recording, for
#' each event (sunrise/sunset, day before/of/after).
#'
#' Returns the difference with the smallest absolute difference.
#'
#' @param sun_times Data frame with the times of the events and recordings
#'
#' @noRd
calc_ss_diff <- function(sun_times) {
  sun_times |>
    dplyr::mutate(
      t2sr_day_of = sun_diff(.data$sunrise, .data$date_time),
      t2sr_before = sun_diff(.data$sunrise_before, .data$date_time),
      t2sr_after = sun_diff(.data$sunrise_after, .data$date_time),
      t2ss_day_of = sun_diff(.data$sunset, .data$date_time),
      t2ss_before = sun_diff(.data$sunset_before, .data$date_time),
      t2ss_after = sun_diff(.data$sunset_after, .data$date_time),
      doy = lubridate::yday(.data$date)
    ) |>
    dplyr::mutate(
      t2sr = purrr::pmap_dbl(
        list(.data$t2sr_day_of, .data$t2sr_before, .data$t2sr_after), min_abs
      ),
      t2ss = purrr::pmap_dbl(
        list(.data$t2ss_day_of, .data$t2ss_before, .data$t2ss_after), min_abs
      )
    )
}

#' Calculate the diff time in minutes
#'
#' @param t1 Vector (POSIXct) First times
#' @param t2 Vector (POSIXct) Times to compare
#'
#' @noRd
sun_diff <- function(t1, t2) {
  as.numeric(difftime(t2, t1, units = "mins"))
}


#' Calculate abs min of three values
#'
#' Return the value where the absolute is the minimum. Returns only 1, even if
#' more than one matches the min(abs).
#'
#' @param x A value that you can take a minimum of (numeric, date, time, etc).
#' @param y A value that you can take a minimum of (numeric, date, time, etc).
#' @param z A value that you can take a minimum of (numeric, date, time, etc).
#'
#' @noRd
min_abs <- function(x, y, z) {
  c(x, y, z)[abs(c(x, y, z)) == pmin(abs(x), abs(y), abs(z))][1]
}
