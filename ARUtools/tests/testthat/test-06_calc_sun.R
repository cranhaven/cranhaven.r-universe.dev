# Data for tests ---------------------
expect_silent({
  dts <- dplyr::tibble(
    date = lubridate::as_date(c("2022-06-27", "2021-08-10")),
    longitude = c(-94.10, -93.00),
    latitude = c(52.02, 51.90)
  )
})

test_that("calc_ss()", {
  # Compared to https://gml.noaa.gov/grad/solcalc/ ==> Good

  expect_silent(ss <- calc_ss(dts, tz = "America/Toronto"))
  expect_named(ss, c("date", "longitude", "latitude", "sunrise", "sunset"))
  expect_equal(ss[, 1:3], dts)


  skip_on_os(os = "mac", arch = "aarch64")
  # Results in UTC but correct 'local' time
  expect_equal(
    ss$sunrise,
    lubridate::as_datetime(c(
      "2022-06-27 05:59:28",
      "2021-08-10 06:50:07"
    ), tz = "UTC")
  )
  expect_equal(
    ss$sunset,
    lubridate::as_datetime(c(
      "2022-06-27 22:41:50",
      "2021-08-10 21:47:13"
    ), tz = "UTC")
  )

  # Handles different time zones
  expect_silent(ss2 <- calc_ss(dts, tz = "America/Winnipeg"))
  expect_equal(ss, dplyr::mutate(ss2,
    sunrise = sunrise + lubridate::hours(1),
    sunset = sunset + lubridate::hours(1)
  ))

  # Suffix
  expect_silent(ss <- calc_ss(dts, tz = "America/Toronto", suffix = "_test"))
  expect_named(ss, c(
    "date_test", "longitude", "latitude",
    "sunrise_test", "sunset_test"
  ))
})

test_that("calc_all_ss()", {
  expect_silent(ss <- calc_all_ss(dts, tz = "America/Toronto"))
  expect_named(ss, c(
    "date", "longitude", "latitude", "sunrise", "sunset",
    "date_before", "sunrise_before", "sunset_before",
    "date_after", "sunrise_after", "sunset_after"
  ))

  # Expect dates sequential
  expect_true(all(ss$date == dts$date))
  expect_true(all(ss$date_before == ss$date - lubridate::days(1)))
  expect_true(all(ss$date == ss$date_after - lubridate::days(1)))

  # Expect each day sunrise/sunset earlier (both dates after solstice)
  expect_true(all(ss$sunrise_after > ss$sunrise))
  expect_true(all(ss$sunrise > ss$sunrise_before))
  expect_true(all(ss$sunset_after > ss$sunset))
  expect_true(all(ss$sunset > ss$sunset_before))
})

test_that("sun_diff()", {
  t <- lubridate::as_datetime(c(
    "2022-06-27 05:59:28",
    "2022-06-27 06:30:00"
  ))

  expect_silent(d1 <- sun_diff(t[1], t[2]))
  expect_type(d1, "double")
  expect_equal(d1, 30.53333, tolerance = 0.001)

  # Expect non-abs values
  expect_silent(d2 <- sun_diff(t[2], t[1]))
  expect_equal(d1, -d2)
})

test_that("calc_ss_diff()", {
  ss <- calc_all_ss(dts, tz = "America/Toronto") |>
    dplyr::mutate(date_time = lubridate::as_datetime(paste0(date, " 06:30:00")))

  expect_silent(diff <- calc_ss_diff(ss))
  expect_true(all(c(
    "t2sr", "t2sr_day_of", "t2sr_before", "t2sr_after",
    "t2ss", "t2ss_day_of", "t2ss_before", "t2ss_after", "doy"
  ) %in%
    names(diff)))
  expect_equal(diff$t2sr, c(30.53333, -20.11667), tolerance = 0.0001)
  expect_equal(diff$t2ss, c(468.0833, 520.9000), tolerance = 0.0001)
  skip_on_os(os = "mac", arch = "aarch64")
  expect_true(all(floor(abs(dplyr::select(diff, starts_with("t2")))) < (48 * 60)) )# at most two day away
})

test_that("calc_sun()", {
  # "local" timezone from lon/lat
  expect_silent(s1 <- calc_sun(example_clean))
  expect_equal(example_clean, dplyr::select(s1, -"tz", -"t2sr", -"t2ss"))
  expect_equal(
    unique(s1$tz),
    c(
      "America/Toronto", "America/Detroit", "America/Winnipeg",
      "America/Chicago"
    )
  )

  # Same sunrise/sunset for each unique combo of date, loc, and tz
  expect_equal(
    dplyr::distinct(s1, date, longitude, latitude, tz, t2sr, t2ss) |> nrow(),
    dplyr::distinct(s1, date, longitude, latitude, tz) |> nrow()
  )

  # Specified timezone
  expect_silent(s2 <- calc_sun(example_clean, aru_tz = "America/Toronto"))
  expect_equal(example_clean, dplyr::select(s2, -"tz", -"t2sr", -"t2ss"))
  expect_equal(unique(s2$tz), "America/Toronto")

  # Same sunrise/sunset for each unique combo of date and loc
  expect_equal(
    dplyr::distinct(s2, date, longitude, latitude, t2sr, t2ss) |> nrow(),
    dplyr::distinct(s2, date, longitude, latitude) |> nrow()
  )

  # Expect offsets compared to "local" timezones
  i <- which(s1$tz %in% c("America/Winnipeg", "America/Chicago"))
  expect_equal(s1$t2sr[i], 60 + s2$t2sr[i]) # After sunrise
  expect_equal(s1$t2ss[i], s2$t2ss[i] + 60) # Before sunset

  # Expect others to be the same
  expect_equal(s1$t2sr[-i], s2$t2sr[-i])


  # SF input - Works and returns sf output
  example_clean_sf <- sf::st_as_sf(example_clean,
    coords = c("longitude", "latitude"),
    crs = 4326
  )

  expect_silent(s_sf <- calc_sun(example_clean_sf))
  expect_equal(example_clean_sf, dplyr::select(s_sf, -"tz", -"t2sr", -"t2ss"))
  expect_equal(df_to_sf(s1, s_sf), s_sf)
})

test_that("calc_sun() errors etc.", {
  # Timezone problems
  e <- dplyr::mutate(example_clean, date_time = lubridate::force_tz(date_time, "America/Toronto"))
  expect_message(
    s1 <- calc_sun(e),
    "Removing timezone specification"
  )
  expect_equal(example_clean, dplyr::select(s1, -"tz", -"t2sr", -"t2ss"))
  expect_equal(
    unique(s1$tz),
    c(
      "America/Toronto", "America/Detroit", "America/Winnipeg",
      "America/Chicago"
    )
  )

  # Don't replace dates
  e <- example_clean
  e$date_time[1:5] <- NA
  expect_silent(s2 <- calc_sun(e))
  expect_equal(e$date_time, s2$date_time)
  expect_equal(e$date, s2$date)
  expect_equal(is.na(e$date_time), is.na(s2$t2sr))
})
