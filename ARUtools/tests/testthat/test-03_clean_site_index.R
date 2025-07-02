test_that("clean_site_index()", {
  # Text file
  unlink("test.csv")
  readr::write_csv(example_sites, "test.csv")

  expect_error(clean_site_index("test.csv"), "Problems with data") |>
    suppressMessages()

  expect_message(
    i1 <- clean_site_index(
      "test.csv",
      name_aru_id = "ARU",
      name_site_id = "Sites",
      name_date_time = c("Date_set_out", "Date_removed"),
      name_coords = c("lon", "lat")
    ),
    "overlapping date ranges"
  )

  expect_s3_class(i1, "data.frame")
  expect_named(i1, c(
    "site_id", "aru_id", "date_time_start", "date_time_end",
    "date_start", "date_end", "longitude", "latitude"
  ))
  expect_s3_class(i1[["date_time_start"]], "POSIXct")
  expect_s3_class(i1[["date_end"]], "Date")
  expect_s3_class(i1[["date_start"]], "Date")
  expect_s3_class(i1[["date_end"]], "Date")
  expect_equal(example_sites$lon, i1$longitude)
  expect_equal(example_sites$lat, i1$latitude)
  unlink("test.csv")

  # Data Frame
  expect_message(
    i2 <- clean_site_index(
      example_sites,
      name_aru_id = "ARU",
      name_site_id = "Sites",
      name_date_time = c("Date_set_out", "Date_removed"),
      name_coords = c("lon", "lat")
    ),
    "overlapping date ranges"
  )

  expect_equal(i1, i2)

  # Tibble
  expect_message(
    i3 <- clean_site_index(
      dplyr::as_tibble(example_sites),
      name_aru_id = "ARU",
      name_site_id = "Sites",
      name_date_time = c("Date_set_out", "Date_removed"),
      name_coords = c("lon", "lat")
    ),
    "overlapping date ranges"
  )

  expect_equal(i1, i3)

  # sf
  example_sites_sf <- sf::st_as_sf(example_sites,
    coords = c("lon", "lat"),
    crs = 4326
  )
  expect_message(
    i4 <- clean_site_index(
      example_sites_sf,
      name_aru_id = "ARU",
      name_site_id = "Sites",
      name_date_time = c("Date_set_out", "Date_removed")
    ),
    "overlapping date ranges"
  )

  expect_equal(
    sf::st_drop_geometry(i4),
    dplyr::select(i2, -"longitude", -"latitude")
  )
})

test_that("clean_site_index() extra cols", {
  expect_message(i <- clean_site_index(
    example_sites,
    name_aru_id = "ARU",
    name_site_id = "Sites",
    name_date_time = c("Date_set_out", "Date_removed"),
    name_coords = c("lon", "lat"),
    name_extra = c("Plots", "Subplot")
  ))

  expect_named(i, c(
    "site_id", "aru_id", "date_time_start", "date_time_end",
    "date_start", "date_end", "longitude", "latitude",
    "Plots", "Subplot"
  ))

  expect_equal(example_sites$Plots, i$Plots)
  expect_equal(example_sites$Subplot, i$Subplot)

  expect_message(i <- clean_site_index(
    example_sites,
    name_aru_id = "ARU",
    name_site_id = "Sites",
    name_date_time = c("Date_set_out", "Date_removed"),
    name_coords = c("lon", "lat"),
    name_extra = c("plot" = "Plots", "sub" = "Subplot")
  ))

  expect_named(i, c(
    "site_id", "aru_id", "date_time_start", "date_time_end",
    "date_start", "date_end", "longitude", "latitude",
    "plot", "sub"
  ))

  expect_equal(example_sites$Plots, i$plot)
  expect_equal(example_sites$Subplot, i$sub)
})

test_that("clean_site_index() overlapping dates", {
  m <- example_sites
  m$Sites <- "first"
  m$Date_removed[2] <- m$Date_set_out[3]

  expect_message(
    i <- clean_site_index(
      m,
      name_aru_id = "ARU",
      name_site_id = "Sites",
      name_date_time = c("Date_set_out", "Date_removed"),
      name_coords = c("lon", "lat")
    ),
    "overlapping date ranges"
  )

  expect_true(all(lubridate::hour(i$date_time_start) == 12))
  expect_true(all(lubridate::hour(i$date_time_end) == 12))
})

test_that("clean_site_index() single date", {
  expect_silent(i <- clean_site_index(
    example_sites,
    name_aru_id = "ARU",
    name_site_id = "Sites",
    name_date_time = c("Date_set_out"),
    name_coords = c("lon", "lat")
  ))
})

test_that("clean_site_index() date_times", {
  expect_silent(clean_site_index(example_sites_clean,
    name_date_time = c("date_time_start")
  ))
})

test_that("clean_site_index() no dates", {
  # tibble
  expect_silent(s <- clean_site_index(
    example_sites_clean,
    name_date_time = NULL
  ))
  expect_named(s, c("site_id", "aru_id", "longitude", "latitude"))

  # sf
  sites_sf <- sf::st_as_sf(
    example_sites_clean,
    coords = c("longitude", "latitude"), crs = 4326
  )
  expect_silent(s <- clean_site_index(sites_sf, name_date_time = NULL))
  expect_s3_class(s, "sf")
  expect_named(s, c("site_id", "aru_id", "geometry"))
})

test_that("clean_site_index() errors etc.", {
  e <- dplyr::mutate(example_sites_clean, date = "2020-05-06 01:00:00")
  expect_silent(clean_site_index(e))

  e <- dplyr::mutate(example_sites_clean, date = "14/05/2020")
  expect_error(clean_site_index(e), "not a Date or Date-Time column")

  # Timezone problems
  e <- dplyr::mutate(
    example_sites_clean,
    date_time = lubridate::with_tz(date_time_start, "America/Toronto")
  )
  expect_message(
    clean_site_index(e, name_date_time = "date_time"),
    "Removing timezone specification"
  )
  expect_error(
    clean_site_index(e, name_date_time = c("date_time", "date_time_end")),
    "Multiple timezones detected in `date_time` columns"
  )

  # No problem with shifting to noon
  expect_message(s1 <- clean_site_index( # Original
    example_sites_clean,
    name_date_time = c("date_start", "date_end")
  ))
  expect_message(
    s2 <- clean_site_index( # TZ set by user
      e,
      name_date_time = c("date_start", "date_end")
    ),
    "overlapping date ranges"
  )
  expect_equal(s1, s2) # Same in the end


  # With sf
  e <- dplyr::mutate(
    example_sites_clean,
    date_time = lubridate::with_tz(date_time_start, "America/Toronto")
  ) |>
    sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
  expect_message(
    clean_site_index(e, name_date_time = "date_time"),
    "Removing timezone specification"
  )
  expect_error(
    clean_site_index(e, name_date_time = c("date_time", "date_time_end")),
    "Multiple timezones detected in `date_time` columns"
  )
})
