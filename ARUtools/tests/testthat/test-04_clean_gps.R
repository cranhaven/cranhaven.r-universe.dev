test_that("check_gps_files()", {
  p <- test_gps()
  expect_silent(g <- check_gps_files(data.frame(path = p, gps_ext = "csv")))
  expect_s3_class(g, "data.frame")
  expect_named(g, c("path", "skip", "problems_dt", "problems_tm", "problems_ll", "gps_ext"))
  expect_equal(g$skip, 2, ignore_attr = TRUE)

  p <- test_gps(skips = 3)
  expect_silent(g <- check_gps_files(data.frame(path = p, gps_ext = "csv")))
  expect_s3_class(g, "data.frame")
  expect_named(g, c("path", "skip", "problems_dt", "problems_tm", "problems_ll", "gps_ext"))
  expect_equal(g$skip, 3, ignore_attr = TRUE)

  p <- test_gps("lon", "lat")
  expect_silent(g <- check_gps_files(data.frame(path = p, ext = "csv")))

  p <- test_gps(time = "HH:MM")
  expect_silent(g <- check_gps_files(data.frame(path = p, ext = "csv")))

  p <- test_gps(date = "DD/MM/YY")
  expect_silent(g <- check_gps_files(data.frame(path = p, ext = "csv")))

  p <- test_gps("lon", "lat", "time", "date")
  expect_silent(g <- check_gps_files(data.frame(path = p, ext = "csv")))

  # Errors
  p <- test_gps("xxx", "yyy")
  expect_silent(g <- check_gps_files(data.frame(path = p, gps_ext = "csv")))
  expect_true(is.na(g$skip))
  expect_true(g$problems_ll)
  expect_true(!g$problems_dt & !g$problems_tm)

  p <- test_gps(time = "xxx", date = "yyyy")
  expect_silent(g <- check_gps_files(data.frame(path = p, gps_ext = "csv")))
  expect_true(is.na(g$skip))
  expect_true(g$problems_dt & g$problems_tm)
  expect_true(!g$problems_ll)

  unlink(p)
})

test_that("fmt_gps()", {
  # BARLT
  g1 <- dplyr::tibble(
    `Latitude (decimal degrees)` = c(45, 55),
    `Longitude (decimal degrees)` = c(-76, -84.3),
    `HH/MM` = c("07:30", "16:08"),
    `DD/MM/YY` = c("25/05/2021", "03/06/2021")
  ) |>
    readr::type_convert() |>
    suppressMessages()

  # SongMeter
  g2 <- dplyr::tibble(
    `LAT` = c(45, 55),
    `...2` = c("N", "n"),
    `LON` = c(76, 84.3),
    `...3` = c("w", "W"),
    TIME = c("07:30", "16:08"),
    DATE = c("2021-05-25", "2021-06-03")
  ) |>
    readr::type_convert() |>
    suppressMessages()

  # GPX
  g3 <- dplyr::tibble(
    time = c("2021-05-25 07:30:00", "2021-06-03 16:08:00"),
    lat = c(45, 55),
    lon = c(-76, -84.3)
  ) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

  g <- list(g1, g2, g3)
  ext <- c("csv", "csv", "gpx")
  # Expect the same results
  for (i in 1:3) {
    expect_silent(f <- fmt_gps(g[[i]], gps_ext = ext[i]))
    expect_s3_class(f, "data.frame")
    expect_named(f, c("longitude", "latitude", "date", "date_time"))
    expect_equal(f$longitude, g1[[2]]) # Compare to g1
    expect_equal(f$latitude, g1[[1]]) # Compare to g1
    expect_equal(f$date, lubridate::as_date(c("2021-05-25", "2021-06-03")))
    expect_equal(f$date_time, lubridate::as_datetime(c(
      "2021-05-25 07:30:00",
      "2021-06-03 16:08:00"
    )))
  }
})


test_that("check_gps_dist()", {
  # Warn if > 100
  expect_warning(
    g1 <- check_gps_dist(example_sites_clean,
      crs = 3161,
      dist_cutoff = 100, dist_by = "aru_id"
    ),
    "Within site distances are greater than cutoff"
  )

  # No Warning if dist < cutoff
  expect_silent(g2 <- check_gps_dist(example_sites_clean,
    crs = 3161,
    dist_cutoff = 1000000, dist_by = "aru_id"
  ))

  # Either way same data
  expect_equal(g1, g2)

  # No message if check turned off
  expect_silent(g3 <- check_gps_dist(example_sites_clean,
    crs = 3161,
    dist_cutoff = Inf, dist_by = "aru_id"
  ))

  # Similar data but no max_dist column
  expect_equal(dplyr::select(g1, -"max_dist"), g3)

  # No check if only 1 unique site per group
  expect_message(
    g4 <- check_gps_dist(
      example_sites_clean,
      crs = 3161,
      dist_cutoff = 100, dist_by = c("site_id", "aru_id")
    ),
    "Skipping distance check"
  )

  # Same as if check turned ff
  expect_equal(g3, g4)


  # Warning
  expect_warning(g5 <- check_gps_dist(example_sites_clean,
    crs = 4326,
    dist_cutoff = 100, dist_by = "aru_id"
  ))

  # Slightly different distances if use a different CRS
  expect_false(all(g1$max_dist == g5$max_dist)) # Not the same
  expect_equal(g1$max_dist, g5$max_dist, tolerance = 0.001) # But close enough
})

test_that("clean_gps()", {
  p <- test_gps(path = c("gps1.csv", "gps2.csv", "test.wav"))
  m <- clean_metadata(test_path(), quiet = TRUE) |>
    suppressMessages() |>
    dplyr::filter(fs::path_ext(file_name) != "R")

  expect_message(g <- clean_gps(m), "can be unreliable") |>
    expect_message("Skipping distance check") |>
    suppressMessages()

  expect_s3_class(g, "data.frame")
  expect_named(g, c(names(m), "gps_ext", "longitude", "latitude"), ignore.order = TRUE)
  expect_equal(g$longitude, rep(c(-76, -84.3), 2))
  expect_equal(g$latitude, rep(c(45, 55), 2))
  expect_equal(
    g$date,
    lubridate::as_date(rep(c("2021-05-25", "2021-06-03"), 2))
  )
  expect_equal(
    g$date_time,
    lubridate::as_datetime(rep(c("2021-05-25 07:30:00", "2021-06-03 16:08:00"), 2))
  )

  expect_error(clean_gps(m[0, ]), "No GPS data provided and no GPS log files")

  unlink(p)
})
