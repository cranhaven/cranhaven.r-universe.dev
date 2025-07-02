test_that("add_sites()", {
  # Prep test files
  meta <- clean_metadata(project_files = example_files, quiet = TRUE)
  m <- dplyr::mutate(meta, site_id = NA_character_)

  # Add site_ids by datetime
  expect_message(
    m1 <- add_sites(m, example_sites_clean, by = "aru_id"),
    "Joining by columns `date_time_start` and `date_time_end`"
  )
  expect_equal(
    dplyr::arrange(meta, file_name),
    dplyr::arrange(m1[names(meta)], file_name)
  )

  # Check that lon/lat added correctly
  # All site/aru/date/coord combos exist in site index data
  dts <- dplyr::group_by(example_sites_clean, site_id, aru_id, longitude, latitude) |>
    dplyr::reframe(date = seq(date_start, date_end, by = "1 day"))
  expect_equal(
    dplyr::anti_join(dplyr::select(m1, site_id, aru_id, date, longitude, latitude),
      dts,
      by = c("site_id", "aru_id", "date")
    ) |> nrow(),
    0
  )

  # Add site_ids by date
  expect_message(
    add_sites(m, example_sites_clean, by = "aru_id", by_date = "date"),
    "matched multiple site references"
  ) |>
    suppressMessages()

  # sf
  example_sites_clean_sf <- df_to_sf(example_sites_clean, crs = 4326)
  expect_message(m1 <- add_sites(m, example_sites_clean_sf, by = "aru_id")) |>
    suppressMessages()
  expect_s3_class(m1, "sf")
  expect_equal(nrow(m), nrow(m1))
})

test_that("add_sites() no dates", {
  # Prep test files
  m <- clean_metadata(project_files = example_files, quiet = TRUE)
  s <- dplyr::select(example_sites_clean, -"date_time_start", -"date_time_end") |>
    clean_site_index(name_date_time = NULL)

  # tibble
  expect_message(m1 <- add_sites(m, s, by_date = NULL), "Ignoring dates")
  expect_equal(
    dplyr::left_join(m, s, by = c("site_id", "aru_id")) |>
      dplyr::arrange(file_name),
    m1 |> dplyr::arrange(file_name)
  )

  # sf
  s_sf <- sf::st_as_sf(s, coords = c("longitude", "latitude"), crs = 4326)
  expect_message(m2 <- add_sites(m, s_sf, by_date = NULL), "Ignoring dates")
  expect_equal(
    dplyr::left_join(m, s_sf, by = c("site_id", "aru_id")) |>
      dplyr::arrange(file_name) |>
      sf::st_as_sf(),
    m2 |> dplyr::arrange(file_name)
  )
})

test_that("add_sites() average over coords", {
  i <- data.frame(
    site_id = c("P01", "P01", "P02", "P02"),
    aru_id = c("A", "A", "B", "B"),
    date = rep("2020-05-01", 4),
    longitude = c(-88, -88.3, -99, -99.1),
    latitude = c(50, 50.1, 52, 52.2)
  ) |>
    clean_site_index()

  meta <- dplyr::select(i, -"longitude", -"latitude") |>
    dplyr::mutate(file_name = "a", path = "b", type = "wav")

  expect_message(
    m <- add_sites(meta, i, by_date = "date"),
    "Taking mean coordinates"
  ) |>
    suppressMessages()
  expect_named(m, c(names(meta), "longitude", "latitude"))
  expect_equal(m$longitude, c(-88.15, -88.15, -99.05, -99.05))
  expect_equal(m$latitude, c(50.05, 50.05, 52.1, 52.1))
})


test_that("add_sites() errors etc.", {
  # Timezone problems
  m <- clean_metadata(project_files = example_files, quiet = TRUE) |>
    dplyr::mutate(date_time = lubridate::force_tz(date_time, "America/Toronto"))

  expect_message(
    m <- add_sites(m, example_sites_clean, by = "aru_id"),
    "Removing timezone specification"
  ) |>
    suppressMessages()

  # sf - timezone
  m <- clean_metadata(project_files = example_files, quiet = TRUE) |>
    dplyr::mutate(date_time = lubridate::force_tz(date_time, "America/Toronto"))

  example_sites_clean_sf <- df_to_sf(example_sites_clean, crs = 4326)

  expect_message(
    m <- add_sites(m, example_sites_clean_sf, by = "aru_id"),
    "Removing timezone specification"
  ) |>
    suppressMessages()

  # sf - missing sites
  example_sites_clean_sf <- df_to_sf(example_sites_clean, crs = 4326) |>
    dplyr::slice(-1)
  expect_warning(
    m1 <- add_sites(m, example_sites_clean_sf, by = "aru_id"),
    "Cannot have missing coordinates"
  ) |>
    suppressMessages()
  expect_s3_class(m1, "data.frame")
  expect_equal(nrow(m), nrow(m1))
})
