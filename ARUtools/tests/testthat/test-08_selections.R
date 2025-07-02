test_that("sim_selection_weights()", {
  # Return params
  expect_silent(g <- sim_selection_weights(plot = FALSE))
  expect_type(g, "list")
  expect_named(g, c(
    "min_range", "min_mean", "min_sd",
    "day_range", "day_mean", "day_sd", "offset",
    "return_log", "selection_fun"
  ))

  expect_silent(g <- sim_selection_weights(selection_fun = "lognorm", offset = 71, plot = FALSE))
  expect_silent(g <- sim_selection_weights(selection_fun = "cauchy", plot = FALSE))
  expect_silent(g <- sim_selection_weights(return_log = FALSE, plot = FALSE))

  # Dates
  expect_silent(g <- sim_selection_weights(
    day_range = c("2023-01-01", "2023-09-01"),
    day_mean = "2023-06-02", plot = FALSE
  ))
  expect_equal(g$day_range, lubridate::yday(c("2023-01-01", "2023-09-01")))
  expect_equal(g$day_mean, lubridate::yday("2023-06-02"))
  expect_silent(g <- sim_selection_weights(day_range = c(1, 244), plot = FALSE))
  expect_equal(g$day_range, c(1, 244))

  # Plots - Defaults
  withr::with_seed(123, expect_silent(g <- sim_selection_weights(return_params = FALSE)))
  vdiffr::expect_doppelganger("sim_selection_weights1", g)

  # Plots - Change defaults
  withr::with_seed(
    123,
    expect_silent(g <- sim_selection_weights(
      min_range = c(-70, 240),
      day_range = c(121, 201),
      day_sd = 20,
      return_log = TRUE,
      selection_var = "psel_normalized",
      return_params = FALSE
    ))
  )
  vdiffr::expect_doppelganger("sim_selection_weights2", g)

  withr::with_seed(
    123,
    expect_silent(
      g <- sim_selection_weights(
        min_range = c(-60, 60 * 4),
        min_mean = -30,
        min_sd = 120,
        day_range = c(152, 210),
        day_mean = 170,
        day_sd = 200,
        return_log = FALSE,
        return_params = FALSE
      )
    )
  )
  vdiffr::expect_doppelganger("sim_selection_weights3", g)
})

test_that("calc_selection_weights()", {
  s <- clean_site_index(example_sites_clean,
    name_date = c("date_time_start", "date_time_end")
  )
  m <- clean_metadata(project_files = example_files, quiet = TRUE) |>
    add_sites(s, quiet = TRUE) |>
    calc_sun()

  p <- sim_selection_weights(plot = FALSE)
  withr::with_seed(123, expect_silent(pr1 <- calc_selection_weights(m, params = p)))


  # Use DOY rather than date
  m$doy <- lubridate::yday(m$date)
  withr::with_seed(123, expect_silent(pr2 <- calc_selection_weights(m, params = p, col_day = doy)))

  expect_equal(pr1, pr2)

  # Check lognormal and offsets
  p <- sim_selection_weights(plot = FALSE, selection_fun = "lognorm")
  expect_error(
    calc_selection_weights(m, params = p, col_day = doy),
    "you must provide an `offset`"
  )
  p <- sim_selection_weights(plot = FALSE, selection_fun = "lognorm", offset = 200)
  withr::with_seed(123, expect_silent(calc_selection_weights(m, params = p, col_day = doy)))

  # From calc_selection-weights above pr1
  # Moving to end to avoid skipping above tests
  skip_on_ci()
  expect_snapshot_value(pr1, style = "json2", tolerance = 0.0005)
})

test_that("sample_recordings()", {
  s <- clean_site_index(example_sites_clean,
    name_date = c("date_time_start", "date_time_end")
  )
  m <- clean_metadata(project_files = example_files, quiet = TRUE) |>
    add_sites(s, quiet = TRUE) |>
    calc_sun() |>
    calc_selection_weights(params = sim_selection_weights(plot = FALSE))

  m_sf <- df_to_sf(m, crs = 3161)

  # No stratification
  expect_silent(r1 <- sample_recordings(m, n = 12, os = 0.2, col_site_id = NULL, seed = 1234))
  expect_warning(r2 <- sample_recordings(m, n = 12, os = 0.2, seed = 1234))
  expect_silent(r3 <- sample_recordings(m_sf, n = 12, os = 0.2, col_site_id = NULL, seed = 1234))


  expect_equal(r1, r2)
  expect_equal(r1, r3, list_as_map = TRUE)

  expect_s3_class(r1, "sp_design")
  expect_named(r1, c(
    "sites_legacy", "sites_base", "sites_over",
    "sites_near", "design"
  ))
  sbase <- r1[["sites_base"]]
  expect_s3_class(sbase, "data.frame")
  expect_equal(nrow(sbase), 12)
  expect_true(all(sbase$site_id %in% m$site_id))

  expect_equal(nrow(r1[["sites_over"]]), round(12 * 0.2))
  expect_null(r1[["sites_legacy"]])
  expect_null(r1[["sites_near"]])

  # Stratify by site
  expect_silent(r4 <- sample_recordings(m, n = list(P01_1 = 2, P02_1 = 5, P03_1 = 2), os = 0.2, seed = 1234))
  expect_silent(r5 <- sample_recordings(m_sf, n = list(P01_1 = 2, P02_1 = 5, P03_1 = 2), os = 0.2, seed = 1234))
  expect_silent(r6 <- sample_recordings(m, n = c(P01_1 = 2, P02_1 = 5, P03_1 = 2), os = 0.2, seed = 1234))
  expect_silent(r7 <- sample_recordings(m,
    n = data.frame(
      site_id = c("P01_1", "P02_1", "P03_1"),
      n = c(2, 5, 2),
      n_os = c(0, 1, 0)
    ),
    seed = 1234
  ))
  expect_silent(r8 <- sample_recordings(m_sf, n = 12, os = 0, col_site_id = NULL, seed = 1234))
  expect_silent(r9 <- sample_recordings(m,
                                        n = data.frame(
                                          site_id = c("P01_1", "P02_1", "P03_1"),
                                          n = c(2, 5, 2),
                                          n_os = c(0, 0, 0)
                                        ),
                                        seed = 1234
  ))

  expect_equal(r4, r5, list_as_map = TRUE)
  expect_equal(r4, r6)
  expect_equal(r4, r7)
  expect_equal(r8$sites_over,  NULL)
  expect_equal(r9$sites_over,  NULL)


  # Errors
  expect_error(
    sample_recordings(m, n = 30, os = 0.2, col_site_id = NULL),
    "Cannot sample \\(n \\+ oversampling\\) more points than there are in the data"
  )
  expect_error(
    sample_recordings(m, n = 30, os = 0, col_site_id = NULL),
    "Cannot sample \\(n \\+ oversampling\\) more points than there are in the data"
  )
  expect_error(
    sample_recordings(m, n = 5, os = c(0.2, 0.5), col_site_id = NULL),
    "`os` must be a single value unless using stratification"
  )
  expect_error(
    sample_recordings(m, n = 30, os = 0.2, col_site_id = NULL),
    "samples, but only"
  )
  expect_error(
    sample_recordings(m, n = c(P01_1 = 2, P02_1 = 5, P03_1 = 5), os = 0.2),
    "Selected more samples than exist in some sites"
  )
  expect_error(
    sample_recordings(m, n = c(P01_1 = 2, P02_1 = 5, P03_1 = 5), os = 0),
    "Selected more samples than exist in some sites"
  )
  expect_error(
    sample_recordings(m, n = c(P01_1 = 2, P02_1 = 5, P03_1 = 2)),
    "`os` can only be NULL if"
  )
  expect_error(
    sample_recordings(m, n = c(P01_1 = 2, P02_1 = 5, P03_1 = 2), os = 2),
    "`os` as a single value is a proportion"
  )
  expect_error(
    sample_recordings(m, n = c(PXX_1 = 2, P02_1 = 5, P03_1 = 2), os = 0.1),
    "Not all requirements met for sampling with stratification by site"
  )


  # Snapshots cannot be tested interactively
  skip_on_ci()
  expect_snapshot_value(r1, style = "json2", tolerance = 0.0005)
  expect_snapshot_value(r4, style = "json2", tolerance = 0.0005)
})
