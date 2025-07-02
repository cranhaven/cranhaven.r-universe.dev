test_that("count_files()", {
  e <- temp_files()

  expect_silent(cnt <- count_files(fs::path_temp()))
  expect_s3_class(cnt, "data.frame")
  expect_named(cnt, c("dir", "n"))
  expect_type(cnt$dir, "character")
  expect_type(cnt$n, "integer")
  expect_gte(nrow(cnt), length(unique(fs::path_dir(example_files)))) # >= because may have other temp dirs

  # Subsets
  expect_silent(cnt2 <- count_files(fs::path_temp(), subset = "BARL"))
  expect_lt(nrow(cnt2), nrow(cnt))
  expect_equal(nrow(cnt2), sum(stringr::str_detect(cnt2$dir, "BARL")))

  # Subsets negated
  expect_silent(cnt3 <- count_files(fs::path_temp(), subset = "BARL", subset_type = "omit"))
  expect_lt(nrow(cnt3), nrow(cnt))
  expect_equal(nrow(cnt3), sum(stringr::str_detect(cnt3$dir, "BARL", negate = TRUE)))

  unlink(fs::path_dir(e), recursive = TRUE)
})

test_that("check_meta()", {
  m <- clean_metadata(project_files = example_files, quiet = TRUE)
  expect_silent(chk <- check_meta(m))
  expect_s3_class(chk, "data.frame")
  expect_named(
    chk, c(
      "site_id", "aru_type", "aru_id", "type", "n_files", "n_dirs",
      "n_days", "min_date", "max_date", "min_time", "max_time"
    )
  )
  expect_true(all(m$site_id %in% chk$site_id))
  expect_true(all(m$aru_id %in% chk$aru_id))


  expect_silent(chk2 <- check_meta(m, date = TRUE))
  expect_s3_class(chk2, "data.frame")
  expect_named(
    chk2, c(
      "site_id", "aru_type", "aru_id", "type", "date", "n_files", "n_dirs",
      "n_days", "min_time", "max_time"
    )
  )
  expect_true(all(m$date %in% chk2$date))
  expect_gt(nrow(chk2), nrow(chk))
})


test_that("check_problems() - basic meta", {
  m <- clean_metadata(project_files = example_files, quiet = TRUE)
  m <- dplyr::arrange(m, match(.data$path, sort(example_files))) # Keep original order for this test
  m$aru_id[c(3, 7, 10)] <- NA_character_
  m$site_id[7] <- m$site_id[3]

  # Pull out problems
  expect_silent(chk <- check_problems(m))
  expect_equal(
    chk,
    m[c(3, 7, 10), c("path", "aru_id", "site_id","tz_offset", "date_time", "date")]
  )
  expect_equal(nrow(chk), 3)

  # Summarize by date
  expect_silent(chk2 <- check_problems(m, date = TRUE))
  expect_equal(nrow(chk2), 2)
  expect_equal(chk2$date_time_n, c(1, 2))
  expect_equal(chk2$date_time_n_na, c(0, 0))

  # Identify missing by date
  m$date_time[7] <- NA
  expect_silent(chk3 <- check_problems(m, date = TRUE))
  expect_equal(chk3$date_time_n, c(1, 2))
  expect_equal(chk3$date_time_n_na, c(0, 1))

  # Return paths
  expect_silent(chk4 <- check_problems(m, path = TRUE))
  expect_equal(chk4, dplyr::pull(m, "path")[c(3, 7, 10)])
})


test_that("check_problems() - meta with coords", {
  m <- clean_metadata(project_files = example_files, quiet = TRUE) |>
    add_sites(example_sites_clean, quiet = TRUE) |>
    dplyr::arrange(path)
  m$aru_id[c(3, 7, 10)] <- NA_character_
  m$site_id[7] <- m$site_id[3]
  m$latitude[3] <- NA_real_
  m$longitude[7] <- NA_real_

  # Pull out problems
  expect_silent(chk <- check_problems(m))
  expect_equal(
    chk,
    m[c(3, 7, 10), c("path", "aru_id", "site_id", "tz_offset", "date_time", "date", "longitude", "latitude")]
  )
  expect_equal(nrow(chk), 3)

  # Summarize by date
  expect_silent(chk2 <- check_problems(m, date = TRUE))
  expect_equal(nrow(chk2), 2)
  expect_equal(chk2$date_time_n, c(1, 2))
  expect_equal(chk2$date_time_n_na, c(0, 0))
  expect_equal(chk2$longitude_n, c(1, 2))
  expect_equal(chk2$latitude_n, c(1, 2))
  expect_equal(chk2$longitude_n_na, c(0, 1))
  expect_equal(chk2$latitude_n_na, c(0, 1))

  # Return paths
  expect_silent(chk4 <- check_problems(m, path = TRUE))
  expect_equal(chk4, dplyr::pull(m, "path")[c(3, 7, 10)])
})


test_that("check_file()", {
  p <- test_path("test.txt")
  writeLines(paste0(paste0("test", 1:20), collapse = "\n"), p)

  expect_silent(f <- check_file(p))
  expect_equal(paste0("test", 1:10), f)

  expect_silent(f <- check_file(p, n_max = 20, progress = FALSE))
  expect_equal(paste0("test", 1:20), f)

  unlink(p)
})

test_that("add_wildtrax()", {
  suppressMessages(m <- clean_metadata(project_files = example_files))
  expect_false("wildtrax_file_name" %in% names(m))
  expect_silent(m <- add_wildtrax(m))
  expect_true("wildtrax_file_name" %in% names(m))
  expect_equal(
    m$wildtrax_file_name[1:2],
    c(
      "P01_1_20200502_050000",
      "P01_1_20200503_052000"
    )
  )
})

test_that("acoustic_indices()", {
  w <- tuneR::sine(440, duration = 300000) # > 5s
  tuneR::writeWave(w, "test_wave.wav")

  invisible(capture.output(expect_message(acoustic_indices("test_wave.wav"))))
  expect_silent(a <- acoustic_indices("test_wave.wav", quiet = TRUE))
  expect_s3_class(a, "data.frame")
  expect_named(a)

  unlink("test_wave.wav")
})

test_that("acoustic_indices() errors", {
  w <- tuneR::sine(440, duration = 30000) # < 5s
  tuneR::writeWave(w, "test_wave.wav")

  invisible(capture.output(
    expect_error(
      acoustic_indices("test_wave.wav"),
      "Error in `acoustic_complexity\\(\\)` from the soundecology package"
    ) |>
      expect_message("Calculating acoustic indices")
  ))

  unlink("test_wave.wav")
})


test_that("patterns",
          {
            expect_equal(
              .arutools$pattern_aru_type,
              get_pattern("pattern_aru_type")
            )
            x <- c(.arutools$pattern_aru_type,"the aru")
            names(x)[length(x)] <- "pattern"
            expect_equal(
              x,
              {add_pattern_aru_type("pattern", "the aru")
              get_pattern("pattern_aru_type")}
            )
            expect_error(add_pattern_aru_type(4, "cats"),
                         '`pattern` must be text')
            expect_error(add_pattern_aru_type("cats", 4),
                         '`aru_type` must be text')


            expect_equal(
              .arutools$pattern_check,
              get_pattern("pattern_check")
            )
            expect_equal(
              .arutools$pattern_data,
              get_pattern("pattern_data")
            )
            expect_equal(
              .arutools$pattern_date_time,
              get_pattern("pattern_date_time")
            )
            expect_equal(
              .arutools$pattern_sr,
              get_pattern("pattern_sr")
            )
            expect_equal(
              .arutools$pattern_ss,
              get_pattern("pattern_ss")
            )

            expect_error(
              set_pattern(1, "test"),
              "`pattern_name` must be text"
            )
            expect_error(
              set_pattern("bad_var", "test"),
              "`pattern_name` must be among"
            )
            expect_error(
              set_pattern("pattern_data", "not a list"),
              "pattern_data must be a list with the following values:"
            )

            expect_error(
              set_pattern("pattern_data", list(meta_serial = "test")),
              "pattern_data must be a list with the following values:"
            )

            og_pat <- get_pattern("pattern_date_time")
            set_pattern("pattern_date_time", create_pattern_date())

            expect_equal(
              create_pattern_date(),
              get_pattern("pattern_date_time")
            )

            set_pattern("pattern_date_time", og_pat)

            expect_equal(
              og_pat,
              get_pattern("pattern_date_time")
            )





          })
