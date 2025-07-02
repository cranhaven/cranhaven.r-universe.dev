test_that("clean_metadata()", {
  expect_message(
    m <- clean_metadata(project_files = example_files),
    "Extracting ARU info.."
  ) |>
    expect_message("Extracting Dates and Times...")

  expect_s3_class(m, "data.frame")
  expect_named(m, c(
    "file_name", "type", "path",  "aru_id",
    'manufacturer', 'model',"aru_type",
    "site_id", "tz_offset",
    "date_time", "date"
  ))
  expect_equal(nrow(m), length(example_files))
  expect_equal(unique(m$aru_id), unique(example_clean$aru_id))
  expect_equal(unique(m$type), "wav")
  expect_equal(unique(m$aru_type), c("BARLT", "SongMeter"))
  expect_equal(unique(m$site_id), unique(example_clean$site_id))
  expect_true(all(!is.na(m$date_time)))
  expect_true(all(!is.na(m$date)))
})



test_that("clean_metadata() with multiple patterns in create_pattern_XXX", {
  f <- c(
    "P01_1/2020-01-01T09:00:00.wav",
    "P02_01/01012020 10:00.wav"
  )

  # Problem date/time, problem site_id
  expect_message(m <- clean_metadata(project_files = f)) |> suppressMessages()
  expect_equal(m$date_time, lubridate::as_datetime(c("2020-01-01 09:00:00", NA)))
  expect_equal(m$site_id, c("P01_1", "P02_0"))

  # Fix pattern matching of date
  expect_message(
    m <- clean_metadata(
      project_files = f,
      pattern_date = create_pattern_date(c("mdy", "ymd"), sep = c("", "-")),
      pattern_time = create_pattern_time(seconds = "maybe"),
      pattern_dt_sep = create_pattern_dt_sep(sep = c("T", " "))
    )
  ) |>
    suppressMessages()
  expect_equal(
    m$date_time,
    lubridate::as_datetime(c(
      "2020-01-01 09:00:00",
      "2001-01-20 20:10:00"
    ))
  )

  # Fix parsing of date
  expect_message(
    m <- clean_metadata(
      project_files = f,
      pattern_date = create_pattern_date(c("mdy", "ymd"), sep = c("", "-")),
      pattern_time = create_pattern_time(seconds = "maybe"),
      pattern_dt_sep = create_pattern_dt_sep(sep = c("T", " ")),
      order_date = c("mdy", "ymd")
    )
  ) |>
    suppressMessages()
  expect_equal(
    m$date_time,
    lubridate::as_datetime(c(
      "2020-01-01 09:00:00",
      "2020-01-01 10:00:00"
    ))
  )

  # Fix pattern matching of site_id
  expect_message(
    m <- clean_metadata(
      project_files = f,
      pattern_date = create_pattern_date(c("mdy", "ymd"), sep = c("", "-")),
      pattern_time = create_pattern_time(sep = ":", seconds = "maybe"),
      pattern_dt_sep = create_pattern_dt_sep(sep = c("T", " ")),
      order_date = c("mdy", "ymd"),
      pattern_site_id = create_pattern_site_id(s_digits = c(1, 2))
    )
  ) |>
    suppressMessages()
  expect_equal(m$site_id, c("P01_1", "P02_01"))
})

test_that("clean_metadata() with multiple patterns in args", {
  f <- c(
    "P01_1/2020-01-01T09:00:00.wav",
    "P02_01/01012020 10:00.wav"
  )

  # Fix pattern matching of date
  expect_message(
    m <- clean_metadata(
      project_files = f,
      pattern_date = c(
        create_pattern_date("ymd", sep = "-"),
        create_pattern_date("mdy", sep = "")
      ),
      pattern_time = c(
        create_pattern_time(seconds = "yes"),
        create_pattern_time(seconds = "no")
      ),
      pattern_dt_sep = c("T", " ")
    )
  ) |>
    suppressMessages()
  expect_equal(
    m$date_time,
    lubridate::as_datetime(c(
      "2020-01-01 09:00:00",
      "2001-01-20 20:10:00"
    ))
  )

  # Fix parsing of date
  expect_message(
    m <- clean_metadata(
      project_files = f,
      pattern_date = c(
        create_pattern_date("ymd", sep = "-"),
        create_pattern_date("mdy", sep = "")
      ),
      pattern_time = c(
        create_pattern_time(seconds = "yes"),
        create_pattern_time(seconds = "no")
      ),
      pattern_dt_sep = c("T", " "),
      order_date = c("mdy", "ymd")
    )
  ) |>
    suppressMessages()
  expect_equal(
    m$date_time,
    lubridate::as_datetime(c(
      "2020-01-01 09:00:00",
      "2020-01-01 10:00:00"
    ))
  )

  # Fix pattern matching of site_id
  expect_message(
    m <- clean_metadata(
      project_files = f,
      pattern_date = c(
        create_pattern_date("ymd", sep = "-"),
        create_pattern_date("mdy", sep = "")
      ),
      pattern_time = c(
        create_pattern_time(seconds = "yes"),
        create_pattern_time(seconds = "no")
      ),
      pattern_dt_sep = c("T", " "),
      order_date = c("mdy", "ymd"),
      pattern_site_id = c(
        create_pattern_site_id(),
        create_pattern_site_id(s_digits = 2)
      )
    )
  ) |>
    suppressMessages()
  expect_equal(m$site_id, c("P01_1", "P02_01"))
})

test_that("clean_metadata() with numeric aru_id", {
  bad <- example_files |>
    stringr::str_remove("BARLT|S4A") %>%
    clean_metadata(project_files = .) |>
    suppressMessages()
  expect_true(all(is.na(bad$aru_id)))

  good <- example_files |>
    stringr::str_remove("BARLT|S4A") %>%
    clean_metadata(
      project_files = .,
      pattern_aru_id = create_pattern_aru_id(
        arus = "", n_digits = 5, prefix = "(?<=_)", suffix = "(?=_)"
      )
    ) |>
    suppressMessages()
  expect_true(all(!is.na(good$aru_id)))
})
