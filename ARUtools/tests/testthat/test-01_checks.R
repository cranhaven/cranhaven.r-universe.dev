test_that("check_ext()", {
  expect_silent(check_ext("csv", "csv"))
  expect_silent(check_ext("csv", c("xlsx", "csv")))
  expect_error(check_ext("csv", "txt"), "File extension must be")
  expect_error(check_ext("csv", c("txt", "xlsx")), "File extension must be")
})

test_that("check_value()", {
  expect_silent(check_value(1, "x", type = "numeric"))
  expect_error(check_value("1", "x", type = "numeric"))
  expect_silent(check_value(TRUE, "x", type = "logical"))
  expect_error(check_value("TRUE", "x", type = "logical"))
  expect_silent(check_value("birds", "x", type = "text"))
  expect_error(check_value(1, "x", type = "text"), "must be text")

  expect_silent(check_value(1:3, "x", type = "numeric", n = 3))
  expect_silent(check_value(NULL, "x", type = "numeric", not_null = FALSE))
  expect_silent(check_value(1, "x", opts = 1, type = "numeric"))
  expect_error(check_value(1:3, "x", type = "numeric", n = 1), "must have 1")
  expect_error(check_value(NULL, "x", type = "numeric"), "cannot be `NULL`")
  expect_error(check_value(1, "x", opts = c(2, 4), type = "numeric"), "must be among")

  expect_silent(check_value(1:3, "x", type = "numeric", range = c(0, Inf)))
  expect_error(
    check_value(1:3, "x", type = "numeric", range = c(4, Inf)),
    "must be between"
  )
})

test_that("check_cols()", {
  f <- function(col1, col2) {
    check_cols(mtcars, c(!!enquo(col1), !!enquo(col2)))
  }

  expect_silent(f(cyl, mpg))
  expect_silent(f(cyl, c(am, mpg)))

  expect_error(f(cyl, hi), "Column 'hi' does not exist")
  expect_error(f(cyl, c(hi, test)), "Column 'hi' does not exist")
  expect_error(f(cyl, c(hi, test)), "Column 'test' does not exist")
})

test_that("check_names()", {
  col1 <- "cyl"
  col2 <- "mpg"
  col3 <- "testing"
  col4 <- NULL

  expect_silent(check_names(mtcars, names = c(col1, col2)))

  expect_error(
    check_names(mtcars, names = c(col1, col2, col3)),
    "Column 'testing' does not exist"
  )

  expect_error(
    check_names(mtcars, dates = TRUE),
    "No date or date range columns"
  )
})

test_that("check_dates()", {
  s <- dplyr::mutate(example_sites,
    date = lubridate::ymd(Date_set_out),
    date_time = as.POSIXct(date)
  )

  expect_silent(check_dates(s, "date"))
  expect_silent(check_dates(s, c("date", "date_time")))

  s <- dplyr::mutate(example_sites, date = "13/05/2020")
  expect_error(check_dates(s, cols = "date"), "Problems with")
})

test_that("check_doy()", {
  site <- LETTERS[1:10]
  doy1 <- 1:10
  doy2 <- -5:4
  date <- lubridate::as_date(1:10, origin = "2023-01-01") - lubridate::days(1)
  date_time <- lubridate::as_datetime(date)

  # Error
  expect_error(d <- check_doy(site), "`site` must contain dates")
  expect_error(d <- check_doy(doy2), "`doy2` contains integers, but")

  # No change with DOY
  expect_silent(d <- check_doy(doy1))
  expect_equal(d, doy1)

  # Create `doy` column with date or datetime
  expect_silent(d <- check_doy(date))
  expect_equal(d, doy1)
  expect_silent(d <- check_doy(date_time))
  expect_equal(d, doy1)
})

test_that("check_df_file()", {
  expect_silent(check_df_file("test.xlsx"))
  expect_silent(check_df_file(mtcars))
  expect_error(check_df_file(Sys.Date()))
})

test_that("check_date_joins()", {
  df <- data.frame(date = "2020-01-01")
  expect_message(v <- check_date_joins(df, by_date = "date"), "`date` using buffers")
  expect_equal(v, "date")
  expect_error(check_date_joins(df, by_date = "date_time"), "Cannot find")

  df <- data.frame(date_time = "2020-01-01 00:00:00")
  expect_message(
    v <- check_date_joins(df, by_date = "date_time"),
    "`date_time` using buffers"
  )
  expect_equal(v, "date_time")
  expect_error(check_date_joins(df, by_date = "date"), "Cannot find")

  df <- data.frame(date_start = "2020-01-01", date_end = "2020-02-01")
  expect_message(
    v <- check_date_joins(df, by_date = "date"),
    "`date_start` and `date_end`"
  )
  expect_equal(v, c("date_start", "date_end"))
  expect_error(check_date_joins(df, by_date = "date_time"), "Cannot find")

  df <- data.frame(
    date_time_start = "2020-01-01 00:00:00",
    date_time_end = "2020-01-01 05:00:00"
  )
  expect_message(
    v <- check_date_joins(df, by_date = "date_time"),
    "`date_time_start` and `date_time_end`"
  )
  expect_equal(v, c("date_time_start", "date_time_end"))
  expect_error(check_date_joins(df, by_date = "date"), "Cannot find")
})

test_that("check_tz()", {
  expect_silent(check_tz("local"))
  expect_silent(check_tz("America/Winnipeg"))
  expect_error(check_tz("lsdjkf/skdjfl"))
})
