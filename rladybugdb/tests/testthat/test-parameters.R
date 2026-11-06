test_that("parameter lists require unique non-empty names", {
  conn <- make_test_conn()
  expect_error(lb_execute(conn, "RETURN $x", list(1)),
               class = "rladybugdb_error_invalid_arg")
  expect_error(lb_execute(conn, "RETURN $x", setNames(list(1), "")),
               class = "rladybugdb_error_invalid_arg")
  expect_error(lb_execute(conn, "RETURN $x", setNames(list(1, 2), c("x", "x"))),
               class = "rladybugdb_error_invalid_arg")
})

test_that("all typed scalar missing values bind as NULL", {
  conn <- make_test_conn()
  values <- list(
    logical = NA,
    integer = NA_integer_,
    double = NA_real_,
    character = NA_character_,
    date = as.Date(NA),
    timestamp = as.POSIXct(NA, tz = "UTC")
  )
  for (name in names(values)) {
    out <- lb_query(conn, "RETURN $value IS NULL AS missing",
                    parameters = list(value = values[[name]]))
    expect_true(out$missing, info = name)
  }
  if (requireNamespace("bit64", quietly = TRUE)) {
    out <- lb_query(conn, "RETURN $value IS NULL AS missing",
                    parameters = list(value = bit64::NA_integer64_))
    expect_true(out$missing)
  }
})

test_that("supported parameter types preserve their values", {
  conn <- make_test_conn()
  timestamp <- as.POSIXct("2026-09-11 12:34:56", tz = "UTC")
  out <- lb_query(
    conn,
    paste(
      "RETURN $logical AS logical, $integer AS integer, $double AS double,",
      "$text AS text, $date AS date, $timestamp AS timestamp, $items AS items"
    ),
    parameters = list(
      logical = TRUE,
      integer = 42L,
      double = 3.5,
      text = "héllo",
      date = as.Date("2026-09-11"),
      timestamp = timestamp,
      items = c(1L, NA_integer_, 3L)
    )
  )
  expect_true(out$logical)
  expect_equal(out$integer, 42)
  expect_equal(out$double, 3.5)
  expect_equal(out$text, "héllo")
  expect_equal(out$date, as.Date("2026-09-11"))
  expect_equal(out$timestamp, timestamp)
  expect_equal(out$items[[1]][c(1, 3)], list(1, 3))
  expect_null(out$items[[1]][[2]])

  blob <- as.raw(c(0x00, 0xaa, 0xff))
  expect_identical(
    lb_query(conn, "RETURN CAST($value AS BLOB) AS value",
             parameters = list(value = blob))$value[[1]],
    blob
  )

  nested <- lb_query(conn, "RETURN $value AS value",
                     parameters = list(value = list(a = 1L, b = "two")))
  expect_equal(nested$value[[1]]$a, 1)
  expect_equal(nested$value[[1]]$b, "two")
})

test_that("binding failures name the parameter in a typed error", {
  conn <- make_test_conn()
  expect_error(
    lb_execute(conn, "RETURN $count + 1", parameters = list(count = "not numeric")),
    "count",
    class = "rladybugdb_error_bind"
  )
})

test_that("integer64 parameters round-trip exactly when available", {
  skip_if_not_installed("bit64")
  conn <- make_test_conn()
  value <- bit64::as.integer64("9007199254740993")
  old <- options(rladybugdb.bigint = "integer64")
  on.exit(options(old), add = TRUE)
  out <- lb_query(conn, "RETURN $value AS value", parameters = list(value = value))
  expect_identical(out$value, value)
})
