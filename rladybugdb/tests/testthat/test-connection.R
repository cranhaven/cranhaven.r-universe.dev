test_that("lb_database() validates path argument", {
  expect_error(lb_database(123), class = "rladybugdb_error_invalid_arg")
  expect_error(lb_database(c("a", "b")), class = "rladybugdb_error_invalid_arg")
})

test_that("lb_database() returns lb_database object for :memory:", {
  db <- lb_database(":memory:")
  on.exit(lb_close(db))
  expect_s3_class(db, "lb_database")
  expect_equal(db$path, ":memory:")
  expect_false(db$read_only)
})

test_that("lb_connection() requires lb_database input", {
  expect_error(lb_connection("not_a_db"), class = "rladybugdb_error_invalid_arg")
})

test_that("lb_connection() returns lb_connection object", {
  db   <- lb_database(":memory:")
  conn <- lb_connection(db)
  on.exit({ lb_close(conn); lb_close(db) })
  expect_s3_class(conn, "lb_connection")
})

test_that("lb_close() runs without error on connection and database", {
  db   <- lb_database(":memory:")
  conn <- lb_connection(db)
  expect_invisible(lb_close(conn))
  expect_invisible(lb_close(db))
})

test_that("print.lb_database() outputs path info", {
  db  <- lb_database(":memory:")
  on.exit(lb_close(db))
  out <- capture.output(print(db))
  expect_match(out, ":memory:")
})

test_that("print.lb_connection() outputs db path", {
  db   <- lb_database(":memory:")
  conn <- lb_connection(db)
  on.exit({ lb_close(conn); lb_close(db) })
  out <- capture.output(print(conn))
  expect_match(out, ":memory:")
})
