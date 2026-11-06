test_that("result conversion and display preserve the cursor", {
  conn <- make_test_conn()
  result <- lb_execute(conn, "UNWIND range(1, 20) AS x RETURN x ORDER BY x")
  on.exit(lb_close(result))

  expected <- as.data.frame(result)
  expect_identical(as.data.frame(result), expected)
  expect_length(capture.output(print(result, n = 3)), 6L)
  expect_match(format(result, n = 3), "with|x", all = FALSE)
  expect_identical(as.data.frame(result), expected)
  expect_equal(lb_fetch(result, 2)$x, c(1, 2))
  expect_equal(lb_fetch(result, 2)$x, c(3, 4))
  expect_true(lb_has_next(result))
  expect_invisible(lb_reset(result))
  expect_equal(lb_fetch(result, 1)$x, 1)
})

test_that("lifecycle operations reject unsafe close and tolerate double close", {
  db <- lb_database(":memory:")
  conn <- lb_connection(db)
  result <- lb_execute(conn, "RETURN 1 AS x")

  expect_error(lb_close(conn), "result", class = "rladybugdb_error_query")
  expect_error(lb_close(db), "connection", class = "rladybugdb_error_query")
  expect_invisible(lb_close(result))
  expect_invisible(lb_close(result))
  expect_error(as.data.frame(result), class = "rladybugdb_error_closed")
  expect_invisible(lb_close(conn))
  expect_invisible(lb_close(conn))
  expect_invisible(lb_close(db))
  expect_invisible(lb_close(db))
})

test_that("multi-statement results and summaries are exposed", {
  conn <- make_test_conn()
  first <- lb_execute(conn, "RETURN 1 AS first; RETURN 2 AS second;")
  on.exit(lb_close(first))

  info <- lb_result_info(first)
  summary <- lb_query_summary(first)
  expect_equal(info$column_names, "first")
  expect_true(info$has_next_result)
  expect_true(all(c("compiling_time_ms", "execution_time_ms") %in% names(summary)))

  second <- lb_next_result(first)
  expect_s3_class(second, "lb_result")
  on.exit(lb_close(second), add = TRUE)
  expect_equal(as.data.frame(second)$second, 2)
  expect_null(lb_next_result(second))
})

test_that("native handles survive repeated creation and garbage collection", {
  for (i in seq_len(30)) {
    db <- lb_database(":memory:")
    conn <- lb_connection(db)
    result <- lb_execute(conn, "UNWIND range(1, 10) AS x RETURN x")
    expect_equal(nrow(as.data.frame(result)), 10L)
    lb_close(result)
    lb_close(conn)
    lb_close(db)
    rm(result, conn, db)
    if (i %% 5L == 0L) gc()
  }
  succeed()
})
