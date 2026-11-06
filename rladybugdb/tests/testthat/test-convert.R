test_that("as.data.frame.lb_result() returns data frame with correct dimensions", {
  conn <- make_test_conn()
  lb_execute(conn, "CREATE NODE TABLE Item (id INT64, val DOUBLE, PRIMARY KEY(id))")
  lb_execute(conn, "CREATE (:Item {id: 1, val: 1.5})")
  lb_execute(conn, "CREATE (:Item {id: 2, val: 2.5})")
  result <- lb_execute(conn, "MATCH (i:Item) RETURN i.id AS id, i.val AS val")
  df <- as.data.frame(result)
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2L)
  expect_equal(ncol(df), 2L)
  expect_true(all(c("id", "val") %in% names(df)))
})

test_that("as.data.frame.lb_result() handles integer types", {
  conn <- make_test_conn()
  lb_execute(conn, "CREATE NODE TABLE Nums (n INT32, PRIMARY KEY(n))")
  lb_execute(conn, "CREATE (:Nums {n: 42})")
  df <- lb_query(conn, "MATCH (x:Nums) RETURN x.n AS n")
  expect_equal(df$n, 42L)
})

test_that("as.data.frame.lb_result() handles string types", {
  conn <- make_test_conn()
  lb_execute(conn, "CREATE NODE TABLE Words (w STRING, PRIMARY KEY(w))")
  lb_execute(conn, "CREATE (:Words {w: 'hello'})")
  df <- lb_query(conn, "MATCH (x:Words) RETURN x.w AS w")
  expect_equal(df$w, "hello")
})

test_that("as.data.frame.lb_result() handles boolean types", {
  conn <- make_test_conn()
  lb_execute(conn, "CREATE NODE TABLE Flags (f BOOLEAN, id INT64, PRIMARY KEY(id))")
  lb_execute(conn, "CREATE (:Flags {id: 1, f: true})")
  df <- lb_query(conn, "MATCH (x:Flags) RETURN x.f AS f")
  expect_true(is.logical(df$f))
  expect_true(df$f)
})

test_that("as_tibble.lb_result() returns a tibble", {
  skip_if_not(requireNamespace("tibble", quietly = TRUE), "tibble not available")
  conn <- make_test_conn()
  lb_execute(conn, "CREATE NODE TABLE T (x INT64, PRIMARY KEY(x))")
  lb_execute(conn, "CREATE (:T {x: 1})")
  result <- lb_execute(conn, "MATCH (t:T) RETURN t.x AS x")
  tbl    <- as_tibble(result)
  expect_s3_class(tbl, "tbl_df")
  expect_equal(nrow(tbl), 1L)
})

test_that("as_arrow_table.lb_result() returns an Arrow Table", {
  skip_if_not(requireNamespace("arrow", quietly = TRUE), "arrow not available")
  conn <- make_test_conn()
  lb_execute(conn, "CREATE NODE TABLE T (x INT64, PRIMARY KEY(x))")
  lb_execute(conn, "CREATE (:T {x: 99})")
  result <- lb_execute(conn, "MATCH (t:T) RETURN t.x AS x")
  at     <- as_arrow_table(result)
  expect_true(inherits(at, "ArrowTabular") || inherits(at, "Table"))
  expect_equal(nrow(at), 1L)
})

test_that("Arrow fetches are bounded and full conversion preserves the cursor", {
  skip_if_not_installed("arrow")
  conn <- make_test_conn()
  result <- lb_execute(conn, "UNWIND range(1, 25) AS x RETURN x ORDER BY x")
  on.exit(lb_close(result))

  first <- lb_fetch_arrow(result, 10)
  expect_equal(nrow(first), 10L)
  expect_equal(as.data.frame(first)$x, 1:10)
  expect_equal(lb_result_info(result)$rows_fetched, 10)

  all_rows <- as_arrow_table(result)
  expect_equal(nrow(all_rows), 25L)
  expect_equal(lb_result_info(result)$rows_fetched, 10)

  second <- lb_fetch_arrow(result, 10)
  final <- lb_fetch_arrow(result, 10)
  expect_equal(as.data.frame(second)$x, 11:20)
  expect_equal(as.data.frame(final)$x, 21:25)
  rm(first, all_rows, second, final)
  gc()
})
