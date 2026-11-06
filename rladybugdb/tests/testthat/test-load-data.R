test_that("CSV loading quotes identifiers and paths", {
  conn <- make_test_conn()
  table <- "odd` table Ω"
  ddl <- "CREATE NODE TABLE `odd`` table Ω` (id INT64, value STRING, PRIMARY KEY(id))"
  lb_close(lb_execute(conn, ddl))

  directory <- withr::local_tempdir()
  path <- file.path(directory, "quote's file.csv")
  writeLines(c("id,value", "1,café"), path, useBytes = TRUE)
  result <- lb_copy_from_csv(conn, path, table)
  lb_close(result)

  out <- lb_query(conn, "MATCH (n:`odd`` table Ω`) RETURN n.id AS id, n.value AS value")
  expect_equal(out$id, 1)
  expect_equal(out$value, "café")

  expect_error(lb_copy_from_csv(conn, path, "x`) MATCH (n) DETACH DELETE n //"),
               class = "rladybugdb_error_query")
  expect_equal(nrow(lb_query(conn, "MATCH (n:`odd`` table Ω`) RETURN n")), 1L)
})

test_that("Arrow data-frame loading distinguishes empty and missing strings", {
  skip_if_not_installed("arrow")
  conn <- make_test_conn()
  lb_close(lb_execute(
    conn,
    "CREATE NODE TABLE Payload (id INT64, value STRING, PRIMARY KEY(id))"
  ))
  input <- data.frame(id = c(1, 2), value = c("", NA_character_))
  result <- lb_copy_from_df(conn, input, "Payload")
  lb_close(result)

  out <- lb_query(conn, "MATCH (n:Payload) RETURN n.value AS value ORDER BY n.id")
  expect_identical(out$value, c("", NA_character_))
})

test_that("COPY validation rejects unsafe options before native execution", {
  conn <- make_test_conn()
  path <- withr::local_tempfile(lines = "id\n1")
  expect_error(lb_copy_from_csv(conn, path, "T", delim = "::"),
               class = "rladybugdb_error_invalid_arg")
  args <- list(conn = conn, path = path, table = "T", header = TRUE)
  args[["bad option!"]] <- 1
  expect_error(do.call(lb_copy_from_csv, args), class = "rladybugdb_error_invalid_arg")
})
