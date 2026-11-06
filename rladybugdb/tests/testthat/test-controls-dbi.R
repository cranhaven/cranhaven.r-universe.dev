test_that("transactions commit and roll back", {
  conn <- make_test_conn()
  lb_close(lb_execute(conn, "CREATE NODE TABLE Tx (id INT64, PRIMARY KEY(id))"))

  expect_invisible(lb_begin(conn))
  lb_close(lb_execute(conn, "CREATE (:Tx {id: 1})"))
  expect_invisible(lb_rollback(conn))
  expect_equal(nrow(lb_query(conn, "MATCH (n:Tx) RETURN n")), 0L)

  lb_begin(conn)
  lb_close(lb_execute(conn, "CREATE (:Tx {id: 2})"))
  lb_commit(conn)
  expect_equal(lb_query(conn, "MATCH (n:Tx) RETURN n.id AS id")$id, 2)
})

test_that("query timeout and configuration are validated", {
  conn <- make_test_conn()
  expect_invisible(lb_set_timeout(conn, 10))
  expect_invisible(lb_set_timeout(conn, 0))
  expect_invisible(lb_interrupt(conn))
  expect_error(lb_set_timeout(conn, -1), class = "rladybugdb_error_invalid_arg")
  expect_error(lb_database(":memory:", config = list(not_an_option = 1)),
               class = "rladybugdb_error_invalid_arg")

  db <- lb_database(":memory:", config = list(
    max_num_threads = 1,
    enable_compression = TRUE,
    enable_checksums = TRUE,
    auto_checkpoint = TRUE
  ))
  expect_invisible(lb_close(db))
})

test_that("extension helpers expose official metadata and validate names", {
  conn <- make_test_conn()
  extensions <- lb_list_extensions(conn)
  expect_s3_class(extensions, "data.frame")
  expect_gt(nrow(extensions), 0L)
  expect_error(lb_install_extension(conn, "bad name!"),
               class = "rladybugdb_error_invalid_arg")
  expect_error(lb_install_extension(conn, "algo"),
               class = "rladybugdb_error_unsupported")
  expect_error(lb_update_extension(conn, "algo"),
               class = "rladybugdb_error_unsupported")
})

test_that("DBI query, binding, metadata, and transactions work", {
  dbi_conn <- DBI::dbConnect(Ladybug(), dbname = ":memory:", bigint = "character")
  on.exit(DBI::dbDisconnect(dbi_conn))
  expect_true(DBI::dbIsValid(dbi_conn))
  expect_false(DBI::dbIsReadOnly(dbi_conn))

  DBI::dbExecute(dbi_conn, "CREATE NODE TABLE Person (id INT64, name STRING, PRIMARY KEY(id))")
  DBI::dbExecute(
    dbi_conn,
    "CREATE (:Person {id: $id, name: $name})",
    params = list(id = 1L, name = "Ada")
  )
  expect_equal(DBI::dbListTables(dbi_conn), "Person")
  expect_equal(DBI::dbListFields(dbi_conn, "Person"), c("id", "name"))
  expect_true(DBI::dbExistsTable(dbi_conn, "Person"))

  result <- DBI::dbSendQuery(
    dbi_conn,
    "MATCH (p:Person) RETURN p.id AS id, p.name AS name"
  )
  expect_false(DBI::dbHasCompleted(result))
  expect_equal(DBI::dbFetch(result, 1)$name, "Ada")
  expect_true(DBI::dbHasCompleted(result))
  expect_true(DBI::dbClearResult(result))

  pending <- DBI::dbSendQuery(dbi_conn, "RETURN $value AS value")
  expect_true(DBI::dbIsValid(pending))
  expect_false(DBI::dbHasCompleted(pending))
  expect_error(DBI::dbFetch(pending), "dbBind")
  DBI::dbBind(pending, list(value = "bound later"))
  expect_equal(DBI::dbFetch(pending)$value, "bound later")
  DBI::dbClearResult(pending)

  DBI::dbBegin(dbi_conn)
  DBI::dbExecute(dbi_conn, "CREATE (:Person {id: 2, name: 'Grace'})")
  DBI::dbRollback(dbi_conn)
  expect_equal(nrow(DBI::dbGetQuery(dbi_conn, "MATCH (p:Person) RETURN p")), 1L)
})
