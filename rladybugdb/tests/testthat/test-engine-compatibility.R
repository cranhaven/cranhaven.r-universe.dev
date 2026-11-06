test_that("LadybugDB 0.20.4 opens a database created by 0.15.2", {
  path <- test_path("..", "fixtures", "ladybug-v0.15.2.lbug")
  expect_true(file.exists(path))

  db <- lb_database(path, read_only = TRUE)
  conn <- lb_connection(db)
  on.exit({
    lb_close(conn)
    lb_close(db)
  })

  out <- lb_query(
    conn,
    "MATCH (n:Compatibility) RETURN n.id AS id, n.value AS value"
  )
  expect_equal(out$id, 1)
  expect_equal(out$value, "from 0.15.2")
})
