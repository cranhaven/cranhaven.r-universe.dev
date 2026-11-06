test_that("wide numbers, JSON, BLOB, ARRAY, UNION, and intervals are explicit", {
  conn <- make_test_conn()
  old <- options(rladybugdb.bigint = "character")
  on.exit(options(old), add = TRUE)

  out <- lb_query(
    conn,
    paste(
      "RETURN CAST(170141183460469231731687303715884105727 AS INT128) AS i128,",
      "CAST(18446744073709551615 AS UINT64) AS u64,",
      "CAST('123.450' AS DECIMAL(10,3)) AS decimal,",
      "CAST('{}' AS JSON) AS json,",
      "BLOB('\\x00\\xAA\\xFF') AS blob,",
      "CAST([1,2,3] AS INT64[3]) AS array_value,",
      "union_value(age := 36) AS union_value,",
      "INTERVAL('2 months 3 days 4 microseconds') AS interval_value"
    )
  )
  expect_equal(out$i128, "170141183460469231731687303715884105727")
  expect_equal(out$u64, "18446744073709551615")
  expect_equal(out$decimal, "123.450")
  expect_equal(out$json, "{}")
  expect_identical(out$blob[[1]], as.raw(c(0x00, 0xaa, 0xff)))
  expect_equal(out$array_value[[1]], list("1", "2", "3"))
  expect_s3_class(out$union_value[[1]], "lb_union")
  expect_equal(out$union_value[[1]]$value, "36")
  expect_s3_class(out$interval_value[[1]], "lb_interval")
  expect_equal(as.numeric(out$interval_value[[1]]), c(2, 3, 4))

  result <- lb_execute(conn, "RETURN CAST('{}' AS JSON) AS json")
  expect_equal(lb_result_info(result)$column_types, "JSON")
  lb_close(result)

  options(rladybugdb.bigint = "integer64")
  expect_equal(
    lb_query(conn, "RETURN CAST(18446744073709551615 AS UINT64) AS value")$value,
    "18446744073709551615"
  )
})

test_that("recursive paths retain nodes and relationships", {
  conn <- make_test_conn()
  lb_close(lb_execute(conn, "CREATE NODE TABLE N (id INT64, PRIMARY KEY(id))"))
  lb_close(lb_execute(conn, "CREATE REL TABLE Next (FROM N TO N)"))
  for (id in 1:3) {
    lb_close(lb_execute(conn, "CREATE (:N {id: $id})", list(id = id)))
  }
  lb_close(lb_execute(
    conn,
    paste(
      "MATCH (a:N {id: 1}), (b:N {id: 2}), (c:N {id: 3})",
      "CREATE (a)-[:Next]->(b), (b)-[:Next]->(c)"
    )
  ))
  out <- lb_query(
    conn,
    "MATCH p = (a:N {id: 1})-[:Next*2..2]->(b:N) RETURN p"
  )
  expect_s3_class(out$p[[1]], "lb_path")
  expect_length(out$p[[1]]$nodes, 3L)
  expect_length(out$p[[1]]$relationships, 2L)
})
