test_that("lb_execute() validates query argument", {
  conn <- make_test_conn()
  expect_error(lb_execute(conn, 123), class = "rladybugdb_error_invalid_arg")
  expect_error(lb_execute(conn, c("a", "b")), class = "rladybugdb_error_invalid_arg")
})

test_that("lb_execute() returns lb_result for DDL", {
  conn <- make_test_conn()
  result <- lb_execute(conn, "CREATE NODE TABLE T (id INT64, PRIMARY KEY(id))")
  expect_s3_class(result, "lb_result")
})

test_that("lb_execute() returns lb_result for MATCH query", {
  conn <- make_test_conn()
  lb_execute(conn, "CREATE NODE TABLE Person (name STRING, age INT64, PRIMARY KEY(name))")
  lb_execute(conn, "CREATE (:Person {name: 'Alice', age: 30})")
  result <- lb_execute(conn, "MATCH (p:Person) RETURN p.name, p.age")
  expect_s3_class(result, "lb_result")
})

test_that("lb_query() returns a data.frame", {
  conn <- make_test_conn()
  lb_execute(conn, "CREATE NODE TABLE Person (name STRING, age INT64, PRIMARY KEY(name))")
  lb_execute(conn, "CREATE (:Person {name: 'Alice', age: 30})")
  lb_execute(conn, "CREATE (:Person {name: 'Bob',   age: 25})")
  df <- lb_query(conn, "MATCH (p:Person) RETURN p.name AS name, p.age AS age ORDER BY p.name")
  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 2L)
  expect_equal(sort(df$name), c("Alice", "Bob"))
})

test_that("lb_execute() passes named parameters", {
  conn <- make_test_conn()
  lb_execute(conn, "CREATE NODE TABLE Person (name STRING, age INT64, PRIMARY KEY(name))")
  lb_execute(conn, "CREATE (:Person {name: $name, age: $age})",
             parameters = list(name = "Charlie", age = 40L))
  df <- lb_query(conn, "MATCH (p:Person) RETURN p.name AS name")
  expect_equal(df$name, "Charlie")
})

test_that("lb_execute() gives informative error on bad Cypher", {
  conn <- make_test_conn()
  expect_error(lb_execute(conn, "THIS IS NOT CYPHER"),
               class = "rladybugdb_error_query")
})
