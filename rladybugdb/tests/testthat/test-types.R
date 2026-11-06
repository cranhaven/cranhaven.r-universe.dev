test_that("INT64 round-trips correctly", {
  conn <- make_test_conn()
  lb_execute(conn, "CREATE NODE TABLE T64 (v INT64, PRIMARY KEY(v))")
  lb_execute(conn, "CREATE (:T64 {v: 9007199254740992})")  # 2^53 — exact in double
  df <- lb_query(conn, "MATCH (x:T64) RETURN x.v AS v")
  expect_true(is.numeric(df$v))
  expect_equal(df$v, 9007199254740992)
})

test_that("INT32 becomes integer vector", {
  conn <- make_test_conn()
  lb_execute(conn, "CREATE NODE TABLE T32 (v INT32, PRIMARY KEY(v))")
  lb_execute(conn, "CREATE (:T32 {v: 42})")
  df <- lb_query(conn, "MATCH (x:T32) RETURN x.v AS v")
  expect_true(is.integer(df$v))
  expect_equal(df$v, 42L)
})

test_that("BOOL becomes logical vector", {
  conn <- make_test_conn()
  lb_execute(conn, "CREATE NODE TABLE TBool (id INT64, v BOOLEAN, PRIMARY KEY(id))")
  lb_execute(conn, "CREATE (:TBool {id: 1, v: true})")
  lb_execute(conn, "CREATE (:TBool {id: 2, v: false})")
  df <- lb_query(conn, "MATCH (x:TBool) RETURN x.v AS v ORDER BY x.id")
  expect_true(is.logical(df$v))
  expect_equal(df$v, c(TRUE, FALSE))
})

test_that("DOUBLE becomes numeric vector", {
  conn <- make_test_conn()
  lb_execute(conn, "CREATE NODE TABLE TDbl (v DOUBLE, PRIMARY KEY(v))")
  lb_execute(conn, "CREATE (:TDbl {v: 3.14})")
  df <- lb_query(conn, "MATCH (x:TDbl) RETURN x.v AS v")
  expect_true(is.double(df$v))
  expect_equal(df$v, 3.14)
})

test_that("STRING becomes character vector", {
  conn <- make_test_conn()
  lb_execute(conn, "CREATE NODE TABLE TStr (v STRING, PRIMARY KEY(v))")
  lb_execute(conn, "CREATE (:TStr {v: 'hello world'})")
  df <- lb_query(conn, "MATCH (x:TStr) RETURN x.v AS v")
  expect_true(is.character(df$v))
  expect_equal(df$v, "hello world")
})

test_that("NULL values become NA", {
  conn <- make_test_conn()
  lb_execute(conn, "CREATE NODE TABLE TNullable (id INT64, v STRING, PRIMARY KEY(id))")
  lb_execute(conn, "CREATE (:TNullable {id: 1, v: 'present'})")
  lb_execute(conn, "CREATE (:TNullable {id: 2})")
  df <- lb_query(conn, "MATCH (x:TNullable) RETURN x.v AS v ORDER BY x.id")
  expect_equal(df$v[1], "present")
  expect_true(is.na(df$v[2]))
})

test_that("DATE becomes Date vector", {
  conn <- make_test_conn()
  lb_execute(conn, "CREATE NODE TABLE TDate (id INT64, d DATE, PRIMARY KEY(id))")
  lb_execute(conn, "CREATE (:TDate {id: 1, d: date('2024-03-15')})")
  df <- lb_query(conn, "MATCH (x:TDate) RETURN x.d AS d")
  expect_true(inherits(df$d, "Date"))
  expect_equal(format(df$d), "2024-03-15")
})

test_that("TIMESTAMP becomes POSIXct vector", {
  conn <- make_test_conn()
  lb_execute(conn, "CREATE NODE TABLE TTs (id INT64, ts TIMESTAMP, PRIMARY KEY(id))")
  lb_execute(conn, "CREATE (:TTs {id: 1, ts: timestamp('2024-01-01T00:00:00')})")
  df <- lb_query(conn, "MATCH (x:TTs) RETURN x.ts AS ts")
  expect_true(inherits(df$ts, "POSIXct"))
})

test_that("NODE column returns list with _ID and _LABEL", {
  skip_if_not(requireNamespace("igraph", quietly = TRUE), "igraph not available")
  conn <- make_test_conn()
  lb_execute(conn, "CREATE NODE TABLE Person (name STRING, PRIMARY KEY(name))")
  lb_execute(conn, "CREATE (:Person {name: 'Alice'})")
  df <- lb_query(conn, "MATCH (p:Person) RETURN p")
  expect_true(is.list(df$p))
  first <- df$p[[1]]
  expect_true("_ID"    %in% names(first))
  expect_true("_LABEL" %in% names(first))
  expect_equal(first[["_LABEL"]], "Person")
})

test_that("REL column returns list with _SRC, _DST, _LABEL", {
  skip_if_not(requireNamespace("igraph", quietly = TRUE), "igraph not available")
  conn <- make_test_conn()
  lb_execute(conn, "CREATE NODE TABLE Person (name STRING, PRIMARY KEY(name))")
  lb_execute(conn, "CREATE REL TABLE Knows (FROM Person TO Person)")
  lb_execute(conn, "CREATE (:Person {name: 'A'})")
  lb_execute(conn, "CREATE (:Person {name: 'B'})")
  lb_execute(conn,
    "MATCH (a:Person {name:'A'}), (b:Person {name:'B'}) CREATE (a)-[:Knows]->(b)")
  df <- lb_query(conn, "MATCH (a:Person)-[r:Knows]->(b:Person) RETURN r")
  expect_true(is.list(df$r))
  first <- df$r[[1]]
  expect_true("_SRC"   %in% names(first))
  expect_true("_DST"   %in% names(first))
  expect_true("_LABEL" %in% names(first))
  expect_equal(first[["_LABEL"]], "Knows")
})

test_that("LIST column returns list-of-list", {
  conn <- make_test_conn()
  lb_execute(conn, "CREATE NODE TABLE TList (id INT64, tags STRING[], PRIMARY KEY(id))")
  lb_execute(conn, "CREATE (:TList {id: 1, tags: ['a', 'b', 'c']})")
  df <- lb_query(conn, "MATCH (x:TList) RETURN x.tags AS tags")
  expect_true(is.list(df$tags))
  expect_equal(length(df$tags[[1]]), 3L)
})

test_that("parameterised query with STRING works", {
  conn <- make_test_conn()
  lb_execute(conn, "CREATE NODE TABLE PQ (name STRING, PRIMARY KEY(name))")
  lb_execute(conn, "CREATE (:PQ {name: $n})", parameters = list(n = "Charlie"))
  df <- lb_query(conn, "MATCH (x:PQ) RETURN x.name AS name")
  expect_equal(df$name, "Charlie")
})

test_that("parameterised query with INTEGER works", {
  conn <- make_test_conn()
  lb_execute(conn, "CREATE NODE TABLE PQi (v INT64, PRIMARY KEY(v))")
  lb_execute(conn, "CREATE (:PQi {v: $n})", parameters = list(n = 99L))
  df <- lb_query(conn, "MATCH (x:PQi) RETURN x.v AS v")
  expect_equal(df$v, 99)
})
