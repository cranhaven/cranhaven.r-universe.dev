test_that("as_igraph.lb_result() errors without igraph installed (mocked)", {
  result <- structure(
    list(ptr = NULL, conn = NULL, query = NULL,
         col_names = character(0), col_types = character(0)),
    class = "lb_result"
  )
  local_mocked_bindings(
    requireNamespace = function(pkg, ...) if (pkg == "igraph") FALSE else TRUE,
    .package = "base"
  )
  expect_error(as_igraph(result), class = "rladybugdb_error_missing_pkg")
})

test_that("as_igraph.lb_result() builds graph from node/rel columns", {
  skip_if_not(requireNamespace("igraph", quietly = TRUE), "igraph not available")
  conn <- make_test_conn()
  lb_execute(conn, "CREATE NODE TABLE Person (name STRING, PRIMARY KEY(name))")
  lb_execute(conn, "CREATE REL TABLE Knows (FROM Person TO Person, since INT64)")
  lb_execute(conn, "CREATE (:Person {name: 'Alice'})")
  lb_execute(conn, "CREATE (:Person {name: 'Bob'})")
  lb_execute(conn,
    "MATCH (a:Person {name:'Alice'}), (b:Person {name:'Bob'}) CREATE (a)-[:Knows {since: 2020}]->(b)")
  result <- lb_execute(conn,
    "MATCH (a:Person)-[r:Knows]->(b:Person) RETURN a, r, b")
  g <- as_igraph(result)
  expect_true(igraph::is_igraph(g))
  expect_equal(igraph::vcount(g), 2L)
  expect_equal(igraph::ecount(g), 1L)
})

test_that("as_igraph.lb_result() errors when no graph columns present", {
  skip_if_not(requireNamespace("igraph", quietly = TRUE), "igraph not available")
  conn <- make_test_conn()
  lb_execute(conn, "CREATE NODE TABLE T (x INT64, PRIMARY KEY(x))")
  lb_execute(conn, "CREATE (:T {x: 1})")
  result <- lb_execute(conn, "MATCH (t:T) RETURN t.x AS x")
  expect_error(as_igraph(result), class = "rladybugdb_error_no_graph_cols")
})

test_that("as_tbl_graph.lb_result() builds tbl_graph from node/rel columns", {
  skip_if_not(requireNamespace("igraph",    quietly = TRUE), "igraph not available")
  skip_if_not(requireNamespace("tidygraph", quietly = TRUE), "tidygraph not available")
  conn <- make_test_conn()
  lb_execute(conn, "CREATE NODE TABLE Person (name STRING, PRIMARY KEY(name))")
  lb_execute(conn, "CREATE REL TABLE Knows (FROM Person TO Person)")
  lb_execute(conn, "CREATE (:Person {name: 'X'})")
  lb_execute(conn, "CREATE (:Person {name: 'Y'})")
  lb_execute(conn,
    "MATCH (a:Person {name:'X'}), (b:Person {name:'Y'}) CREATE (a)-[:Knows]->(b)")
  result <- lb_execute(conn,
    "MATCH (a:Person)-[r:Knows]->(b:Person) RETURN a, r, b")
  tg <- as_tbl_graph(result)
  expect_s3_class(tg, "tbl_graph")
})

test_that("empty structural results produce an empty graph", {
  skip_if_not_installed("igraph")
  conn <- make_test_conn()
  lb_close(lb_execute(conn, "CREATE NODE TABLE EmptyNode (id INT64, PRIMARY KEY(id))"))
  result <- lb_execute(conn, "MATCH (n:EmptyNode) RETURN n")
  on.exit(lb_close(result))
  graph <- as_igraph(result)
  expect_equal(igraph::vcount(graph), 0L)
  expect_equal(igraph::ecount(graph), 0L)
})

test_that("graph conversion preserves labels, isolated nodes, and self-loops", {
  skip_if_not_installed("igraph")
  conn <- make_test_conn()
  lb_close(lb_execute(conn, "CREATE NODE TABLE Person2 (id INT64, PRIMARY KEY(id))"))
  lb_close(lb_execute(conn, "CREATE NODE TABLE Place2 (id INT64, PRIMARY KEY(id))"))
  lb_close(lb_execute(conn, "CREATE REL TABLE Knows2 (FROM Person2 TO Person2)"))
  lb_close(lb_execute(conn, "CREATE REL TABLE Visits2 (FROM Person2 TO Place2)"))
  lb_close(lb_execute(conn, "CREATE (:Person2 {id: 1}), (:Person2 {id: 2}), (:Place2 {id: 3})"))
  lb_close(lb_execute(
    conn,
    paste(
      "MATCH (a:Person2 {id: 1}), (b:Place2 {id: 3})",
      "CREATE (a)-[:Knows2]->(a), (a)-[:Visits2]->(b)"
    )
  ))

  result <- lb_execute(
    conn,
    paste(
      "MATCH (a:Person2)-[r:Knows2]->(b:Person2) RETURN a, r, b",
      "UNION ALL",
      "MATCH (a:Person2)-[r:Visits2]->(b:Place2) RETURN a, r, b",
      "UNION ALL",
      "MATCH (a:Person2)-[r:Visits2]->(b:Place2) RETURN a, r, b"
    )
  )
  on.exit(lb_close(result))
  graph <- as_igraph(result)

  expect_equal(igraph::vcount(graph), 2L)
  expect_equal(igraph::ecount(graph), 2L)
  expect_setequal(igraph::vertex_attr(graph, "_LABEL"), c("Person2", "Place2"))
  expect_setequal(igraph::edge_attr(graph, "_LABEL"), c("Knows2", "Visits2"))
  expect_true(any(igraph::as_edgelist(graph)[, 1] == igraph::as_edgelist(graph)[, 2]))

  isolated <- lb_execute(conn, "MATCH (n) RETURN n")
  on.exit(lb_close(isolated), add = TRUE)
  isolated_graph <- as_igraph(isolated)
  expect_equal(igraph::vcount(isolated_graph), 3L)
})
