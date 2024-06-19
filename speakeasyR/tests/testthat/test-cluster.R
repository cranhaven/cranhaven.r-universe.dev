seed <- 1234
graph <- igraph::graph.famous("zachary")
expected_membership <- c(
  9, 9, 9, 9, 1, 1, 1, 9, 6, 5, 1, 7, 9, 9, 4, 4, 1, 9, 4, 9, 4, 9, 4, 8, 3,
  3, 2, 8, 3, 2, 6, 3, 4, 4
)

test_that("matrix works", {
  graph_i <- as.matrix(as.matrix(graph))
  actual <- speakeasyR::cluster(graph_i, seed = seed)
  expect_equal(actual, expected_membership)
})

test_that("sparse matrix works", {
  graph_i <- as.matrix(graph)
  actual <- speakeasyR::cluster(graph_i, seed = seed)
  expect_equal(actual, expected_membership)
})

test_that("igraph works", {
  actual <- speakeasyR::cluster(graph, seed = seed)
  expect_equal(actual, expected_membership)
})
