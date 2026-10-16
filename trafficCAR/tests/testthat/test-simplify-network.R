test_that("build_network(simplify=TRUE) keeps edges/graph consistent", {
  roads_dup <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)) # duplicate
    ),
    crs = 4326
  )

  net <- build_network(roads_dup, simplify = TRUE)

  expect_equal(nrow(net$edges), 1)
  expect_equal(igraph::ecount(net$graph), 1)
  expect_equal(Matrix::nnzero(net$A), 2)
})


test_that("simplify_network removes parallel edges and keeps isolates/vertices", {
  roads_dup <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)) # duplicate
    ),
    crs = 4326
  )

  net <- build_network(roads_dup, simplify = FALSE)

  expect_equal(nrow(net$edges), 2)
  expect_equal(igraph::ecount(net$graph), 2)

  net_s <- simplify_network(net)

  expect_equal(nrow(net_s$nodes), nrow(net$nodes)) # vertices unchanged
  expect_equal(nrow(net_s$edges), 1) # duplicate removed
  expect_equal(igraph::ecount(net_s$graph), 1)
  expect_equal(Matrix::nnzero(net_s$A), 2) # one undirected edge => 2 nonzeros
})


test_that("adjacency invariants hold (symmetric, zero diagonal, matches igraph degrees)", {
  toy_plus <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 2, 0), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(1, -1, 1, 1), ncol = 2, byrow = TRUE))
    ),
    crs = 4326
  )

  net <- build_network(toy_plus, node_intersections = TRUE, simplify = TRUE)

  n <- nrow(net$nodes)
  expect_equal(dim(net$A), c(n, n))
  expect_true(isTRUE(all.equal(net$A, Matrix::t(net$A))))

  expect_true(all(Matrix::diag(net$A) == 0))

  deg_A <- as.integer(Matrix::rowSums(net$A))
  deg_g <- as.integer(igraph::degree(net$graph))
  expect_equal(deg_A, deg_g)
})


test_that("disconnected components and isolates are preserved", {
  roads_disc <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(10, 0, 11, 0), ncol = 2, byrow = TRUE))
    ),
    crs = 4326
  )

  net <- build_network(roads_disc, simplify = TRUE)

  expect_equal(nrow(net$nodes), 4)
  expect_equal(igraph::components(net$graph)$no, 2)

  # no adjacency between the two components: exactly 2 undirected edges => 4 nonzeros
  expect_equal(Matrix::nnzero(net$A), 4)
})


test_that("snap_tol merges nearly identical endpoints", {
  roads_near <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 1, 0), ncol = 2, byrow = TRUE)),
      sf::st_linestring(matrix(c(1 + 1e-6, 0, 2, 0), ncol = 2, byrow = TRUE))
    ),
    crs = 4326
  )

  net0 <- build_network(roads_near, snap_tol = 0, simplify = TRUE)
  expect_equal(igraph::components(net0$graph)$no, 2)

  net1 <- build_network(roads_near, snap_tol = 0.5, simplify = TRUE)
  expect_equal(igraph::components(net1$graph)$no, 1)
})
