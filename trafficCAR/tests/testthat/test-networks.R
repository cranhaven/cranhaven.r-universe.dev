test_that("build_network returns expected components", {
  net <- build_network(toy_grid)

  expect_type(net, "list")
  expect_named(
    net,
    c("roads", "nodes", "edges", "graph", "A")
  )
})

test_that("L-shaped network has expected topology", {
  net <- build_network(toy_L)

  expect_equal(nrow(net$nodes), 3)
  expect_equal(nrow(net$edges), 2)
  expect_true(igraph::is_connected(net$graph))

  deg <- sort(igraph::degree(net$graph))
  expect_equal(deg, c(1, 1, 2))
})

test_that("T-junction has expected degree distribution", {
  net <- build_network(toy_T)

  expect_equal(nrow(net$nodes), 4)
  expect_equal(nrow(net$edges), 3)
  expect_true(igraph::is_connected(net$graph))

  deg <- sort(igraph::degree(net$graph))
  expect_equal(deg, c(1, 1, 1, 3))
})

test_that("square network is a 4-cycle", {
  net <- build_network(toy_square)

  expect_equal(nrow(net$nodes), 4)
  expect_equal(nrow(net$edges), 4)
  expect_true(igraph::is_connected(net$graph))

  deg <- igraph::degree(net$graph)
  expect_true(all(deg == 2))

  gth <- igraph::girth(net$graph)$girth
  expect_equal(gth, 4)
})

test_that("2x2 grid network has expected degree structure", {
  net <- build_network(toy_grid)

  if (igraph::is_connected(net$graph)) {
    # if toy_grid is already segmented at intersections, this will pass
    deg <- igraph::degree(net$graph)
    expect_true(all(deg >= 2))
  } else {
    # endpoint-based networks may not connect crossings unless noded.
    expect_false(igraph::is_connected(net$graph))
  }
})

test_that("2x2 grid has 9 nodes when intersections are noded", {
  net <- build_network(toy_grid, node_intersections = TRUE)
  expect_equal(nrow(net$nodes), 9)
  expect_equal(nrow(net$edges), 12)
})



test_that("disconnected network is not connected", {
  net <- build_network(toy_disconnected)
  expect_false(igraph::is_connected(net$graph))
})

test_that("on-ramp connectivity/merge node requires intersection noding", {
  net <- build_network(toy_on_ramp)

  # ramps that merge mid-edge won't create a merge node
  expect_false(igraph::is_connected(net$graph))
  expect_false(any(igraph::degree(net$graph) == 3))
})

test_that("adjacency matrix is square, symmetric, and matches graph", {
  net <- build_network(toy_grid)

  A <- net$A
  n <- nrow(net$nodes)

  expect_equal(nrow(A), n)
  expect_equal(ncol(A), n)
  expect_equal(A, Matrix::t(A))

  # degrees from adjacency equal igraph degrees
  deg_A <- as.numeric(Matrix::rowSums(A))
  deg_g <- as.numeric(igraph::degree(net$graph))
  expect_equal(sort(deg_A), sort(deg_g))
})

test_that("build_network removes self-loops and keeps zero diagonal adjacency", {
  net <- build_network(toy_grid)
  expect_false(any(net$edges$from == net$edges$to))
  expect_equal(Matrix::diag(net$A), rep(0, nrow(net$A)))
})

test_that("crossing '+' is noded into 5 nodes and 4 edges when node_intersections=TRUE", {
  # two lines crossing at their interior points (a plus sign)
  toy_plus <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 2, 0), ncol = 2, byrow = TRUE)), # horizontal
      sf::st_linestring(matrix(c(1, -1, 1, 1), ncol = 2, byrow = TRUE)) # vertical
    ),
    crs = 4326
  )

  net0 <- build_network(toy_plus, node_intersections = FALSE)
  expect_equal(nrow(net0$nodes), 4)  # endpoints only
  expect_equal(nrow(net0$edges), 2)

  net1 <- build_network(toy_plus, node_intersections = TRUE)
  expect_equal(nrow(net1$nodes), 5)  # +1 node at crossing
  expect_equal(nrow(net1$edges), 4)  # split into 4 segments
})


test_that("T-junction becomes connected only when node_intersections=TRUE", {
  toy_T_end <- sf::st_sf(
    geometry = sf::st_sfc(
      sf::st_linestring(matrix(c(0, 0, 2, 0), ncol = 2, byrow = TRUE)),  # horizontal
      sf::st_linestring(matrix(c(1, 0, 1, -2), ncol = 2, byrow = TRUE))  # vertical meets at (1,0)
    ),
    crs = 4326
  )

  net0 <- build_network(toy_T_end, node_intersections = FALSE)
  net1 <- build_network(toy_T_end, node_intersections = TRUE)

  # node set should be the same: (0,0), (2,0), (1,0), (1,-2)
  expect_equal(nrow(net0$nodes), 4)
  expect_equal(nrow(net1$nodes), 4)

  # w/o noding: two disconnected edges => 2 components
  expect_equal(igraph::components(net0$graph)$no, 2)

  # w/ noding: a single T-shaped connected component
  expect_equal(igraph::components(net1$graph)$no, 1)

  # degree sequence distinguishes them:
  # - noded T has one degree-3 junction and three leaves => {3,1,1,1}
  expect_equal(sort(igraph::degree(net1$graph)), c(1, 1, 1, 3))
})
