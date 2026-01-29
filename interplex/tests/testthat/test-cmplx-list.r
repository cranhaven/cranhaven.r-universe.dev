context("lists in the TDA package `cmplx` list format")

skip_if_not_installed("TDA")

# TODO: use a more complicated example
t <- 2 * pi * c(0, 1, 3, 6) / 7
cp_rf <- TDA::ripsFiltration(
  cbind(x = cos(t), y = sin(t)),
  maxdimension = 2L, maxscale = 1.7
)$cmplx
cp_rf_vc <- length(unique(unlist(cp_rf)))
cp_rf_el <- t(sapply(cp_rf[sapply(cp_rf, length) == 2L], identity))

test_that("list-to-'igraph' conversion preserves vertices", {
  skip_if_not_installed("igraph")
  ig_rf <- as_igraph(cp_rf)
  expect_equal(cp_rf_vc, igraph::gorder(ig_rf))
  expect_true(all(sort_el(cp_rf_el) == sort_el(igraph::as_edgelist(ig_rf))))
})

test_that("list-to-'network' conversion preserves vertices", {
  skip_if_not_installed("network")
  nw_rf <- as_network(cp_rf)
  expect_equal(cp_rf_vc, network::network.size(nw_rf))
  expect_true(all(sort_el(cp_rf_el) == sort_el(network::as.edgelist(nw_rf))))
})

test_that("list-to-'Rcpp_SimplexTree' conversion preserves 0,1-simplices", {
  skip_if_not_installed("simplextree")
  st_rf <- as_rcpp_simplextree(cp_rf)
  expect_equal(cp_rf_vc, st_rf$n_simplices[[1L]])
  expect_true(all(sort_el(cp_rf_el) == st_rf$edges))
})

test_that("list-to-GUDHI conversion preserves all simplices", {
  skip_if_not_installed("reticulate")
  gd_rf <- as_py_gudhi_simplextree(cp_rf)
  expect_equal(cp_rf_vc, gd_rf$num_vertices())
  gd_sl <- reticulate::iterate(gd_rf$get_simplices(), function(s) s[[1L]])
  expect_equal(sort_lst(gd_sl), sort_lst(cp_rf))
})

cp_rf <- lapply(cp_rf, function(s) s + 2L)
cp_vi <- sort(unique(unlist(cp_rf)))

test_that("list-to-'igraph' conversion encodes indices", {
  skip_if_not_installed("igraph")
  ig_rf <- as_igraph(cp_rf, index = "id")
  expect_true("id" %in% igraph::vertex_attr_names(ig_rf))
  expect_equal(cp_vi, igraph::vertex_attr(ig_rf, "id"))
})

test_that("list-to-'network' conversion encodes indices", {
  skip_if_not_installed("network")
  nw_rf <- as_network(cp_rf, index = "id")
  expect_true("id" %in% network::list.vertex.attributes(nw_rf))
  expect_equal(cp_vi, network::get.vertex.attribute(nw_rf, "id"))
})
