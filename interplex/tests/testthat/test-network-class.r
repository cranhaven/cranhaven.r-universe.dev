context("'network' objects")

# skip_if_not_installed("network")

data(flo, package = "network")
nw_flo <- network::network(flo, directed = FALSE)
nw_flo_el <- network::as.edgelist(nw_flo)

test_that("'network'-to-'igraph' conversion preserves vertices", {
  skip_if_not_installed("igraph")
  ig_flo <- as_igraph(nw_flo)
  expect_equal(network::network.size(nw_flo), igraph::gorder(ig_flo))
  expect_true(all(nw_flo_el == igraph::as_edgelist(ig_flo)))
})

test_that("'network'-to-'Rcpp_SimplexTree' conversion preserves 0,1-simplices", {
  skip_if_not_installed("simplextree")
  st_flo <- as_rcpp_simplextree(nw_flo)
  expect_equal(network::network.size(nw_flo), st_flo$n_simplices[[1L]])
  expect_true(all(nw_flo_el == st_flo$edges))
})

test_that("'network'-to-GUDHI conversion preserves 0,1-simplices", {
  skip_if_not_installed("reticulate")
  gd_flo <- as_py_gudhi_simplextree(nw_flo)
  expect_equal(network::network.size(nw_flo), gd_flo$num_vertices())
  gd_el <- reticulate::iterate(gd_flo$get_skeleton(1L), function(s) s[[1L]])
  gd_el <- sort_el(do.call(rbind, gd_el[sapply(gd_el, length) == 2L]))
  expect_true(all(nw_flo_el == gd_el))
})

test_that("'network'-to-list conversion preserves 0,1-simplices", {
  cp_flo <- as_cmplx(nw_flo)
  expect_equal(network::network.size(nw_flo), length(unique(unlist(cp_flo))))
  expect_true(all(
    nw_flo_el ==
      t(sapply(cp_flo[as.logical(sapply(cp_flo, length) - 1)], identity))
  ))
})

v_id <- 1L + sample(network::network.size(nw_flo))
nw_flo <- network::set.vertex.attribute(nw_flo, "id", v_id)
stopifnot("id" %in% network::list.vertex.attributes(nw_flo))

test_that("'network'-to-'igraph' conversion preserves attributes", {
  skip_if_not_installed("igraph")
  ig_flo <- as_igraph(nw_flo)
  expect_true("id" %in% igraph::vertex_attr_names(ig_flo))
})

test_that("'network'-to-'Rcpp_SimplexTree' conversion uses indices", {
  skip_if_not_installed("simplextree")
  st_flo <- as_rcpp_simplextree(nw_flo, index = "id")
  nw_id <- network::get.vertex.attribute(nw_flo, "id")
  expect_equal(sort(nw_id), st_flo$vertices)
  # reindex and sort edges by index
  el <- sort_el(cbind(nw_id[nw_flo_el[, 1L]], nw_id[nw_flo_el[, 2L]]))
  expect_true(all(el == st_flo$edges))
})
