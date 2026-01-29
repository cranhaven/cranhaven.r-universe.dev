context("'Rcpp_SimplexTree' objects")

# skip_if_not_installed("simplextree")

st_ex <- simplextree::simplex_tree()
st_ex$insert(list(1:2, 2:5, 5:6, c(4,7), c(5,7), 6:7, 7:9))
st_sl <- simplextree_list(st_ex)

test_that("'Rcpp_SimplexTree'-to-'igraph' conversion preserves vertices", {
  skip_if_not_installed("igraph")
  ig_ex <- as_igraph(st_ex)
  expect_equal(st_ex$n_simplices[[1L]], igraph::gorder(ig_ex))
  expect_true(all(st_ex$edges == sort_el(igraph::as_edgelist(ig_ex))))
})

test_that("'Rcpp_SimplexTree'-to-'network' conversion preserves vertices", {
  skip_if_not_installed("network")
  nw_ex <- as_network(st_ex)
  expect_equal(st_ex$n_simplices[[1L]], network::network.size(nw_ex))
  expect_true(all(st_ex$edges == sort_el(network::as.edgelist(nw_ex))))
})

test_that("'Rcpp_SimplexTree'-to-list conversion preserves 0,1-simplices", {
  cp_ex <- as_cmplx(st_ex)
  expect_equal(st_ex$n_simplices[[1L]], length(unique(unlist(cp_ex))))
  expect_true(all(st_ex$edges ==
                    t(sapply(cp_ex[sapply(cp_ex, length) == 2L], identity))))
})

test_that("'Rcpp_SimplexTree'-to-GUDHI conversion preserves all simplices", {
  skip_if_not_installed("reticulate")
  gd_ex <- as_py_gudhi_simplextree(st_ex)
  expect_equal(length(st_ex$vertices), gd_ex$num_vertices())
  gd_sl <- reticulate::iterate(gd_ex$get_simplices(), function(s) s[[1L]])
  gd_sl <- sort_lst(gd_sl)
  expect_equal(gd_sl, st_sl)
})

st_ex$clear()
st_ex$insert(list(3:5, c(5, 8, 13), c(3, 13), 13:14, 14:15))

test_that("'Rcpp_SimplexTree'-to-'igraph' conversion encodes indices", {
  skip_if_not_installed("igraph")
  ig_ex <- as_igraph(st_ex, index = "id")
  expect_true("id" %in% igraph::vertex_attr_names(ig_ex))
  expect_equal(st_ex$vertices, igraph::vertex_attr(ig_ex, "id"))
})

test_that("'Rcpp_SimplexTree'-to-'network' conversion encodes indices", {
  skip_if_not_installed("network")
  nw_ex <- as_network(st_ex, index = "id")
  expect_true("id" %in% network::list.vertex.attributes(nw_ex))
  expect_equal(st_ex$vertices, network::get.vertex.attribute(nw_ex, "id"))
})
