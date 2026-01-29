\dontrun{

if (requireNamespace("reticulate", quietly = TRUE)) {
  # print GUDHI simplices
  print_py_gudhi <- function(x) {
    reticulate::iterate(
      x$get_skeleton(x$dimension()),
      function(s) print(s[[1]]),
      simplify = FALSE
    )
  }
}

if (requireNamespace("TDA", quietly = TRUE)) {
  # convert a TDA filtration object
  t <- 2 * pi * c(0, 1, 3, 6) / 7
  rf <- TDA::ripsFiltration(
    cbind(x = cos(t), y = sin(t)),
    maxdimension = 2L, maxscale = 1.7
  )
  print(rf$cmplx)
  gd_rf <- as_py_gudhi_simplextree(rf)
  print_py_gudhi(gd_rf)
  gd_rf2 <- as_py_gudhi_simplextree(rf$cmplx)
  print_py_gudhi(gd_rf2)
}

if (requireNamespace("simplextree", quietly = TRUE)) {
  # convert a simplextree object
  st <- simplextree::simplex_tree()
  st$insert(list(3:5, 5:6, 8))
  gd_st <- as_py_gudhi_simplextree(st)
  print_py_gudhi(gd_st)
}

if (requireNamespace("igraph", quietly = TRUE)) {
  # convert an igraph object
  ig <- igraph::graph(c(1,2, 2,3, 1,3, 3,4))
  print(ig)
  gd_ig <- as_py_gudhi_simplextree(ig)
  print_py_gudhi(gd_ig)
  
  # specify 0-simplex indices
  set.seed(0L)
  ig <- igraph::set_vertex_attr(ig, "id", value = sample(igraph::vcount(ig)) + 1L)
  igraph::V(ig)$id
  igraph::as_edgelist(ig)
  gd_ig2 <- as_py_gudhi_simplextree(ig, index = "id")
  print_py_gudhi(gd_ig2)
}

if (requireNamespace("network", quietly = TRUE)) {
  # convert a network object
  el <- data.frame(tails = c(1, 2, 1, 3), heads = c(2, 3, 3, 4))
  nw <- network::network.edgelist(el, network::network.initialize(4))
  print(nw)
  gd_nw <- as_py_gudhi_simplextree(nw)
  print_py_gudhi(gd_nw)
}
}
