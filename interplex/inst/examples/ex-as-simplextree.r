if (requireNamespace("TDA", quietly = TRUE)) {
  # convert a TDA filtration object
  t <- 2 * pi * c(0, 1, 3, 6) / 7
  rf <- TDA::ripsFiltration(
    cbind(x = cos(t), y = sin(t)),
    maxdimension = 2L, maxscale = 1.7
  )
  print(rf$cmplx)
  st_rf <- as_rcpp_simplextree(rf)
  print(st_rf)
  st_rf2 <- as_rcpp_simplextree(rf$cmplx)
  print(st_rf2)
}

\dontrun{

if (requireNamespace("reticulate", quietly = TRUE)) {
  # convert a Python GUDHI simplex tree
  gd <- reticulate::import("gudhi")
  gd_st <- gd$SimplexTree()
  for (s in list(3:5, 5:6, 8)) gd_st$insert(as.list(s))
  st_gd <- as_rcpp_simplextree(gd_st)
  st_gd$as_list()
}
}

if (requireNamespace("igraph", quietly = TRUE)) {
  # convert an igraph object
  ig <- igraph::graph(c(1,2, 2,3, 1,3, 3,4))
  print(ig)
  st_ig <- as_rcpp_simplextree(ig)
  print(st_ig)
  
  # specify 0-simplex indices
  set.seed(0L)
  ig <- igraph::set_vertex_attr(ig, "id", value = sample(igraph::vcount(ig)) + 1L)
  igraph::V(ig)$id
  igraph::as_edgelist(ig)
  st_ig <- as_rcpp_simplextree(ig, index = "id")
  st_ig$vertices
  st_ig$edges
}

if (requireNamespace("network", quietly = TRUE)) {
  # convert a network object
  el <- data.frame(tails = c(1, 2, 1, 3), heads = c(2, 3, 3, 4))
  nw <- network::network.edgelist(el, network::network.initialize(4))
  print(nw)
  st_nw <- as_rcpp_simplextree(nw)
  print(st_nw)
}
