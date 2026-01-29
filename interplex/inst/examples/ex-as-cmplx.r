if (requireNamespace("TDA", quietly = TRUE)) {
  # pick the simplicial complex from a TDA filtration object
  t <- 2 * pi * c(0, 1, 3, 6) / 7
  rf <- TDA::ripsFiltration(
    cbind(x = cos(t), y = sin(t)),
    maxdimension = 2L, maxscale = 1.7
  )
  print(rf$cmplx)
  cp_rf <- as_cmplx(rf)
  print(cp_rf)
}

if (requireNamespace("simplextree", quietly = TRUE)) {
  # convert a simplextree object
  st <- simplextree::simplex_tree()
  st$insert(list(3:5, 5:6, 8))
  cp_st <- as_cmplx(st)
  print(cp_st)
}

\dontrun{

if (requireNamespace("reticulate", quietly = TRUE)) {
  # convert a Python GUDHI simplex tree
  gd <- reticulate::import("gudhi")
  gd_st <- gd$SimplexTree()
  for (s in list(3:5, 5:6, 8)) gd_st$insert(as.list(s))
  cp_gd <- as_cmplx(gd_st)
  print(cp_gd)
}
}

if (requireNamespace("igraph", quietly = TRUE)) {
  # convert an igraph object
  ig <- igraph::graph(c(1,2, 2,3, 1,3, 3,4))
  set.seed(0L)
  ig <- igraph::set_vertex_attr(ig, "id", value = sample(igraph::vcount(ig)))
  print(ig)
  cp_ig <- as_cmplx(ig, index = "id")
  print(cp_ig)
}

if (requireNamespace("network", quietly = TRUE)) {
  # convert a network object
  el <- data.frame(tails = c(1, 2, 1, 3), heads = c(2, 3, 3, 4))
  nw <- network::network.edgelist(el, network::network.initialize(4))
  print(nw)
  cp_nw <- as_cmplx(nw)
  print(cp_nw)
}
