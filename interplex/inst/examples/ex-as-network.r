if (requireNamespace("TDA", quietly = TRUE)) {
  # convert a TDA filtration object
  t <- 2 * pi * c(0, 1, 3, 6) / 7
  rf <- TDA::ripsFiltration(
    cbind(x = cos(t), y = sin(t)),
    maxdimension = 2L, maxscale = 1.7
  )
  print(rf$cmplx)
  nw_rf <- as_network(rf)
  print(nw_rf)
  nw_rf2 <- as_network(rf$cmplx)
  print(nw_rf2)
}

if (requireNamespace("simplextree", quietly = TRUE)) {
  # convert a simplextree object
  st <- simplextree::simplex_tree()
  st$insert(list(3:5, 5:6, 8))
  nw_st <- as_network(st)
  print(nw_st)
}

\dontrun{

if (requireNamespace("reticulate", quietly = TRUE)) {
  # convert a Python GUDHI simplex tree
  gd <- reticulate::import("gudhi")
  gd_st <- gd$SimplexTree()
  for (s in list(3:5, 5:6, 8)) gd_st$insert(as.list(s))
  nw_gd <- as_network(gd_st, index = "id")
  print(nw_gd)
}
}

if (requireNamespace("igraph", quietly = TRUE)) {
  # convert an igraph object
  ig <- igraph::graph(c(1,2, 2,3, 1,3, 3,4))
  print(ig)
  nw_ig <- as_network(ig)
  print(nw_ig)
}
