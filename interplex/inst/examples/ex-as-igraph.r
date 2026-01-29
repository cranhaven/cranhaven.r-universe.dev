if (requireNamespace("TDA", quietly = TRUE)) {
  # convert a TDA filtration object
  t <- 2 * pi * c(0, 1, 3, 6) / 7
  rf <- TDA::ripsFiltration(
    cbind(x = cos(t), y = sin(t)),
    maxdimension = 2L, maxscale = 1.7
  )
  print(rf$cmplx)
  ig_rf <- as_igraph(rf)
  print(ig_rf)
  ig_rf2 <- as_igraph(rf$cmplx)
  print(ig_rf2)
}

if (requireNamespace("simplextree", quietly = TRUE)) {
  # convert a simplextree object
  st <- simplextree::simplex_tree()
  st$insert(list(3:5, 5:6, 8))
  ig_st <- as_igraph(st)
  print(ig_st)
}

\dontrun{

if (requireNamespace("reticulate", quietly = TRUE)) {
  # convert a Python GUDHI simplex tree
  gd <- reticulate::import("gudhi")
  gd_st <- gd$SimplexTree()
  for (s in list(3:5, 5:6, 8)) gd_st$insert(as.list(s))
  ig_gd <- as_igraph(gd_st, index = "id")
  print(ig_gd)
}
}

if (requireNamespace("network", quietly = TRUE)) {
  # convert a network object
  el <- data.frame(tails = c(1, 2, 1, 3), heads = c(2, 3, 3, 4))
  nw <- network::network.edgelist(el, network::network.initialize(4))
  print(nw)
  ig_nw <- as_igraph(nw)
  print(ig_nw)
}
