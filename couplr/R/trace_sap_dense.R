# ==============================================================================
# Reference trace for the "sap_dense" production method
# ==============================================================================
# src/solvers/sap_dense/sap_dense_solve.cpp runs successive shortest paths:
# each augmentation starts at a row still unassigned, runs Dijkstra on reduced
# costs, and shifts the potentials by Johnson's rule before augmenting along the
# path it found. Its priority queue is a linear scan over the columns rather
# than a heap, which costs O(m) per extraction and suits a dense cost matrix.
#
# The search and the potential update are the ones trace_csflow already walks,
# so this trace shares that algorithm body and differs only in the meta block.
# What separates the two production solvers is the queue and the code path, not
# the sequence of augmentations.
# ==============================================================================

#' @keywords internal
#' @noRd
trace_sap_dense <- function(cost, maximize = FALSE, ...) {
  out <- trace_csflow(cost, maximize = maximize, ...)
  out$meta$algorithm <- "sap_dense"
  out$meta$description <- paste0(
    "Successive shortest paths with Dijkstra and Johnson potentials. The ",
    "column with the smallest tentative distance is found by scanning every ",
    "unfinalized column, so an augmentation costs O(m^2) and a full solve ",
    "O(n * m^2)."
  )
  out
}

register_trace("sap_dense", trace_sap_dense)
