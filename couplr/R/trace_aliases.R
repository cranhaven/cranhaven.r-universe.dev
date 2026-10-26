# ==============================================================================
# Alias traces: methods that are honestly the same algorithm as one already
# implemented, differing only in data structures or implementation detail.
# ==============================================================================
# These registrations make `lap_animate(cost, method = "X")` work for every
# method string accepted by `assignment()`, while being honest that the
# underlying algorithmic story is shared with another trace.
#
# Coverage by aliasing keeps the package's single-source-of-truth design: one
# faithful R reference per algorithm family, not one near-duplicate per cpp
# implementation. The trace function is reused; only the algorithm-name meta
# and the description differ.
# ==============================================================================

#' @keywords internal
#' @noRd
trace_sap <- function(cost, maximize = FALSE, ...) {
  # SAP = Successive Shortest Path = the shortest-augmenting-path Hungarian
  # primal-dual algorithm. Algorithmically identical to method = "hungarian";
  # the C++ implementations differ only in which solver lives in which file.
  out <- trace_hungarian(cost, maximize = maximize, ...)
  out$meta$algorithm <- "sap"
  out$meta$description <- paste0(
    "Successive shortest paths (SSP / SAP). Algorithmically identical to ",
    "method = \"hungarian\": grow a Dijkstra shortest-path tree on reduced ",
    "costs from each free row, augment along the path, update duals. ",
    "couplr ships both names because the literature uses each at different ",
    "times - SSP is the network-flow community's name, Hungarian is the ",
    "combinatorial-optimization name."
  )
  out
}

#' @keywords internal
#' @noRd
trace_lapmod <- function(cost, maximize = FALSE, ...) {
  # LAPMOD = the sparse-input variant of Jonker-Volgenant 1987. Same three
  # pre-stages, same shortest-augmenting-path main loop; the C++ implementation
  # uses CSR storage instead of a dense matrix. For animation the algorithmic
  # story is identical to method = "jv".
  out <- trace_jv(cost, maximize = maximize, ...)
  out$meta$algorithm <- "lapmod"
  out$meta$description <- paste0(
    "LAPMOD. The sparse-input variant of Jonker-Volgenant (1987): identical ",
    "three pre-stages (column reduction, reduction transfer, augmenting row ",
    "reduction) and the same Dijkstra shortest-augmenting-path main loop as ",
    "method = \"jv\". The production C++ implementation stores only the ",
    "finite entries (CSR), so the algorithmic trace is the same."
  )
  out
}

register_trace("sap", trace_sap)
register_trace("lapmod", trace_lapmod)
