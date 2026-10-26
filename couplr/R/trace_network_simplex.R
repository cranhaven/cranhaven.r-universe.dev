# ==============================================================================
# Reference trace for the "network_simplex" production method
# ==============================================================================
# The C++ src/solvers/network_simplex/solve_network_simplex.cpp is the only
# truly distinct algorithm among the "MCF-based" production methods (push_relabel,
# csa, network_simplex): it maintains a spanning tree basis over the
# LAP-as-MCF graph and pivots one arc at a time, as the file claims. It reports
# status "optimal" only when block pricing has scanned every arc without finding
# a negative reduced cost, "iteration_limit" when its pivot cap ends the loop
# first, and raises an infeasibility error when the allowed edges admit no
# assignment covering every row.
#
# The frames emitted here come from trace_csflow's successive-shortest-paths
# body, so the animation shows augmenting paths over the residual graph. Both
# algorithms terminate at the same optimal matching, which is the quantity
# tests/testthat/test-trace-parity.R compares frame by frame against the C++
# oracle.
# ==============================================================================

#' @keywords internal
#' @noRd
trace_network_simplex <- function(cost, maximize = FALSE, ...) {
  out <- trace_csflow(cost, maximize = maximize, ...)
  out$meta$algorithm <- "network_simplex"
  out$meta$description <- paste0(
    "The production C++ solver dispatched by assignment(method = ",
    "\"network_simplex\") implements a primal network-simplex tree-pivot ",
    "algorithm: it maintains a spanning-tree basis on the LAP-as-MCF graph and ",
    "iteratively pivots the most-negative-reduced-cost non-basic arc against a ",
    "blocking basic arc. This animation is built from the successive shortest ",
    "paths body, so its frames show augmenting paths over the residual graph. ",
    "Both terminate at the same optimal matching."
  )
  out
}

register_trace("network_simplex", trace_network_simplex)
