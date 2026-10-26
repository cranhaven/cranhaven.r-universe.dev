# ==============================================================================
# The trace for the "csa" production method
# ==============================================================================
# src/solvers/solve_csa.cpp runs Goldberg and Kennedy's CSA-Q. Its
# double-push with implicit row prices moves the same prices and matches as
# the auction bid (their Fig. 4), over a stack of active rows with epsilon
# divided by 10 each refine. The fourth-best heuristic changes how a row finds
# its two cheapest arcs, not which ones it finds, so the states it passes
# through are trace_auction_scaled's at alpha = 10.
# ==============================================================================

#' @keywords internal
#' @noRd
trace_csa <- function(cost, maximize = FALSE, ...) {
  out <- trace_auction_scaled(cost, maximize = maximize, alpha = 10, ...)
  out$meta$algorithm <- "csa"
  out$meta$description <- paste0(
    "Cost-scaling assignment CSA-Q (Goldberg & Kennedy 1995). Each refine ",
    "divides eps by 10, clears the matching and makes every row active. An ",
    "active row is taken from a stack and double-pushed: it takes its cheapest ",
    "column, the row holding that column becomes active, and the column's ",
    "price falls to the row's second-cheapest reduced cost less eps. A row ",
    "keeps its three cheapest arcs between scans and rescans only when fewer ",
    "than two of them are still below the fourth-smallest reduced cost of the ",
    "last scan. After the last refine a repair step makes the assignment ",
    "optimal."
  )
  out
}

register_trace("csa", trace_csa)
