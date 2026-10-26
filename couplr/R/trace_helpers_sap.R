# ==============================================================================
# Shared pieces of the shortest-augmenting-path traces
# ==============================================================================
# trace_hungarian, trace_ramshaw_tarjan and trace_ssap_bucket each grow a
# shortest-path tree on reduced costs and lift the duals by the same rule. What
# separates them is how the next column is chosen (linear scan, heap, Dial
# bucket) and how each step is narrated, so the search loop and its
# descriptions stay in each trace and the two pieces below are shared.
# ==============================================================================

#' Edges of the shortest-path tree grown so far
#'
#' `pred_row[j]` is the row the search reached column j from, and a column
#' counts as being in the tree once it has been scanned. `map` converts an
#' internal (row, col) pair to the orientation the frames report in; the
#' default reports the pair unchanged.
#'
#' @keywords internal
#' @noRd
sp_tree_edges <- function(pred_row, scanned, map = NULL) {
  in_tree <- which(scanned & pred_row > 0L)
  if (length(in_tree) == 0L) return(list())
  out <- lapply(in_tree, function(j) c(pred_row[j], j))
  if (!is.null(map)) out <- lapply(out, function(e) map(e[1], e[2]))
  out
}

#' Lift the duals along the shortest-path tree
#'
#' After the search reaches a free column at distance `delta`, every scanned
#' column and every row the search entered is shifted so that the path's edges
#' become tight and the already-matched edges stay tight. `row_entry[i]` is the
#' distance at which the search entered row i; the start row is entered at
#' distance 0 and is lifted by the full `delta`.
#'
#' @keywords internal
#' @noRd
sp_dual_lift <- function(u, v, dist, scanned, delta, scanned_rows, row_entry,
                         start_row) {
  cols <- which(scanned)
  if (length(cols) > 0L) v[cols] <- v[cols] + dist[cols] - delta
  u[start_row] <- u[start_row] + delta
  rows <- setdiff(scanned_rows, start_row)
  if (length(rows) > 0L) u[rows] <- u[rows] + (delta - row_entry[rows])
  list(u = u, v = v)
}
