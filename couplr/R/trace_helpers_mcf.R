# ==============================================================================
# The min-cost-flow graph the cycle-cancelling trace runs on
# ==============================================================================
# The standard LAP-as-min-cost-flow network:
#
#         source --(cap 1, cost 0)--> row_i --(cap 1, cost c[i,j])--> col_j
#                                                                       |
#                                                                       v
#                                          col_j --(cap 1, cost 0)--> sink
#
# Every edge is stored alongside its residual reverse edge, with cap, cost, and
# a rev_idx cross-pointer. Pushing flow on edge e decrements e$cap and
# increments edges[[e$rev_idx]]$cap. Reverse-edge cost is the negation of the
# forward. The graph lives in an environment so mutation is in place: an
# algorithm grabs a graph, runs its inner loop, and reads back state without a
# copy.
#
# Klein's cycle cancelling is the one algorithm here the compiled solver does
# not run, which is why it is written in R. The successive-shortest-paths trace
# reads src/flow/flow_solve.cpp's own per-search record instead; see
# R/trace_csflow.R.
# ==============================================================================

#' Construct an empty min-cost-flow graph
#'
#' Node indices are 1-based. Edges are added with add_mcf_edge(); each call
#' allocates a forward edge and a reverse residual edge, with cross-pointers.
#'
#' @keywords internal
#' @noRd
new_mcf_graph <- function(n_nodes) {
  g <- new.env(parent = emptyenv())
  g$n_nodes <- as.integer(n_nodes)
  g$edges <- list()
  g$out_edges <- vector("list", n_nodes)
  for (i in seq_len(n_nodes)) g$out_edges[[i]] <- integer(0)
  g
}

#' Add a directed edge (from -> to) with capacity and cost.
#'
#' Internally allocates the forward and reverse residual edges.
#' Returns the integer index of the forward edge (so callers can retrieve
#' flow afterwards as `orig_cap - edges[[idx]]$cap`).
#'
#' @keywords internal
#' @noRd
add_mcf_edge <- function(g, fr, to, cap, cost) {
  fr <- as.integer(fr); to <- as.integer(to)
  fwd_idx <- length(g$edges) + 1L
  rev_idx <- fwd_idx + 1L
  g$edges[[fwd_idx]] <- list(to = to, rev_idx = rev_idx,
                             cap = cap, cost = cost, orig_cap = cap)
  g$edges[[rev_idx]] <- list(to = fr, rev_idx = fwd_idx,
                             cap = 0,   cost = -cost, orig_cap = 0)
  g$out_edges[[fr]] <- c(g$out_edges[[fr]], fwd_idx)
  g$out_edges[[to]] <- c(g$out_edges[[to]], rev_idx)
  fwd_idx
}

#' Build the standard LAP-as-MCF graph
#'
#' Returns a list with:
#'   graph         - the mcf_graph environment
#'   source, sink  - node indices
#'   row_node(i)   - converter: row i (1..n)  -> graph node index
#'   col_node(j)   - converter: col j (1..m)  -> graph node index
#'   row_col_edge(i, j) - integer index of the (row_i -> col_j) edge, or NA
#'                       if forbidden (NA/Inf cost)
#'   n_orig, m_orig
#'
#' Sign convention for costs:
#'   maximize = FALSE: cost = c(i, j)
#'   maximize = TRUE:  cost = -c(i, j)
#' Forbidden entries (NA/Inf) are simply not added as edges.
#'
#' @keywords internal
#' @noRd
build_lap_mcf <- function(cost, maximize = FALSE) {
  n <- nrow(cost); m <- ncol(cost)
  source_node <- 1L
  row_node <- function(i) 1L + i
  col_node <- function(j) 1L + n + j
  sink_node <- 2L + n + m
  g <- new_mcf_graph(sink_node)

  # source -> row_i (cap 1, cost 0)
  for (i in seq_len(n)) add_mcf_edge(g, source_node, row_node(i), cap = 1L, cost = 0)
  # col_j -> sink (cap 1, cost 0)
  for (j in seq_len(m)) add_mcf_edge(g, col_node(j), sink_node, cap = 1L, cost = 0)
  # row_i -> col_j (cap 1, cost c[i,j]) for finite cells
  row_col_edge <- matrix(NA_integer_, nrow = n, ncol = m)
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      x <- cost[i, j]
      if (is.finite(x)) {
        cc <- if (maximize) -x else x
        row_col_edge[i, j] <- add_mcf_edge(g, row_node(i), col_node(j), cap = 1L, cost = cc)
      }
    }
  }

  list(
    graph        = g,
    source       = source_node,
    sink         = sink_node,
    row_node     = row_node,
    col_node     = col_node,
    row_col_edge = row_col_edge,
    n_orig       = n,
    m_orig       = m,
    maximize     = maximize
  )
}

#' Map an edge index back to the (row, col) cell it carries
#'
#' Returns a function of one edge index that gives `c(i, j)` for the row->col
#' edge of cell (i, j), for that edge and for its residual twin, and NULL for
#' the source and sink edges a trace does not show. The index is built once, so
#' each lookup is a single subscript.
#'
#' @keywords internal
#' @noRd
mcf_edge_rowcol_lookup <- function(mcf) {
  n <- mcf$n_orig; m <- mcf$m_orig
  edges <- mcf$graph$edges
  cell_of_edge <- integer(length(edges))

  for (j in seq_len(m)) {
    for (i in seq_len(n)) {
      e_idx <- mcf$row_col_edge[i, j]
      if (is.na(e_idx)) next
      cell <- (j - 1L) * n + i
      cell_of_edge[e_idx] <- cell
      rev_idx <- edges[[e_idx]]$rev_idx
      if (!is.null(rev_idx) && !is.na(rev_idx)) cell_of_edge[rev_idx] <- cell
    }
  }

  function(e_idx) {
    if (is.na(e_idx) || e_idx < 1L || e_idx > length(cell_of_edge)) return(NULL)
    cell <- cell_of_edge[e_idx]
    if (cell == 0L) return(NULL)
    c((cell - 1L) %% n + 1L, (cell - 1L) %/% n + 1L)
  }
}

#' Recover row->col matching from an MCF graph after solving
#'
#' For each row i, finds the unique col_j such that the (row_i -> col_j) edge
#' is fully saturated (cap == 0 after pushing 1 unit). Returns integer vector
#' of length n_orig, with 0 for unmatched rows.
#'
#' @keywords internal
#' @noRd
mcf_extract_matching <- function(mcf) {
  n <- mcf$n_orig; m <- mcf$m_orig
  out <- integer(n)
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      e_idx <- mcf$row_col_edge[i, j]
      if (!is.na(e_idx)) {
        e <- mcf$graph$edges[[e_idx]]
        if (e$orig_cap - e$cap > 0L) {   # carrying flow
          out[i] <- j
          break
        }
      }
    }
  }
  out
}

#' Strict matching extractor: zero out double-bookings
#'
#' Preflow algorithms (push-relabel, CSA) can have intermediate states where
#' multiple rows have saturated edges to the same column, with the column
#' holding the resulting excess until it pushes back. Standard
#' mcf_extract_matching would report a "matching" with the same column on
#' two rows. For visualisation we want a strictly-valid partial matching at
#' every frame: any column with more than one incoming saturated edge is
#' shown as unmatched (set to 0) on all the contending rows. Once the
#' algorithm settles (no node excess), this returns the true matching.
#'
#' @keywords internal
#' @noRd
mcf_extract_matching_strict <- function(mcf) {
  base <- mcf_extract_matching(mcf)
  matched <- base[base > 0L]
  if (length(matched) == 0L) return(base)
  tab <- tabulate(matched, nbins = mcf$m_orig)
  bad_cols <- which(tab > 1L)
  if (length(bad_cols) > 0L) {
    base[base %in% bad_cols] <- 0L
  }
  base
}

#' Find a negative-cost cycle in the residual graph, or NULL if none.
#'
#' Used by Klein's cycle-canceling algorithm. The cycle is returned as an
#' integer vector of edge indices, in cycle order.
#'
#' @keywords internal
#' @noRd
mcf_find_negative_cycle <- function(g) {
  N <- g$n_nodes
  dist <- numeric(N)           # init to 0 everywhere -> finds any reachable neg cycle
  prev_node <- integer(N)
  prev_edge <- integer(N)
  last_relaxed <- 0L
  for (iter in seq_len(N)) {
    last_relaxed <- 0L
    for (u in seq_len(N)) {
      for (e_idx in g$out_edges[[u]]) {
        e <- g$edges[[e_idx]]
        if (e$cap <= 0) next
        nd <- dist[u] + e$cost
        if (nd < dist[e$to] - 1e-12) {
          dist[e$to]      <- nd
          prev_node[e$to] <- u
          prev_edge[e$to] <- e_idx
          last_relaxed    <- e$to
        }
      }
    }
    if (last_relaxed == 0L) return(NULL)   # no negative cycle
  }
  # Walk back N steps from last_relaxed to land inside the cycle
  v <- last_relaxed
  for (k in seq_len(N)) v <- prev_node[v]
  start <- v
  cycle_edges <- integer(0)
  repeat {
    cycle_edges <- c(prev_edge[v], cycle_edges)
    v <- prev_node[v]
    if (v == start) break
  }
  cycle_edges
}

#' Push delta units of flow along a path (sequence of residual edges)
#'
#' @keywords internal
#' @noRd
mcf_push_path <- function(g, edge_indices, delta) {
  for (e_idx in edge_indices) {
    g$edges[[e_idx]]$cap                          <- g$edges[[e_idx]]$cap - delta
    rev_idx <- g$edges[[e_idx]]$rev_idx
    g$edges[[rev_idx]]$cap                        <- g$edges[[rev_idx]]$cap + delta
  }
  invisible(NULL)
}

#' Bottleneck capacity along a path
#'
#' @keywords internal
#' @noRd
mcf_path_bottleneck <- function(g, edge_indices) {
  min(vapply(edge_indices, function(i) g$edges[[i]]$cap, numeric(1)))
}

#' Walk back the predecessor chain from sink to source, returning a vector of
#' edge indices in source->sink order (the augmenting path).
#'
#' @keywords internal
#' @noRd
mcf_walk_back <- function(prev_node, prev_edge, source, sink) {
  edges <- integer(0)
  v <- sink
  while (v != source) {
    e_idx <- prev_edge[v]
    if (e_idx == 0L) return(integer(0))   # no path
    edges <- c(e_idx, edges)
    v <- prev_node[v]
  }
  edges
}
