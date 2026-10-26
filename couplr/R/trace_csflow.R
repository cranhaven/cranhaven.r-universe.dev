# ==============================================================================
# Successive shortest paths with Johnson potentials, frame by frame
# ==============================================================================
# The algorithm runs in src/flow/flow_solve.cpp and this file renders what it
# did. solve_min_cost_flow() writes one record per search -- the distance
# labels, the shortest-path tree, the potentials after the shift, the path and
# the units moved -- and every frame here is built from that record. The state
# an animation needs is state the search already computes; restating the search
# in R would be a second implementation of it, and the two would drift.
#
# The network is the standard one: rows carry a unit of supply, a pair arc goes
# from a row to each column its cost is finite on, and a column arc carries the
# unit to the sink. A search starts at a row still holding its unit, reaches the
# sink through admissible reduced costs, and the shift after it keeps every
# residual reduced cost non-negative -- including on the columns the search
# stopped short of, which is why unreached nodes take the largest label.
#
# Duals are reported in the LAP convention, c[i,j] - u[i] - v[j] >= 0, which the
# node potentials give as u[i] = -pi[row_i] and v[j] = pi[col_j]. Under
# `maximize` the costs are negated first, so the duals are the negated problem's.
# ==============================================================================

#' @keywords internal
#' @noRd
trace_csflow <- function(cost, maximize = FALSE, ...) {
  v_in <- validate_cost_input(cost, "trace_csflow")
  cost_orig <- v_in$cost
  n <- v_in$n; m <- v_in$m
  if (n > m) {
    stop("trace_csflow: requires nrow <= ncol; got ", n, " x ", m, ".", call. = FALSE)
  }

  run <- lap_flow_trace_assignment(cost_orig, maximize = maximize)

  matching_row <- integer(n)
  matching_col <- integer(m)

  # Node potentials to LAP duals. run$row_base and run$col_base are 1-based
  # indices into the potential vector.
  ext_duals <- function(pot) {
    list(
      u = -pot[run$row_base + seq_len(n) - 1L],
      v = pot[run$col_base + seq_len(m) - 1L]
    )
  }

  # The pair arcs of a step, as (row, col) pairs. A step's labelled nodes carry
  # the arc each was reached by, and the ones reached through a column arc or
  # from the start have no pair to show.
  pair_edges <- function(rows, cols) {
    keep <- which(!is.na(rows) & !is.na(cols))
    lapply(keep, function(e) c(rows[e], cols[e]))
  }

  frames <- list()
  step <- 0L

  emit <- function(phase, description, pot,
                   active_edges = list(), path = list()) {
    step <<- step + 1L
    d <- ext_duals(pot)
    frames[[length(frames) + 1L]] <<- make_frame(
      step         = step,
      phase        = phase,
      description  = description,
      matching     = matching_row,
      dual_u       = d$u,
      dual_v       = d$v,
      active_edges = active_edges,
      path         = path
    )
  }

  n_pairs <- sum(is.finite(cost_orig))
  emit(
    "init",
    sprintf(
      paste0(
        "Built the flow network: %d rows each holding one unit, %d pair arcs ",
        "(only finite costs), %d column arcs into the sink. Potentials start ",
        "so that every residual arc has reduced cost c - h[u] + h[v] >= 0."
      ),
      n, n_pairs, m
    ),
    run$potential_initial
  )

  n_aug <- 0L
  for (st in run$steps) {
    tree <- pair_edges(st$tree_row, st$tree_col)
    path <- pair_edges(st$path_row, st$path_col)

    if (!isTRUE(st$reached)) {
      emit(
        "dijkstra",
        sprintf(
          paste0(
            "Search from row %d reached %d node(s) and no free column among ",
            "them, so row %d has no augmenting path and stays unmatched."
          ),
          st$source, length(st$labelled), st$source
        ),
        st$potential,
        active_edges = tree
      )
      next
    }

    n_aug <- n_aug + 1L
    emit(
      "dijkstra",
      sprintf(
        paste0(
          "Augmentation %d: shortest path from row %d to free column %d has ",
          "reduced length %.4g. The tree shown is every pair arc the search ",
          "settled on the way."
        ),
        n_aug, st$source, st$free_col, st$reach
      ),
      st$potential,
      active_edges = tree,
      path = path
    )

    # Flip the matching along the path. A pair arc taken forward gives its row
    # that column; one taken backward is the row giving the column up, and the
    # next forward arc on the path is where it goes instead.
    for (e in seq_along(st$path_row)) {
      i <- st$path_row[e]
      j <- st$path_col[e]
      if (is.na(i) || is.na(j)) next
      if (isTRUE(st$path_forward[e])) {
        matching_row[i] <- j
        matching_col[j] <- i
      } else {
        if (identical(matching_row[i], j)) matching_row[i] <- 0L
        if (identical(matching_col[j], i)) matching_col[j] <- 0L
      }
    }

    emit(
      "augment",
      sprintf(
        paste0(
          "Pushed %g unit of flow along the path and shifted the potentials by ",
          "the distance labels. Matched so far: %d."
        ),
        st$units, sum(matching_row > 0L)
      ),
      st$potential,
      path = path
    )
  }

  matching_row <- ifelse(is.na(run$match), 0L, run$match)
  total <- matching_total_cost(cost_orig, matching_row)

  final_pot <- if (length(run$steps) > 0L) {
    run$steps[[length(run$steps)]]$potential
  } else {
    run$potential_initial
  }

  emit(
    "final",
    sprintf("All %d units of flow shipped. Total cost: %.6g.",
            sum(matching_row > 0L), total),
    final_pot
  )

  list(
    meta = make_meta(
      algorithm   = "csflow",
      n_rows      = n,
      n_cols      = m,
      cost_matrix = cost_orig,
      maximize    = maximize,
      total_cost  = total,
      description = paste0(
        "Successive shortest paths with Johnson potentials (Tomizawa 1971, ",
        "Edmonds-Karp 1972). The assignment is a unit-capacity min-cost flow on ",
        "the bipartite network rows -> columns -> sink. Each augmentation runs ",
        "Dijkstra on reduced costs, pushes one unit along the shortest path, and ",
        "shifts the potentials by the distance labels, which keeps every residual ",
        "reduced cost non-negative even where the raw costs are negative. The ",
        "frames are read from the compiled solver's own per-search record."
      )
    ),
    frames = frames
  )
}

register_trace("csflow", trace_csflow)
