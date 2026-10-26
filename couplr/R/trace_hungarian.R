# ==============================================================================
# Reference Hungarian (shortest augmenting path) with frame-by-frame trace
# ==============================================================================
# This is the *teaching* implementation: a slow, readable R version of the
# Kuhn-Munkres primal-dual algorithm in its modern shortest-augmenting-path
# formulation. The production solver lives in C++ and is reached via
# assignment(cost, method = "hungarian"); this one exists purely to emit a
# step-by-step state trace for lap_animate().
#
# Algorithmic outline (for square cost matrices, nrow(cost) == ncol(cost)):
#
#   1. Initialise duals u, v so that c[i,j] - u[i] - v[j] >= 0 for all (i,j).
#      (Row mins for u, then column mins of c - u for v.)
#   2. For each row k = 1..n:
#      a. Grow a shortest-path tree rooted at row k on the residual graph
#         where the edge cost is the reduced cost c[i,j] - u[i] - v[j].
#         Cols are reached via outgoing edges from rows; rows are reached
#         via the incoming matched edge of a reached col.
#      b. When a free column is popped from the priority queue, the shortest
#         augmenting path has been found.
#      c. Update duals: scanned cols j get v[j] += dist[j] - delta; scanned
#         rows i get u[i] += delta - row_dist[i]. This keeps matched edges
#         tight and exposes the new path edges as tight.
#      d. Flip the matching along the alternating path.
#   3. After n iterations every row is matched.
#
# A frame is emitted at each pedagogically meaningful moment:
#   init / select_row / scan / path / dual_update / augment / final.
# ==============================================================================

#' @keywords internal
#' @noRd
trace_hungarian <- function(cost, maximize = FALSE, ...) {
  vc <- validate_square_cost(cost, "trace_hungarian", maximize, solver_hint = "hungarian")
  cost <- vc$cost; n <- vc$n; m <- vc$m
  cost_work <- vc$cost_work

  u <- numeric(n)
  v <- numeric(m)
  matching_row <- integer(n)   # row -> col, 0 = unmatched
  matching_col <- integer(m)   # col -> row, 0 = unmatched

  frames <- list()
  step <- 0L

  emit <- function(phase, description,
                   active_edges = list(), path = list()) {
    step <<- step + 1L
    frames[[length(frames) + 1L]] <<- make_frame(
      step         = step,
      phase        = phase,
      description  = description,
      matching     = matching_row,
      dual_u       = u,
      dual_v       = v,
      active_edges = active_edges,
      path         = path
    )
  }

  # --- Initialise duals ---------------------------------------------------
  for (i in seq_len(n)) u[i] <- min(cost_work[i, ])
  for (j in seq_len(m)) v[j] <- min(cost_work[, j] - u)

  emit(
    "init",
    paste0(
      "Initialise duals so every reduced cost is non-negative. ",
      "u[i] = min over j of c[i,j]; v[j] = min over i of (c[i,j] - u[i]). ",
      "Edges where c[i,j] - u[i] - v[j] = 0 are 'tight' and become candidate matching edges."
    )
  )

  # --- Main loop: match each row in turn ---------------------------------
  for (k in seq_len(n)) {

    dist     <- rep(Inf, m)
    pred     <- integer(m)
    scanned  <- logical(m)
    row_dist <- numeric(n)        # row entry-distance; 0 for the root row k
    scanned_rows <- integer(0)

    i <- k
    delta <- 0
    free_col <- 0L

    emit(
      "select_row",
      sprintf(
        "Start matching row %d. Grow a shortest-path tree on reduced costs through alternating row-column steps.",
        k
      )
    )

    repeat {
      # Relax outgoing edges from the current row i ------------------------
      for (j in seq_len(m)) {
        if (!scanned[j]) {
          rc <- cost_work[i, j] - u[i] - v[j]   # reduced cost, >= 0 by invariant
          new_dist <- delta + rc
          if (new_dist < dist[j]) {
            dist[j] <- new_dist
            pred[j] <- i
          }
        }
      }

      # Pick the closest unscanned column ---------------------------------
      candidates <- which(!scanned)
      j_min <- candidates[which.min(dist[candidates])]
      delta <- dist[j_min]
      scanned[j_min] <- TRUE

      tree <- sp_tree_edges(pred, scanned)

      if (matching_col[j_min] == 0L) {
        free_col <- j_min
        emit(
          "scan",
          sprintf(
            "Reach column %d at distance %.4g. Column is unmatched - shortest augmenting path found.",
            j_min, delta
          ),
          active_edges = tree
        )
        break
      }

      next_row <- matching_col[j_min]
      row_dist[next_row] <- delta
      scanned_rows <- c(scanned_rows, next_row)
      emit(
        "scan",
        sprintf(
          "Reach column %d at distance %.4g. It is matched to row %d - alternate through that matched edge and keep growing.",
          j_min, delta, next_row
        ),
        active_edges = tree
      )
      i <- next_row
    }

    # --- Build alternating path (for the path-highlight frame) ----------
    path_edges <- list()
    j <- free_col
    while (j != 0L) {
      i_aug <- pred[j]
      path_edges[[length(path_edges) + 1L]] <- c(i_aug, j)
      j <- matching_row[i_aug]    # walk to the col i_aug was matched to (0 stops the loop)
    }

    emit(
      "path",
      sprintf(
        "Alternating path from row %d to free column %d. Unmatched edges (to be added) and matched edges (to be removed) interleave.",
        k, free_col
      ),
      path = path_edges
    )

    # --- Dual update ---------------------------------------------------
    lifted <- sp_dual_lift(u, v, dist, scanned, delta, scanned_rows, row_dist, k)
    u <- lifted$u
    v <- lifted$v

    emit(
      "dual_update",
      sprintf(
        paste0(
          "Update duals to expose the path: scanned columns get v[j] += dist[j] - %.4g; ",
          "scanned rows get u[i] += %.4g - row_dist[i]. ",
          "Previously-matched edges stay tight; the new path edges also become tight."
        ),
        delta, delta
      ),
      path = path_edges
    )

    # --- Augment along path -------------------------------------------
    j <- free_col
    while (j != 0L) {
      i_aug <- pred[j]
      j_prev <- matching_row[i_aug]
      matching_row[i_aug] <- j
      matching_col[j] <- i_aug
      j <- j_prev
    }

    emit(
      "augment",
      sprintf(
        "Flip the alternating path: previously unmatched edges become matched and vice versa. Row %d is now assigned.",
        k
      )
    )
  }

  total <- matching_total_cost(cost, matching_row)

  emit(
    "final",
    sprintf("All %d rows matched. Total cost: %.6g.", n, total)
  )

  list(
    meta = make_meta(
      algorithm   = "hungarian",
      n_rows      = n,
      n_cols      = m,
      cost_matrix = cost,
      maximize    = maximize,
      total_cost  = total,
      description = paste0(
        "Kuhn-Munkres primal-dual algorithm (1955), shortest-augmenting-path variant. ",
        "Maintains row duals u and column duals v with c[i,j] - u[i] - v[j] >= 0. ",
        "For each row, finds a shortest alternating path to a free column using Dijkstra on reduced costs, ",
        "lifts the duals so new path edges become tight, and augments the matching."
      )
    ),
    frames = frames
  )
}

register_trace("hungarian", trace_hungarian)
