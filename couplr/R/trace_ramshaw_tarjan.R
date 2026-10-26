# ==============================================================================
# Reference Ramshaw-Tarjan rectangular assignment with frame-by-frame trace
# ==============================================================================
# Mirrors src/solvers/solve_ramshaw_tarjan.cpp. The Ramshaw-Tarjan paper (2012,
# "On Minimum-Cost Assignments in Unbalanced Bipartite Graphs") shows that the
# shortest-augmenting-path Hungarian algorithm runs unchanged on rectangular
# n x m matrices, provided one (a) auto-transposes to ensure n <= m, and
# (b) stops after n augmentations - leaving m - n columns unmatched, which is
# optimal for the unbalanced case.
#
# Algorithmic outline (for n x m cost matrices, n <= m after possible transpose):
#
#   1. Initialise u[i] = min over j of c[i,j]; v[j] = 0. This keeps all
#      reduced costs c[i,j] - u[i] - v[j] >= 0.
#   2. For each row k = 1..n:
#      a. Dijkstra on reduced costs from row k. Scan columns; for each scanned
#         column j that is currently matched, alternate through the matched
#         edge to its row and continue growing the tree.
#      b. Stop when an unmatched column is reached - shortest augmenting path
#         found, of length delta = dist[end_col].
#      c. Update duals: scanned columns j get v[j] -= (delta - dist[j]);
#         start row gets u += delta; scanned rows i (that were entered through
#         matched edges) get u[i] += (delta - dist[entry_col]).
#      d. Flip the alternating path.
#   3. After n iterations all rows of the (possibly transposed) matrix are
#      matched. If we transposed at the start, invert the assignment to recover
#      the original orientation. m_orig - n_orig original rows remain unmatched
#      when the original input had more rows than columns.
#
# Frames are emitted in the *original* orientation so the visualisation always
# matches the user's coordinate system.
# ==============================================================================

#' @keywords internal
#' @noRd
trace_ramshaw_tarjan <- function(cost, maximize = FALSE, ...) {
  v_in <- validate_cost_input(cost, "trace_ramshaw_tarjan")
  cost_orig <- v_in$cost
  n_orig <- v_in$n; m_orig <- v_in$m

  # --- Auto-transpose so the internal matrix has n <= m -------------------
  orient <- trace_orient_wide(cost_orig, n_orig, m_orig)
  transposed <- orient$transposed
  cost_int <- orient$cost
  n <- orient$n; m <- orient$m

  prep <- prepare_cost_work(cost_int, maximize)
  W <- prep$cost_work
  forbidden <- !prep$finite_mask

  # --- Dual potentials -----------------------------------------------------
  u <- numeric(n); v <- numeric(m)
  for (i in seq_len(n)) u[i] <- min(W[i, ])

  # --- Matching state (internal orientation) ------------------------------
  row_to_col <- integer(n)
  col_to_row <- integer(m)

  # External matching: always reported in original orientation, length n_orig.
  external_matching <- function() {
    trace_external_matching(row_to_col, n_orig, transposed)
  }

  # External dual vectors (only meaningful in original orientation when the
  # algorithm ran without transposition; for the transposed case they label
  # the wrong axis, so we suppress them rather than mislead the viewer).
  external_duals <- function() {
    if (transposed) list(u = NULL, v = NULL) else list(u = u, v = v)
  }

  frames <- list()
  step <- 0L

  emit <- function(phase, description,
                   active_edges = list(), path = list()) {
    step <<- step + 1L
    d <- external_duals()
    frames[[length(frames) + 1L]] <<- make_frame(
      step         = step,
      phase        = phase,
      description  = description,
      matching     = external_matching(),
      dual_u       = d$u,
      dual_v       = d$v,
      active_edges = active_edges,
      path         = path
    )
  }

  # Map an internal (row, col) edge back to original-orientation (row, col).
  ext_edge <- function(i_int, j_int) trace_ext_edge(i_int, j_int, transposed)

  init_desc <- if (transposed) {
    sprintf(
      paste0(
        "Auto-transposed: original was %d x %d, internal view is %d x %d so n <= m. ",
        "Initialise duals u[i] = min reduced row cost, v[j] = 0. ",
        "All reduced costs c[i,j] - u[i] - v[j] are now non-negative."
      ),
      n_orig, m_orig, n, m
    )
  } else {
    sprintf(
      paste0(
        "Rectangular %d x %d input with n <= m: no transposition needed. ",
        "Initialise duals u[i] = min reduced row cost, v[j] = 0. ",
        "All reduced costs c[i,j] - u[i] - v[j] are now non-negative."
      ),
      n, m
    )
  }
  emit("init", init_desc)

  # --- Main loop: augment one internal row at a time ----------------------
  for (start_row in seq_len(n)) {

    dist        <- rep(Inf, m)
    prev_col    <- integer(m)
    prev_row    <- integer(m)
    col_visited <- logical(m)
    scanned_rows<- integer(0)   # internal rows reached through a matched edge
    row_entry_d <- numeric(n)   # dist of the column that brought us into row

    # Seed: scan from start_row to every column
    for (j in seq_len(m)) {
      rc <- W[start_row, j] - u[start_row] - v[j]
      dist[j]     <- rc
      prev_row[j] <- start_row
    }

    emit(
      "select_row",
      sprintf(
        "Internal row %d: relax outgoing reduced-cost edges to every column to seed Dijkstra.",
        start_row
      ),
      active_edges = lapply(seq_len(m), function(j) ext_edge(start_row, j))
    )

    end_col <- 0L
    repeat {
      # Pick the closest unvisited column
      cand <- which(!col_visited)
      if (length(cand) == 0L) {
        stop("trace_ramshaw_tarjan: infeasible (no augmenting path).", call. = FALSE)
      }
      j_min <- cand[which.min(dist[cand])]
      d_min <- dist[j_min]
      if (!is.finite(d_min)) {
        stop("trace_ramshaw_tarjan: infeasible (no augmenting path).", call. = FALSE)
      }
      col_visited[j_min] <- TRUE

      tree <- sp_tree_edges(prev_row, col_visited, ext_edge)

      if (col_to_row[j_min] == 0L) {
        end_col <- j_min
        emit(
          "scan",
          sprintf(
            "Reach unmatched column %d at distance %.4g. Shortest augmenting path found.",
            j_min, d_min
          ),
          active_edges = tree
        )
        break
      }

      next_row <- col_to_row[j_min]
      scanned_rows <- c(scanned_rows, next_row)
      row_entry_d[next_row] <- d_min

      emit(
        "scan",
        sprintf(
          paste0(
            "Reach column %d at distance %.4g. It is matched to row %d - alternate ",
            "through that matched edge and relax edges from row %d."
          ),
          j_min, d_min, next_row, next_row
        ),
        active_edges = tree
      )

      # Relax edges from the newly-entered row
      for (j in seq_len(m)) {
        if (!col_visited[j]) {
          rc <- W[next_row, j] - u[next_row] - v[j]
          nd <- d_min + rc
          if (nd < dist[j]) {
            dist[j]     <- nd
            prev_col[j] <- j_min
            prev_row[j] <- next_row
          }
        }
      }
    }

    # --- Build augmenting path (in internal orientation, then map to ext) -
    path_int <- list()
    cur_col <- end_col
    cur_row <- prev_row[cur_col]
    repeat {
      path_int[[length(path_int) + 1L]] <- c(cur_row, cur_col)
      if (cur_row == start_row) break
      cur_col <- prev_col[cur_col]
      cur_row <- prev_row[cur_col]
    }
    path_ext <- lapply(path_int, function(e) ext_edge(e[1], e[2]))

    emit(
      "path",
      sprintf(
        "Alternating path from internal row %d to free column %d. Matched and unmatched edges interleave.",
        start_row, end_col
      ),
      path = path_ext
    )

    # --- Dual update --------------------------------------------------------
    delta <- dist[end_col]
    lifted <- sp_dual_lift(u, v, dist, col_visited, delta, scanned_rows,
                           row_entry_d, start_row)
    u <- lifted$u
    v <- lifted$v

    emit(
      "dual_update",
      sprintf(
        paste0(
          "Lift duals: u[start_row] += %.4g; for each scanned column j, ",
          "v[j] -= (%.4g - dist[j]); for each scanned row i, u[i] += (%.4g - row_entry[i])."
        ),
        delta, delta, delta
      ),
      path = path_ext
    )

    # --- Augment along path -------------------------------------------------
    cur_col <- end_col
    cur_row <- prev_row[cur_col]
    repeat {
      row_to_col[cur_row] <- cur_col
      col_to_row[cur_col] <- cur_row
      if (cur_row == start_row) break
      cur_col <- prev_col[cur_col]
      cur_row <- prev_row[cur_col]
    }

    emit(
      "augment",
      sprintf(
        "Flip the path: internal row %d is now matched to column %d.",
        start_row, end_col
      )
    )
  }

  # --- Compute total in original orientation ------------------------------
  ext <- external_matching()
  total <- matching_total_cost(cost_orig, ext)

  unmatched_msg <- if (n_orig > m_orig) {
    sprintf(" %d of %d original rows are unmatched (optimal: m < n).",
            n_orig - m_orig, n_orig)
  } else ""
  emit(
    "final",
    sprintf("Ramshaw-Tarjan complete after %d augmentations. Total cost: %.6g.%s",
            n, total, unmatched_msg)
  )

  list(
    meta = make_meta(
      algorithm   = "ramshaw_tarjan",
      n_rows      = n_orig,
      n_cols      = m_orig,
      cost_matrix = cost_orig,
      maximize    = maximize,
      total_cost  = total,
      description = paste0(
        "Ramshaw-Tarjan algorithm (2012) for unbalanced bipartite assignment. ",
        "Runs the shortest-augmenting-path Hungarian directly on a rectangular ",
        "n x m matrix without padding. Auto-transposes if n > m so the algorithm ",
        "operates on the smaller-dimension side; after n augmentations all rows ",
        "are matched optimally and m - n columns remain unmatched. Reuses the ",
        "Hungarian dual-update machinery: lift potentials so the augmenting path ",
        "becomes tight, flip, repeat."
      )
    ),
    frames = frames
  )
}

register_trace("ramshaw_tarjan", trace_ramshaw_tarjan)
