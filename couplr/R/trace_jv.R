# ==============================================================================
# Reference Jonker-Volgenant (LAPJV) with frame-by-frame trace
# ==============================================================================
# Teaching implementation. Mirrors src/solvers/jv_core.cpp byte for byte: the
# three Jonker-Volgenant 1987 pre-stages followed by the Dijkstra-style
# shortest-augmenting-path main loop. The production solver lives in C++ and
# is reached via assignment(cost, method = "jv"); this R version exists purely
# to emit a step-by-step state trace for lap_animate().
#
# Reference: R. Jonker & A. Volgenant, "A Shortest Augmenting Path Algorithm
# for Dense and Sparse Linear Assignment Problems," Computing 38 (1987)
# 325-340.
#
# Algorithmic outline (square cost matrix, all entries finite):
#
#   --- Pre-stages ---
#   1. COLUMN REDUCTION (CR). For each column j (back to front), find the row
#      i* of minimum cost, set v[j] = c[i*,j], and tentatively assign (i*, j).
#      If row i* was already claimed by some earlier column j', keep whichever
#      side has the cheaper v.
#   2. REDUCTION TRANSFER (RT). For every row that ended up with exactly one
#      tentative assignment (j1), decrease v[j1] by the gap to the next-best
#      column for that row. Tightens the dual without changing the matching.
#   3. AUGMENTING ROW REDUCTION (ARR). Two passes over the still-free rows.
#      For each free row find its best and second-best reduced-cost column;
#      push v[j1] down by the gap, claim col j1 (displacing its prior owner
#      if any), and queue the displaced row.
#
#   --- Main loop ---
#   4. For each row still unmatched after the pre-stages, run a single
#      Dijkstra-style shortest-augmenting-path search on reduced costs and
#      flip the matching along the path. Identical in spirit to the
#      shortest-augmenting-path Hungarian, only the dual-update formula is
#      written in the LAPJV "scanned cols get v -= delta, scanned rows get
#      u += delta, unscanned cols get minv -= delta" form so that the
#      maintained minv array stays valid across the outer loop.
#
# The pre-stages assume the matrix is square and fully connected. For inputs
# with any forbidden (NA / Inf) entry, the pre-stages are skipped and the
# main loop processes every row from scratch - matching the C++ guard
# `safe_warm_start = (n == m) && (no forbidden)`.
# ==============================================================================

#' @keywords internal
#' @noRd
trace_jv <- function(cost, maximize = FALSE, ...) {
  vc <- validate_square_cost(cost, "trace_jv", maximize, solver_hint = "jv")
  cost <- vc$cost; n <- vc$n; m <- vc$m
  cost_orig <- cost
  any_forbidden <- vc$any_forbidden
  # Forbidden cells carry a big sentinel so they never win a min(), reduction
  # transfer, or ARR comparison; the algorithm just steers around them.
  BIG <- vc$big_m
  cost_work <- vc$cost_work

  u <- numeric(n)
  v <- numeric(m)
  matching_row <- integer(n)   # 0 = unmatched
  matching_col <- integer(m)

  frames <- list()
  step <- 0L

  emit <- function(phase, description,
                   active_edges = list(), path = list(),
                   show_duals = TRUE) {
    step <<- step + 1L
    frames[[length(frames) + 1L]] <<- make_frame(
      step         = step,
      phase        = phase,
      description  = description,
      matching     = matching_row,
      dual_u       = if (show_duals) u else NULL,
      dual_v       = if (show_duals) v else NULL,
      active_edges = active_edges,
      path         = path
    )
  }

  emit(
    "init",
    sprintf(
      paste0(
        "Jonker-Volgenant (LAPJV). Three pre-stages get most of the matching done greedily, ",
        "then a Dijkstra-style shortest-augmenting-path main loop finishes the rest. ",
        "%s"
      ),
      if (any_forbidden)
        "Cost matrix has forbidden entries; pre-stages skipped (matches C++ safe_warm_start guard)."
      else
        "Cost matrix is square and fully connected - all three pre-stages run."
    )
  )

  free0 <- integer(0)            # rows still needing main-loop augmentation
  matches <- integer(n)          # count of tentative assignments per row (CR phase)

  # ---------------------------------------------------------------------------
  # Pre-stage 1: COLUMN REDUCTION
  # ---------------------------------------------------------------------------
  if (!any_forbidden) {
    for (j in seq.int(m, 1L, by = -1L)) {
      col_vec <- cost_work[, j]
      imin <- which.min(col_vec)
      mn <- col_vec[imin]
      v[j] <- mn
      matches[imin] <- matches[imin] + 1L

      assigned <- FALSE
      switched <- FALSE
      prev_j <- 0L

      if (matches[imin] == 1L) {
        matching_row[imin] <- j
        matching_col[j] <- imin
        assigned <- TRUE
      } else {
        prev_j <- matching_row[imin]
        if (v[j] < v[prev_j]) {
          matching_row[imin] <- j
          matching_col[j] <- imin
          matching_col[prev_j] <- 0L
          assigned <- TRUE
          switched <- TRUE
        } else {
          matching_col[j] <- 0L
        }
      }

      descr <- if (assigned && !switched) {
        sprintf("Column %d: minimum at row %d (cost %.4g). Row %d is free - claim (%d, %d) and set v[%d] = %.4g.",
                j, imin, mn, imin, imin, j, j, mn)
      } else if (assigned && switched) {
        sprintf("Column %d: minimum at row %d (cost %.4g). Row %d was already claimed by column %d (v=%.4g) but %d is cheaper - switch. Column %d becomes unassigned.",
                j, imin, mn, imin, prev_j, v[prev_j], j, prev_j)
      } else {
        sprintf("Column %d: minimum at row %d (cost %.4g). Row %d already claimed by column %d at cheaper v=%.4g - leave column %d unassigned for now.",
                j, imin, mn, imin, matching_row[imin], v[matching_row[imin]], j)
      }

      emit("column_reduction", descr,
           active_edges = list(c(imin, j)),
           path = if (assigned) list(c(imin, j)) else list())
    }

    # -------------------------------------------------------------------------
    # Pre-stage 2: REDUCTION TRANSFER
    # -------------------------------------------------------------------------
    for (i in seq_len(n)) {
      if (matches[i] == 0L) {
        free0 <- c(free0, i)
      } else if (matches[i] == 1L) {
        j1 <- matching_row[i]
        mn <- Inf
        for (j in seq_len(m)) {
          if (j == j1) next
          h <- cost_work[i, j] - v[j]
          if (h < mn) mn <- h
        }
        if (is.finite(mn)) {
          old_v <- v[j1]
          v[j1] <- v[j1] - mn
          emit("reduction_transfer",
               sprintf("Row %d has exactly one tentative match (column %d). Push v[%d] down by the gap to its second-cheapest column: %.4g -> %.4g. Matched edge stays tight.",
                       i, j1, j1, old_v, v[j1]),
               path = list(c(i, j1)))
        }
      }
      # matches[i] >= 2 rows keep their current state; nothing to do
    }

    # -------------------------------------------------------------------------
    # Pre-stage 3: AUGMENTING ROW REDUCTION (two passes)
    # -------------------------------------------------------------------------
    for (loopcnt in seq_len(2)) {
      if (length(free0) == 0L) break

      cur <- free0
      free0 <- integer(0)
      k <- 1L
      prv <- length(cur)

      while (k <= prv) {
        i <- cur[k]
        k <- k + 1L

        umin <- Inf
        usubmin <- Inf
        j1 <- 0L; j2 <- 0L
        for (j in seq_len(m)) {
          if (cost_work[i, j] >= BIG) next   # forbidden (shouldn't happen here)
          h <- cost_work[i, j] - v[j]
          if (h < usubmin) {
            if (h >= umin) {
              usubmin <- h; j2 <- j
            } else {
              usubmin <- umin; j2 <- j1
              umin <- h; j1 <- j
            }
          }
        }

        if (j1 == 0L) {
          free0 <- c(free0, i)
          next
        }

        i0 <- matching_col[j1]
        tied <- FALSE
        dual_change <- 0
        if (j2 > 0L && umin < usubmin) {
          dual_change <- usubmin - umin
          v[j1] <- v[j1] - dual_change
        } else if (i0 > 0L) {
          # Tie on umin: swap to second-best to avoid useless ping-pong.
          if (j2 > 0L) {
            j1 <- j2
            i0 <- matching_col[j2]
            tied <- TRUE
          }
        }

        if (matching_row[i] != 0L) {
          # row i may have been displaced earlier in this pass - clear stale
          matching_col[matching_row[i]] <- 0L
        }
        matching_row[i] <- j1
        matching_col[j1] <- i
        if (i0 > 0L) {
          matching_row[i0] <- 0L
          if (j2 > 0L && umin < usubmin) {
            # Reprocess the displaced row in this same pass (paper convention).
            k <- k - 1L
            cur[k] <- i0
          } else {
            free0 <- c(free0, i0)
          }
        }

        descr <- if (tied) {
          sprintf("ARR pass %d, row %d: best/second-best tied (both cost %.4g). Take second-best column %d to break the tie; displaces row %d.",
                  loopcnt, i, umin, j1, i0)
        } else {
          sprintf("ARR pass %d, row %d: best column %d (reduced cost %.4g), second-best column %d (reduced cost %.4g). Push v[%d] down by gap %.4g and claim column %d%s.",
                  loopcnt, i, j1, umin, j2, usubmin, j1, dual_change, j1,
                  if (i0 > 0L) sprintf(" (displacing row %d)", i0) else "")
        }

        active <- if (j2 > 0L) list(c(i, j1), c(i, j2)) else list(c(i, j1))
        emit("arr", descr,
             active_edges = active,
             path = list(c(i, j1)))
      }
    }
  } else {
    # Forbidden present: pre-stages skipped, every row needs augmentation.
    free0 <- seq_len(n)
  }

  # ---------------------------------------------------------------------------
  # Reconstruct u from current matching (matches the C++ post-ARR step).
  # ---------------------------------------------------------------------------
  for (i in seq_len(n)) {
    j <- matching_row[i]
    if (j > 0L && matching_col[j] == i && cost_work[i, j] < BIG) {
      u[i] <- cost_work[i, j] - v[j]
    }
  }

  if (!any_forbidden && length(free0) > 0L) {
    emit(
      "pre_done",
      sprintf("Pre-stages complete. %d of %d rows matched greedily; %d rows still need shortest-augmenting-path search.",
              n - length(free0), n, length(free0))
    )
  } else if (!any_forbidden) {
    emit(
      "pre_done",
      "Pre-stages complete and produced a perfect matching - no augmentation needed."
    )
  }

  # ---------------------------------------------------------------------------
  # Main loop: Dijkstra-style shortest augmenting path for each remaining row.
  # Translated from jv_core.cpp's 1-based augmentation; uses the LAPJV-style
  # dual update (scanned cols: v -= delta, owner rows: u += delta).
  # ---------------------------------------------------------------------------
  for (i_aug in free0) {

    minv <- rep(Inf, m)
    used <- logical(m)
    way  <- integer(m)         # way[j] = previous col on SP tree (0 = root)
    pred_row <- integer(m)     # pred_row[j] = row that proposed j

    # Initial relaxation from row i_aug
    for (j in seq_len(m)) {
      cur <- cost_work[i_aug, j] - u[i_aug] - v[j]
      minv[j] <- cur
      pred_row[j] <- i_aug
      # way[j] stays 0 (root)
    }

    j0 <- 0L
    delta <- 0
    free_col <- 0L

    repeat {
      # Pick the closest unscanned column
      j1 <- which.min(ifelse(used, Inf, minv))
      delta <- minv[j1]
      used[j1] <- TRUE
      j0 <- j1

      i0 <- matching_col[j0]
      if (i0 == 0L) {
        free_col <- j0
        break
      }

      # Relax from the row that holds j0
      for (j in seq_len(m)) {
        if (used[j]) next
        cur <- cost_work[i0, j] - u[i0] - v[j]
        # minv[] holds distances relative to the running offset delta, so the
        # distance to j through i0 is delta plus the reduced cost of (i0, j).
        new_minv <- delta + cur
        if (new_minv < minv[j]) {
          minv[j] <- new_minv
          way[j] <- j0
          pred_row[j] <- i0
        }
      }
    }

    # ---- Dual update (LAPJV style) ----
    # For every scanned col j: v[j] -= (delta - dist_to_j) = v[j] += dist_to_j - delta
    # For owning row of scanned col j: u[row] += (delta - dist_to_j)
    # This is mathematically equivalent to the shift form used in trace_hungarian.
    for (j in seq_len(m)) {
      if (used[j]) {
        dist_j <- minv[j]
        v[j] <- v[j] + dist_j - delta
        owner <- matching_col[j]
        if (owner > 0L) {
          u[owner] <- u[owner] + (delta - dist_j)
        }
      }
    }
    # The starting row i_aug also has its u lifted by delta
    u[i_aug] <- u[i_aug] + delta

    # ---- Recover augmenting path ----
    path_edges <- list()
    j <- free_col
    repeat {
      i_edge <- pred_row[j]
      path_edges[[length(path_edges) + 1L]] <- c(i_edge, j)
      j_prev <- way[j]
      if (j_prev == 0L) break
      j <- j_prev
    }

    emit(
      "main_path",
      sprintf(
        "Main loop, row %d: Dijkstra-style shortest augmenting path of length %d ends at free column %d (distance %.4g). Augment and lift duals.",
        i_aug, length(path_edges), free_col, delta
      ),
      path = path_edges
    )

    # ---- Augment along path: flip matched/unmatched ----
    j <- free_col
    repeat {
      i_edge <- pred_row[j]
      j_prev_match <- matching_row[i_edge]
      matching_row[i_edge] <- j
      matching_col[j] <- i_edge
      j_prev <- way[j]
      if (j_prev == 0L) break
      j <- j_prev
    }

    emit(
      "main_augment",
      sprintf("Row %d is now matched. %d row(s) remaining.",
              i_aug, sum(matching_row == 0L))
    )
  }

  total <- sum(cost_orig[cbind(seq_len(n), matching_row)], na.rm = TRUE)
  emit(
    "final",
    sprintf("All %d rows matched. Total cost: %.6g.", n, total)
  )

  list(
    meta = make_meta(
      algorithm   = "jv",
      n_rows      = n,
      n_cols      = m,
      cost_matrix = cost_orig,
      maximize    = maximize,
      total_cost  = total,
      description = paste0(
        "Jonker-Volgenant (1987). Three pre-stages - column reduction, reduction transfer, ",
        "augmenting row reduction - greedily produce most of the matching with O(nm) work. ",
        "Remaining unmatched rows are completed by Dijkstra-style shortest-augmenting-path ",
        "search on reduced costs, identical in spirit to the modern Hungarian."
      )
    ),
    frames = frames
  )
}

register_trace("jv", trace_jv)
