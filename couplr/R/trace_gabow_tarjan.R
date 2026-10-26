# ==============================================================================
# Reference Gabow-Tarjan bit-scaling with frame-by-frame trace
# ==============================================================================
# Teaching implementation. The bit-scaling outer loop is implemented in R; the
# inner match_gt primitives (eligibility-graph construction, Hopcroft-Karp-style
# maximal-vertex-disjoint augmenting paths on the eligibility graph, and the
# bucket-array Hungarian search on cost-length with 1-feasibility) are the
# production C++ functions exposed via Rcpp:
#
#   - gt_build_equality_graph
#   - gt_find_maximal_augmenting_paths
#   - gt_augment_along_path
#   - gt_hungarian_step_one_feasible
#
# Driving those primitives from R one call at a time lets the trace emit a
# frame after each Step 1 / Step 2 event, while keeping the algorithm
# byte-faithful to Gabow & Tarjan 1989 as implemented in src/gabow_tarjan/.
#
# Algorithmic outline (square cost matrix, integer-valued):
#
#   1. Round costs to integers (GT requires integer inputs). Shift by -min
#      so every cost is >= 0. Multiply by (n+1). This guarantees that a
#      1-optimal matching for the scaled costs is *exactly* optimal for the
#      originals (the (n+1) factor opens a gap larger than n*1 = the worst
#      total slack a 1-optimal matching can have).
#
#   2. Process bits MSB to LSB. At each phase s:
#      - c_current[i,j] = 2 * c_current[i,j] + bit_s(scaled[i,j])
#      - y_u <- 2 * y_u - 1; y_v <- 2 * y_v - 1
#      The dual update "doubles" the previous phase's certificate and shifts
#      it by -1 on every coordinate, which is the warm-start that makes the
#      next phase's local problem nearly already-solved.
#
#   3. scale_match: build c' = c_current - y_u - y_v (the reduced costs of the
#      current scale's "free" problem). Run match_gt from an empty matching
#      with canonical 1-feasible duals: y_u_loc = 0, y_v_loc[j] = min_i(c'_ij + 1).
#
#   4. match_gt: while the local matching is not perfect, alternate
#        Step 1 - build eligibility graph, find maximal vertex-disjoint
#                 augmenting paths on it (Hopcroft-Karp), augment each,
#                 decrease y_v_loc[j] by 1 for every column on any path.
#        Step 2 - if Step 1 found no path, one Hungarian search on cost-length
#                 finds one path and lifts the duals to maintain 1-feasibility.
#      Both step types preserve the 1-feasibility invariant
#         y_u + y_v <= c + 1 everywhere, with equality on matched edges
#         y_u + y_v >= c on matched edges.
#
#   5. After match_gt converges, add the local duals to the global ones, copy
#      the local matching to the global state, and move on to the next bit.
#
#   6. After the LSB phase the matching is exactly optimal. The recovered
#      duals are in scaled space; we map them back to original units by
#      dividing by (n+1) and adding the shift.
# ==============================================================================

#' @keywords internal
#' @noRd
trace_gabow_tarjan <- function(cost, maximize = FALSE, ...) {
  vc <- validate_square_cost(cost, "trace_gabow_tarjan", maximize,
                             solver_hint = "gabow_tarjan")
  cost <- vc$cost; n <- vc$n; m <- vc$m
  cost_orig <- cost
  cost_signed <- vc$cost_signed
  finite_mask <- vc$finite_mask

  # Round to integers (GT is defined on integer costs).
  cost_int <- cost_signed
  cost_int[finite_mask] <- as.numeric(round(cost_signed[finite_mask]))
  was_integer <- max(abs(cost_signed[finite_mask] - cost_int[finite_mask])) < 1e-9

  # Shift to non-negative.
  min_c <- min(cost_int[finite_mask])
  shift_by <- if (min_c < 0) -min_c else 0
  cost_shifted <- cost_int
  cost_shifted[finite_mask] <- cost_int[finite_mask] + shift_by

  # Scale by (n+1).
  scale_factor <- n + 1L
  scaled <- cost_shifted
  scaled[finite_mask] <- cost_shifted[finite_mask] * scale_factor

  C_max <- if (any(finite_mask)) max(scaled[finite_mask]) else 0
  k_bits <- if (C_max <= 0) 0L else as.integer(floor(log2(C_max)) + 1L)

  # --- Frame state -----------------------------------------------------------
  matching_row <- integer(n)     # 0 = unmatched, 1-based col otherwise
  matching_col <- integer(m)
  y_u_global <- numeric(n)       # global scaled duals
  y_v_global <- numeric(m)

  frames <- list()
  step <- 0L

  # `matching` defaults to the global matching_row, but the inner loop
  # passes its local row_loc so the on-screen matched edges show the
  # actual within-phase rebuild GT does at every bit boundary.
  #
  # `bits_resolved` is the GT-specific progress metric (matched-count is
  # the wrong metric here -- GT solves the problem fully at each scale,
  # so edges yo-yo while bit-precision is monotonic). The progress_text
  # field overrides the widget's default "k/n matched" counter.
  bits_resolved <- 0L
  emit <- function(phase, description,
                   active_edges = list(), path = list(),
                   show_duals = FALSE,
                   matching = matching_row) {
    step <<- step + 1L
    frames[[length(frames) + 1L]] <<- make_frame(
      step, phase, description,
      matching      = matching,
      dual_u        = if (show_duals) y_u_global else NULL,
      dual_v        = if (show_duals) y_v_global else NULL,
      active_edges  = active_edges,
      path          = path,
      progress_text = sprintf("Bit %d / %d resolved", bits_resolved, k_bits)
    )
  }

  emit(
    "init",
    sprintf(
      paste0(
        "Gabow-Tarjan bit-scaling. %sCosts shifted by %s and multiplied by (n+1) = %d. ",
        "Maximum scaled cost = %s, processed across %d bit phases (MSB to LSB). ",
        "Each phase maintains 1-feasibility (u+v <= c+1 everywhere, equality on matched edges)."
      ),
      if (was_integer) "" else "Costs first rounded to integers. ",
      format(shift_by), scale_factor, format(C_max), k_bits
    )
  )

  if (k_bits == 0L) {
    matching_row <- seq_len(n)
    matching_col <- seq_len(m)
    emit("final",
         "All scaled costs are zero; the diagonal assignment is optimal.")
    return(.gt_make_result(cost_orig, matching_row, n, m, maximize, frames))
  }

  # c_current accumulates the top (k - s) bits of scaled. Forbidden positions
  # are kept as NA (which the C++ Rcpp wrappers convert to BIG_INT).
  c_current <- matrix(0, n, m)
  c_current[!finite_mask] <- NA_real_

  # ---------------------------------------------------------------------------
  # Bit-scaling outer loop
  # ---------------------------------------------------------------------------
  for (s in seq.int(from = k_bits - 1L, to = 0L, by = -1L)) {

    # Update c_current: c <- 2*c + bit_s(scaled)
    bit_s_matrix <- matrix(0, n, m)
    bit_s_matrix[finite_mask] <- bitwAnd(
      bitwShiftR(as.integer(scaled[finite_mask]), s), 1L
    )
    c_current[finite_mask] <- c_current[finite_mask] * 2 + bit_s_matrix[finite_mask]

    # Update global duals: y <- 2*y - 1
    y_u_global <- 2 * y_u_global - 1
    y_v_global <- 2 * y_v_global - 1

    phase_idx <- k_bits - s
    max_c_now <- if (any(finite_mask)) max(c_current[finite_mask]) else 0
    emit(
      "phase_start",
      sprintf(
        paste0(
          "Phase %d/%d - incorporate bit %d (value 2^%d). c_current[i,j] doubled and OR'd ",
          "with bit %d of the (n+1)-scaled true cost. Largest entry now: %s. ",
          "Doubling the carried duals as y <- 2*y - 1 keeps them within 1 of feasible for the new scale. ",
          "GT does scale_match from an empty matching at every bit boundary, so the matched-counter resets."
        ),
        phase_idx, k_bits, s, s, s, format(max_c_now)
      ),
      show_duals = TRUE,
      matching = integer(n)
    )

    # -----------------------------------------------------------------------
    # scale_match: c' = c_current - y_global on a fresh empty matching.
    # The C++ scale_match does the same fresh-restart at every phase; carrying
    # the previous matching across the (n+1) scaling boundary is generally not
    # 1-feasible on the new c' (matched edges may violate the lower bound).
    # -----------------------------------------------------------------------
    cost_prime <- c_current - outer(y_u_global, y_v_global, "+")
    cost_prime[!finite_mask] <- NA_real_

    row_loc <- integer(n)
    col_loc <- integer(m)
    y_u_loc <- numeric(n)
    y_v_loc <- numeric(m)

    # Canonical 1-feasible duals for an empty matching: y_u_loc = 0,
    # y_v_loc[j] = min over i of (c'_ij + 1).
    for (j in seq_len(m)) {
      col_vals <- cost_prime[, j]
      finite_in_col <- is.finite(col_vals)
      if (any(finite_in_col)) {
        y_v_loc[j] <- min(col_vals[finite_in_col]) + 1
      } else {
        y_v_loc[j] <- 0
      }
    }

    # -----------------------------------------------------------------------
    # match_gt inner loop: alternate Step 1 and Step 2 until perfect.
    # -----------------------------------------------------------------------
    inner_iter <- 0L
    max_inner <- 4L * n * n + 10L

    while (any(row_loc == 0L) && inner_iter < max_inner) {
      inner_iter <- inner_iter + 1L

      eq_graph <- gt_build_equality_graph(cost_prime, row_loc, y_u_loc, y_v_loc)
      eligible_edges <- list()
      n_eligible <- 0L
      for (i in seq_along(eq_graph)) {
        for (j in eq_graph[[i]]) {
          n_eligible <- n_eligible + 1L
          eligible_edges[[n_eligible]] <- c(i, as.integer(j))
        }
      }

      paths <- gt_find_maximal_augmenting_paths(eq_graph, row_loc, col_loc)

      if (length(paths) > 0L) {
        # ----- Step 1: maximal vertex-disjoint augmenting paths --------
        all_path_edges <- list()
        path_cols <- integer(0)
        for (p in paths) {
          for (e_idx in seq_len(nrow(p))) {
            all_path_edges[[length(all_path_edges) + 1L]] <-
              c(as.integer(p[e_idx, 1]), as.integer(p[e_idx, 2]))
            path_cols <- c(path_cols, p[e_idx, 2])
          }
        }
        path_cols <- unique(path_cols)

        emit(
          "step1_find",
          sprintf(
            paste0(
              "Step 1 (iter %d): build eligibility graph (%d edges where u+v = cl(e)). ",
              "Hopcroft-Karp finds %d vertex-disjoint augmenting paths covering %d edges. ",
              "Augmenting all of them in one shot is the move that buys GT its O(sqrt(n)) ",
              "match_gt iteration count."
            ),
            inner_iter, n_eligible, length(paths), length(all_path_edges)
          ),
          active_edges = eligible_edges,
          path = all_path_edges,
          matching = row_loc
        )

        # Augment each path; track path columns
        for (p in paths) {
          res <- gt_augment_along_path(p, row_loc, col_loc)
          row_loc <- res$row_match
          col_loc <- res$col_match
        }
        # Step 1 dual update: y_v_loc[j] -= 1 for every column on any path.
        y_v_loc[path_cols] <- y_v_loc[path_cols] - 1

        emit(
          "step1_augment",
          sprintf(
            paste0(
              "Step 1 (iter %d): augment all %d paths simultaneously, then decrease ",
              "v[j] by 1 on each of the %d covered columns. ",
              "Matched edges stay tight (u+v = c); other edges still satisfy u+v <= c+1."
            ),
            inner_iter, length(paths), length(path_cols)
          ),
          matching = row_loc
        )

      } else {
        # ----- Step 2: one Hungarian search on cost-length --------------
        emit(
          "step2_search",
          sprintf(
            paste0(
              "Step 1 (iter %d) found no augmenting path on the %d-edge eligibility graph. ",
              "Step 2: a single Hungarian search on cost-length (cl = c+1 outside the matching, ",
              "c inside) finds one augmenting path and lifts the duals just enough to expose it."
            ),
            inner_iter, n_eligible
          ),
          active_edges = eligible_edges,
          matching = row_loc
        )

        res <- gt_hungarian_step_one_feasible(
          cost_prime, row_loc, col_loc, y_u_loc, y_v_loc
        )
        if (!isTRUE(res$found)) {
          stop("Gabow-Tarjan Step 2 failed to find an augmenting path - ",
               "no perfect matching exists for this cost matrix.",
               call. = FALSE)
        }
        # Identify newly matched edges to highlight as path
        new_path_edges <- list()
        for (i in seq_len(n)) {
          if (row_loc[i] == 0L && res$row_match[i] > 0L) {
            new_path_edges[[length(new_path_edges) + 1L]] <-
              c(i, as.integer(res$row_match[i]))
          }
        }

        row_loc <- res$row_match
        col_loc <- res$col_match
        y_u_loc <- res$y_u
        y_v_loc <- res$y_v

        emit(
          "step2_done",
          sprintf(
            paste0(
              "Step 2: augment along the one path found; duals are lifted to match. ",
              "1-feasibility preserved; %d row(s) now newly matched."
            ),
            length(new_path_edges)
          ),
          path = new_path_edges,
          matching = row_loc
        )
      }
    }

    if (inner_iter >= max_inner) {
      stop("trace_gabow_tarjan: match_gt inner loop exceeded ",
           max_inner, " iterations - cost matrix may be infeasible.",
           call. = FALSE)
    }

    # Update global duals and matching after the phase
    y_u_global <- y_u_global + y_u_loc
    y_v_global <- y_v_global + y_v_loc
    matching_row <- row_loc
    matching_col <- col_loc
    bits_resolved <- phase_idx

    cur_total <- sum(cost_orig[cbind(seq_len(n), matching_row)], na.rm = TRUE)
    emit(
      "phase_end",
      sprintf(
        "Phase %d done after %d match_gt iteration%s. Local matching is 1-optimal for c' at this bit-precision. Original-cost value of current matching: %.6g.",
        phase_idx, inner_iter, if (inner_iter == 1L) "" else "s", cur_total
      ),
      show_duals = TRUE
    )
  }

  total <- sum(cost_orig[cbind(seq_len(n), matching_row)], na.rm = TRUE)
  emit(
    "final",
    sprintf(
      "All %d bit phases processed. Matching is exactly optimal at full precision. Total cost: %.6g.",
      k_bits, total
    ),
    show_duals = TRUE
  )

  .gt_make_result(cost_orig, matching_row, n, m, maximize, frames)
}

#' @keywords internal
#' @noRd
.gt_make_result <- function(cost_orig, matching_row, n, m, maximize, frames) {
  total <- sum(cost_orig[cbind(seq_len(n), matching_row)], na.rm = TRUE)
  list(
    meta = make_meta(
      "gabow_tarjan", n, m, cost_orig, maximize, total,
      description = paste0(
        "Gabow-Tarjan (1989) bit-scaling with maintained 1-feasibility. ",
        "Multiply integer costs by (n+1), process bits MSB to LSB. Each scale phase runs ",
        "match_gt: alternate Step 1 (maximal vertex-disjoint augmenting paths on the ",
        "eligibility graph, Hopcroft-Karp) with Step 2 (one Hungarian search on cost-length ",
        "to lift the duals and expose a new path). After the LSB phase the matching is ",
        "exactly optimal."
      )
    ),
    frames = frames
  )
}

register_trace("gabow_tarjan", trace_gabow_tarjan)
