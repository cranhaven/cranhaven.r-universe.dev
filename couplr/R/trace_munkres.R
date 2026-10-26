# ==============================================================================
# Reference matrix-form Kuhn-Munkres with frame-by-frame trace
# ==============================================================================
# Mirrors src/solvers/solve_munkres.cpp byte for byte: row reduction, then the
# star/prime/cover bookkeeping that defines the classical "matrix algorithm"
# version of Kuhn-Munkres (Munkres 1957). The production solver lives in C++
# and is reached via assignment(cost, method = "munkres"); this R version
# exists purely to emit a step-by-step state trace for lap_animate().
#
# Reference: J. Munkres, "Algorithms for the Assignment and Transportation
# Problems," J. SIAM 5 (1957) 32-38. This is the matrix-form O(n^4) algorithm,
# kept as a reference implementation because it's the canonical pedagogical
# formulation - most LAP teaching material uses star/prime/cover notation.
#
# Algorithmic outline (square cost matrix, all entries finite):
#
#   --- Setup ---
#   1. Row-reduce: for each row, subtract its minimum entry. This bumps the
#      row potential u[i] by that minimum; the working matrix now has at least
#      one zero per row.
#   2. Star initial zeros: greedy pass. For each zero, if its column has no
#      starred zero yet, star it. Cover the columns of all starred zeros.
#
#   --- Main loop (repeat until n columns are covered) ---
#   3. Find an uncovered zero.
#      - If none exists, run a DUAL UPDATE: let d = smallest uncovered value;
#        add d to covered rows, subtract d from uncovered columns. This
#        decreases u on covered rows by d and increases v on uncovered columns
#        by d (so c - u - v stays >= 0 and at least one new zero appears).
#        Repeat 3.
#      - Else PRIME the uncovered zero (i, j).
#        - If row i has no starred zero, an AUGMENTING PATH has been found:
#          walk prime -> star (same column) -> prime (same row) -> ... until
#          a column with no star is reached. Unstar the old stars on this
#          path, star the primes. Clear all primes, reset covers, go to 3.
#        - Else cover row i, uncover the column of the star in row i, repeat 3.
#
# A frame is emitted at every pedagogically meaningful moment:
#   init / row_reduce / star_initial / cover_stars /
#   prime_zero / cover_uncover / update_costs / augment / final.
#
# The dual potentials u, v are maintained explicitly so the animation shows
# the underlying primal-dual structure (covered rows are rows whose u is being
# decreased; uncovered cols are cols whose v is being increased).
# ==============================================================================

#' @keywords internal
#' @noRd
trace_munkres <- function(cost, maximize = FALSE, ...) {
  vc <- validate_square_cost(cost, "trace_munkres", maximize, solver_hint = "munkres")
  cost <- vc$cost; n <- vc$n; m <- vc$m
  cost_orig <- cost
  cost_signed <- vc$cost_signed
  finite_mask <- vc$finite_mask
  for (i in seq_len(n)) {
    if (!any(finite_mask[i, ])) {
      stop("Row ", i, " has no finite (allowed) entries.", call. = FALSE)
    }
  }

  TOL <- 1e-9

  # Working matrix C: forbidden cells held as Inf so they never qualify as a
  # zero, an uncovered minimum, or a target for stars/primes. Matches the
  # `INF` substitution in solve_munkres.cpp.
  C <- cost_signed
  C[!finite_mask] <- Inf

  # Dual potentials. Maintained so working[i,j] = cost_signed[i,j] - u[i] - v[j]
  # holds throughout (modulo forbidden cells). Row reduction increases u[i];
  # the step-3 dual update decreases u on covered rows and increases v on
  # uncovered columns, exactly mirroring the cpp's in-place matrix update.
  u <- numeric(n)
  v <- numeric(m)

  # Munkres state (1-based; 0 = no star/prime). matching_row is the public
  # view used by frames.matching.
  star_col_of_row <- integer(n)      # row -> col (0 = no star)
  star_row_of_col <- integer(m)      # col -> row (0 = no star)
  prime_col_of_row <- integer(n)     # row -> col (0 = no prime)
  row_cov <- integer(n)
  col_cov <- integer(m)

  matching_row <- function() star_col_of_row

  frames <- list()
  step <- 0L

  emit <- function(phase, description,
                   active_edges = list(), path = list(),
                   show_duals = TRUE) {
    step <<- step + 1L
    frames[[length(frames) + 1L]] <<- make_frame(
      step, phase, description,
      matching     = matching_row(),
      dual_u       = if (show_duals) u else NULL,
      dual_v       = if (show_duals) v else NULL,
      active_edges = active_edges,
      path         = path
    )
  }

  emit(
    "init",
    paste0(
      "Matrix-form Munkres (1957). Row-reduce first to expose zeros, then star ",
      "an initial set of independent zeros. Cover columns with stars; if fewer ",
      "than n columns are covered, alternately prime an uncovered zero and ",
      "either augment the matching or shuffle covers until a smaller uncovered ",
      "minimum forces a dual update."
    )
  )

  # ---------------------------------------------------------------------------
  # Row reduction: subtract row min from each row
  # ---------------------------------------------------------------------------
  for (i in seq_len(n)) {
    finite_row <- is.finite(C[i, ])
    mn <- min(C[i, finite_row])
    if (!is.finite(mn)) {
      stop("Row ", i, " has no finite values.", call. = FALSE)
    }
    C[i, finite_row] <- C[i, finite_row] - mn
    u[i] <- u[i] + mn
    emit(
      "row_reduce",
      sprintf(
        "Row %d: subtracted min %.4g. u[%d] is now %.4g; row has at least one zero.",
        i, mn, i, u[i]
      )
    )
  }

  is_zero <- function(i, j) {
    x <- C[i, j]
    is.finite(x) && abs(x) <= TOL
  }

  # ---------------------------------------------------------------------------
  # Initial starring: greedy pass over zeros
  # ---------------------------------------------------------------------------
  # cpp uses col_cov as a scratch flag to mark "this column already has a star
  # in some earlier row", so the same column can't receive two stars.
  col_cov[] <- 0L
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      if (is_zero(i, j) && col_cov[j] == 0L) {
        star_col_of_row[i] <- j
        star_row_of_col[j] <- i
        col_cov[j] <- 1L
        emit(
          "star_initial",
          sprintf("Starred zero at (%d, %d).", i, j),
          active_edges = list(c(i, j))
        )
        break
      }
    }
  }

  # Reset col_cov to its "real" meaning: covered iff column has a starred zero.
  col_cov[] <- 0L
  for (j in seq_len(m)) {
    if (star_row_of_col[j] > 0L) col_cov[j] <- 1L
  }
  emit(
    "cover_stars",
    sprintf(
      "Initial covering: %d star%s placed, so %d column%s covered. If %d == n we're done.",
      sum(col_cov), if (sum(col_cov) == 1L) "" else "s",
      sum(col_cov), if (sum(col_cov) == 1L) "" else "s",
      sum(col_cov)
    )
  )

  find_uncovered_zero <- function() {
    for (i in seq_len(n)) {
      if (row_cov[i] == 0L) {
        for (j in seq_len(m)) {
          if (col_cov[j] == 0L && is_zero(i, j)) {
            return(c(i, j))
          }
        }
      }
    }
    NULL
  }

  smallest_uncovered <- function() {
    mn <- Inf
    for (i in seq_len(n)) {
      if (row_cov[i] == 0L) {
        for (j in seq_len(m)) {
          if (col_cov[j] == 0L && is.finite(C[i, j]) && C[i, j] < mn) {
            mn <- C[i, j]
          }
        }
      }
    }
    mn
  }

  max_outer <- as.numeric(n) * m * 100
  outer_iter <- 0

  # ---------------------------------------------------------------------------
  # Outer loop: keep augmenting until n columns are covered
  # ---------------------------------------------------------------------------
  repeat {
    outer_iter <- outer_iter + 1
    if (outer_iter > max_outer) {
      stop("trace_munkres: outer loop exceeded max iterations.", call. = FALSE)
    }

    num_stars <- sum(star_row_of_col > 0L)
    if (num_stars >= n) break

    row_cov[] <- 0L
    col_cov[] <- 0L
    for (j in seq_len(m)) {
      if (star_row_of_col[j] > 0L) col_cov[j] <- 1L
    }
    prime_col_of_row[] <- 0L

    max_inner <- as.numeric(n) * m * 100
    inner_iter <- 0

    # -----------------------------------------------------------------------
    # Inner loop: prime / cover-uncover / dual-update until an augmenting
    # path is found
    # -----------------------------------------------------------------------
    repeat {
      inner_iter <- inner_iter + 1
      if (inner_iter > max_inner) {
        stop("trace_munkres: inner loop exceeded max iterations.", call. = FALSE)
      }

      z <- find_uncovered_zero()

      if (is.null(z)) {
        # --- Dual update (cpp's step 3) ---
        d <- smallest_uncovered()
        if (!is.finite(d)) {
          stop("trace_munkres: no uncovered finite values - infeasible.",
               call. = FALSE)
        }

        # Update C in place: +d on covered rows, -d on uncovered cols.
        for (i in seq_len(n)) {
          for (j in seq_len(m)) {
            if (!is.finite(C[i, j])) next
            if (row_cov[i] == 1L) C[i, j] <- C[i, j] + d
            if (col_cov[j] == 0L) C[i, j] <- C[i, j] - d
          }
        }
        # Mirror the C update in the explicit duals.
        u[row_cov == 1L] <- u[row_cov == 1L] - d
        v[col_cov == 0L] <- v[col_cov == 0L] + d

        emit(
          "update_costs",
          sprintf(
            paste0(
              "No uncovered zero. Smallest uncovered value d = %.4g. Add d to ",
              "covered rows (u decreases by d there) and subtract d from ",
              "uncovered cols (v increases by d there). At least one new ",
              "uncovered zero appears."
            ),
            d
          )
        )
        next
      }

      i <- z[1]; j <- z[2]
      prime_col_of_row[i] <- j

      if (star_col_of_row[i] == 0L) {
        # --- Augmenting path (cpp's step 4) ---
        ii <- i; jj <- j
        path <- list(c(ii, jj))

        repeat {
          i_star <- star_row_of_col[jj]
          if (i_star == 0L) break
          path[[length(path) + 1L]] <- c(i_star, jj)
          j_prime <- prime_col_of_row[i_star]
          jj <- j_prime
          path[[length(path) + 1L]] <- c(i_star, jj)
        }

        emit(
          "augment",
          sprintf(
            paste0(
              "Primed zero at (%d, %d) - its row has no star, so we have an ",
              "augmenting path of length %d. Walk prime -> star (same col) -> ",
              "prime (same row) -> ..., then unstar the old stars and star the ",
              "primes along it."
            ),
            i, j, length(path)
          ),
          active_edges = list(c(i, j)),
          path = path
        )

        # Flip along the path. In cpp's 0-based indexing: even k -> star,
        # odd k -> unstar. In R's 1-based indexing that becomes: odd k -> star
        # (these were the primes), even k -> unstar (these were the old stars).
        for (k in seq_along(path)) {
          rc <- path[[k]]
          r <- rc[1]; c <- rc[2]
          if (k %% 2L == 1L) {
            star_col_of_row[r] <- c
            star_row_of_col[c] <- r
          } else {
            if (star_col_of_row[r] == c) star_col_of_row[r] <- 0L
            if (star_row_of_col[c] == r) star_row_of_col[c] <- 0L
          }
        }

        emit(
          "augment_done",
          sprintf(
            "Stars updated along the path. Matching now has %d star%s.",
            sum(star_row_of_col > 0L),
            if (sum(star_row_of_col > 0L) == 1L) "" else "s"
          )
        )

        break  # leave inner loop -> outer loop checks completion
      } else {
        # --- Cover row, uncover star's column (cpp's else branch) ---
        j_star <- star_col_of_row[i]
        row_cov[i] <- 1L
        col_cov[j_star] <- 0L

        emit(
          "cover_uncover",
          sprintf(
            paste0(
              "Primed zero at (%d, %d). Row %d already has a star at column %d, ",
              "so cover row %d and uncover column %d. Now hunt for the next ",
              "uncovered zero."
            ),
            i, j, i, j_star, i, j_star
          ),
          active_edges = list(c(i, j))
        )
      }
    }
  }

  # ---------------------------------------------------------------------------
  # Final cost
  # ---------------------------------------------------------------------------
  if (any(star_col_of_row == 0L)) {
    stop("trace_munkres: could not produce a complete matching.", call. = FALSE)
  }
  total_signed <- 0
  for (i in seq_len(n)) {
    j <- star_col_of_row[i]
    total_signed <- total_signed + cost_signed[i, j]
  }
  total <- if (maximize) -total_signed else total_signed

  emit(
    "final",
    sprintf(
      "Optimal assignment found. Total cost: %.6g. All n columns are covered by stars.",
      total
    )
  )

  list(
    meta = make_meta(
      "munkres", n, m, cost_orig, maximize, total,
      description = paste0(
        "Matrix-form Kuhn-Munkres (Munkres 1957). Row-reduce, star a maximal ",
        "set of independent zeros, then cover columns with stars. As long as ",
        "fewer than n columns are covered, prime an uncovered zero: either ",
        "trade it for a star via an alternating augmenting path, or cover its ",
        "row and uncover its star's column. When no uncovered zero exists, ",
        "subtract the smallest uncovered value from uncovered columns and add ",
        "it to covered rows - a dual update that exposes a new zero."
      )
    ),
    frames = frames
  )
}

register_trace("munkres", trace_munkres)
