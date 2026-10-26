# ==============================================================================
# Reference SSAP-with-Dial's-bucket-queue with frame-by-frame trace
# ==============================================================================
# Mirrors src/solvers/solve_ssap_bucket.cpp. Identical shortest-augmenting-path
# Hungarian outer loop, but the Dijkstra inside each augmentation uses Dial's
# 1969 bucket queue (one bucket per integer distance) instead of a binary heap.
#
# Why bucketing matters pedagogically: when reduced costs are non-negative
# integers bounded by some C, Dijkstra runs in O(V + E + C) per augmentation
# instead of O((V+E) log V). For LAP this gives total O(n^3 + n*m*C) - faster
# than heap-based SSP whenever C is small (e.g. integer cost matrices with
# bounded entries).
#
# Algorithmic outline:
#
#   1. Scale costs to non-negative integers (smallest exact power-of-ten
#      multiplier; shift if needed).
#   2. Initialise duals u, v so reduced costs c[i,j] - u[i] - v[j] >= 0 and
#      integer.
#   3. For each row k:
#      a. Reset bucket array. Push row k at distance 0.
#      b. Pop the lowest non-empty bucket, scan its outgoing reduced-cost
#         edges. Push neighbours into the bucket indexed by their distance.
#      c. When a popped column is unmatched, the shortest augmenting path is
#         found. Update duals (Hungarian-style) and flip the matching.
# ==============================================================================

#' @keywords internal
#' @noRd
trace_ssap_bucket <- function(cost, maximize = FALSE, ...) {
  v_in <- validate_cost_input(cost, "trace_ssap_bucket")
  cost_orig <- v_in$cost
  n_orig <- v_in$n; m_orig <- v_in$m

  # --- Auto-transpose so internal matrix has n <= m -----------------------
  orient <- trace_orient_wide(cost_orig, n_orig, m_orig)
  transposed <- orient$transposed
  cost_int <- orient$cost
  n <- orient$n; m <- orient$m

  finite_mask <- is.finite(cost_int)
  if (!any(finite_mask)) {
    stop("`cost` has no finite entries.", call. = FALSE)
  }

  # --- Cost scaling to non-negative integers ------------------------------
  finite_vals <- cost_int[finite_mask]
  shift <- if (min(finite_vals) < 0) -min(finite_vals) else 0
  shifted <- finite_vals + shift
  # Smallest power of ten that renders every shifted cost integral, mirroring
  # src/solvers/solve_ssap_bucket.cpp. The tolerance is fixed in scaled units,
  # not proportional to the scale: a proportional tolerance grows to exceed half
  # a quantum at large scales, at which point every real reads as integral and
  # the optimum can flip via silent rounding. When no bounded
  # power of ten is exact, or the scaled magnitudes exceed the bucket solver's
  # integer range, refuse rather than round -- exactly as the production solver.
  INF_INT <- 1e9
  SCALE_INT_TOL <- 1e-6
  BUCKET_BUDGET <- 1e8
  max_abs <- max(abs(shifted))
  scale <- NA_integer_
  s <- 1
  repeat {
    if (all(abs(shifted * s - round(shifted * s)) <= SCALE_INT_TOL)) {
      scale <- s; break
    }
    if (max_abs * s * 10 >= INF_INT) break
    s <- s * 10
  }
  redirect <- "; use method = 'jv' or 'auction'"
  if (is.na(scale)) {
    stop("ssap_bucket: costs need finer fractional precision than the integer ",
         "bucket solver can represent exactly", redirect, call. = FALSE)
  }
  max_scaled <- max(round(shifted * scale))
  if (max_scaled >= INF_INT || n * max_scaled > BUCKET_BUDGET) {
    stop("ssap_bucket: cost magnitudes too large for the integer bucket solver",
         redirect, call. = FALSE)
  }
  scale <- as.integer(scale)

  # Build internal integer cost matrix. Forbidden cells get a very large int.
  big_int <- as.integer(round((max(shifted) + 1) * scale * (n + m + 1)))
  W <- matrix(big_int, nrow = n, ncol = m)
  for (i in seq_len(n)) {
    for (j in seq_len(m)) {
      if (finite_mask[i, j]) {
        W[i, j] <- as.integer(round((cost_int[i, j] + shift) * scale))
      }
    }
  }
  if (maximize) {
    cmax <- max(W[W < big_int])
    W_max <- W
    W_max[W < big_int] <- cmax - W[W < big_int]
    W <- W_max
  }

  # --- Hungarian-style duals ---------------------------------------------
  u <- integer(n); v <- integer(m)
  for (i in seq_len(n)) u[i] <- min(W[i, ])

  row_to_col <- integer(n)
  col_to_row <- integer(m)

  external_matching <- function() {
    trace_external_matching(row_to_col, n_orig, transposed)
  }

  ext_edge <- function(i_int, j_int) trace_ext_edge(i_int, j_int, transposed)

  frames <- list()
  step <- 0L
  emit <- function(phase, description, active_edges = list(), path = list()) {
    step <<- step + 1L
    frames[[length(frames) + 1L]] <<- make_frame(
      step         = step,
      phase        = phase,
      description  = description,
      matching     = external_matching(),
      dual_u       = if (transposed) NULL else as.numeric(u) / scale - shift,
      dual_v       = if (transposed) NULL else as.numeric(v) / scale,
      active_edges = active_edges,
      path         = path
    )
  }

  init_desc <- sprintf(
    paste0(
      "Scale factor %d, shift %g (internal costs are non-negative integers in [0, %d]). ",
      "Bucket queue: bucket[d] holds nodes at distance d. Initialise duals so reduced ",
      "costs c[i,j] - u[i] - v[j] are non-negative integers."
    ),
    scale, shift, big_int
  )
  emit("init", init_desc)

  # --- Main loop ----------------------------------------------------------
  for (start_row in seq_len(n)) {

    # Bucket queue state. We allocate buckets lazily as integers are pushed.
    INF_INT <- big_int * (n + m + 1L)
    dist <- rep(INF_INT, m)
    prev_col <- integer(m)
    prev_row <- integer(m)
    visited <- logical(m)
    scanned_rows <- integer(0)
    row_entry_d <- rep(INF_INT, n)
    row_entry_d[start_row] <- 0L

    # Buckets: a list where buckets[[d+1]] holds the indices currently waiting
    # at integer distance d (note R's 1-based indexing).
    buckets <- list(c())
    cursor <- 0L
    maxd <- 0L

    push_bucket <- function(d, item) {
      d <- as.integer(d)
      if (d < 0L) d <- 0L
      idx <- d + 1L
      while (length(buckets) < idx) buckets[[length(buckets) + 1L]] <<- integer(0)
      buckets[[idx]] <<- c(buckets[[idx]], item)
      if (d > maxd) maxd <<- d
    }

    # Seed Dijkstra: from start_row, relax to every column.
    for (j in seq_len(m)) {
      rc <- W[start_row, j] - u[start_row] - v[j]
      dist[j] <- as.integer(rc)
      prev_row[j] <- start_row
      push_bucket(dist[j], j)
    }

    emit(
      "select_row",
      sprintf(
        "Row %d: relax outgoing reduced-cost edges and push each column into bucket[dist].",
        start_row
      ),
      active_edges = lapply(seq_len(m), function(j) ext_edge(start_row, j))
    )

    end_col <- 0L
    repeat {
      # Advance cursor to next non-empty bucket
      while (cursor <= maxd &&
             (cursor + 1L > length(buckets) || length(buckets[[cursor + 1L]]) == 0L)) {
        cursor <- cursor + 1L
      }
      if (cursor > maxd) {
        stop("trace_ssap_bucket: infeasible (no augmenting path).", call. = FALSE)
      }

      # Pop one item from this bucket
      bk <- buckets[[cursor + 1L]]
      j_pop <- bk[length(bk)]
      buckets[[cursor + 1L]] <- bk[-length(bk)]

      if (visited[j_pop] || dist[j_pop] != cursor) next   # stale entry
      visited[j_pop] <- TRUE

      bucket_counts <- vapply(buckets, length, integer(1))
      bucket_summary <- paste(
        "bucket[", seq_along(bucket_counts) - 1L, "]=", bucket_counts,
        sep = "", collapse = " "
      )

      tree <- sp_tree_edges(prev_row, visited, ext_edge)

      if (col_to_row[j_pop] == 0L) {
        end_col <- j_pop
        emit(
          "scan",
          sprintf(
            "Pop column %d from bucket %d (distance %d). Column is unmatched - augmenting path found. State: %s",
            j_pop, cursor, dist[j_pop], bucket_summary
          ),
          active_edges = tree
        )
        break
      }

      next_row <- col_to_row[j_pop]
      scanned_rows <- c(scanned_rows, next_row)
      row_entry_d[next_row] <- dist[j_pop]

      emit(
        "scan",
        sprintf(
          "Pop column %d from bucket %d. Matched to row %d - relax row %d's outgoing edges. State: %s",
          j_pop, cursor, next_row, next_row, bucket_summary
        ),
        active_edges = tree
      )

      # Relax edges from next_row
      d_here <- dist[j_pop]
      for (j in seq_len(m)) {
        if (!visited[j]) {
          rc <- W[next_row, j] - u[next_row] - v[j]
          nd <- d_here + as.integer(rc)
          if (nd < dist[j]) {
            dist[j] <- nd
            prev_col[j] <- j_pop
            prev_row[j] <- next_row
            push_bucket(nd, j)
          }
        }
      }
    }

    # --- Build path (internal) -----------------------------------------------
    path_int <- list()
    cc <- end_col; cr <- prev_row[cc]
    repeat {
      path_int[[length(path_int) + 1L]] <- c(cr, cc)
      if (cr == start_row) break
      cc <- prev_col[cc]
      cr <- prev_row[cc]
    }
    path_ext <- lapply(path_int, function(e) ext_edge(e[1], e[2]))

    emit(
      "path",
      sprintf("Alternating path from row %d to free column %d.", start_row, end_col),
      path = path_ext
    )

    # --- Dual update (Hungarian-style) -------------------------------------
    delta <- dist[end_col]
    lifted <- sp_dual_lift(u, v, dist, visited, delta, scanned_rows,
                           row_entry_d, start_row)
    u <- lifted$u
    v <- lifted$v

    emit(
      "dual_update",
      sprintf(
        "Lift duals by integer increments: u[start] += %d; v[j] -= (%d - dist[j]) for visited j; u[i] += (%d - row_entry[i]) for scanned i.",
        delta, delta, delta
      ),
      path = path_ext
    )

    # --- Flip path ----------------------------------------------------------
    cc <- end_col; cr <- prev_row[cc]
    repeat {
      row_to_col[cr] <- cc
      col_to_row[cc] <- cr
      if (cr == start_row) break
      cc <- prev_col[cc]
      cr <- prev_row[cc]
    }
    emit("augment", sprintf("Flip path: row %d now matched to column %d.", start_row, end_col))
  }

  ext <- external_matching()
  total <- matching_total_cost(cost_orig, ext)
  emit("final",
       sprintf("All %d internal rows matched. Total cost (original scale): %.6g.", n, total))

  list(
    meta = make_meta(
      algorithm   = "ssap_bucket",
      n_rows      = n_orig,
      n_cols      = m_orig,
      cost_matrix = cost_orig,
      maximize    = maximize,
      total_cost  = total,
      description = paste0(
        "Successive shortest augmenting paths with Dial's bucket queue (1969). ",
        "Identical Hungarian dual-update outer loop, but Dijkstra inside each ",
        "augmentation uses an integer-indexed array of buckets instead of a ",
        "binary heap: bucket[d] holds nodes currently at distance d, and we ",
        "pop the lowest non-empty bucket. When reduced costs are non-negative ",
        "integers bounded by C, this gives O(V + E + C) per augmentation - ",
        "faster than O((V+E) log V) heap-Dijkstra whenever C is small."
      )
    ),
    frames = frames
  )
}

register_trace("ssap_bucket", trace_ssap_bucket)
