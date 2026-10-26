# ==============================================================================
# Reference Hopcroft-Karp on 0/1 cost matrices with frame-by-frame trace
# ==============================================================================
# Mirrors src/solvers/solve_hk01.cpp. Restricted to cost matrices whose finite
# entries form an all-equal set or a binary {0, 1} palette: the production
# solver picks zero-cost edges (or the smaller-cost class under maximize) and
# runs Hopcroft-Karp on the resulting bipartite cardinality-matching graph.
#
# Hopcroft-Karp algorithmic outline:
#
#   Repeat:
#     a. BFS from every currently-unmatched left vertex, alternating
#        unmatched -> matched -> unmatched edges, building integer layer
#        distances. Stop when at least one free right vertex is reached.
#     b. For every free left vertex, DFS along the layered DAG looking for
#        an augmenting path. Each augmenting path uses vertex-disjoint
#        edges; flip the path to grow the matching by 1.
#   Until a BFS phase finds no free right vertex reachable.
#
# Each phase guarantees at least one augmenting path, and shortest paths
# strictly lengthen between phases, giving overall O(E * sqrt(V)).
# ==============================================================================

#' @keywords internal
#' @noRd
trace_hk01 <- function(cost, maximize = FALSE, ...) {
  v_in <- validate_cost_input(cost, "trace_hk01")
  cost_orig <- v_in$cost
  n <- v_in$n; m <- v_in$m
  if (n > m) {
    stop("trace_hk01: requires nrow <= ncol; got ", n, " x ", m, ".", call. = FALSE)
  }

  # --- Identify cost palette (must be all-equal or binary {0,1}) -----------
  finite_mask <- is.finite(cost_orig)
  if (!any(finite_mask)) {
    stop("`cost` has no finite entries.", call. = FALSE)
  }
  finite_vals <- unique(round(cost_orig[finite_mask], 9))
  if (length(finite_vals) > 2L) {
    stop("trace_hk01 requires all-equal or binary {0,1} finite costs; got ",
         length(finite_vals), " distinct values.", call. = FALSE)
  }
  is_binary <- length(finite_vals) == 2L && setequal(finite_vals, c(0, 1))

  # --- Build bipartite graph: 'preferred' edges = zero-cost (or 1 under max)
  if (is_binary) {
    preferred_cost <- if (maximize) 1 else 0
  } else {
    preferred_cost <- finite_vals[1]   # all-equal case: every allowed edge works
  }
  adj <- vector("list", n)
  for (i in seq_len(n)) {
    cols <- integer(0)
    for (j in seq_len(m)) {
      if (finite_mask[i, j] &&
          (length(finite_vals) == 1L ||
           abs(cost_orig[i, j] - preferred_cost) < 1e-9)) {
        cols <- c(cols, j)
      }
    }
    adj[[i]] <- cols
  }

  pairU <- integer(n)   # left -> right (0 = unmatched)
  pairV <- integer(m)   # right -> left (0 = unmatched)
  dist  <- integer(n)   # BFS layer per left node
  INF   <- .Machine$integer.max

  frames <- list()
  step <- 0L
  emit <- function(phase, description,
                   active_edges = list(), path = list()) {
    step <<- step + 1L
    frames[[length(frames) + 1L]] <<- make_frame(
      step         = step,
      phase        = phase,
      description  = description,
      matching     = pairU,
      dual_u       = as.numeric(dist),   # layer distances shown as "u" on left
      dual_v       = NULL,
      active_edges = active_edges,
      path         = path
    )
  }

  emit(
    "init",
    sprintf(
      paste0(
        "Build bipartite graph using only edges of preferred cost %g. ",
        "Each left vertex has %s. Goal: maximum-cardinality matching via Hopcroft-Karp."
      ),
      preferred_cost,
      paste0("[", paste(vapply(adj, length, integer(1)), collapse = ", "), "] eligible right partners")
    )
  )

  # ---- BFS: build alternating-layer graph from unmatched-left ------------
  bfs <- function() {
    q <- integer(0)
    for (u in seq_len(n)) {
      if (pairU[u] == 0L) {
        dist[u] <<- 0L
        q <- c(q, u)
      } else {
        dist[u] <<- INF
      }
    }
    reached_free <- FALSE
    while (length(q) > 0L) {
      u <- q[1L]; q <- q[-1L]
      for (v in adj[[u]]) {
        u_next <- pairV[v]
        if (u_next == 0L) {
          reached_free <- TRUE
        } else if (dist[u_next] == INF) {
          dist[u_next] <<- dist[u] + 1L
          q <- c(q, u_next)
        }
      }
    }
    reached_free
  }

  # ---- DFS: find augmenting path along layered DAG ----------------------
  augmenting_edges <- list()   # accumulated for the current dfs() call

  dfs <- function(u) {
    for (v in adj[[u]]) {
      u_next <- pairV[v]
      ok <- if (u_next == 0L) {
        TRUE
      } else if (dist[u_next] == dist[u] + 1L) {
        dfs(u_next)
      } else {
        FALSE
      }
      if (ok) {
        augmenting_edges[[length(augmenting_edges) + 1L]] <<- c(u, v)
        pairU[u] <<- v
        pairV[v] <<- u
        return(TRUE)
      }
    }
    dist[u] <<- INF
    return(FALSE)
  }

  # ---- Main HK loop -----------------------------------------------------
  phase <- 0L
  total_matched <- 0L
  while (bfs()) {
    phase <- phase + 1L
    free_left <- which(pairU == 0L)
    max_dist <- max(dist[dist != INF])
    emit(
      "bfs_done",
      sprintf(
        "Phase %d BFS: shortest augmenting path has length 2*%d + 1. Layer distances on the left: %s. %d free left vertices to try.",
        phase, max_dist,
        paste(dist, collapse = ", "),
        length(free_left)
      )
    )

    augs_this_phase <- 0L
    for (u in free_left) {
      if (pairU[u] == 0L) {
        augmenting_edges <- list()
        if (dfs(u)) {
          augs_this_phase <- augs_this_phase + 1L
          total_matched   <- total_matched + 1L
          emit(
            "augment",
            sprintf(
              "Augment from free left vertex %d. New matching size %d. Path edges flipped.",
              u, total_matched
            ),
            path = augmenting_edges
          )
        }
      }
    }

    if (augs_this_phase == 0L) {
      emit(
        "phase_end",
        sprintf("Phase %d: BFS suggested a path but no DFS found one. Stop.", phase)
      )
      break
    }
  }

  if (any(pairU == 0L)) {
    # The C++ solver throws in this case (binary {0,1} without perfect matching
    # in the zero-cost subgraph). For the teaching trace we still finish but
    # mark the assignment as partial - the parity test won't exercise this
    # because we constrain test cases to ones with perfect zero-matchings.
    msg <- sprintf("No more augmenting paths. %d of %d rows matched; %d remain unmatched.",
                   total_matched, n, n - total_matched)
    emit("final", msg)
  } else {
    total <- matching_total_cost(cost_orig, pairU)
    emit("final",
         sprintf("Maximum matching reached: all %d rows matched in %d phases. Total cost: %.6g.",
                 n, phase, total))
  }

  total <- matching_total_cost(cost_orig, pairU)

  list(
    meta = make_meta(
      algorithm   = "hk01",
      n_rows      = n,
      n_cols      = m,
      cost_matrix = cost_orig,
      maximize    = maximize,
      total_cost  = total,
      description = paste0(
        "Hopcroft-Karp (1973) maximum bipartite matching on 0/1 (or all-equal) ",
        "cost matrices. Builds a bipartite graph using the preferred-cost edges ",
        "(zero-cost for minimize, one-cost for maximize on binary inputs) and ",
        "alternates BFS-to-build-layers with DFS-to-pack-augmenting-paths. ",
        "Each phase strictly lengthens the shortest augmenting path, giving ",
        "O(E * sqrt(V)) overall - asymptotically faster than naive augmenting-path ",
        "matching on dense bipartite graphs."
      )
    ),
    frames = frames
  )
}

register_trace("hk01", trace_hk01)
