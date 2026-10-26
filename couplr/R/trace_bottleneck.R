# ==============================================================================
# Reference Bottleneck Assignment Problem solver with frame-by-frame trace
# ==============================================================================
# Mirrors src/solvers/solve_bottleneck.cpp: binary-search on the threshold,
# at each threshold check perfect-matching feasibility via Hopcroft-Karp on
# the subgraph of edges <= threshold. The production solver is reached via
# bottleneck_assignment(cost); the returned `total_cost` here is the
# bottleneck value (max edge in the matching), NOT the sum of edges.
#
# Algorithmic outline (minimize, square cost matrix):
#
#   --- Setup ---
#   1. Collect all unique finite edge costs and sort ascending.
#
#   --- Binary search ---
#   2. lo, hi = 0, |unique_costs|-1.
#      Repeat while lo <= hi:
#        mid = (lo + hi) / 2; t = unique_costs[mid].
#        Build the threshold subgraph (edges <= t for minimize, >= t for max).
#        Run Hopcroft-Karp to find a maximum matching.
#        If matching is perfect, record t as best so far and shrink hi.
#        Else widen lo.
#
#   --- Hopcroft-Karp (per threshold check) ---
#   Repeat:
#     a. BFS from all free L nodes through unmatched/matched edges, building
#        layer distances on L. Stop when a free R is reached.
#     b. For each free L node, DFS along the layered DAG; whenever a free R is
#        found, flip the alternating path (augment).
#   Until BFS finds no free R reachable.
#
# A frame is emitted at every pedagogically meaningful moment:
#   init / threshold_test / bfs_done / augment / threshold_pass /
#   threshold_fail / final.
# ==============================================================================

#' @keywords internal
#' @noRd
trace_bottleneck <- function(cost, maximize = FALSE, ...) {
  vc <- validate_square_cost(cost, "trace_bottleneck", maximize)
  cost <- vc$cost; n <- vc$n; m <- vc$m
  cost_orig <- cost
  finite_mask <- vc$finite_mask

  unique_costs <- sort(unique(cost[finite_mask]))
  if (maximize) unique_costs <- rev(unique_costs)

  matching_state <- integer(n)   # current "best so far" matching shown in frames
  pairL <- integer(n)            # HK left -> right (0 = unmatched)
  pairR <- integer(m)            # HK right -> left (0 = unmatched)

  frames <- list()
  step <- 0L

  emit <- function(phase, description, matching = matching_state,
                   active_edges = list(), path = list()) {
    step <<- step + 1L
    frames[[length(frames) + 1L]] <<- make_frame(
      step, phase, description,
      matching     = matching,
      active_edges = active_edges,
      path         = path
    )
  }

  emit(
    "init",
    sprintf(
      paste0(
        "Bottleneck assignment: %s the largest edge in the matching. There ",
        "are %d unique edge costs to bisect over. At each threshold t we ",
        "%s and ask Hopcroft-Karp whether a perfect matching exists on the ",
        "remaining graph."
      ),
      if (maximize) "maximise" else "minimise",
      length(unique_costs),
      if (maximize) "keep edges with cost >= t" else "keep edges with cost <= t"
    ),
    matching = integer(n)
  )

  # ---------------------------------------------------------------------------
  # Build adjacency at threshold t (returns list of int vectors, 1-based cols)
  # ---------------------------------------------------------------------------
  adj_at_threshold <- function(t) {
    keep <- finite_mask
    if (maximize) keep <- keep & (cost >= t)
    else          keep <- keep & (cost <= t)
    lapply(seq_len(n), function(i) which(keep[i, ]))
  }

  # ---------------------------------------------------------------------------
  # Hopcroft-Karp at a fixed threshold. Returns list(size = matching size,
  # pairL = the matching). Emits frames during BFS/DFS.
  # ---------------------------------------------------------------------------
  hk_max_matching <- function(adj, threshold) {
    pairL[] <<- 0L
    pairR[] <<- 0L
    matching_size <- 0L

    # Build a flat description of the candidate edges for active_edges display
    cand_edges <- list()
    for (i in seq_len(n)) for (j in adj[[i]]) cand_edges[[length(cand_edges) + 1L]] <- c(i, j)

    bfs_iter <- 0L
    repeat {
      bfs_iter <- bfs_iter + 1L
      # BFS: distance from each free L; -1 sentinel
      dist <- rep(-1L, n)
      queue <- integer(0)
      for (u in seq_len(n)) {
        if (pairL[u] == 0L) {
          dist[u] <- 0L
          queue <- c(queue, u)
        }
      }
      found_free_R <- FALSE
      while (length(queue) > 0L) {
        u <- queue[1]; queue <- queue[-1]
        for (v in adj[[u]]) {
          w <- pairR[v]
          if (w == 0L) {
            found_free_R <- TRUE
          } else if (dist[w] < 0L) {
            dist[w] <- dist[u] + 1L
            queue <- c(queue, w)
          }
        }
      }

      if (!found_free_R) break

      # DFS: try to augment from each free L along the layered DAG
      hk_dfs <- function(u) {
        for (v in adj[[u]]) {
          w <- pairR[v]
          if (w == 0L || (dist[w] == dist[u] + 1L && hk_dfs(w))) {
            pairL[u] <<- v
            pairR[v] <<- u
            return(TRUE)
          }
        }
        dist[u] <<- -1L
        FALSE
      }

      augmented_this_round <- 0L
      for (u in seq_len(n)) {
        if (pairL[u] == 0L && hk_dfs(u)) {
          augmented_this_round <- augmented_this_round + 1L
          matching_size <- matching_size + 1L
        }
      }

      emit(
        "bfs_done",
        sprintf(
          paste0(
            "Threshold %.4g, HK round %d: BFS reached a free right node; DFS ",
            "augmented %d path%s. Matching size now %d / %d."
          ),
          threshold, bfs_iter, augmented_this_round,
          if (augmented_this_round == 1L) "" else "s",
          matching_size, n
        ),
        matching = pairL,
        active_edges = cand_edges
      )

      if (augmented_this_round == 0L) break  # safety
    }

    list(size = matching_size, pairL = pairL)
  }

  # ---------------------------------------------------------------------------
  # First check: feasibility at the loosest threshold (max for min, min for max)
  # ---------------------------------------------------------------------------
  if (length(unique_costs) == 0L) {
    stop("trace_bottleneck: no candidate thresholds.", call. = FALSE)
  }

  hi_idx <- length(unique_costs)
  loose_threshold <- unique_costs[hi_idx]
  emit(
    "threshold_test",
    sprintf(
      "Feasibility check at the loosest threshold %.4g (all finite edges available).",
      loose_threshold
    ),
    matching = integer(n)
  )
  loose_adj <- adj_at_threshold(loose_threshold)
  loose <- hk_max_matching(loose_adj, loose_threshold)
  if (loose$size < n) {
    stop("trace_bottleneck: no perfect matching exists - problem infeasible.",
         call. = FALSE)
  }
  matching_state <- loose$pairL
  emit(
    "threshold_pass",
    sprintf(
      "Loosest threshold %.4g is feasible (size %d == n). Bisect downwards.",
      loose_threshold, n
    ),
    matching = matching_state
  )

  # ---------------------------------------------------------------------------
  # Binary search for the tightest feasible threshold
  # ---------------------------------------------------------------------------
  lo <- 1L
  hi <- hi_idx
  best_idx <- hi_idx
  best_matching <- matching_state

  while (lo <= hi) {
    mid <- lo + (hi - lo) %/% 2L
    t <- unique_costs[mid]
    emit(
      "threshold_test",
      sprintf(
        "Bisect: lo=%d, hi=%d, mid=%d. Test threshold %.4g (rank %d of %d).",
        lo, hi, mid, t, mid, length(unique_costs)
      ),
      matching = best_matching
    )

    adj <- adj_at_threshold(t)
    res <- hk_max_matching(adj, t)

    if (res$size >= n) {
      best_idx <- mid
      best_matching <- res$pairL
      matching_state <- best_matching
      emit(
        "threshold_pass",
        sprintf(
          "Threshold %.4g admits a perfect matching. Tighten further: hi <- mid - 1.",
          t
        ),
        matching = best_matching
      )
      hi <- mid - 1L
    } else {
      emit(
        "threshold_fail",
        sprintf(
          "Threshold %.4g is too tight - only %d of %d matched. Loosen: lo <- mid + 1.",
          t, res$size, n
        ),
        matching = matching_state
      )
      lo <- mid + 1L
    }
  }

  bottleneck_value <- unique_costs[best_idx]
  matching_state <- best_matching

  emit(
    "final",
    sprintf(
      paste0(
        "Optimal bottleneck threshold: %.4g (rank %d of %d). The matching uses ",
        "no edge worse than this; tightening any further would break ",
        "feasibility. Note: total_cost reports the BOTTLENECK value, not the ",
        "sum of edges."
      ),
      bottleneck_value, best_idx, length(unique_costs)
    ),
    matching = matching_state,
    path = lapply(seq_len(n), function(i) c(i, matching_state[i]))
  )

  list(
    meta = make_meta(
      "bottleneck", n, m, cost_orig, maximize, as.numeric(bottleneck_value),
      description = paste0(
        "Bottleneck assignment. Different objective from standard LAP: instead ",
        "of minimising the SUM of matched-edge costs, minimise the MAX (or, ",
        "with maximize = TRUE, maximise the min). Solved by bisecting on the ",
        "edge-cost threshold: at each candidate t, keep only edges with ",
        "cost <= t (>= t for maximize) and check perfect-matching feasibility ",
        "via Hopcroft-Karp."
      )
    ),
    frames = frames
  )
}

register_trace("bottleneck", trace_bottleneck)
