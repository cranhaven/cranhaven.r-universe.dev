# ==============================================================================
# Reference Klein's negative-cycle canceling for LAP with frame-by-frame trace
# ==============================================================================
# Mirrors src/solvers/solve_cycle_cancel.cpp. Solves LAP as a min-cost flow
# via Klein 1967: start with any feasible flow, then iteratively detect a
# negative-cost cycle in the residual graph and push flow around it. Each
# cycle cancellation strictly lowers the total cost; the algorithm terminates
# when no negative cycle remains, at which point the flow is provably optimal.
#
# Algorithmic outline:
#
#   1. Build the LAP-as-MCF graph (n + m + 2 nodes).
#   2. Find an initial feasible flow of n units source -> sink (we just
#      pick any source -> sink path via BFS and push one unit at a time,
#      n times). Cost is irrelevant at this stage - we only need feasibility.
#   3. Repeat:
#      a. Bellman-Ford on the residual graph looking for a negative cycle.
#         If none exists, the current flow is optimal - stop.
#      b. Walk the cycle, find its bottleneck residual capacity, push that
#         many units of flow around the cycle. Total cost drops.
#   4. Recover row -> col matching from saturated row -> col edges.
#
# This algorithm is pedagogically beautiful but computationally slow: cycle
# detection is O(VE) per iteration and the number of cancellations can be
# pseudo-polynomial. Production code uses csflow / push_relabel for speed;
# Klein survives in textbooks as the cleanest min-cost flow algorithm to
# understand.
# ==============================================================================

#' @keywords internal
#' @noRd
trace_cycle_cancel <- function(cost, maximize = FALSE, ...) {
  v_in <- validate_cost_input(cost, "trace_cycle_cancel")
  cost_orig <- v_in$cost
  n <- v_in$n; m <- v_in$m
  if (n > m) {
    stop("trace_cycle_cancel: requires nrow <= ncol; got ", n, " x ", m, ".", call. = FALSE)
  }

  mcf <- build_lap_mcf(cost_orig, maximize = maximize)
  g <- mcf$graph

  frames <- list()
  step <- 0L

  edge_to_rowcol <- mcf_edge_rowcol_lookup(mcf)

  current_cost <- function() {
    total <- 0
    for (i in seq_len(n)) {
      for (j in seq_len(m)) {
        ee <- mcf$row_col_edge[i, j]
        if (!is.na(ee)) {
          e <- g$edges[[ee]]
          if (e$orig_cap - e$cap > 0L) total <- total + cost_orig[i, j]
        }
      }
    }
    total
  }

  emit <- function(phase, description, active_edges = list(), path = list()) {
    step <<- step + 1L
    frames[[length(frames) + 1L]] <<- make_frame(
      step         = step,
      phase        = phase,
      description  = description,
      matching     = mcf_extract_matching(mcf),
      dual_u       = NULL,
      dual_v       = NULL,
      active_edges = active_edges,
      path         = path
    )
  }

  emit(
    "init",
    sprintf(
      "Built LAP-as-MCF graph: %d source-edges, %d row-col edges (finite cells), %d col-sink edges. No flow yet.",
      n, sum(!is.na(mcf$row_col_edge)), m
    )
  )

  # --- Phase 1: build initial feasible flow via BFS-augmentation ---------
  feasible_path_bfs <- function() {
    N <- g$n_nodes
    visited <- logical(N)
    prev_node <- integer(N)
    prev_edge <- integer(N)
    q <- mcf$source
    visited[mcf$source] <- TRUE
    while (length(q) > 0L) {
      u <- q[1L]; q <- q[-1L]
      if (u == mcf$sink) break
      for (e_idx in g$out_edges[[u]]) {
        e <- g$edges[[e_idx]]
        if (e$cap <= 0) next
        if (!visited[e$to]) {
          visited[e$to] <- TRUE
          prev_node[e$to] <- u
          prev_edge[e$to] <- e_idx
          q <- c(q, e$to)
        }
      }
    }
    if (!visited[mcf$sink]) return(integer(0))
    mcf_walk_back(prev_node, prev_edge, mcf$source, mcf$sink)
  }

  for (k in seq_len(n)) {
    pth <- feasible_path_bfs()
    if (length(pth) == 0L) {
      stop("trace_cycle_cancel: cannot establish feasible matching (graph disconnected).", call. = FALSE)
    }
    mcf_push_path(g, pth, 1L)
    path_rc <- list()
    for (e_idx in pth) {
      rc <- edge_to_rowcol(e_idx)
      if (!is.null(rc)) path_rc[[length(path_rc) + 1L]] <- rc
    }
    emit(
      "feasible_push",
      sprintf("Feasible flow build: pushed unit %d/%d along a BFS path. Total cost so far: %.6g.",
              k, n, current_cost()),
      path = path_rc
    )
  }

  emit(
    "feasible_done",
    sprintf("Initial feasible matching established (cost %.6g, not yet optimal). Begin cycle-canceling loop.",
            current_cost())
  )

  # --- Phase 2: cancel negative cycles -----------------------------------
  cycle_iter <- 0L
  max_cycles <- 2L * n * m + 100L
  repeat {
    cyc <- mcf_find_negative_cycle(g)
    if (is.null(cyc)) {
      emit("no_more_cycles",
           sprintf("Bellman-Ford finds no negative cycle. Flow is optimal. Cost: %.6g.", current_cost()))
      break
    }
    cycle_iter <- cycle_iter + 1L
    if (cycle_iter > max_cycles) {
      warning("trace_cycle_cancel: hit cycle iteration cap.")
      break
    }

    # Bottleneck capacity around the cycle
    delta <- mcf_path_bottleneck(g, cyc)
    cycle_cost <- sum(vapply(cyc, function(i) g$edges[[i]]$cost, numeric(1)))

    # Render the cycle in user coords
    path_rc <- list()
    for (e_idx in cyc) {
      rc <- edge_to_rowcol(e_idx)
      if (!is.null(rc)) path_rc[[length(path_rc) + 1L]] <- rc
    }

    emit(
      "found_cycle",
      sprintf(
        "Iteration %d: found negative cycle of cost %.4g (length %d edges, bottleneck cap = %d). Pushing flow around it.",
        cycle_iter, cycle_cost, length(cyc), delta
      ),
      path = path_rc
    )

    mcf_push_path(g, cyc, delta)

    emit(
      "cancelled",
      sprintf("Cycle cancelled: pushed %d units. New total cost: %.6g (improvement %.4g).",
              delta, current_cost(), abs(cycle_cost) * delta),
      path = path_rc
    )
  }

  matching_final <- mcf_extract_matching(mcf)
  total <- matching_total_cost(cost_orig, matching_final)
  emit("final",
       sprintf("Klein's algorithm complete in %d cancellations. Total cost: %.6g.",
               cycle_iter, total))

  list(
    meta = make_meta(
      algorithm   = "cycle_cancel",
      n_rows      = n,
      n_cols      = m,
      cost_matrix = cost_orig,
      maximize    = maximize,
      total_cost  = total,
      description = paste0(
        "Klein's negative-cycle canceling algorithm (1967). Start with any ",
        "feasible flow (here built by greedy BFS-augmentation, ignoring cost) ",
        "then iteratively detect a negative-cost cycle in the residual graph ",
        "via Bellman-Ford and push the maximum admissible flow around it. ",
        "Each cancellation strictly lowers total cost; termination is guaranteed ",
        "when no negative cycle remains, and the resulting flow is provably optimal."
      )
    ),
    frames = frames
  )
}

register_trace("cycle_cancel", trace_cycle_cancel)
