# ==============================================================================
# Reference forward auction (Bertsekas 1988) with frame-by-frame trace
# ==============================================================================
# Teaching implementation. The production solver lives in C++ and is reached
# via assignment(cost, method = "auction"); this R version exists purely to
# emit a step-by-step state trace for lap_animate().
#
# Algorithmic outline (minimization, square cost matrix):
#
#   1. Treat as a maximization of v[i,j] := -c[i,j] (or +c if maximize=TRUE).
#      Each object j carries a price p[j], initially 0. A person i's "net
#      profit" from object j is a[i,j] := v[i,j] - p[j].
#   2. While any person is unassigned:
#      a. Pick an unassigned person i.
#      b. Identify their best object j* = argmax_j a[i,j] (profit v_best)
#         and second-best (profit w_best).
#      c. Person i bids on j*, raising its price by
#                p[j*] += (v_best - w_best) + eps.
#         The margin (v_best - w_best) is the smallest amount that, at the new
#         price, leaves i indifferent between j* and their next favourite; the
#         +eps ensures strict progress.
#      d. If j* was held by some person k, k is displaced (becomes unassigned).
#         Then i is assigned to j*.
#   3. The fixed eps is chosen small enough that an eps-equilibrium implies
#      an exactly optimal assignment. For integer costs eps < 1/n is sufficient;
#      for real-valued costs we pick eps proportional to the cost scale.
# ==============================================================================

#' @keywords internal
#' @noRd
trace_auction <- function(cost, maximize = FALSE,
                          sweep = c("lifo", "gauss_seidel"), ...) {
  sweep <- match.arg(sweep)
  vc <- validate_square_cost(cost, sprintf("trace_auction (sweep=%s)", sweep),
                             maximize, solver_hint = "auction")
  cost <- vc$cost; n <- vc$n; m <- vc$m

  # --- Internal max-problem view ---------------------------------------------
  # vmat[i,j] is the "value" person i gets from object j. Auction maximises total
  # value, equivalently minimising total cost, so the values are the negated
  # minimisation view (cost_signed). Forbidden entries become very *negative*
  # (using the same big-M as prepare_cost_work) so no one ever bids on them.
  vmat <- -vc$cost_signed
  finite_mask <- vc$finite_mask
  scale <- vc$scale
  big <- vc$big_m
  vmat[!finite_mask] <- -big

  # --- Pick eps ---------------------------------------------------------------
  # Integer costs => eps < 1/n suffices for exact optimality.
  # Real costs    => use a small fraction of the cost scale.
  is_integer_cost <- all(abs(cost[finite_mask] - round(cost[finite_mask])) < .Machine$double.eps^0.5)
  eps <- if (is_integer_cost) 1 / (n + 1) else max(scale, 1) / max(n * n * 100, 1)

  # --- State ----------------------------------------------------------------
  p <- numeric(m)                       # object prices
  assign_object <- integer(n)           # person -> object (0 = unassigned)
  assign_person <- integer(m)           # object -> person (0 = unassigned)

  frames <- list()
  step <- 0L

  emit <- function(phase, description,
                   active_edges = list(), path = list()) {
    step <<- step + 1L
    frames[[length(frames) + 1L]] <<- make_frame(
      step         = step,
      phase        = phase,
      description  = description,
      matching     = assign_object,
      dual_u       = NULL,      # auction has no row potentials
      dual_v       = p,         # column prices shown as "v=" in the widget
      active_edges = active_edges,
      path         = path
    )
  }

  sweep_label <- switch(sweep,
    lifo          = "LIFO queue of unassigned/displaced persons (Bertsekas's original).",
    gauss_seidel  = "Gauss-Seidel sweep through person indices 1..n in order, restarting after a full pass."
  )

  emit(
    "init",
    sprintf(
      paste0(
        "Initialise: every object has price 0 and no person is assigned. ",
        "Bidding increment eps = %.4g. Selection rule: %s ",
        "Persons (rows) will repeatedly bid on objects (cols); ",
        "winning bids raise the object's price, displacing the previous holder if any."
      ),
      eps, sweep_label
    )
  )

  # --- Main loop ------------------------------------------------------------
  iter <- 0L
  max_iter <- 200L * n * n            # safety cap

  # Selection state. lifo: an explicit stack of unassigned persons, with
  # newly-displaced persons pushed on top. gauss_seidel: a cursor that walks
  # 1..n cyclically, skipping persons that are already assigned.
  if (sweep == "lifo") {
    queue <- rev(seq_len(n))           # stack: pop from end gives 1, 2, 3, ...
    pick_next <- function() {
      while (length(queue) > 0L) {
        i <- queue[length(queue)]
        queue <<- queue[-length(queue)]
        if (assign_object[i] == 0L) return(i)
      }
      NA_integer_
    }
    push_back <- function(i) { queue <<- c(queue, i) }
  } else {
    cursor <- 0L
    pick_next <- function() {
      for (k in seq_len(n)) {
        cursor <<- (cursor %% n) + 1L
        if (assign_object[cursor] == 0L) return(cursor)
      }
      NA_integer_
    }
    push_back <- function(i) invisible(NULL)   # displaced persons are caught on the next sweep
  }

  while (any(assign_object == 0L) && iter < max_iter) {
    iter <- iter + 1L

    i <- pick_next()
    if (is.na(i)) break

    # Net profit of each object for person i
    a <- vmat[i, ] - p

    j_star <- which.max(a)
    v_best <- a[j_star]
    a_tmp <- a
    a_tmp[j_star] <- -Inf
    j_second <- which.max(a_tmp)
    w_best <- a_tmp[j_second]
    if (!is.finite(w_best)) w_best <- v_best   # degenerate case (only one valid object)

    bid <- (v_best - w_best) + eps

    emit(
      "select_person",
      sprintf(
        "Person %d is unassigned. They evaluate net profit a[%d,j] = v[%d,j] - p[j] across every object.",
        i, i, i
      ),
      active_edges = lapply(seq_len(m), function(jj) c(i, jj))
    )

    emit(
      "bid",
      sprintf(
        paste0(
          "Best object for person %d is %d (profit %.4g); second-best is object %d (profit %.4g). ",
          "Person %d bids on object %d, raising its price by (best - second + eps) = %.4g."
        ),
        i, j_star, v_best, j_second, w_best, i, j_star, bid
      ),
      active_edges = list(c(i, j_star), c(i, j_second))
    )

    # Apply the bid
    p[j_star] <- p[j_star] + bid

    old_holder <- assign_person[j_star]
    if (old_holder > 0L && old_holder != i) {
      assign_object[old_holder] <- 0L
      assign_person[j_star] <- 0L
      push_back(old_holder)
      emit(
        "displace",
        sprintf(
          "Object %d was held by person %d - they are displaced and become unassigned. Price of object %d is now %.4g.",
          j_star, old_holder, j_star, p[j_star]
        )
      )
    }

    assign_object[i] <- j_star
    assign_person[j_star] <- i

    emit(
      "assign",
      sprintf(
        "Person %d is now assigned to object %d (current price %.4g).",
        i, j_star, p[j_star]
      ),
      path = list(c(i, j_star))
    )
  }

  if (iter >= max_iter) {
    warning(
      "Auction inner loop hit iteration cap (",
      max_iter,
      ") - eps may be too small or the cost matrix is degenerate."
    )
  }

  total <- matching_total_cost(cost, assign_object)

  emit(
    "final",
    sprintf(
      "All %d persons assigned after %d bids. Total cost: %.6g.",
      n, iter, total
    )
  )

  algorithm_name <- if (sweep == "gauss_seidel") "auction_gs" else "auction"
  description <- if (sweep == "gauss_seidel") {
    paste0(
      "Bertsekas forward auction (1988), Gauss-Seidel sweep variant. ",
      "Same bid / displace / assign mechanics as the standard auction, but ",
      "unassigned persons are visited in a fixed cyclic order (1, 2, ..., n, 1, 2, ...) ",
      "rather than from a LIFO queue. Each bid immediately updates the object's price, ",
      "so the next person in the sweep sees the newly-raised price. ",
      "Often converges in fewer bids on spatially-structured problems where ",
      "neighbouring persons compete for overlapping objects."
    )
  } else {
    paste0(
      "Bertsekas forward auction (1988). Persons (rows) bid on objects (cols); ",
      "each object carries a price, and a winning bid raises the price by the gap to the ",
      "second-best alternative plus eps. The previous holder of the contested object is displaced ",
      "and re-bids later. Iteration ends when every person is assigned."
    )
  }

  list(
    meta = make_meta(
      algorithm   = algorithm_name,
      n_rows      = n,
      n_cols      = m,
      cost_matrix = cost,
      maximize    = maximize,
      total_cost  = total,
      description = description
    ),
    frames = frames
  )
}

#' @keywords internal
#' @noRd
trace_auction_gs <- function(cost, maximize = FALSE, ...) {
  trace_auction(cost, maximize = maximize, sweep = "gauss_seidel", ...)
}

register_trace("auction",    trace_auction)
register_trace("auction_gs", trace_auction_gs)
