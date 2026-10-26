# ==============================================================================
# Reference epsilon-scaled auction (Bertsekas, "C reference" formulation) with
# frame-by-frame trace
# ==============================================================================
# Mirrors src/solvers/solve_auction.cpp:solve_auction_scaled_params (the
# implementation that the named-schedule wrapper solve_auction_scaled forwards
# to). The production solver lives in C++ and is reached via
# assignment(cost, method = "auction_scaled"); this R version exists purely to
# emit a step-by-step state trace for lap_animate().
#
# Reference: D. P. Bertsekas, "The auction algorithm for assignment and other
# network flow problems: a tutorial," Interfaces 20 (1990) 133-149. The
# epsilon-scaling outer loop is from Bertsekas & Eckstein (1988).
#
# Algorithmic outline (square cost matrix):
#
#   --- Setup ---
#   * Prices p[j] start at 0 (and are kept across phases).
#   * Initial epsilon eps_init = span of the finite costs * initial_epsilon_factor.
#   * Final epsilon eps_final = 1/100 of the median, over rows, of the mean gap
#     between a row's four smallest distinct costs, and at least four ulps of
#     the largest cost magnitude, unless overridden.
#   * Use the "minimize cost - price" formulation that the production code
#     uses: person i prefers the column with the smallest reduced cost
#     rc[i,j] = c[i,j] - p[j]. When person i wins column j*, the price p[j*]
#     DECREASES by (gamma + eps), so the next bidder sees a higher rc on j*
#     and is steered toward something else.
#
#   --- Outer phase loop (until eps drops to eps_final) ---
#   1. Divide eps by alpha (clamp to eps_final from below). Discard the
#      current matching; prices persist.
#   2. Push all persons onto the unmatched stack and run a forward auction
#      at this eps until no one is unmatched.
#   3. After the phase, prices reflect an eps-CS equilibrium at the current
#      (looser) eps. The next phase tightens.
#
#   --- Pedagogical core ---
#   Watching prices stabilise across phases is the point. Early phases
#   (eps large) take big strides; late phases (eps small) make tiny tweaks.
#
# A frame is emitted at every pedagogically meaningful moment:
#   init / phase_start / bid / phase_end / final.
# ==============================================================================

#' @keywords internal
#' @noRd
trace_auction_scaled <- function(cost,
                                  maximize = FALSE,
                                  alpha = 7,
                                  initial_epsilon_factor = 1.0,
                                  final_epsilon = NULL,
                                  ...) {
  vc <- validate_square_cost(cost, "trace_auction_scaled", maximize,
                             solver_hint = "auction_scaled")
  cost <- vc$cost; n <- vc$n; m <- vc$m
  if (!is.numeric(alpha) || length(alpha) != 1L || alpha <= 1) {
    stop("`alpha` must be a single number > 1.", call. = FALSE)
  }

  cost_orig <- cost
  cost_signed <- vc$cost_signed
  finite_mask <- vc$finite_mask
  for (i in seq_len(n)) {
    if (!any(finite_mask[i, ])) {
      stop("Row ", i, " has no finite (allowed) entries.", call. = FALSE)
    }
  }

  # Epsilon schedule (matches epsilon_schedule() in solve_auction.cpp)
  max_abs_cost <- vc$scale
  sched <- auction_epsilon_schedule(cost_signed, finite_mask, initial_epsilon_factor)
  eps_init <- sched$start
  eps_final <- if (is.null(final_epsilon)) sched$terminal
               else as.numeric(final_epsilon)
  if (!is.finite(eps_final) || eps_final <= 0) {
    stop("`final_epsilon` must be a positive finite number.", call. = FALSE)
  }

  # Price overflow guard (matches cpp's price_bound).
  price_bound <- max(1e12, max_abs_cost * n * 1000)

  # Prices persist across phases.
  p <- numeric(m)
  assign_object <- integer(n)   # person -> object (0 = unassigned)
  assign_person <- integer(m)   # object -> person (0 = unassigned)

  # eps state shown in dual labels via a side channel: the renderer reads
  # dual_u (NULL here) and dual_v (we put prices there). Phase + eps are
  # carried in the per-frame description.
  frames <- list()
  step <- 0L

  emit <- function(phase_lbl, description,
                   active_edges = list(), path = list()) {
    step <<- step + 1L
    frames[[length(frames) + 1L]] <<- make_frame(
      step, phase_lbl, description,
      matching     = assign_object,
      dual_v       = p,
      active_edges = active_edges,
      path         = path
    )
  }

  # Estimated number of phases for the init-frame narrative
  est_phases <- if (eps_init <= eps_final) 1L else
                ceiling(log(eps_init / eps_final) / log(alpha))

  emit(
    "init",
    sprintf(
      paste0(
        "Epsilon-scaled auction. Start with a loose eps = %.4g, divide by alpha = %g ",
        "each phase until eps reaches eps_final = %.4g (~%d phases). Prices persist ",
        "across phases - early phases give cheap warm-start prices for later, ",
        "tighter rounds. Bidders prefer the column with smallest reduced cost ",
        "c[i,j] - p[j] (cost-price minimize formulation, matching the production ",
        "C++ implementation)."
      ),
      eps_init, alpha, eps_final, est_phases
    )
  )

  max_iter <- as.numeric(n) * m * 100
  eps <- eps_init
  phase_idx <- 0L

  # ---------------------------------------------------------------------------
  # Outer epsilon-scaling loop
  # ---------------------------------------------------------------------------
  repeat {
    phase_idx <- phase_idx + 1L
    eps <- eps / alpha
    if (eps < eps_final) eps <- eps_final

    # Discard matching; prices kept.
    assign_object[] <- 0L
    assign_person[] <- 0L

    emit(
      "phase_start",
      sprintf(
        paste0(
          "Phase %d: eps = %.4g. Matching discarded, prices retained from the ",
          "previous phase. Every person starts unassigned and will rebid at the ",
          "current (tighter) eps."
        ),
        phase_idx, eps
      )
    )

    unmatched <- seq_len(n)        # stack of unmatched persons (1-based)
    bid_count <- 0L

    # -----------------------------------------------------------------------
    # Inner forward-auction loop at this eps
    # -----------------------------------------------------------------------
    while (length(unmatched) > 0L) {
      bid_count <- bid_count + 1L
      if (bid_count > max_iter) {
        stop(
          sprintf(
            "trace_auction_scaled: iteration guard exceeded at phase %d.",
            phase_idx
          ),
          call. = FALSE
        )
      }

      # Pop the last person (matches cpp's vector::pop_back)
      i <- unmatched[length(unmatched)]
      unmatched <- unmatched[-length(unmatched)]

      # Reduced cost for person i across all columns
      rc <- cost_signed[i, ] - p
      rc[!finite_mask[i, ]] <- Inf

      j_star <- which.min(rc)
      best_rc <- rc[j_star]
      if (!is.finite(best_rc)) {
        stop("trace_auction_scaled: person has no valid neighbors.", call. = FALSE)
      }

      rc_tmp <- rc
      rc_tmp[j_star] <- Inf
      j_second <- which.min(rc_tmp)
      second_rc <- rc_tmp[j_second]

      # gamma = second_rc - best_rc, clamped (matches cpp)
      if (!is.finite(second_rc)) {
        gamma <- eps
        second_label <- "(none)"
      } else {
        gamma <- second_rc - best_rc
        if (gamma > price_bound) gamma <- price_bound
        if (gamma < 0) gamma <- 0
        second_label <- sprintf("%d (rc %.4g)", j_second, second_rc)
      }

      # Price update: DECREASE by (gamma + eps), clamped to -price_bound.
      old_price <- p[j_star]
      new_price <- old_price - (gamma + eps)
      if (new_price < -price_bound) new_price <- -price_bound
      p[j_star] <- new_price

      # Apply the assignment
      old_holder <- assign_person[j_star]
      assign_person[j_star] <- i
      assign_object[i] <- j_star
      if (old_holder > 0L && old_holder != i) {
        assign_object[old_holder] <- 0L
        unmatched <- c(unmatched, old_holder)
      }

      displaced_text <- if (old_holder > 0L && old_holder != i)
        sprintf(" Displaces person %d (back to unmatched).", old_holder)
      else
        ""

      emit(
        "bid",
        sprintf(
          paste0(
            "Phase %d (eps %.4g) bid %d: person %d takes column %d with ",
            "smallest reduced cost %.4g (second-best column %s); gamma = %.4g. ",
            "Price[%d] drops from %.4g to %.4g.%s"
          ),
          phase_idx, eps, bid_count,
          i, j_star, best_rc, second_label, gamma,
          j_star, old_price, new_price,
          displaced_text
        ),
        active_edges = if (is.finite(second_rc))
          list(c(i, j_star), c(i, j_second))
        else
          list(c(i, j_star)),
        path = list(c(i, j_star))
      )
    }

    # Compute partial cost using ORIGINAL costs
    partial <- sum(cost_orig[cbind(seq_len(n), assign_object)])

    emit(
      "phase_end",
      sprintf(
        paste0(
          "Phase %d done after %d bids. Matching complete at eps = %.4g; ",
          "total cost on the current matching: %.6g."
        ),
        phase_idx, bid_count, eps, partial
      )
    )

    if (eps <= eps_final) break
  }

  total <- sum(cost_orig[cbind(seq_len(n), assign_object)])

  emit(
    "final",
    sprintf(
      "Optimal matching after %d phases. Total cost: %.6g (eps floor %.4g).",
      phase_idx, total, eps_final
    )
  )

  list(
    meta = make_meta(
      "auction_scaled", n, m, cost_orig, maximize, total,
      description = paste0(
        "Bertsekas auction with epsilon-scaling. The outer loop reduces eps by ",
        "a factor alpha each phase (default alpha = 7); within each phase a ",
        "forward auction in cost-price minimize formulation drives the system ",
        "to an eps-complementary-slackness equilibrium. Prices persist across ",
        "phases; the matching is discarded each phase. Coarse early phases ",
        "give cheap warm-start prices for the tight final phase."
      )
    ),
    frames = frames
  )
}

register_trace("auction_scaled", trace_auction_scaled)

# Start and terminal epsilon read off the costs, as epsilon_schedule() in
# src/solvers/solve_auction.cpp computes them for a square problem.
auction_epsilon_schedule <- function(cost, finite_mask, initial_epsilon_factor) {
  vals <- cost[finite_mask]
  max_abs <- if (length(vals)) max(abs(vals)) else 0
  span <- if (length(vals) && max(vals) > min(vals)) max(vals) - min(vals) else 1
  gaps <- vapply(seq_len(nrow(cost)), function(i) {
    v <- utils::head(sort(unique(cost[i, finite_mask[i, ]])), 4L)
    if (length(v) < 2L) NA_real_ else (v[length(v)] - v[1L]) / (length(v) - 1L)
  }, numeric(1))
  gaps <- gaps[!is.na(gaps)]
  row_gap <- if (length(gaps)) sort(gaps)[length(gaps) %/% 2L + 1L] else span
  ulp <- if (max_abs > 0) 2^(floor(log2(max_abs)) - 52) else 2^-1074
  terminal <- max(row_gap / 100, 4 * ulp)
  list(start = max(span * initial_epsilon_factor, terminal), terminal = terminal)
}
