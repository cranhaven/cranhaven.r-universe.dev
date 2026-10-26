# ==============================================================================
# Cost-scaling push-relabel, frame by frame
# ==============================================================================
# src/flow/flow_push_relabel.cpp reaches the min-cost flow through a sequence of
# eps-optimal ones. Each phase divides eps, saturates the arcs the smaller eps
# no longer admits, and restores conservation with two local operations: a push
# moves excess along an arc whose reduced cost is negative, and a relabel lowers
# the price of a node that holds excess with no such arc out of it, by the least
# amount that gives it one.
#
# The frames show what those operations do to the prices and the matching. A
# frame's duals are the phase's prices in LAP form, so a matched pair sits at or
# just below zero reduced cost and the gap to zero is the eps the phase is at.
# ==============================================================================

#' @keywords internal
#' @noRd
trace_push_relabel <- function(cost, maximize = FALSE, ...) {
  v_in <- validate_cost_input(cost, "trace_push_relabel")
  cost_orig <- v_in$cost
  n <- v_in$n; m <- v_in$m
  if (n > m) {
    stop("trace_push_relabel: requires nrow <= ncol; got ", n, " x ", m, ".",
         call. = FALSE)
  }

  run <- lap_flow_trace_push_relabel(cost_orig, maximize = maximize)

  matching_row <- integer(n)
  frames <- list()
  step <- 0L

  emit <- function(phase, description, u, v, active_edges = list()) {
    step <<- step + 1L
    frames[[length(frames) + 1L]] <<- make_frame(
      step         = step,
      phase        = phase,
      description  = description,
      matching     = matching_row,
      dual_u       = u,
      dual_v       = v,
      active_edges = active_edges
    )
  }

  n_pairs <- sum(is.finite(cost_orig))
  emit(
    "init",
    sprintf(
      paste0(
        "Built the flow network: %d rows each holding one unit of excess, %d ",
        "pair arcs (only finite costs), %d column arcs into the sink. Prices ",
        "start at zero, where every flow is eps-optimal for eps = %.4g."
      ),
      n, n_pairs, m, run$eps_start
    ),
    numeric(n), numeric(m)
  )

  for (ph in run$phases) {
    matching_row <- ifelse(is.na(ph$match), 0L, ph$match)
    tight <- lapply(which(matching_row > 0L),
                    function(i) c(i, matching_row[i]))

    emit(
      "scale",
      sprintf(
        paste0(
          "eps = %.4g. The phase saturated %d arc(s) the smaller eps no longer ",
          "admitted, then cleared the excess with %d push(es) and %d ",
          "relabel(s). %d of %d rows are matched."
        ),
        ph$eps, ph$n_saturated, ph$n_pushes, ph$n_relabels,
        sum(matching_row > 0L), n
      ),
      ph$dual_u, ph$dual_v,
      active_edges = tight
    )
  }

  matching_row <- ifelse(is.na(run$match), 0L, run$match)
  total <- matching_total_cost(cost_orig, matching_row)

  last <- if (length(run$phases) > 0L) run$phases[[length(run$phases)]] else NULL
  emit(
    "final",
    sprintf(
      paste0(
        "eps fell below 1/(n+1), where two distinct integer totals cannot both ",
        "be eps-optimal, so the flow is optimal. Total cost: %.6g."
      ),
      total
    ),
    if (is.null(last)) numeric(n) else last$dual_u,
    if (is.null(last)) numeric(m) else last$dual_v,
    active_edges = lapply(which(matching_row > 0L),
                          function(i) c(i, matching_row[i]))
  )

  list(
    meta = make_meta(
      algorithm   = "push_relabel",
      n_rows      = n,
      n_cols      = m,
      cost_matrix = cost_orig,
      maximize    = maximize,
      total_cost  = total,
      description = paste0(
        "Goldberg-Tarjan cost-scaling push-relabel (1990). The assignment is a ",
        "min-cost flow, and the optimum is reached through a sequence of ",
        "eps-optimal flows: every residual arc stays at reduced cost >= -eps, ",
        "and each phase divides eps by 8. A push moves excess along an arc of ",
        "negative reduced cost; a relabel lowers the price of a node that holds ",
        "excess and has no such arc, by exactly the amount that gives it one ",
        "and no more. Below eps = 1/(n+1) an eps-optimal flow on integer costs ",
        "is optimal, which is what ends the scaling. The frames are read from ",
        "the compiled solver's own per-phase record."
      )
    ),
    frames = frames
  )
}

register_trace("push_relabel", trace_push_relabel)
