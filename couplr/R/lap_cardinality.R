# ==============================================================================
# Cardinality
# ==============================================================================
# How many pairs an assignment is asked to produce, and what it costs to leave a
# unit out. All three modes reduce to one mechanism: append dummy columns that
# every row may take, priced so that the solver's own optimum is the requested
# objective. A row matched to a dummy column is a row left unmatched.
#
#   complete  no dummy columns; every row must take a real column.
#   maximum   one dummy per row, priced so that a real pair is always worth
#             taking. The optimum is the largest number of pairs, and among
#             those the cheapest.
#   fixed     exactly (n - n_matches) dummies, priced so that all of them are
#             taken. The optimum is the cheapest matching of exactly
#             n_matches pairs.
#
# `unmatched_penalty` replaces the sentinel price under "maximum" with a real
# one, which turns the lexicographic objective into a single objective: a pair
# costing more than the penalty is worth dropping.
# ==============================================================================

# Largest total cost a sentinel-padded solve may reach and still be ordered
# exactly by a double. Above it, adding a sentinel to a running total can be
# rounded away and the cardinality-before-cost ordering stops holding.
PAD_PRECISION_LIMIT <- 2^53

# Weights that make a sum of tiers read in priority order. `counts` gives the
# largest number of units each tier can contribute, ordered from the tier that
# yields first to the tier that yields last, and `base_magnitude` bounds the
# total of the unweighted quantity all of them sit above.
#
# A tier's weight has to exceed everything beneath it can accumulate, so that one
# unit of it is never worth trading for any amount of the lower tiers. Building
# the weights upward from `base_magnitude` gives each one that property in turn,
# and the running total is the largest objective the weighted problem reaches.
#
# Returns NULL when that total passes the range a double orders exactly, because
# the ordering the weights encode stops holding there.
.lex_tier_weights <- function(counts, base_magnitude) {
  counts <- as.numeric(counts)
  if (!length(counts)) {
    return(numeric(0))
  }
  if (anyNA(counts) || any(!is.finite(counts)) || any(counts < 0) ||
      length(base_magnitude) != 1L || !is.finite(base_magnitude) ||
      base_magnitude < 0) {
    return(NULL)
  }

  weights <- numeric(length(counts))
  reach <- base_magnitude
  for (t in seq_along(counts)) {
    weights[t] <- reach + 1
    reach <- reach + counts[t] * weights[t]
  }

  if (!all(is.finite(weights)) || !is.finite(reach) ||
      reach > PAD_PRECISION_LIMIT) {
    return(NULL)
  }
  weights
}

# Magnitude a per-unit sentinel has to exceed for a minimum-cost solve to order
# matchings by cardinality before cost. A matching of k pairs has real cost
# within [k * lo, k * hi], so the cost a rearrangement of the real edges can
# recover sits under (k + 1) * (|lo| + |hi|), and a sentinel above that can never
# be traded away for such a saving: dropping one sentinel edge always beats any
# rearrangement of the rest.
#
# This is the two-tier case of .lex_tier_weights(): cardinality over cost.
.cardinality_sentinel <- function(real_costs, k) {
  if (!length(real_costs)) {
    return(NULL)
  }
  span <- abs(max(real_costs)) + abs(min(real_costs))
  weights <- .lex_tier_weights(counts = k, base_magnitude = (k + 1) * span)
  if (is.null(weights)) {
    return(NULL)
  }
  weights[[1L]]
}

.validate_cardinality_args <- function(cardinality, n_matches,
                                       unmatched_penalty, n, m) {
  cardinality <- match.arg(cardinality, c("complete", "maximum", "fixed"))

  if (cardinality != "fixed" && !is.null(n_matches)) {
    stop("`n_matches` applies only to cardinality = \"fixed\".", call. = FALSE)
  }
  if (cardinality == "complete" && !is.null(unmatched_penalty)) {
    stop("`unmatched_penalty` has no meaning when cardinality = \"complete\", ",
         "where no unit may go unmatched.", call. = FALSE)
  }
  if (cardinality == "fixed") {
    if (is.null(n_matches)) {
      stop("cardinality = \"fixed\" requires `n_matches`.", call. = FALSE)
    }
    if (!is.numeric(n_matches) || length(n_matches) != 1L || is.na(n_matches) ||
        n_matches != as.integer(n_matches)) {
      stop("`n_matches` must be a single whole number.", call. = FALSE)
    }
    n_matches <- as.integer(n_matches)
    if (n_matches < 0L || n_matches > min(n, m)) {
      stop("`n_matches` must be between 0 and ", min(n, m),
           " for a ", n, " by ", m, " problem.", call. = FALSE)
    }
    if (!is.null(unmatched_penalty)) {
      stop("`unmatched_penalty` has no effect when the number of pairs is ",
           "fixed; it would add a constant to every feasible objective.",
           call. = FALSE)
    }
  }
  if (!is.null(unmatched_penalty)) {
    if (!is.numeric(unmatched_penalty) || length(unmatched_penalty) != 1L ||
        is.na(unmatched_penalty) || !is.finite(unmatched_penalty)) {
      stop("`unmatched_penalty` must be a single finite number.", call. = FALSE)
    }
  }
  list(cardinality = cardinality, n_matches = n_matches,
       unmatched_penalty = unmatched_penalty)
}

# Build the matrix the solver actually sees, plus everything needed to read its
# answer back in the caller's terms.
.cardinality_reduction <- function(cost, cardinality, n_matches,
                                   unmatched_penalty, maximize) {
  n <- nrow(cost)
  m <- ncol(cost)

  if (cardinality == "complete") {
    return(list(work = cost, n_dummy = 0L, n_required = min(n, m)))
  }

  real <- cost[is.finite(cost) & abs(cost) < BIG_COST]
  if (!length(real)) {
    stop("Cost matrix has no admissible entries.", call. = FALSE)
  }

  n_dummy <- if (cardinality == "maximum") n else n - n_matches

  if (!is.null(unmatched_penalty)) {
    price <- unmatched_penalty
  } else {
    sentinel <- .cardinality_sentinel(real, min(n, m))
    if (is.null(sentinel)) {
      stop("The cost range is too wide to order matchings by cardinality ",
           "exactly in double precision. Rescale the costs, or supply ",
           "`unmatched_penalty` to state the trade-off directly.", call. = FALSE)
    }
    # Under "maximum" a real pair must always beat a dummy; under "fixed" every
    # dummy must be taken, so the preference is reversed. `maximize` flips both.
    prefer_real <- cardinality == "maximum"
    price <- if (prefer_real == !maximize) sentinel else -sentinel
  }

  work <- cbind(cost, matrix(price, nrow = n, ncol = n_dummy))
  # Under "maximum" the request is every row, and falling short of it is what
  # "partial" reports. Under "fixed" the request is exactly n_matches.
  list(work = work, n_dummy = as.integer(n_dummy),
       n_required = if (cardinality == "fixed") n_matches else n,
       dummy_price = price)
}

# Map a solve over the padded matrix back to the caller's problem: dummy columns
# become unmatched rows, and the objective is recomputed over real pairs only,
# so a sentinel never leaks into a reported cost.
.cardinality_restore <- function(match_padded, cost, n_dummy) {
  m <- ncol(cost)
  match_out <- as.integer(match_padded)
  match_out[match_out > m] <- 0L
  matched <- which(match_out > 0L)
  total <- if (length(matched)) {
    sum(cost[cbind(matched, match_out[matched])])
  } else {
    0
  }
  list(match = match_out, total_cost = total, n_matched = length(matched),
       unmatched = which(match_out == 0L))
}
