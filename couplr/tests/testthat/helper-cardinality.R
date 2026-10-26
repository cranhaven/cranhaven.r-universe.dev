# Fixtures for the balance-constrained cardinality match. Everything here is
# built without the solver, so a test that compares the two is comparing an
# answer against an independent statement of the problem.

# A tiny instance: nested categories from the prefixes of g1 and g2, two
# covariates for the moment constraints, and a distance matrix with a few
# forbidden cells.
card_instance <- function(seed, max_units = 4L, max_levels = 2L,
                          forbid = 0.1) {
  set.seed(seed)
  n_left <- sample.int(max_units, 1L)
  n_right <- sample.int(max_units, 1L)
  n_levels <- sample.int(max_levels, 1L)

  side <- function(n) {
    data.frame(g1 = sample.int(2L, n, replace = TRUE),
               g2 = sample.int(2L, n, replace = TRUE),
               x = round(stats::rnorm(n), 2),
               y = round(stats::rnorm(n), 2))
  }
  left <- side(n_left)
  right <- side(n_right)

  cost <- matrix(round(stats::runif(n_left * n_right, 0, 3), 3),
                 n_left, n_right)
  cost[matrix(stats::runif(n_left * n_right) < forbid, n_left, n_right)] <- Inf

  list(left = left, right = right, cost = cost,
       refined = c("g1", "g2")[seq_len(n_levels)],
       exact = sample(0:n_levels, 1L),
       n_left = n_left, n_right = n_right, n_levels = n_levels, seed = seed)
}

card_build <- function(inst) {
  hier <- .refined_hierarchy(inst$left, inst$right, inst$refined,
                             exact = inst$exact)
  built <- .balance_flow_problem(inst$cost, hier)
  list(problem = built$problem, index = built$index, hier = hier)
}

card_coefs <- function(inst, moments = NULL, max_std_diff = NULL, vars = NULL) {
  specs <- .moment_specs(moments = moments, max_std_diff = max_std_diff,
                         vars = vars, left = inst$left, right = inst$right)
  list(specs = specs,
       coefs = lapply(specs, .moment_coefficients, left = inst$left,
                      right = inst$right))
}

# Every partial injection from left into right over the admissible cells.
card_all_matchings <- function(cost) {
  n <- nrow(cost)
  m <- ncol(cost)
  allowed <- .is_valid_cost(cost)
  out <- vector("list", 0L)
  walk <- function(i, used, cur) {
    if (i > n) {
      out[[length(out) + 1L]] <<- cur
      return(invisible(NULL))
    }
    walk(i + 1L, used, cur)
    for (j in seq_len(m)) {
      if (!used[[j]] && allowed[i, j]) {
        nxt <- cur
        nxt[[i]] <- j
        seen <- used
        seen[[j]] <- TRUE
        walk(i + 1L, seen, nxt)
      }
    }
    invisible(NULL)
  }
  walk(1L, rep(FALSE, m), integer(n))
  out
}

# The best matched set by enumeration: admissible for the design, satisfying
# every moment row, and cheapest under the objective the network encodes.
card_brute <- function(inst, built, coefs = list()) {
  hier <- built$hier
  index <- built$index
  best <- Inf
  best_pairs <- 0L
  best_imbalance <- rep(0, hier$n_levels)
  for (matching in card_all_matchings(inst$cost)) {
    flow <- .balance_flow_encode(matching, index)
    if (is.null(flow)) next
    left_idx <- which(matching > 0L)
    right_idx <- matching[left_idx]
    ok <- TRUE
    for (cf in coefs) {
      if (.moment_violation(cf, left_idx, right_idx) > 1e-9) {
        ok <- FALSE
        break
      }
    }
    if (!ok) next
    objective <- sum(built$problem$arcs$cost * flow)
    if (objective < best - 1e-12) {
      best <- objective
      best_pairs <- length(left_idx)
      best_imbalance <- vapply(seq_len(hier$n_levels), function(h) {
        n_h <- hier$n_cats[[h + 1L]]
        sum(pmax(tabulate(hier$codes_left[[h]][left_idx], n_h) -
                   tabulate(hier$codes_right[[h]][right_idx], n_h), 0))
      }, numeric(1))
    }
  }
  list(objective = best, n_pairs = best_pairs, imbalance = best_imbalance)
}

# The imbalance a report carries, level by level, in the form card_brute()
# states it.
card_report_imbalance <- function(report, n_levels) {
  vapply(seq_len(n_levels), function(h) {
    sum(pmax(report$balance$imbalance[report$balance$level == h], 0))
  }, numeric(1))
}

# Every moment row recomputed against the pairs a report returned, from the
# coefficients rather than from anything the report carries.
card_pairs_violate <- function(report, coefs) {
  if (!length(coefs)) return(FALSE)
  any(vapply(coefs, function(cf) {
    .moment_violation(cf, report$pairs$left, report$pairs$right)
  }, numeric(1)) > 1e-9)
}

# An instance whose balance requirement is tight enough that the search cannot
# settle it in a handful of nodes.
card_hard_instance <- function(seed = 11L, n = 24L, m = 24L) {
  set.seed(seed)
  left <- data.frame(g1 = sample.int(3L, n, replace = TRUE),
                     x = round(stats::rnorm(n, 0.6), 3),
                     y = round(stats::rnorm(n, 0.4), 3),
                     z = round(stats::rnorm(n, -0.3), 3))
  right <- data.frame(g1 = sample.int(3L, m, replace = TRUE),
                      x = round(stats::rnorm(m, -0.6), 3),
                      y = round(stats::rnorm(m, -0.4), 3),
                      z = round(stats::rnorm(m, 0.3), 3))
  cost <- round(abs(outer(left$x, right$x, "-")) +
                  abs(outer(left$y, right$y, "-")), 3)
  list(left = left, right = right, cost = cost,
       vars = c("x", "y", "z"), max_std_diff = 0.05)
}
