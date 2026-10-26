# ==============================================================================
# The balance network's optimum is the balance-constrained matching optimum
# ==============================================================================
# The claim under test is a bijection, and it is checked in both directions
# without a solver.
#
#   Matching -> flow. Every candidate matched set the design admits has a flow
#   built for it by .balance_flow_encode(). Every arc of that flow lies inside
#   its bounds, every node conserves exactly, and the arc costs sum to
#   P * (K - pairs) + sum_h eps_h * imbalance_h + sum d_ij, with the imbalance
#   recomputed from the matched set through table() rather than from the flow.
#
#   Flow -> matching. Feasible integral flows reached from an encoding by
#   pushing a unit around a residual cycle are read back with
#   .balance_flow_read(), and the matched set that comes out never costs more
#   than the flow that carried it. The difference of two feasible flows
#   decomposes into residual cycles, so a walk over those cycles reaches every
#   feasible integral flow there is.
#
# Together they place the network's optimum at the matching optimum: no matching
# costs less than its own flow, and no flow costs less than the matching it
# reads back as. The sweep then checks that against a brute-force enumeration of
# every matched set, on objective, on cardinality, and on the imbalance at every
# level.
# ==============================================================================

# --- helpers ------------------------------------------------------------------

# A tiny instance: nested categories from the prefixes of g1, g2, g3, so
# nestedness holds by construction and the non-nested case can be built on
# purpose elsewhere.
bal_instance <- function(seed, max_units = 4L, max_levels = 3L) {
  set.seed(seed)
  n_left <- sample.int(max_units, 1L)
  n_right <- sample.int(max_units, 1L)
  n_levels <- sample.int(max_levels, 1L)
  n_coarse <- sample(2:4, 1L)

  side <- function(n) {
    data.frame(g1 = sample.int(n_coarse, n, replace = TRUE),
               g2 = sample.int(2L, n, replace = TRUE),
               g3 = sample.int(2L, n, replace = TRUE))
  }
  left <- side(n_left)
  right <- side(n_right)

  cost <- matrix(round(runif(n_left * n_right, 0, 3), 3), n_left, n_right)
  forbid <- matrix(runif(n_left * n_right) < 0.15, n_left, n_right)
  cost[forbid] <- Inf

  exact <- sample(unique(c(0L, 1L, n_levels)), 1L)

  list(left = left, right = right, cost = cost,
       spec = c("g1", "g2", "g3")[seq_len(n_levels)],
       exact = exact, n_left = n_left, n_right = n_right,
       n_levels = n_levels, seed = seed)
}

bal_build <- function(inst) {
  hier <- .refined_hierarchy(inst$left, inst$right, inst$spec,
                             exact = inst$exact)
  built <- .balance_flow_problem(inst$cost, hier)
  list(problem = built$problem, index = built$index, hier = hier)
}

# Every partial injection from left into right over the admissible cells.
bal_all_matchings <- function(cost) {
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

# The objective the network is meant to encode, computed from the matched set
# alone. The per-level counts come from table() over the hierarchy's own codes,
# so nothing here reads the flow or the index's category map.
bal_objective <- function(matching, hier, index, cost) {
  left_idx <- which(matching > 0L)
  right_idx <- matching[left_idx]
  n_pairs <- length(left_idx)

  imbalance <- vapply(seq_len(hier$n_levels), function(h) {
    lvl <- seq_len(hier$n_cats[[h + 1L]])
    a <- as.integer(table(factor(hier$codes_left[[h]][left_idx], levels = lvl)))
    b <- as.integer(table(factor(hier$codes_right[[h]][right_idx],
                                 levels = lvl)))
    sum(pmax(a - b, 0L))
  }, numeric(1))

  distance <- if (n_pairs) {
    sum(cost[cbind(left_idx, right_idx)]) - n_pairs * index$cost_shift
  } else {
    0
  }

  tiers <- index$tiers
  list(n_pairs = n_pairs, imbalance = imbalance, distance = distance,
       objective = tiers$penalty * (index$total_budget - n_pairs) +
         sum(tiers$eps * imbalance) + distance)
}

# A matched set the design admits: balanced at every level it enforces exactly.
bal_admissible <- function(imbalance, exact) {
  exact == 0L || imbalance[[exact]] == 0
}

bal_tol <- function(x) 1e-8 * max(1, abs(x))

# Push one unit around a residual cycle, reached by a randomised walk. Returns
# NULL when the walk finds none within its step budget.
bal_perturb <- function(problem, flow, n_try = 20L) {
  arcs <- problem$arcs
  up <- which(flow < arcs$upper - 0.5)
  down <- which(flow > arcs$lower + 0.5)
  from <- c(arcs$tail[up], arcs$head[down])
  to <- c(arcs$head[up], arcs$tail[down])
  arc_id <- c(up, down)
  step_sign <- c(rep(1, length(up)), rep(-1, length(down)))
  if (!length(from)) {
    return(NULL)
  }
  adj <- split(seq_along(from), factor(from, levels = seq_len(problem$n_nodes)))

  walk <- function() {
    depth <- integer(problem$n_nodes)
    path_edge <- integer(0)
    node <- sample.int(problem$n_nodes, 1L)
    depth[[node]] <- 1L
    for (step in seq_len(4L * problem$n_nodes)) {
      edges <- adj[[node]]
      if (!length(edges)) {
        return(NULL)
      }
      e <- edges[[sample.int(length(edges), 1L)]]
      path_edge <- c(path_edge, e)
      nxt <- to[[e]]
      if (depth[[nxt]] > 0L) {
        return(path_edge[seq.int(depth[[nxt]], length(path_edge))])
      }
      depth[[nxt]] <- length(path_edge) + 1L
      node <- nxt
    }
    NULL
  }

  for (attempt in seq_len(n_try)) {
    cycle <- walk()
    if (is.null(cycle)) next
    # A cycle may traverse both directions of one arc, and those cancel, so the
    # step is accumulated rather than assigned.
    delta <- numeric(length(flow))
    for (e in cycle) {
      delta[[arc_id[[e]]]] <- delta[[arc_id[[e]]]] + step_sign[[e]]
    }
    if (all(delta == 0)) next
    out <- flow + delta
    if (any(out < arcs$lower - 1e-9) || any(out > arcs$upper + 1e-9)) next
    return(out)
  }
  NULL
}

# Kahn's ordering over the arc set. A network with no cycle at all has no
# negative cycle either, which is what the shortest-path search needs.
bal_acyclic <- function(problem) {
  n <- problem$n_nodes
  arcs <- problem$arcs
  indeg <- tabulate(arcs$head, nbins = n)
  adj <- split(seq_len(nrow(arcs)), factor(arcs$tail, levels = seq_len(n)))
  ready <- which(indeg == 0L)
  seen <- 0L
  while (length(ready)) {
    v <- ready[[1L]]
    ready <- ready[-1L]
    seen <- seen + 1L
    for (a in adj[[v]]) {
      h <- arcs$head[[a]]
      indeg[[h]] <- indeg[[h]] - 1L
      if (indeg[[h]] == 0L) ready <- c(ready, h)
    }
  }
  seen == n
}

# One instance, end to end: every matched set encoded, priced and read back,
# then the network's cheapest flow against the brute-force optimum.
bal_sweep_one <- function(seed, max_units = 4L, n_perturb = 0L) {
  inst <- bal_instance(seed, max_units = max_units)
  built <- bal_build(inst)
  matchings <- bal_all_matchings(inst$cost)

  encoded <- rep(NA_real_, length(matchings))
  brute <- rep(NA_real_, length(matchings))
  best_pairs <- NA_integer_
  best_imbalance <- NULL
  faults <- character(0)
  n_checks <- 0L
  note <- function(msg) faults <<- c(faults, paste0("seed ", seed, ": ", msg))

  for (k in seq_along(matchings)) {
    m <- matchings[[k]]
    obj <- bal_objective(m, built$hier, built$index, inst$cost)
    ok <- bal_admissible(obj$imbalance, inst$exact)
    flow <- .balance_flow_encode(m, built$index)

    if (!ok) {
      if (!is.null(flow)) note("encoded a matched set the design forbids")
      n_checks <- n_checks + 1L
      next
    }
    if (is.null(flow)) {
      note("refused to encode an admissible matched set")
      next
    }

    audit <- .balance_flow_audit(built$problem, built$index, flow, inst$cost)
    if (!audit$feasible) note("encoded flow is not a feasible flow")
    if (abs(audit$identity_gap) > bal_tol(audit$arc_cost)) {
      note("arc costs do not equal the claimed objective")
    }
    if (abs(audit$pairs_identity_gap) > 1e-9) {
      note("pairs is not K - slack - transfers")
    }
    if (audit$n_pairs != obj$n_pairs) note("cardinality read back wrong")
    if (any(abs(audit$imbalance - obj$imbalance) > 1e-9)) {
      note("per-level imbalance read back wrong")
    }
    if (abs(audit$arc_cost - obj$objective) > bal_tol(obj$objective)) {
      note("flow cost differs from the matched set's objective")
    }
    if (audit$n_self_crossing != 0L) {
      note("an optimal encoding crosses a cell with itself")
    }
    if (!identical(.balance_flow_read(built$index, flow)$matching, m)) {
      note("the matched set does not survive the round trip")
    }

    encoded[[k]] <- audit$arc_cost
    brute[[k]] <- obj$objective
    n_checks <- n_checks + 8L

    if (n_perturb > 0L) {
      f <- flow
      for (p in seq_len(n_perturb)) {
        f <- bal_perturb(built$problem, f)
        if (is.null(f)) break
        a2 <- .balance_flow_audit(built$problem, built$index, f, inst$cost)
        if (!a2$feasible) {
          note("residual cycle push left the feasible set")
          break
        }
        if (a2$arc_cost < a2$objective - bal_tol(a2$arc_cost)) {
          note("a feasible flow costs less than the set it reads back as")
        }
        if (!bal_admissible(a2$imbalance, inst$exact)) {
          note("a feasible flow reads back as a matched set the design forbids")
        }
        n_checks <- n_checks + 3L
      }
    }
  }

  if (!any(!is.na(brute))) {
    note("no matched set at all is admissible, not even the empty one")
  } else {
    win <- which.min(brute)
    if (abs(min(encoded, na.rm = TRUE) - min(brute, na.rm = TRUE)) >
        bal_tol(brute[[win]])) {
      note("the cheapest flow is not the brute-force optimum")
    }
    best <- bal_objective(matchings[[which.min(encoded)]], built$hier,
                          built$index, inst$cost)
    ref <- bal_objective(matchings[[win]], built$hier, built$index, inst$cost)
    if (best$n_pairs != ref$n_pairs) note("optimum disagrees on cardinality")
    if (any(abs(best$imbalance - ref$imbalance) > 1e-9)) {
      note("optimum disagrees on per-level imbalance")
    }
    best_pairs <- best$n_pairs
    best_imbalance <- best$imbalance
    n_checks <- n_checks + 3L
  }

  list(faults = faults, n_checks = n_checks, n_matchings = length(matchings),
       best_pairs = best_pairs, best_imbalance = best_imbalance)
}

bal_report <- function(results) {
  faults <- unlist(lapply(results, `[[`, "faults"), use.names = FALSE)
  if (is.null(faults)) faults <- character(0)
  list(first_fault = if (length(faults)) faults[[1L]] else NA_character_,
       n_faults = length(faults),
       n_checks = sum(vapply(results, `[[`, numeric(1), "n_checks")),
       n_matchings = sum(vapply(results, `[[`, numeric(1), "n_matchings")))
}

# --- node layout --------------------------------------------------------------

test_that("the node layout accounts for every node and every arc", {
  left <- data.frame(g1 = c(1, 1, 2, 2), g2 = c(1, 2, 1, 2))
  right <- data.frame(g1 = c(1, 2, 2), g2 = c(2, 1, 2))

  for (exact in 0:2) {
    hier <- .refined_hierarchy(left, right, c("g1", "g2"), exact = exact)
    layout <- .balance_node_layout(4L, 3L, hier)

    n_cats <- hier$n_cats
    expected <- 2L + 4L + 3L + 2L * sum(n_cats[2:3])
    cross <- if (exact > 1L) integer(0) else seq.int(exact, 1L)
    expected <- expected + 2L * sum(n_cats[cross + 1L])
    expect_identical(layout$n_nodes, expected)

    expect_identical(layout$node_left(1:4), 3:6)
    expect_identical(layout$node_right(1:3), 7:9)
    expect_true(all(layout$node_tc(1L, seq_len(n_cats[[2L]])) > 9L))
    expect_identical(layout$cross_levels, as.integer(cross))

    built <- .balance_flow_problem(matrix(1, 4, 3), hier)
    expect_identical(built$problem$n_nodes, layout$n_nodes)
    expect_identical(nrow(built$problem$arcs), sum(vapply(built$index$ranges,
                                                          length, integer(1))))
    # Every node id an arc names comes from the layout.
    expect_true(all(built$problem$arcs$tail <= layout$n_nodes))
    expect_true(all(built$problem$arcs$head <= layout$n_nodes))
  }
})

test_that("the network holds no cycle, so no negative cycle either", {
  for (seed in 1:40) {
    inst <- bal_instance(seed)
    built <- bal_build(inst)
    expect_true(bal_acyclic(built$problem))
    expect_true(all(built$problem$arcs$cost >= 0))
  }
})

# --- configuration A ----------------------------------------------------------

test_that("exact fine balance forces equal category counts on both sides", {
  left <- data.frame(g = c("a", "a", "a", "b"))
  right <- data.frame(g = c("a", "b", "b", "b", "c"))
  hier <- .refined_hierarchy(left, right, "g", exact = 1L)
  built <- .balance_flow_problem(matrix(runif(20), 4, 5), hier)

  # U_c = max(|T_c|, |K_c|) over a, b, c: 3, 3, 1.
  expect_equal(built$index$budget$finest, c(3, 3, 1))
  expect_equal(built$index$total_budget, 7)
  expect_identical(built$index$ranges$cross, integer(0))

  # A matched set with unequal counts in a category has no flow at all: left
  # unit 1 is in a, right unit 2 is in b.
  expect_null(.balance_flow_encode(c(2L, 0L, 0L, 0L), built$index))
  flow <- .balance_flow_encode(c(1L, 0L, 0L, 2L), built$index)
  expect_false(is.null(flow))
  audit <- .balance_flow_audit(built$problem, built$index, flow)
  expect_true(audit$feasible)
  expect_equal(audit$imbalance, 0)
  expect_equal(audit$n_pairs, 2)
})

test_that("no fine and no refined is maximum-cardinality minimum-distance", {
  for (seed in 101:130) {
    set.seed(seed)
    n <- sample.int(4L, 1L)
    m <- sample.int(4L, 1L)
    cost <- matrix(round(runif(n * m, 0, 2), 3), n, m)
    cost[runif(n * m) < 0.2] <- Inf
    left <- data.frame(u = seq_len(n))
    right <- data.frame(u = seq_len(m))

    hier <- .refined_hierarchy(left, right, NULL, exact = 1L)
    expect_identical(hier$n_levels, 1L)
    expect_identical(hier$n_cats, c(1L, 1L))
    built <- .balance_flow_problem(cost, hier)

    matchings <- bal_all_matchings(cost)
    objs <- vapply(matchings, function(m0) {
      bal_objective(m0, hier, built$index, cost)$objective
    }, numeric(1))
    costs <- vapply(matchings, function(m0) {
      fl <- .balance_flow_encode(m0, built$index)
      if (is.null(fl)) NA_real_ else sum(built$problem$arcs$cost * fl)
    }, numeric(1))
    expect_false(anyNA(costs))
    expect_equal(costs, objs)

    n_pairs <- vapply(matchings, function(m0) sum(m0 > 0L), numeric(1))
    dist <- vapply(matchings, function(m0) {
      li <- which(m0 > 0L)
      if (length(li)) sum(cost[cbind(li, m0[li])]) else 0
    }, numeric(1))
    best <- which.min(costs)
    expect_equal(n_pairs[[best]], max(n_pairs))
    expect_equal(dist[[best]], min(dist[n_pairs == max(n_pairs)]))
  }
})

# --- configuration B ----------------------------------------------------------

test_that("a transfer is one unit of signed imbalance priced at P + gamma", {
  left <- data.frame(g1 = c(1, 1, 2, 2), g2 = c(1, 2, 1, 2))
  right <- data.frame(g1 = c(1, 1, 2, 2), g2 = c(2, 1, 2, 1))
  cost <- matrix(0, 4, 4)
  hier <- .refined_hierarchy(left, right, c("g1", "g2"), exact = 1L)
  built <- .balance_flow_problem(cost, hier)
  tiers <- built$index$tiers

  expect_true(tiers$eps[[1L]] > tiers$eps[[2L]])
  expect_true(tiers$penalty > sum(tiers$eps * tiers$counts[1:2]))
  # gamma_h is the sum of eps over the levels finer than h.
  expect_equal(tiers$gamma, c(sum(tiers$eps), tiers$eps[[2L]], 0))

  cross_cost <- built$problem$arcs$cost[built$index$ranges$cross]
  expect_equal(unique(cross_cost),
               tiers$penalty +
                 tiers$gamma[[built$index$cross_level[[1L]] + 1L]])

  # Left unit 1 is (1,1) and right unit 1 is (1,2): one crossing at level 1.
  flow <- .balance_flow_encode(c(1L, 0L, 0L, 0L), built$index)
  audit <- .balance_flow_audit(built$problem, built$index, flow)
  expect_true(audit$feasible)
  expect_equal(audit$imbalance, c(0, 1))
  expect_equal(audit$total_transfers, 1)
  expect_equal(audit$identity_gap, 0)
})

test_that("exact = 0 prices the coarsest level at the root crossing", {
  left <- data.frame(g = c("a", "a"))
  right <- data.frame(g = c("b", "b"))
  cost <- matrix(0, 2, 2)

  strict <- .refined_hierarchy(left, right, "g", exact = 1L)
  built_strict <- .balance_flow_problem(cost, strict)
  expect_null(.balance_flow_encode(c(1L, 0L), built_strict$index))

  loose <- .refined_hierarchy(left, right, "g", exact = 0L)
  built <- .balance_flow_problem(cost, loose)
  expect_identical(built$index$cross_level, 0L)
  flow <- .balance_flow_encode(c(1L, 2L), built$index)
  audit <- .balance_flow_audit(built$problem, built$index, flow)
  expect_true(audit$feasible)
  expect_equal(audit$n_pairs, 2)
  expect_equal(audit$imbalance, 2)
  expect_equal(audit$identity_gap, 0)
})

test_that("exact = H is configuration A on the finest partition", {
  left <- data.frame(g1 = c(1, 1, 2), g2 = c(1, 2, 1))
  right <- data.frame(g1 = c(1, 2, 2), g2 = c(1, 1, 2))
  hier <- .refined_hierarchy(left, right, c("g1", "g2"), exact = 2L)
  built <- .balance_flow_problem(matrix(1, 3, 3), hier)
  expect_identical(built$index$ranges$cross, integer(0))
  expect_identical(built$index$ranges$out_up, integer(0))
  # Left unit 1 is (1,1) and right unit 2 is (2,1): different finest cells.
  expect_null(.balance_flow_encode(c(2L, 0L, 0L), built$index))
  flow <- .balance_flow_encode(c(1L, 0L, 2L), built$index)
  audit <- .balance_flow_audit(built$problem, built$index, flow)
  expect_true(audit$feasible)
  expect_equal(audit$imbalance, c(0, 0))
})

test_that("forced tree budgets make a crossing pay for the imbalance", {
  # One level-1 cell with two level-2 children, the left units in one child and
  # the right units in the other. Matching them is two units of level-2
  # imbalance, and the network has to charge eps_2 for each.
  left <- data.frame(g1 = c(1, 1), g2 = c(1, 1))
  right <- data.frame(g1 = c(1, 1), g2 = c(2, 2))
  cost <- matrix(0, 2, 2)
  hier <- .refined_hierarchy(left, right, c("g1", "g2"), exact = 1L)
  built <- .balance_flow_problem(cost, hier)
  index <- built$index
  ranges <- index$ranges
  tiers <- index$tiers

  encoded <- .balance_flow_encode(c(1L, 2L), index)
  audit <- .balance_flow_audit(built$problem, index, encoded, cost)
  expect_true(audit$feasible)
  expect_equal(audit$imbalance, c(0, 2))
  expect_equal(audit$total_transfers, 2)
  expect_equal(audit$identity_gap, 0)

  # The same matched set, routed so that the treated side of one child and the
  # control side of the other carry the whole budget. Free tree arcs would admit
  # this flow, and it pays nothing for the imbalance it holds.
  dodge <- numeric(index$n_arcs)
  dodge[ranges$budget_in] <- index$total_budget
  dodge[ranges$tree_tc] <- c(2, 2)
  dodge[ranges$tree_cc] <- c(0, 4)
  dodge[ranges$unit_left] <- 1
  dodge[ranges$pair[match(c(1L, 2L + 2L), index$pair_key)]] <- 1
  dodge[ranges$unit_right] <- 1
  dodge[ranges$budget_out] <- index$total_budget
  dodge[ranges$slack] <- c(0, 2)

  loose <- built$problem
  loose$arcs$lower[c(ranges$tree_tc, ranges$tree_cc)] <- 0
  loose$arcs$upper[c(ranges$tree_tc, ranges$tree_cc)] <- index$total_budget
  under <- .balance_flow_audit(loose, index, dodge, cost)
  expect_true(under$feasible)
  expect_equal(under$imbalance, c(0, 2))
  expect_equal(under$total_transfers, 0)
  expect_equal(under$objective - under$arc_cost, 2 * tiers$eps[[2L]])

  # Against the forced tree the same flow is not a flow.
  strict <- .balance_flow_audit(built$problem, index, dodge, cost)
  expect_false(strict$feasible)
  expect_equal(strict$n_below_lower + strict$n_above_upper, 2)
})

# --- the sweep ----------------------------------------------------------------

test_that("every matched set encodes to a flow that prices it exactly", {
  results <- lapply(1:120, bal_sweep_one)
  report <- bal_report(results)
  expect_identical(report$first_fault, NA_character_)
  expect_true(report$n_matchings > 2000)
})

test_that("a feasible flow never costs less than the matched set it reads as", {
  results <- lapply(201:240, bal_sweep_one, n_perturb = 6L)
  report <- bal_report(results)
  expect_identical(report$first_fault, NA_character_)
})

test_that("the cheapest flow is the brute-force optimum over 1000 instances", {
  skip_on_cran()
  results <- lapply(1001:2000, bal_sweep_one)
  report <- bal_report(results)
  expect_identical(report$first_fault, NA_character_)
  expect_identical(report$n_faults, 0L)
  expect_true(report$n_matchings > 20000)
})

test_that("the sweep reaches the larger instances too", {
  skip_on_cran()
  results <- lapply(3001:3060, bal_sweep_one, max_units = 6L, n_perturb = 2L)
  report <- bal_report(results)
  expect_identical(report$first_fault, NA_character_)
})

# --- specification and edge cases ---------------------------------------------

test_that("a partition takes columns or a one-sided formula", {
  left <- data.frame(a = c("p", "q"), b = c("u", "u"))
  right <- data.frame(a = c("p", "p"), b = c("v", "u"))

  by_chr <- .balance_partition(left, right, c("a", "b"))
  by_plus <- .balance_partition(left, right, ~ a + b)
  by_colon <- .balance_partition(left, right, ~ a:b)
  expect_identical(by_chr$left, by_plus$left)
  expect_identical(by_chr$right, by_plus$right)
  expect_identical(by_chr$left, by_colon$left)
  expect_identical(by_chr$n_cats, 3L)

  expect_error(.balance_partition(left, right, "missing"), "no column")
  expect_error(.balance_partition(left, right, 1:2), "character vector")
  expect_error(.refined_hierarchy(left, right, y ~ a), "one-sided")
})

test_that("a refined character vector is its own sequence of prefixes", {
  left <- data.frame(g1 = c(1, 1, 2), g2 = c(1, 2, 1))
  right <- data.frame(g1 = c(1, 2), g2 = c(2, 1))
  hier <- .refined_hierarchy(left, right, c("g1", "g2"), exact = 1L)
  expect_identical(hier$n_levels, 2L)
  expect_identical(hier$columns[[1L]], "g1")
  expect_identical(hier$columns[[2L]], c("g1", "g2"))

  listed <- .refined_hierarchy(left, right, list("g1", c("g1", "g2")),
                               exact = 1L)
  expect_identical(hier$parent, listed$parent)
  expect_identical(hier$codes_left, listed$codes_left)
})

test_that("a refined spec that is not nested names the straddling cell", {
  left <- data.frame(coarse = c("north", "south"), fine = c("site1", "site1"))
  right <- data.frame(coarse = c("north", "south"), fine = c("site2", "site2"))
  expect_error(
    .refined_hierarchy(left, right, list("coarse", "fine"), exact = 1L),
    "not nested"
  )
  expect_error(
    .refined_hierarchy(left, right, list("coarse", "fine"), exact = 1L),
    "site1"
  )
  expect_error(
    .refined_hierarchy(left, right, list("coarse", "fine"), exact = 1L),
    "north"
  )
  expect_error(
    .refined_hierarchy(left, right, list("coarse", "fine"), exact = 1L),
    "south"
  )
})

test_that("a category present on one side only still carries a budget", {
  left <- data.frame(g = c("a", "a", "a"))
  right <- data.frame(g = c("b", "b"))
  hier <- .refined_hierarchy(left, right, "g", exact = 1L)
  built <- .balance_flow_problem(matrix(1, 3, 2), hier)

  expect_equal(built$index$budget$finest, c(3, 2))
  expect_equal(built$index$total_budget, 5)
  # With min() as the budget, category b would carry nothing at all.
  expect_true(all(built$index$budget$finest > 0))

  # Under exact fine balance nothing can be matched across the two categories.
  flow <- .balance_flow_encode(integer(3), built$index)
  audit <- .balance_flow_audit(built$problem, built$index, flow)
  expect_true(audit$feasible)
  expect_equal(audit$n_pairs, 0)
  expect_equal(audit$total_slack, 5)

  loose <- .refined_hierarchy(left, right, "g", exact = 0L)
  built2 <- .balance_flow_problem(matrix(1, 3, 2), loose)
  flow2 <- .balance_flow_encode(c(1L, 2L, 0L), built2$index)
  audit2 <- .balance_flow_audit(built2$problem, built2$index, flow2)
  expect_true(audit2$feasible)
  expect_equal(audit2$n_pairs, 2)
  expect_equal(audit2$identity_gap, 0)
})

test_that("a forbidden pair gets no arc, a forbidden instance no pairs", {
  left <- data.frame(g = c("a", "a"))
  right <- data.frame(g = c("a", "a"))
  cost <- matrix(c(1, Inf, NA, 2), 2, 2)
  hier <- .refined_hierarchy(left, right, "g", exact = 1L)
  built <- .balance_flow_problem(cost, hier)
  expect_identical(length(built$index$ranges$pair), 2L)
  expect_true(all(is.finite(built$problem$arcs$cost)))
  expect_error(.balance_flow_encode(c(2L, 0L), built$index), "forbids")

  none <- .balance_flow_problem(matrix(Inf, 2, 2), hier)
  expect_identical(length(none$index$ranges$pair), 0L)
  flow <- .balance_flow_encode(integer(2), none$index)
  audit <- .balance_flow_audit(none$problem, none$index, flow)
  expect_true(audit$feasible)
  expect_equal(audit$n_pairs, 0)

  big <- .balance_flow_problem(matrix(BIG_COST, 2, 2), hier)
  expect_identical(length(big$index$ranges$pair), 0L)
})

test_that("zero-row inputs build an empty but valid problem", {
  empty <- data.frame(g = character(0))
  right <- data.frame(g = c("a", "b"))

  hier <- .refined_hierarchy(empty, right, "g", exact = 1L)
  built <- .balance_flow_problem(matrix(0, 0, 2), hier)
  expect_s3_class(built$problem, "couplr_flow_problem")
  expect_equal(built$index$total_budget, 2)
  flow <- .balance_flow_encode(integer(0), built$index)
  audit <- .balance_flow_audit(built$problem, built$index, flow)
  expect_true(audit$feasible)
  expect_equal(audit$n_pairs, 0)

  both <- .refined_hierarchy(empty, empty, "g", exact = 1L)
  expect_identical(both$n_cats, c(1L, 0L))
  bare <- .balance_flow_problem(matrix(0, 0, 0), both)
  expect_s3_class(bare$problem, "couplr_flow_problem")
  expect_identical(nrow(bare$problem$arcs), 0L)
  expect_equal(sum(bare$problem$supply), 0)
})

test_that("a cost range too wide to order exactly is refused", {
  expect_error(.balance_tiers(3L, 8, 4, 1e13), "double precision")
  expect_error(.balance_tiers(3L, 8, 4, 1e13), "reduce the depth")

  left <- data.frame(g1 = c(1, 1, 2, 2), g2 = c(1, 2, 1, 2))
  right <- data.frame(g1 = c(1, 2, 1, 2), g2 = c(2, 1, 2, 1))
  hier <- .refined_hierarchy(left, right, c("g1", "g2"), exact = 1L)
  expect_error(
    .balance_flow_problem(matrix(c(0, rep(1e14, 15)), 4, 4), hier),
    "cost range is too wide"
  )
})

test_that("the arguments a design states are checked at the door", {
  left <- data.frame(g = c("a", "b"))
  right <- data.frame(g = c("a", "b"))
  hier <- .refined_hierarchy(left, right, "g", exact = 1L)

  expect_error(.refined_hierarchy(left, right, "g", exact = 2L), "depth")
  expect_error(.refined_hierarchy(left, right, "g", exact = -1L), "negative")
  expect_error(.balance_flow_problem(matrix(1, 3, 2), hier), "one finest-level")
  expect_error(.balance_flow_problem(matrix(1, 2, 2), hier,
                                     codes = list(left = c(1L, 9L),
                                                  right = c(1L, 2L))),
               "category indices")
  expect_error(.balance_flow_problem(matrix(1, 2, 2), hier,
                                     arc_bounds = list(nonsense = 1)),
               "no arc class")

  built <- .balance_flow_problem(matrix(1, 2, 2), hier)
  expect_error(.balance_flow_encode(c(1L, 1L), built$index), "more than once")
  expect_error(.balance_flow_encode(c(1L, 5L), built$index), "right indices")
  expect_error(.balance_flow_encode(1L, built$index), "one right index")
  expect_error(.balance_flow_read(built$index, c(1, 2)), "but the problem has")

  # A missing category value leaves a unit with no place in the partition.
  expect_error(.balance_partition(data.frame(g = c("a", NA)), right, "g"),
               "missing values")
})

test_that("a matched set states its pairs as a vector or as a table", {
  left <- data.frame(g = c("a", "a"))
  right <- data.frame(g = c("a", "a"))
  hier <- .refined_hierarchy(left, right, "g", exact = 1L)
  built <- .balance_flow_problem(matrix(c(1, 2, 3, 4), 2, 2), hier)

  by_vector <- .balance_flow_encode(c(2L, 1L), built$index)
  by_table <- .balance_flow_encode(cbind(c(1L, 2L), c(2L, 1L)), built$index)
  expect_equal(by_vector, by_table)

  read <- .balance_flow_read(built$index, by_vector)
  expect_identical(read$matching, c(2L, 1L))
  expect_identical(read$n_pairs, 2L)
  expect_equal(read$total_distance, 3 + 2 - 2 * built$index$cost_shift)
})
