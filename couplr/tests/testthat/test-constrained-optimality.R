# Constrained matching must be optimal, not merely feasible.
#
# When calipers or max_distance forbid enough pairs, the admissible bipartite
# graph can fail Hall's condition and no complete matching exists. The
# contract in that case is lexicographic: the largest number of admissible
# pairs first, then the smallest total distance among matchings of that size.
# These tests check that contract against exhaustive enumeration rather than
# checking shapes.

# Exhaustive (cardinality, cost) optimum over all partial matchings.
brute_force_partial <- function(cost, admissible) {
  n <- nrow(cost)
  m <- ncol(cost)
  best <- list(k = -1L, cost = Inf)
  recurse <- function(i, used, k, total) {
    if (i > n) {
      if (k > best$k || (k == best$k && total < best$cost - 1e-9)) {
        best <<- list(k = k, cost = total)
      }
      return(invisible(NULL))
    }
    recurse(i + 1L, used, k, total)
    for (j in seq_len(m)) {
      if (!(j %in% used) && admissible[i, j]) {
        recurse(i + 1L, c(used, j), k + 1L, total + cost[i, j])
      }
    }
    invisible(NULL)
  }
  recurse(1L, integer(0), 0L, 0)
  best
}

solve_via_package <- function(cost, admissible) {
  constrained <- cost
  constrained[!admissible] <- Inf
  solved <- couplr:::.solve_with_partial_feasibility(constrained, assignment)
  rows <- solved$matched_rows
  cols <- solved$matched_cols
  if (length(rows) == 0L) {
    return(list(k = 0L, cost = 0))
  }
  list(k = length(rows), cost = sum(cost[cbind(rows, cols)]))
}

test_that("constrained matching attains the brute-force optimum", {
  skip_on_cran()
  set.seed(20260809)
  mismatches <- 0L
  for (trial in seq_len(120)) {
    n <- sample(2:5, 1)
    m <- sample(2:5, 1)
    cost <- matrix(round(runif(n * m, 1, 20), 3), n, m)
    admissible <- matrix(runif(n * m) > 0.45, n, m)
    if (!any(admissible)) next

    expected <- brute_force_partial(cost, admissible)
    actual <- solve_via_package(cost, admissible)

    if (actual$k != expected$k || abs(actual$cost - expected$cost) > 1e-8) {
      mismatches <- mismatches + 1L
    }
  }
  expect_identical(mismatches, 0L)
})

test_that("a Hall violation still yields the maximum number of pairs", {
  # Rows 1-3 can only reach column 1, so at most one of them can match;
  # row 4 has its own column. Maximum cardinality is 2.
  cost <- matrix(Inf, 4, 2)
  cost[1, 1] <- 5
  cost[2, 1] <- 3
  cost[3, 1] <- 9
  cost[4, 2] <- 7

  solved <- couplr:::.solve_with_partial_feasibility(cost, assignment)
  expect_length(solved$matched_rows, 2L)
  # Of the three competitors for column 1, the cheapest must be chosen.
  expect_true(2L %in% solved$matched_rows)
  expect_true(4L %in% solved$matched_rows)
  expect_equal(sum(cost[cbind(solved$matched_rows, solved$matched_cols)]), 10)
})

test_that("the padded solve is never worse than the greedy result", {
  skip_on_cran()
  set.seed(11)
  for (trial in seq_len(40)) {
    n <- sample(4:7, 1)
    m <- sample(4:7, 1)
    cost <- matrix(round(runif(n * m, 1, 50), 3), n, m)
    admissible <- matrix(runif(n * m) > 0.6, n, m)
    if (!any(admissible)) next
    constrained <- cost
    constrained[!admissible] <- Inf

    optimal <- couplr:::.solve_with_partial_feasibility(constrained, assignment)
    k_opt <- length(optimal$matched_rows)

    feasible <- is.finite(constrained)
    keep_rows <- rowSums(feasible) > 0
    keep_cols <- colSums(feasible) > 0
    if (!any(keep_rows) || !any(keep_cols)) next
    sub <- constrained[keep_rows, keep_cols, drop = FALSE]
    greedy <- couplr:::greedy_matching(sub, strategy = "sorted")
    gv <- as.integer(greedy$match)
    k_greedy <- sum(gv > 0)

    expect_gte(k_opt, k_greedy)
    if (k_opt == k_greedy && k_opt > 0) {
      cost_opt <- sum(cost[cbind(optimal$matched_rows, optimal$matched_cols)])
      grows <- which(gv > 0)
      cost_greedy <- sum(sub[cbind(grows, gv[grows])])
      expect_lte(cost_opt, cost_greedy + 1e-8)
    }
  }
})

test_that("match_couples returns an optimal matching under tight calipers", {
  skip_on_cran()
  set.seed(4)
  n <- 8
  left <- data.frame(id = paste0("L", seq_len(n)),
                     x = round(runif(n, 0, 10), 2))
  right <- data.frame(id = paste0("R", seq_len(n)),
                      x = round(runif(n, 0, 10), 2))

  m <- suppressWarnings(
    match_couples(left, right, vars = "x", calipers = list(x = 1.5))
  )

  cost <- abs(outer(left$x, right$x, "-"))
  admissible <- cost <= 1.5
  expected <- brute_force_partial(cost, admissible)

  expect_equal(nrow(m$pairs), expected$k)
  if (expected$k > 0) {
    expect_equal(sum(m$pairs$distance), expected$cost, tolerance = 1e-6)
  }
})
