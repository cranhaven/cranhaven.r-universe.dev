# ==============================================================================
# k-best enumeration: both backends run Murty/Lawler partitioning, so both
# agree with an exhaustive enumeration of the k best assignments.
# ==============================================================================

# Every permutation of a square matrix, ranked by total cost. This is the truth
# a k-best backend is measured against.
kbest_bruteforce <- function(cost, k, maximize = FALSE) {
  n <- nrow(cost)
  perms <- .perms(n)
  totals <- vapply(perms, function(p) sum(cost[cbind(seq_len(n), p)]), numeric(1))
  ord <- order(totals, decreasing = maximize)
  keep <- utils::head(ord, k)
  list(matches = perms[keep], totals = totals[keep])
}

.perms <- function(n) {
  if (n == 1L) return(list(1L))
  out <- list()
  for (i in seq_len(n)) {
    for (rest in .perms(n - 1L)) {
      others <- setdiff(seq_len(n), i)
      out[[length(out) + 1L]] <- c(i, others[rest])
    }
  }
  out
}

murty_totals <- function(cost, k, maximize = FALSE) {
  res <- couplr:::lap_kbest_murty(cost, as.integer(k), maximize, "jv")
  as.numeric(res$totals)
}

lawler_totals <- function(cost, k, maximize = FALSE) {
  res <- couplr:::lap_kbest_lawler(cost, as.integer(k), "jv", maximize)
  vapply(res, function(s) s$total_cost, numeric(1))
}

test_that("the permutation enumerator is right", {
  expect_equal(length(.perms(4L)), 24L)
  expect_true(all(vapply(.perms(4L), function(p) setequal(p, 1:4), logical(1))))
})

test_that("murty matches an exhaustive enumeration of the k best", {
  set.seed(101)
  for (trial in 1:15) {
    n <- sample(3:5, 1)
    cost <- matrix(sample(0:20, n * n, replace = TRUE), n, n)
    k <- min(8L, factorial(n))

    truth <- kbest_bruteforce(cost, k)$totals
    expect_equal(murty_totals(cost, k), truth,
                 info = paste("trial", trial, "n =", n))
  }
})

test_that("lawler matches an exhaustive enumeration of the k best", {
  set.seed(103)
  for (trial in 1:15) {
    n <- sample(3:5, 1)
    cost <- matrix(sample(0:20, n * n, replace = TRUE), n, n)
    k <- min(8L, factorial(n))

    truth <- kbest_bruteforce(cost, k)$totals
    expect_equal(lawler_totals(cost, k), truth,
                 info = paste("trial", trial, "n =", n))
  }
})

test_that("the two k-best backends agree with each other", {
  set.seed(107)
  for (trial in 1:15) {
    n <- sample(3:5, 1)
    cost <- matrix(round(runif(n * n, 0, 10), 3), n, n)
    k <- min(10L, factorial(n))
    expect_equal(murty_totals(cost, k), lawler_totals(cost, k),
                 info = paste("trial", trial))
  }
})

test_that("murty enumerates the k largest when maximizing", {
  set.seed(109)
  for (trial in 1:10) {
    n <- sample(3:4, 1)
    cost <- matrix(sample(0:20, n * n, replace = TRUE), n, n)
    k <- min(6L, factorial(n))

    truth <- kbest_bruteforce(cost, k, maximize = TRUE)$totals
    expect_equal(murty_totals(cost, k, maximize = TRUE), truth,
                 info = paste("trial", trial))
  }
})

test_that("lawler enumerates the k largest when maximizing", {
  set.seed(111)
  for (trial in 1:10) {
    n <- sample(3:4, 1)
    cost <- matrix(sample(0:20, n * n, replace = TRUE), n, n)
    k <- min(6L, factorial(n))

    truth <- kbest_bruteforce(cost, k, maximize = TRUE)$totals
    expect_equal(lawler_totals(cost, k, maximize = TRUE), truth,
                 info = paste("trial", trial))
  }
})

test_that("ties do not cost a solution", {
  # Every assignment has the same total, so every permutation is k-best and a
  # branching scheme whose children overlap drops some of them.
  cost <- matrix(0, 4, 4)
  expect_equal(length(murty_totals(cost, 24L)), 24L)
  expect_equal(length(lawler_totals(cost, 24L)), 24L)
})

test_that("k beyond the number of assignments returns them all, in order", {
  set.seed(113)
  cost <- matrix(sample(0:9, 9, replace = TRUE), 3, 3)
  totals <- murty_totals(cost, 50L)
  expect_equal(length(totals), 6L)
  expect_false(is.unsorted(totals))
  expect_equal(totals, kbest_bruteforce(cost, 6L)$totals)
})

test_that("forbidden edges shrink the enumeration to the feasible assignments", {
  cost <- matrix(c(1, 2, 3,
                   4, 5, 6,
                   7, 8, 9), 3, 3, byrow = TRUE)
  cost[1, 2] <- Inf
  cost[2, 3] <- Inf

  totals <- murty_totals(cost, 10L)
  expect_true(all(is.finite(totals)))
  expect_false(is.unsorted(totals))

  feasible <- Filter(
    function(p) all(is.finite(cost[cbind(1:3, p)])),
    .perms(3L)
  )
  expect_equal(length(totals), length(feasible))
})

# The sentinel is the cheapest value in each of these, so an unmasked solve
# picks it and a masked one cannot: the contrast is what shows the argument is
# read rather than accepted and dropped.
test_that("lap_solve_kbest honours the forbidden sentinel on matrix input", {
  cost <- matrix(c(-1,  5,  6,
                    7, -1,  8,
                    9,  1,  2), 3, 3, byrow = TRUE)

  unmasked <- lap_solve_kbest(cost, k = 1)
  expect_true(any(unmasked$cost == -1))

  masked <- lap_solve_kbest(cost, k = 1, forbidden = -1)
  expect_false(any(masked$cost == -1))
})

test_that("lap_solve_batch honours the forbidden sentinel on list input", {
  problems <- list(
    matrix(c(-1, 5, 7, 2), 2, 2, byrow = TRUE),
    matrix(c(4, -1, 1, 6), 2, 2, byrow = TRUE)
  )
  expect_true(any(lap_solve_batch(problems)$cost == -1))
  expect_false(any(lap_solve_batch(problems, forbidden = -1)$cost == -1))
})

test_that("lap_solve_batch honours the forbidden sentinel on 3D array input", {
  arr <- array(0, dim = c(2, 2, 2))
  arr[, , 1] <- matrix(c(-1, 5, 7, 2), 2, 2, byrow = TRUE)
  arr[, , 2] <- matrix(c(4, -1, 1, 6), 2, 2, byrow = TRUE)

  expect_true(any(lap_solve_batch(arr)$cost == -1))
  expect_false(any(lap_solve_batch(arr, forbidden = -1)$cost == -1))
})

# ------------------------------------------------------------------------------
# Auction feasibility reporting (#18)
# ------------------------------------------------------------------------------

test_that("the auction reports infeasibility, not a convergence failure", {
  # Every row has an allowed column and no perfect matching exists: both rows
  # can only take column 1, so they displace each other forever.
  cost <- matrix(c(1, Inf, 1, Inf), nrow = 2, byrow = TRUE)

  for (meth in c("auction", "auction_gs", "auction_scaled")) {
    expect_error(assignment(cost, method = meth),
                 "admit no complete assignment", info = meth)
  }
})

test_that("a feasible constrained problem still solves under the auction", {
  cost <- matrix(c(1, Inf, Inf, 2), nrow = 2, byrow = TRUE)
  for (meth in c("auction", "auction_gs", "auction_scaled")) {
    res <- assignment(cost, method = meth)
    expect_equal(res$match, c(1L, 2L), info = meth)
    expect_equal(res$total_cost, 3, info = meth)
  }
})
