# full_match(memory_mode = "implicit"): the design loop behind the front door.
#
# The Catch2 cases in cpp_tests/tests/test_flow_implicit.cpp own the loop over
# a compiled design. What this file covers is the layer above it: the caliper
# arriving as the source's distance cut, caliper_sd read in one pass, the groups
# read back from the pairs the search held, and the certificate and search
# record where a caller finds them. Every comparison is against the dense solve
# of the same call, on the groups themselves, since the costs are continuous and
# an optimal grouping is then unique.

full_match_units <- function(n, p, seed) {
  set.seed(seed)
  data.frame(id = paste0("u", seed, "_", seq_len(n)), matrix(stats::rnorm(n * p), n, p))
}

full_match_groups_key <- function(res) {
  g <- res$groups
  if (!nrow(g)) return(character(0))
  sort(vapply(split(g, g$group_id),
              function(x) paste(sort(paste(x$side, x$id)), collapse = ","),
              character(1)), method = "radix")
}

expect_full_match_agrees <- function(left, right, vars, ...) {
  dense <- full_match(left, right, vars = vars, memory_mode = "dense", ...)
  loop <- full_match(left, right, vars = vars, memory_mode = "implicit", ...)
  expect_identical(loop$status, dense$status)
  expect_identical(full_match_groups_key(loop), full_match_groups_key(dense))
  expect_identical(loop$unmatched, dense$unmatched)
  expect_equal(loop$groups$weight[order(loop$groups$id)],
               dense$groups$weight[order(dense$groups$id)])
  invisible(list(dense = dense, loop = loop))
}

test_that("an implicit full matching is the dense one, in both group shapes", {
  vars <- c("X1", "X2", "X3")
  wide <- expect_full_match_agrees(full_match_units(30, 3, 1), full_match_units(90, 3, 2), vars)
  expect_true(wide$loop$certificate$certified_optimal)
  expect_equal(wide$loop$certificate$primal_objective,
               wide$dense$certificate$primal_objective, tolerance = 1e-9)

  tall <- expect_full_match_agrees(full_match_units(90, 3, 3), full_match_units(30, 3, 4), vars)
  expect_true(tall$loop$certificate$certified_optimal)
})

test_that("bounds, Mahalanobis and scaling reach the dense groups", {
  vars <- c("X1", "X2", "X3", "X4")
  left <- full_match_units(30, 4, 5)
  right <- full_match_units(100, 4, 6)
  expect_full_match_agrees(left, right, vars, min_controls = 2, max_controls = 4)
  expect_full_match_agrees(left, right, vars, max_controls = 3)
  expect_full_match_agrees(left, right, vars, distance = "mahalanobis")
  expect_full_match_agrees(left, right, vars, scale = "standardize",
                           weights = c(X1 = 2, X2 = 1, X3 = 1, X4 = 0.5))
})

test_that("a caliper that leaves units out is the dense partial answer", {
  vars <- c("X1", "X2", "X3")
  left <- full_match_units(50, 3, 7)
  right <- full_match_units(150, 3, 8)

  by_value <- expect_full_match_agrees(left, right, vars, caliper = 0.8)
  expect_identical(by_value$loop$status, "partial")
  expect_true(by_value$loop$certificate$max_flow_certified)

  by_sd <- expect_full_match_agrees(left, right, vars, caliper_sd = 0.5)
  expect_identical(by_sd$loop$status, "partial")

  expect_full_match_agrees(left, right, vars, min_controls = 2, caliper = 1.0)
})

test_that("the search record says the loop held a fraction of the pairs", {
  vars <- c("X1", "X2", "X3", "X4", "X5")
  res <- full_match(full_match_units(200, 5, 9), full_match_units(600, 5, 10),
                    vars = vars, max_controls = 5, memory_mode = "implicit")
  expect_true(res$certificate$certified_optimal)
  expect_equal(res$search$possible_edges, 200 * 600)
  expect_lt(res$search$candidate_edges, res$search$possible_edges / 4)
  expect_s3_class(res$search$rounds, "tbl_df")
  expect_equal(nrow(res$search$rounds), res$search$n_rounds)
  expect_true(res$certificate$omitted_proven_floor >= -res$certificate$omitted_tolerance)
})

test_that("bounds no assignment can meet are refused before the loop runs", {
  vars <- c("X1", "X2")
  res <- full_match(full_match_units(10, 2, 11), full_match_units(12, 2, 12),
                    vars = vars, min_controls = 2, memory_mode = "implicit")
  expect_identical(res$status, "infeasible")
  expect_equal(res$info$n_groups, 0L)
})

test_that("the greedy method and the lazy mode are declined", {
  left <- full_match_units(10, 2, 13)
  right <- full_match_units(20, 2, 14)
  expect_error(
    full_match(left, right, vars = c("X1", "X2"), method = "greedy",
               memory_mode = "implicit"),
    "not supported"
  )
  expect_error(
    full_match(left, right, vars = c("X1", "X2"), memory_mode = "lazy"),
    "not supported"
  )
})
