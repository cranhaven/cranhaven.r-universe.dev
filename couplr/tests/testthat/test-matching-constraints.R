# ==============================================================================
# Tests for matching constraints (matching_constraints.R)
# ==============================================================================

test_that("apply_max_distance leaves values below threshold unchanged", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)

  result <- couplr:::apply_max_distance(cost, max_distance = 5)

  expect_equal(result, cost)
})

test_that("apply_max_distance marks values above threshold as forbidden", {
  cost <- matrix(c(1, 5, 10, 2), 2, 2)

  result <- couplr:::apply_max_distance(cost, max_distance = 3)

  expect_equal(result[1, 1], 1)
  expect_equal(result[2, 2], 2)
  expect_true(result[1, 2] > 1e15)  # Forbidden
  expect_true(result[2, 1] > 1e15)  # Forbidden
})

test_that("apply_max_distance handles Inf threshold", {
  cost <- matrix(c(1, 100, 1000, 10000), 2, 2)

  result <- couplr:::apply_max_distance(cost, max_distance = Inf)

  expect_equal(result, cost)
})

test_that("apply_max_distance handles NULL threshold", {
  cost <- matrix(c(1, 100, 1000, 10000), 2, 2)

  result <- couplr:::apply_max_distance(cost, max_distance = NULL)

  expect_equal(result, cost)
})

test_that("apply_max_distance errors on negative threshold", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)

  expect_error(
    couplr:::apply_max_distance(cost, max_distance = -1),
    "must be positive"
  )
})
test_that("apply_max_distance errors on non-numeric threshold", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)

  expect_error(
    couplr:::apply_max_distance(cost, max_distance = "five"),
    "must be a single numeric"
  )
})

test_that("apply_max_distance errors on vector threshold", {
  cost <- matrix(c(1, 2, 3, 4), 2, 2)

  expect_error(
    couplr:::apply_max_distance(cost, max_distance = c(1, 2)),
    "must be a single numeric"
  )
})

test_that("apply_calipers marks violating pairs as forbidden", {
  left <- data.frame(x = c(1, 2, 3), y = c(10, 20, 30))
  right <- data.frame(x = c(1.5, 2.5, 3.5), y = c(15, 25, 35))
  cost <- matrix(1, 3, 3)
  vars <- c("x", "y")
  calipers <- list(y = 3)  # Max difference of 3 on y

  result <- couplr:::apply_calipers(cost, left, right, calipers, vars)

  # Check diagonal should still be valid (y diffs: 5, 5, 5)
  # All pairs should be forbidden since y diff is always 5 > 3
  expect_true(all(result > 1e15))
})

test_that("apply_calipers with NULL returns unchanged", {
  cost <- matrix(1, 3, 3)

  result <- couplr:::apply_calipers(cost, NULL, NULL, NULL, NULL)

  expect_equal(result, cost)
})

test_that("apply_calipers skips variables not in vars", {
  left <- data.frame(x = c(1, 2, 3), z = c(100, 200, 300))
  right <- data.frame(x = c(1.5, 2.5, 3.5), z = c(110, 210, 310))
  cost <- matrix(1, 3, 3)
  vars <- c("x")  # Only x, not z
  calipers <- list(z = 5)  # Caliper on z, but z not in vars

  result <- couplr:::apply_calipers(cost, left, right, calipers, vars)

  # Should be unchanged since z is not in vars
  expect_equal(result, cost)
})

test_that("apply_calipers works with multiple variables", {
  left <- data.frame(x = c(1, 10), y = c(1, 10))
  right <- data.frame(x = c(1.5, 10.5), y = c(1.5, 10.5))
  cost <- matrix(1, 2, 2)
  vars <- c("x", "y")
  calipers <- list(x = 1, y = 1)  # Max diff of 1 on both

  result <- couplr:::apply_calipers(cost, left, right, calipers, vars)

  # Diagonal should be valid, off-diagonal should be forbidden
  expect_equal(result[1, 1], 1)
  expect_equal(result[2, 2], 1)
  expect_true(result[1, 2] > 1e15)
  expect_true(result[2, 1] > 1e15)
})

test_that("mark_forbidden_pairs works with matrix input", {
  cost <- matrix(1, 3, 3)
  forbidden <- matrix(c(1, 2, 2, 3), ncol = 2, byrow = TRUE)

  result <- couplr:::mark_forbidden_pairs(cost, forbidden)

  expect_true(result[1, 2] > 1e15)
  expect_true(result[2, 3] > 1e15)
  expect_equal(result[1, 1], 1)
  expect_equal(result[2, 2], 1)
  expect_equal(result[3, 3], 1)
})

test_that("mark_forbidden_pairs handles NULL", {
  cost <- matrix(1, 3, 3)

  result <- couplr:::mark_forbidden_pairs(cost, NULL)

  expect_equal(result, cost)
})

test_that("mark_forbidden_pairs handles empty matrix", {
  cost <- matrix(1, 3, 3)
  forbidden <- matrix(nrow = 0, ncol = 2)

  result <- couplr:::mark_forbidden_pairs(cost, forbidden)

  expect_equal(result, cost)
})

test_that("apply_all_constraints combines all constraints", {
  left <- data.frame(x = c(1, 10), y = c(1, 10))
  right <- data.frame(x = c(2, 11), y = c(2, 11))
  cost <- matrix(c(1, 5, 5, 1), 2, 2)
  vars <- c("x", "y")

  result <- couplr:::apply_all_constraints(
    cost, left, right, vars,
    max_distance = 3,
    calipers = list(x = 2)
  )

  # (1,1) should be valid: cost=1 < 3, x diff = 1 < 2
  expect_equal(result[1, 1], 1)

  # (2,2) should be valid: cost=1 < 3, x diff = 1 < 2
  expect_equal(result[2, 2], 1)

  # (1,2) should be forbidden: cost=5 > 3
  expect_true(result[1, 2] > 1e15)

  # (2,1) should be forbidden: cost=5 > 3
  expect_true(result[2, 1] > 1e15)
})

test_that("has_valid_pairs returns TRUE when valid pairs exist", {
  cost <- matrix(c(1, Inf, Inf, 1), 2, 2)

  expect_true(couplr:::has_valid_pairs(cost))
})

test_that("has_valid_pairs returns FALSE when no valid pairs", {
  cost <- matrix(couplr:::BIG_COST, 2, 2)

  expect_false(couplr:::has_valid_pairs(cost))
})

test_that("has_valid_pairs handles Inf values", {
  cost <- matrix(Inf, 2, 2)

  expect_false(couplr:::has_valid_pairs(cost))
})

test_that("count_valid_pairs counts correctly", {
  cost <- matrix(c(1, Inf, Inf, 1), 2, 2)

  expect_equal(couplr:::count_valid_pairs(cost), 2)
})

test_that("count_valid_pairs returns 0 for all forbidden", {
  cost <- matrix(Inf, 2, 2)

  expect_equal(couplr:::count_valid_pairs(cost), 0)
})

test_that("count_valid_pairs handles mixed values", {
  cost <- matrix(c(1, 2, couplr:::BIG_COST, 3), 2, 2)

  expect_equal(couplr:::count_valid_pairs(cost), 3)
})

test_that("BIG_COST is very large", {
  expect_true(couplr:::BIG_COST > 1e100)
})

test_that("constraints work end-to-end via match_couples", {
  left <- data.frame(id = 1:3, x = c(1, 2, 3))
  right <- data.frame(id = 4:6, x = c(1.1, 5, 3.1))

  result <- match_couples(left, right, vars = "x", max_distance = 0.5,
                          check_costs = FALSE)

  expect_equal(nrow(result$pairs), 2)
  expect_true(all(result$pairs$distance <= 0.5))
})

test_that("calipers work end-to-end via match_couples", {
  left <- data.frame(
    id = 1:3,
    x = c(1, 2, 3),
    y = c(10, 20, 30)
  )
  right <- data.frame(
    id = 4:6,
    x = c(1.1, 2.1, 3.1),
    y = c(10.5, 25, 30.5)
  )

  result <- match_couples(
    left, right,
    vars = c("x", "y"),
    calipers = list(y = 2),
    check_costs = FALSE
  )

  # Middle pair should be excluded (y diff = 5 > 2)
  expect_equal(nrow(result$pairs), 2)
})

test_that(".is_valid_cost reads NA, Inf and BIG_COST alike", {
  cost <- c(0, 1, NA, Inf, -Inf, couplr:::BIG_COST, couplr:::BIG_COST / 2)
  expect_equal(
    couplr:::.is_valid_cost(cost),
    c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
  )
})

# A cost at or above BIG_COST is finite, so a solver prices it as an ordinary
# expensive edge and returns it when a complete matching demands one. Manhattan,
# because a euclidean distance that large squares to Inf before it gets there.
forbidden_edge_data <- function(design) {
  big <- 1e308
  switch(
    design,
    # L1 and L2 both reach only R1, so one of them has to take a forbidden column.
    one_to_one = list(
      left = data.frame(id = paste0("L", 1:3), x = c(0, 0, big)),
      right = data.frame(id = paste0("R", 1:3), x = c(0.1, big, big))
    ),
    # L1 reaches only R1 and asks for two partners, so its second one is forbidden.
    ratio2 = list(
      left = data.frame(id = c("L1", "L2"), x = c(0, big)),
      right = data.frame(id = paste0("R", 1:4), x = c(0.1, big, big, big))
    ),
    stop("unknown design: ", design)
  )
}

test_that("a pair the optimum was forced onto a forbidden edge is not reported", {
  d <- forbidden_edge_data("one_to_one")

  result <- match_couples(d$left, d$right, vars = "x", distance = "manhattan",
                          check_costs = FALSE)

  expect_true(all(result$pairs$distance < couplr:::BIG_COST))
  expect_equal(nrow(result$pairs), 2L)
  expect_equal(sum(result$pairs$distance), 0.1)
  expect_true("L3" %in% result$pairs$left_id)
  expect_length(result$unmatched$left, 1L)
  expect_length(result$unmatched$right, 1L)
  expect_equal(result$status, "partial")
})

test_that("the k:1 and with-replacement designs drop it too", {
  d <- forbidden_edge_data("ratio2")

  ratio <- match_couples(d$left, d$right, vars = "x", distance = "manhattan",
                         ratio = 2, check_costs = FALSE)
  replaced <- match_couples(d$left, d$right, vars = "x", distance = "manhattan",
                            ratio = 2, replace = TRUE, check_costs = FALSE)

  for (result in list(ratio, replaced)) {
    expect_true(all(result$pairs$distance < couplr:::BIG_COST))
    expect_equal(nrow(result$pairs), 3L)
    expect_equal(sum(result$pairs$distance), 0.1)
    expect_equal(result$status, "partial")
  }
})

test_that("a precomputed distance object drops it on the same fixture", {
  d <- forbidden_edge_data("one_to_one")
  dist_obj <- compute_distances(d$left, d$right, vars = "x",
                                distance = "manhattan")

  result <- match_couples(dist_obj, check_costs = FALSE)

  expect_true(all(result$pairs$distance < couplr:::BIG_COST))
  expect_equal(nrow(result$pairs), 2L)
  expect_equal(sum(result$pairs$distance), 0.1)
  expect_equal(result$status, "partial")
})
