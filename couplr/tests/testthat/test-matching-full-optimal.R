# ==============================================================================
# Tests for Optimal Full Matching (min-cost max-flow)
# ==============================================================================

test_that("optimal full_match produces valid output structure", {
  set.seed(42)
  left <- data.frame(id = 1:5, age = c(25, 35, 45, 55, 65))
  right <- data.frame(id = 6:20, age = runif(15, 20, 70))

  result <- full_match(left, right, vars = "age", method = "optimal")

  expect_s3_class(result, "full_matching_result")
  expect_s3_class(result, "couplr_result")
  expect_true(result$info$n_groups > 0)
  expect_true(result$info$n_groups <= nrow(left))
  expect_equal(result$info$n_left, nrow(left))
  expect_equal(result$info$n_right, nrow(right))
  expect_true(all(c("group_id", "id", "side", "weight") %in% names(result$groups)))
  expect_equal(result$info$method, "full_optimal")
})


test_that("optimal full_match assigns all units when no caliper", {
  set.seed(42)
  left <- data.frame(id = 1:3, x = c(1, 2, 3))
  right <- data.frame(id = 4:8, x = c(1.1, 2.1, 3.1, 4.1, 5.1))

  result <- full_match(left, right, vars = "x", method = "optimal")

  expect_equal(result$info$n_unmatched_left, 0)
  expect_equal(result$info$n_unmatched_right, 0)
  expect_equal(nrow(result$groups), 8)  # 3 left + 5 right
})


test_that("optimal produces equal or better total distance than greedy", {
  set.seed(123)
  left <- data.frame(id = 1:4, x = c(1, 3, 7, 10))
  right <- data.frame(id = 5:12, x = c(1.5, 2.5, 3.5, 6, 7.5, 8, 9.5, 11))

  result_opt <- full_match(left, right, vars = "x", method = "optimal")
  result_greedy <- full_match(left, right, vars = "x", method = "greedy")

  # Compute total distance for each method
  total_dist <- function(result, left, right) {
    total <- 0
    for (g in unique(result$groups$group_id)) {
      grp <- result$groups[result$groups$group_id == g, ]
      l_ids <- grp$id[grp$side == "left"]
      r_ids <- grp$id[grp$side == "right"]
      l_vals <- left$x[left$id %in% as.integer(l_ids)]
      r_vals <- right$x[right$id %in% as.integer(r_ids)]
      for (lv in l_vals) {
        for (rv in r_vals) {
          total <- total + abs(lv - rv)
        }
      }
    }
    total
  }

  dist_opt <- total_dist(result_opt, left, right)
  dist_greedy <- total_dist(result_greedy, left, right)

  # Optimal should be <= greedy (or equal)
  expect_true(dist_opt <= dist_greedy + 1e-8)
})


test_that("optimal full_match respects caliper", {
  left <- data.frame(id = 1:3, x = c(1, 5, 100))
  right <- data.frame(id = 4:6, x = c(1.1, 5.1, 6))

  result <- full_match(left, right, vars = "x", caliper = 2, method = "optimal")

  # Unit with x=100 should be unmatched
  expect_true(result$info$n_unmatched_left > 0)
})


test_that("optimal full_match respects max_controls", {
  set.seed(42)
  left <- data.frame(id = 1:2, x = c(1, 10))
  right <- data.frame(id = 3:12, x = c(1.1, 1.2, 1.3, 1.4, 1.5,
                                        10.1, 10.2, 10.3, 10.4, 10.5))

  result <- full_match(left, right, vars = "x", max_controls = 3,
                       method = "optimal")

  right_per_group <- table(result$groups$group_id[result$groups$side == "right"])
  expect_true(all(right_per_group <= 3))
})


test_that("optimal full_match min_controls filters small groups", {
  left <- data.frame(id = 1:3, x = c(1, 50, 100))
  right <- data.frame(id = 4:6, x = c(1.1, 50.1, 50.2))

  # Three left units each needing two right ones is six against three, so
  # the request cannot be met. Guarding the assertion on a group count made
  # this an empty test on exactly the case it names.
  result <- full_match(left, right, vars = "x", min_controls = 2,
                       method = "optimal")
  expect_identical(result$status, "infeasible")
  expect_equal(result$info$n_groups, 0)

  right2 <- data.frame(id = 4:9,
                       x = c(1.1, 1.2, 50.1, 50.2, 100.1, 100.2))
  ok <- full_match(left, right2, vars = "x", min_controls = 2,
                   method = "optimal")
  expect_gt(ok$info$n_groups, 0)
  right_per_group <- table(ok$groups$group_id[ok$groups$side == "right"])
  expect_true(all(right_per_group >= 2))
})


test_that("optimal full_match weights are correct", {
  left <- data.frame(id = 1:2, x = c(1, 10))
  right <- data.frame(id = 3:6, x = c(1.1, 1.2, 10.1, 10.2))

  result <- full_match(left, right, vars = "x", method = "optimal")

  # Left units always get weight 1
  left_weights <- result$groups$weight[result$groups$side == "left"]
  expect_true(all(left_weights == 1))

  # Right weights within a group should sum to left weight sum
  for (g in unique(result$groups$group_id)) {
    grp <- result$groups[result$groups$group_id == g, ]
    right_w <- sum(grp$weight[grp$side == "right"])
    left_w <- sum(grp$weight[grp$side == "left"])
    expect_equal(right_w, left_w, tolerance = 1e-10)
  }
})


test_that("optimal full_match handles n_left > n_right (transpose)", {
  set.seed(42)
  left <- data.frame(id = 1:10, x = rnorm(10))
  right <- data.frame(id = 11:13, x = rnorm(3))

  result <- full_match(left, right, vars = "x", method = "optimal")

  expect_s3_class(result, "full_matching_result")
  expect_equal(result$info$n_unmatched_left, 0)
  expect_equal(result$info$n_unmatched_right, 0)
  # Should have 3 groups (one per right unit as center)
  expect_true(result$info$n_groups > 0)
  expect_true(result$info$n_groups <= 3)
})


test_that("optimal full_match handles equal sizes (1:1)", {
  left <- data.frame(id = 1:3, x = c(1, 5, 10))
  right <- data.frame(id = 4:6, x = c(1.1, 5.1, 10.1))

  result <- full_match(left, right, vars = "x", method = "optimal")

  expect_equal(result$info$n_groups, 3)
  expect_equal(result$info$n_unmatched_left, 0)
  expect_equal(result$info$n_unmatched_right, 0)

  # Each group should have exactly 1 left and 1 right
  for (g in unique(result$groups$group_id)) {
    grp <- result$groups[result$groups$group_id == g, ]
    expect_equal(sum(grp$side == "left"), 1)
    expect_equal(sum(grp$side == "right"), 1)
  }
})


test_that("optimal full_match handles 1x1 edge case", {
  left <- data.frame(id = 1, x = 5)
  right <- data.frame(id = 2, x = 5.5)

  result <- full_match(left, right, vars = "x", method = "optimal")

  expect_equal(result$info$n_groups, 1)
  expect_equal(nrow(result$groups), 2)
})


test_that("optimal full_match handles single left unit", {
  left <- data.frame(id = 1, x = 5)
  right <- data.frame(id = 2:6, x = c(4, 5.1, 6, 7, 8))

  result <- full_match(left, right, vars = "x", method = "optimal")

  expect_equal(result$info$n_groups, 1)
  expect_equal(result$info$n_unmatched_left, 0)
  expect_equal(result$info$n_unmatched_right, 0)
})


test_that("greedy method parameter preserved", {
  set.seed(42)
  left <- data.frame(id = 1:5, age = c(25, 35, 45, 55, 65))
  right <- data.frame(id = 6:20, age = runif(15, 20, 70))

  result <- full_match(left, right, vars = "age", method = "greedy")

  expect_s3_class(result, "full_matching_result")
  expect_equal(result$info$method, "full_greedy")
  expect_true(result$info$n_groups > 0)
})


test_that("optimal full_match with multivariate works", {
  set.seed(42)
  left <- data.frame(id = 1:5, x = rnorm(5), y = rnorm(5))
  right <- data.frame(id = 6:15, x = rnorm(10), y = rnorm(10))

  result <- full_match(left, right, vars = c("x", "y"), method = "optimal")

  expect_s3_class(result, "full_matching_result")
  expect_true(result$info$n_groups > 0)
  expect_equal(result$info$n_unmatched_left, 0)
  expect_equal(result$info$n_unmatched_right, 0)
})


test_that("optimal full_match known optimal solution", {
  # Small problem where optimal solution is deterministic:
  # Left: x = 0, 10
  # Right: x = 1, 2, 9, 11
  # Optimal: group1 = {L1, R1, R2}, group2 = {L2, R3, R4}
  # Total distance: |0-1| + |0-2| + |10-9| + |10-11| = 1+2+1+1 = 5
  # Greedy might assign R1->L1, R3->L2 first, then R2->L1, R4->L2 = same
  # But with different data, greedy can fail
  left <- data.frame(id = 1:2, x = c(0, 10))
  right <- data.frame(id = 3:6, x = c(1, 2, 9, 11))

  result <- full_match(left, right, vars = "x", method = "optimal")

  expect_equal(result$info$n_groups, 2)
  expect_equal(result$info$n_unmatched_left, 0)
  expect_equal(result$info$n_unmatched_right, 0)

  # Each group should have 1 left and 2 right
  for (g in unique(result$groups$group_id)) {
    grp <- result$groups[result$groups$group_id == g, ]
    expect_equal(sum(grp$side == "left"), 1)
    expect_equal(sum(grp$side == "right"), 2)
  }
})

test_that("min_controls counts right units whichever side is larger", {
  # Six left units against two right ones cannot give every left unit two
  # right partners, so the request is infeasible. Compiling the smaller side
  # as the group centres instead answered a different question: it returned
  # two groups of three left units and one right unit each, with status
  # "optimal", against a stated minimum of two right units per group.
  left  <- data.frame(id = paste0("L", 1:6), x = c(0, 0.1, 0.2, 5, 5.1, 5.2))
  right <- data.frame(id = paste0("R", 1:2), x = c(0.1, 5.1))

  res <- full_match(left, right, vars = "x", min_controls = 2)
  expect_identical(res$status, "infeasible")
  expect_equal(nrow(res$groups), 0L)

  # The orientation that can satisfy it still does, and every group holds at
  # least the requested number of right units.
  ok <- full_match(right, left, vars = "x", min_controls = 2)
  expect_identical(ok$status, "optimal")
  for (g in unique(ok$groups$group_id)) {
    grp <- ok$groups[ok$groups$group_id == g, ]
    expect_equal(sum(grp$side == "left"), 1)
    expect_gte(sum(grp$side == "right"), 2)
  }
})

test_that("full_match() refuses negative distances rather than mis-solving them", {
  # A cheapest edge cover is inclusion-minimal only under non-negative costs.
  # With -100 on the diagonal and -1 off it the solve takes all four arcs and
  # the minimality prune then drops the diagonal, returning the off-diagonal
  # pairing at -2 and reporting status "optimal" against a full matching that
  # costs -200.
  left  <- data.frame(id = c("a", "b"), x = c(0, 1))
  right <- data.frame(id = c("c", "d"), x = c(0, 1))
  neg <- function(lm, rm) {
    outer(lm[, 1], rm[, 1], function(u, v) ifelse(u == v, -100, -1))
  }
  expect_error(full_match(left, right, vars = "x", distance = neg),
               "non-negative distances")

  # A shifted version of the same function is accepted and solved.
  shifted <- function(lm, rm) {
    outer(lm[, 1], rm[, 1], function(u, v) ifelse(u == v, 0, 99))
  }
  ok <- full_match(left, right, vars = "x", distance = shifted)
  expect_identical(ok$status, "optimal")
  expect_equal(sum(ok$groups$side == "left"), 2)
})

test_that("a group centred on a right unit weighs it by the leaves it carries", {
  # More left units than right, so at min_controls = 1 the groups are centred
  # on the right units and a group is k left units around one right unit. Every
  # left unit weighs 1 and the group's right unit weighs k, so the two sides of
  # a group weigh the same.
  set.seed(2)
  left  <- data.frame(id = paste0("L", 1:9), age = runif(9, 20, 70))
  right <- data.frame(id = paste0("R", 1:3), age = runif(3, 20, 70))
  res <- full_match(left, right, vars = "age")
  expect_identical(res$status, "optimal")

  g <- res$groups
  expect_true(all(g$weight[g$side == "left"] == 1))
  for (gid in unique(g$group_id)) {
    grp <- g[g$group_id == gid, ]
    n_left  <- sum(grp$side == "left")
    n_right <- sum(grp$side == "right")
    expect_equal(sum(grp$weight[grp$side == "right"]), n_left)
    expect_equal(grp$weight[grp$side == "right"],
                 rep(n_left / n_right, n_right))
  }

  # The fixture has to hold the shape the test is about.
  expect_true(any(tapply(g$side, g$group_id, function(s) sum(s == "left")) > 1))
})
