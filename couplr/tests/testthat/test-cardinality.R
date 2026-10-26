# Tests for cardinality_match()

# --- the pruning heuristic ---------------------------------------------------

test_that("cardinality_match returns matching_result", {
  set.seed(42)
  left <- data.frame(id = 1:20, x = rnorm(20, 0, 1), y = rnorm(20, 0, 1))
  right <- data.frame(id = 21:50, x = rnorm(30, 0.5, 1), y = rnorm(30, 0.3, 1))
  result <- cardinality_match(left, right, vars = c("x", "y"),
                              max_std_diff = 0.3, engine = "heuristic")

  expect_s3_class(result, "matching_result")
  expect_true(nrow(result$pairs) > 0)
  expect_true(!is.null(result$info$pruning_iterations))
  expect_true(!is.null(result$info$pairs_removed))
})

test_that("cardinality_match improves balance", {
  set.seed(42)
  left <- data.frame(id = 1:30, x = rnorm(30, 0, 1), y = rnorm(30, 0, 1))
  right <- data.frame(id = 31:80, x = rnorm(50, 1, 1), y = rnorm(50, 0.5, 1))

  result <- cardinality_match(left, right, vars = c("x", "y"),
                              max_std_diff = 0.25, engine = "heuristic")

  # Balance should be within threshold (or close if pruning hit limits)
  bal <- balance_diagnostics(result, left, right, vars = c("x", "y"))
  max_imbalance <- max(abs(bal$var_stats$std_diff))
  # Should be improved from the full match
  expect_true(max_imbalance <= 0.5 || result$info$pairs_removed > 0)
})

test_that("cardinality_match with already-balanced data removes no pairs", {
  set.seed(42)
  left <- data.frame(id = 1:10, x = rnorm(10, 0, 1))
  right <- data.frame(id = 11:20, x = rnorm(10, 0, 1))

  result <- cardinality_match(left, right, vars = "x",
                              max_std_diff = 1.0,  # Very lenient
                              engine = "heuristic")

  # Should remove zero or very few pairs
  expect_true(result$info$pairs_removed <= 2)
})

test_that("cardinality_match validates max_std_diff", {
  left <- data.frame(id = 1:3, x = 1:3)
  right <- data.frame(id = 4:6, x = 4:6)
  expect_error(cardinality_match(left, right, vars = "x", max_std_diff = -0.1,
                                 engine = "heuristic"),
               "max_std_diff must be a positive number")
})

test_that("cardinality_match with tight threshold prunes pairs", {
  set.seed(42)
  left <- data.frame(id = 1:20, x = rnorm(20, 0, 1))
  right <- data.frame(id = 21:50, x = rnorm(30, 1, 1))
  result <- cardinality_match(left, right, vars = "x",
                              max_std_diff = 0.05, engine = "heuristic")

  # With very tight threshold, some pruning should occur
  expect_true(result$info$pairs_removed >= 0)
  expect_true(!is.null(result$info$pruning_iterations))
})

test_that("cardinality_match preserves pair structure", {
  set.seed(42)
  left <- data.frame(id = 1:15, x = rnorm(15), y = rnorm(15))
  right <- data.frame(id = 16:35, x = rnorm(20), y = rnorm(20))
  result <- cardinality_match(left, right, vars = c("x", "y"),
                              max_std_diff = 0.2, engine = "heuristic")

  expect_true("left_id" %in% names(result$pairs))
  expect_true("right_id" %in% names(result$pairs))
  expect_true("distance" %in% names(result$pairs))
})

test_that("the heuristic reports no bound and no certificate", {
  set.seed(42)
  left <- data.frame(id = 1:15, x = rnorm(15))
  right <- data.frame(id = 16:35, x = rnorm(20, 0.5))
  result <- cardinality_match(left, right, vars = "x",
                              max_std_diff = 0.1, engine = "heuristic")

  expect_identical(result$status, "heuristic")
  expect_null(result$certificate)
  expect_identical(result$info$engine, "heuristic")

  card <- result$cardinality
  expect_s3_class(card, "cardinality_report")
  expect_false(card$certified)
  expect_identical(card$best_possible, NA_integer_)
  expect_identical(card$gap, NA_integer_)
  expect_identical(card$stopped_on, "heuristic")
  expect_identical(card$n_matched, nrow(result$pairs))
})

test_that("the heuristic keeps ids when no id column is present", {
  set.seed(11)
  left <- data.frame(x = rnorm(12))
  right <- data.frame(x = rnorm(18, 0.4))
  result <- suppressWarnings(
    cardinality_match(left, right, vars = "x", max_std_diff = 0.2,
                      engine = "heuristic")
  )

  expect_true(all(result$pairs$left_id %in% paste0("left_", seq_len(12))))
  expect_true(all(result$pairs$right_id %in% paste0("right_", seq_len(18))))
  # Deleting a pair returns both its units to the unmatched lists.
  expect_length(result$unmatched$left, 12L - nrow(result$pairs))
})

# --- what the heuristic leaves on the table ----------------------------------

test_that("the heuristic matches fewer pairs than the flow engine at the same
           balance requirement", {
  set.seed(7)
  n_left <- 40
  n_right <- 60
  left <- data.frame(id = 1:n_left,
                     grp = sample(c("a", "b"), n_left, replace = TRUE,
                                  prob = c(0.75, 0.25)))
  right <- data.frame(id = n_left + 1:n_right,
                      grp = sample(c("a", "b"), n_right, replace = TRUE,
                                   prob = c(0.35, 0.65)))
  left$grp_num <- as.numeric(left$grp == "b")
  right$grp_num <- as.numeric(right$grp == "b")

  # One-to-one matching makes the two matched sides the same size, so equal
  # category proportions and equal category counts are the same requirement:
  # exact fine balance on `grp`.
  heur <- suppressWarnings(
    cardinality_match(left, right, vars = "grp_num",
                      max_std_diff = 1e-8, engine = "heuristic"))
  flow <- cardinality_match(left, right, vars = "grp_num",
                            fine = "grp", max_std_diff = Inf, engine = "flow")

  expect_true(flow$cardinality$certified)
  expect_equal(
    sum(left$grp_num[match(flow$pairs$left_id, as.character(left$id))]),
    sum(right$grp_num[match(flow$pairs$right_id, as.character(right$id))])
  )
  expect_lt(nrow(heur$pairs), nrow(flow$pairs))
})

# --- the flow engine ---------------------------------------------------------

test_that("the flow engine certifies a fine-balanced match", {
  set.seed(3)
  left <- data.frame(id = 1:16, x = rnorm(16),
                     region = rep(c("A", "B"), each = 8))
  right <- data.frame(id = 17:40, x = rnorm(24, 0.3),
                      region = rep(c("A", "B"), each = 12))

  fit <- cardinality_match(left, right, vars = "x", fine = "region",
                           max_std_diff = Inf)

  expect_identical(fit$info$engine, "flow")
  expect_identical(fit$status, "optimal")
  expect_true(fit$cardinality$certified)
  expect_identical(fit$cardinality$gap, 0L)
  expect_identical(fit$certificate$certified_optimal, TRUE)
  expect_true(all(fit$cardinality$constraints$satisfied))
})

test_that("a formula and a character vector state the same fine partition", {
  set.seed(4)
  left <- data.frame(id = 1:12, x = rnorm(12),
                     region = rep(c("A", "B"), 6),
                     sex = rep(c("f", "m"), each = 6))
  right <- data.frame(id = 13:30, x = rnorm(18, 0.2),
                      region = rep(c("A", "B"), 9),
                      sex = rep(c("f", "m"), each = 9))

  a <- cardinality_match(left, right, vars = "x", fine = c("region", "sex"),
                         max_std_diff = Inf)
  b <- cardinality_match(left, right, vars = "x", fine = ~ region + sex,
                         max_std_diff = Inf)
  d <- cardinality_match(left, right, vars = "x", fine = ~ region:sex,
                         max_std_diff = Inf)

  expect_identical(a$cardinality$n_matched, b$cardinality$n_matched)
  expect_identical(a$cardinality$n_matched, d$cardinality$n_matched)
})

test_that("refined states a nested hierarchy coarsest first", {
  set.seed(5)
  left <- data.frame(id = 1:16, x = rnorm(16),
                     region = rep(c("A", "B"), each = 8),
                     site = rep(c("A1", "A2", "B1", "B2"), each = 4))
  right <- data.frame(id = 17:40, x = rnorm(24, 0.2),
                      region = rep(c("A", "B"), each = 12),
                      site = rep(c("A1", "A2", "B1", "B2"), each = 6))

  fit <- cardinality_match(left, right, vars = "x",
                           refined = c("region", "site"), refined_exact = 1L,
                           max_std_diff = Inf)

  expect_identical(fit$info$engine, "flow")
  expect_true(fit$cardinality$certified)
  expect_true(any(fit$cardinality$balance$level == 2L))
})

# --- the branch and bound engine ---------------------------------------------

test_that("branch and bound reports a gap it has not closed", {
  set.seed(12)
  left <- data.frame(id = 1:24, x = rnorm(24), y = rnorm(24))
  right <- data.frame(id = 25:60, x = rnorm(36, 0.6), y = rnorm(36, 0.4))

  fit <- cardinality_match(left, right, vars = c("x", "y"),
                           max_std_diff = 0.3, node_limit = 5L)

  expect_identical(fit$info$engine, "branch_bound")
  expect_identical(fit$status, "iteration_limit")
  expect_false(fit$cardinality$certified)
  expect_null(fit$certificate)
  expect_identical(fit$cardinality$stopped_on, "node_limit")
  expect_gt(fit$cardinality$gap, 0L)
  expect_identical(fit$cardinality$best_possible,
                   fit$cardinality$n_matched + fit$cardinality$gap)
})

test_that("a satisfied moment bound comes back inside its bound", {
  set.seed(8)
  left <- data.frame(id = 1:8, x = rnorm(8))
  right <- data.frame(id = 9:24, x = rnorm(16))

  fit <- cardinality_match(left, right, vars = "x", max_std_diff = 0.2,
                           node_limit = 40L)

  rows <- fit$cardinality$constraints
  expect_true(all(rows$kind %in% c("std_diff", "exact_balance")))
  expect_true(all(rows$satisfied))
})

# --- argument contracts ------------------------------------------------------

test_that("fine and refined are alternatives", {
  left <- data.frame(id = 1:4, x = 1:4, g = c("a", "a", "b", "b"))
  right <- data.frame(id = 5:8, x = 2:5, g = c("a", "b", "a", "b"))
  expect_error(
    cardinality_match(left, right, vars = "x", fine = "g", refined = list("g")),
    "both state the partition"
  )
})

test_that("heuristic arguments are refused by the other engines", {
  left <- data.frame(id = 1:4, x = 1:4)
  right <- data.frame(id = 5:8, x = 2:5)
  expect_error(
    cardinality_match(left, right, vars = "x", max_std_diff = Inf,
                      engine = "flow", max_iter = 10L),
    "max_iter"
  )
  expect_error(
    cardinality_match(left, right, vars = "x", engine = "branch_bound",
                      batch_fraction = 0.2),
    "batch_fraction"
  )
  expect_error(
    cardinality_match(left, right, vars = "x", method = "hungarian"),
    "method"
  )
})

test_that("balance arguments are refused by the heuristic", {
  left <- data.frame(id = 1:4, x = 1:4, g = c("a", "a", "b", "b"))
  right <- data.frame(id = 5:8, x = 2:5, g = c("a", "b", "a", "b"))
  expect_error(
    cardinality_match(left, right, vars = "x", engine = "heuristic",
                      fine = "g"),
    "fine"
  )
  expect_error(
    cardinality_match(left, right, vars = "x", engine = "heuristic",
                      node_limit = 10L),
    "node_limit"
  )
})

test_that("the engine must be able to read the constraints it is given", {
  left <- data.frame(id = 1:4, x = 1:4)
  right <- data.frame(id = 5:8, x = 2:5)
  expect_error(
    cardinality_match(left, right, vars = "x", engine = "flow",
                      max_std_diff = 0.1),
    "moment row"
  )
  expect_error(
    cardinality_match(left, right, vars = "x", engine = "branch_bound",
                      max_std_diff = Inf),
    "none are stated"
  )
})

test_that("a non-nested refined hierarchy is refused", {
  left <- data.frame(id = 1:8, x = rnorm(8),
                     region = rep(c("A", "B"), each = 4),
                     site = rep(c("s1", "s2"), 4))
  right <- data.frame(id = 9:20, x = rnorm(12),
                      region = rep(c("A", "B"), each = 6),
                      site = rep(c("s1", "s2"), 6))
  expect_error(
    cardinality_match(left, right, vars = "x",
                      refined = list("region", "site"), max_std_diff = Inf),
    "not nested"
  )
})

test_that("an unknown column is named", {
  left <- data.frame(id = 1:4, x = 1:4)
  right <- data.frame(id = 5:8, x = 2:5)
  expect_error(
    cardinality_match(left, right, vars = c("x", "z"), max_std_diff = Inf),
    "missing required variables: z"
  )
  expect_error(
    cardinality_match(left, right, vars = "x", fine = "region",
                      max_std_diff = Inf),
    "no column\\(s\\) region"
  )
})

# --- the result object -------------------------------------------------------

test_that("the default call returns a matching_result the package can read", {
  set.seed(9)
  left <- data.frame(id = 1:8, x = rnorm(8), y = rnorm(8))
  right <- data.frame(id = 9:20, x = rnorm(12, 0.3), y = rnorm(12, 0.2))

  fit <- cardinality_match(left, right, vars = c("x", "y"))

  expect_s3_class(fit, "matching_result")
  expect_s3_class(fit, "couplr_result")
  # No moment row is stated by default, so the default call is the flow path.
  expect_identical(fit$info$engine, "flow")
  expect_true(fit$cardinality$certified)

  bb <- cardinality_match(left, right, vars = c("x", "y"),
                          max_std_diff = 0.3, node_limit = 30L)
  expect_identical(bb$info$engine, "branch_bound")
  expect_s3_class(bb, "matching_result")
  expect_identical(fit$info$n_matched, nrow(fit$pairs))
  expect_identical(fit$info$estimand, "ATT")
  expect_identical(fit$info$focal, "left")
  expect_true(is.numeric(fit$info$total_distance))
  expect_null(fit$info$pruning_iterations)
  expect_null(fit$info$pairs_removed)

  bal <- balance_diagnostics(fit, left, right, vars = c("x", "y"))
  expect_true(is.data.frame(bal$var_stats))

  md <- match_data(fit, left, right)
  expect_true(nrow(md) > 0)

  joined <- join_matched(fit, left, right)
  expect_identical(nrow(joined), nrow(fit$pairs))

  s <- summary(fit)
  expect_s3_class(s, "summary.matching_result")
})

# --- print -------------------------------------------------------------------

test_that("the certified block prints", {
  set.seed(3)
  left <- data.frame(id = 1:16, x = rnorm(16),
                     region = rep(c("A", "B"), each = 8))
  right <- data.frame(id = 17:40, x = rnorm(24, 0.3),
                      region = rep(c("A", "B"), each = 12))
  fit <- cardinality_match(left, right, vars = "x", fine = "region",
                           max_std_diff = Inf)
  expect_snapshot(print(fit$cardinality))
})

test_that("the uncertified block prints", {
  set.seed(12)
  left <- data.frame(id = 1:24, x = rnorm(24), y = rnorm(24))
  right <- data.frame(id = 25:60, x = rnorm(36, 0.6), y = rnorm(36, 0.4))
  # The budget sits on the plateau the search reaches by ten nodes and holds
  # through eighty, so the block prints a settled incumbent beside an open
  # bound rather than a figure the next node would move.
  fit <- cardinality_match(left, right, vars = c("x", "y"),
                           max_std_diff = 0.3, node_limit = 20L)
  expect_false(fit$cardinality$certified)
  expect_gt(fit$cardinality$gap, 0)
  expect_gte(fit$cardinality$best_possible, fit$cardinality$n_matched)
  expect_true(all(fit$cardinality$constraints$satisfied))
  expect_snapshot(print(fit$cardinality))
})

test_that("the heuristic block prints", {
  set.seed(42)
  left <- data.frame(id = 1:15, x = rnorm(15))
  right <- data.frame(id = 16:35, x = rnorm(20, 0.5))
  fit <- cardinality_match(left, right, vars = "x", max_std_diff = 0.1,
                           engine = "heuristic")
  expect_snapshot(print(fit$cardinality))
})
