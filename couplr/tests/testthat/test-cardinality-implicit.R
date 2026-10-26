# cardinality_match(memory_mode = "implicit"): the balance network solved over
# generated pairs.
#
# Every comparison is against the dense solve of the same call, on the matched
# set itself, since the distances are continuous and an optimal set is then
# unique. The fixtures put the pairs an optimum needs outside every unit's
# nearest neighbours, so the solves only reach the dense answer by pricing them
# in.

cardinality_units <- function(n, seed, shift = 0, regions = c("A", "B")) {
  set.seed(seed)
  data.frame(id = paste0("u", seed, "_", seq_len(n)),
             x = stats::rnorm(n, shift), y = stats::rnorm(n, shift / 2),
             region = sample(regions, n, replace = TRUE),
             site = sample(c("s1", "s2"), n, replace = TRUE))
}

cardinality_key <- function(res) sort(paste(res$pairs$left_id, res$pairs$right_id))

expect_cardinality_agrees <- function(left, right, vars, ...) {
  dense <- suppressWarnings(cardinality_match(left, right, vars = vars,
                                              memory_mode = "dense", ...))
  loop <- suppressWarnings(cardinality_match(left, right, vars = vars,
                                             memory_mode = "implicit", ...))
  expect_identical(cardinality_key(loop), cardinality_key(dense))
  expect_equal(loop$info$total_distance, dense$info$total_distance, tolerance = 1e-9)
  expect_identical(loop$cardinality$certified, dense$cardinality$certified)
  expect_identical(loop$cardinality$n_matched, dense$cardinality$n_matched)
  invisible(list(dense = dense, loop = loop))
}

test_that("fine balance whose partners are nobody's nearest is priced in", {
  # Every left unit is in region A, and the region-A units on the right sit far
  # from them behind a crowd of nearer region-B units, so exact balance on
  # region needs pairs no nearest-neighbour seed holds.
  set.seed(101)
  left <- data.frame(id = paste0("L", 1:25), x = stats::rnorm(25), y = stats::rnorm(25),
                     region = "A")
  near <- data.frame(id = paste0("N", 1:300), x = stats::rnorm(300), y = stats::rnorm(300),
                     region = "B")
  far <- data.frame(id = paste0("F", 1:40), x = stats::rnorm(40, 6), y = stats::rnorm(40, 6),
                    region = "A")
  right <- rbind(near, far)

  res <- expect_cardinality_agrees(left, right, c("x", "y"), fine = "region")
  expect_true(res$loop$cardinality$certified)
  expect_equal(res$loop$cardinality$n_matched, 25L)
  expect_true(all(startsWith(res$loop$pairs$right_id, "F")))
  expect_gt(res$loop$search$candidate_edges,
            25 * res$loop$search$seed_width)
  expect_lt(res$loop$search$candidate_edges, res$loop$search$possible_edges)
})

test_that("refined balance, calipers and a distance cut reach the dense set", {
  left <- cardinality_units(40, 102, shift = 0.6)
  right <- cardinality_units(300, 103)
  vars <- c("x", "y")
  expect_cardinality_agrees(left, right, vars, refined = c("region", "site"))
  expect_cardinality_agrees(left, right, vars, fine = "region", max_distance = 1.0)
  expect_cardinality_agrees(left, right, vars, fine = "site", calipers = c(x = 0.4))
})

test_that("moment constraints searched by branch and bound reach the dense set", {
  left <- cardinality_units(30, 104, shift = 1)
  right <- cardinality_units(250, 105)
  vars <- c("x", "y")
  root <- expect_cardinality_agrees(left, right, vars, max_std_diff = 0.1,
                                    node_limit = 25L)
  expect_identical(root$loop$cardinality$stopped_on, root$dense$cardinality$stopped_on)
  expect_cardinality_agrees(left, right, vars, fine = "region",
                            moments = c(x = 0.05), node_limit = 25L)
})

test_that("the heuristic engine declines the implicit mode", {
  left <- cardinality_units(10, 106)
  right <- cardinality_units(30, 107)
  expect_error(
    cardinality_match(left, right, vars = c("x", "y"), engine = "heuristic",
                      memory_mode = "implicit"),
    "does not support memory_mode"
  )
  expect_error(
    cardinality_match(left, right, vars = c("x", "y"), memory_mode = "lazy"),
    "not supported"
  )
})
