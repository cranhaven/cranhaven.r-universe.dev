# A user-supplied distance function on the lazy and implicit paths.
#
# The function is called on a block of left rows against every right unit, and
# every comparison here is against the dense path calling it once on the whole
# problem. The distances are continuous, so an optimal set is unique and the
# comparisons are on the pairs themselves.

callback_units <- function(n, seed, shift = 0) {
  set.seed(seed)
  data.frame(id = paste0("u", seed, "_", seq_len(n)),
             x = stats::rnorm(n, shift), y = stats::rnorm(n), z = stats::rnorm(n),
             region = sample(c("A", "B"), n, replace = TRUE))
}

# Weighted city-block distance, written the way a user would: vectorized over
# the two matrices it is handed.
weighted_l1 <- function(l, r) {
  w <- c(1, 0.5, 2)
  out <- matrix(0, nrow(l), nrow(r))
  for (k in seq_len(ncol(l))) out <- out + w[k] * abs(outer(l[, k], r[, k], "-"))
  out
}

pair_key <- function(res) sort(paste(res$pairs$left_id, res$pairs$right_id))

test_that("a distance function reaches the dense pairs on the lazy and implicit paths", {
  left <- callback_units(40, 201)
  right <- callback_units(160, 202)
  vars <- c("x", "y", "z")
  dense <- match_couples(left, right, vars = vars, distance = weighted_l1,
                         memory_mode = "dense")
  for (mode in c("lazy", "implicit")) {
    got <- match_couples(left, right, vars = vars, distance = weighted_l1,
                         memory_mode = mode)
    expect_identical(pair_key(got), pair_key(dense), info = mode)
    expect_equal(sum(got$pairs$distance), sum(dense$pairs$distance),
                 tolerance = 1e-9, info = mode)
  }
  implicit <- match_couples(left, right, vars = vars, distance = weighted_l1,
                            memory_mode = "implicit")
  expect_true(implicit$certificate$certified_optimal)
})

test_that("an asymmetric function survives the transposition a tall problem takes", {
  # f(l, r) != f(r, l): a shortfall on x counts three times an excess.
  directed <- function(l, r) {
    d <- outer(l[, 1], r[, 1], "-")
    ifelse(d > 0, d, -3 * d) + abs(outer(l[, 2], r[, 2], "-"))
  }
  left <- callback_units(60, 203)
  right <- callback_units(25, 204)
  vars <- c("x", "y")
  dense <- match_couples(left, right, vars = vars, distance = directed,
                         memory_mode = "dense")
  for (mode in c("lazy", "implicit")) {
    got <- match_couples(left, right, vars = vars, distance = directed,
                         memory_mode = mode)
    expect_identical(pair_key(got), pair_key(dense), info = mode)
  }
  auction <- match_couples(left, right, vars = vars, distance = directed,
                           method = "auction", memory_mode = "lazy")
  expect_equal(sum(auction$pairs$distance), sum(dense$pairs$distance),
               tolerance = 1e-6)
})

test_that("replace, ratio and constraints run on a distance function", {
  left <- callback_units(25, 205)
  right <- callback_units(120, 206)
  vars <- c("x", "y", "z")
  for (args in list(list(replace = TRUE, ratio = 2L), list(ratio = 2L),
                    list(max_distance = 3), list(calipers = c(x = 0.5)))) {
    dense <- suppressWarnings(do.call(match_couples, c(
      list(left, right, vars = vars, distance = weighted_l1, memory_mode = "dense"),
      args)))
    got <- suppressWarnings(do.call(match_couples, c(
      list(left, right, vars = vars, distance = weighted_l1,
           memory_mode = "implicit"), args)))
    expect_identical(pair_key(got), pair_key(dense),
                     info = paste(names(args), args, collapse = " "))
  }
})

test_that("full_match and cardinality_match run on a distance function", {
  left <- callback_units(30, 207)
  right <- callback_units(90, 208)
  vars <- c("x", "y", "z")
  groups <- function(res) {
    g <- res$groups
    sort(unname(vapply(split(g, g$group_id),
                       function(x) paste(sort(paste(x$side, x$id)), collapse = ","), "")))
  }
  fm_dense <- full_match(left, right, vars = vars, distance = weighted_l1,
                         max_controls = 4, memory_mode = "dense")
  fm_loop <- full_match(left, right, vars = vars, distance = weighted_l1,
                        max_controls = 4, memory_mode = "implicit")
  expect_identical(groups(fm_loop), groups(fm_dense))

  cm_dense <- cardinality_match(left, right, vars = vars, distance = weighted_l1,
                                fine = "region", memory_mode = "dense")
  cm_loop <- cardinality_match(left, right, vars = vars, distance = weighted_l1,
                               fine = "region", memory_mode = "implicit")
  expect_identical(pair_key(cm_loop), pair_key(cm_dense))
})

test_that("a full match refuses a function that returns negative distances", {
  left <- callback_units(10, 209)
  right <- callback_units(30, 210)
  shifted <- function(l, r) weighted_l1(l, r) - 1
  expect_error(
    full_match(left, right, vars = c("x", "y", "z"), distance = shifted,
               memory_mode = "implicit"),
    "non-negative distances"
  )
})

test_that("a function whose value depends on the other units in its call is caught", {
  # Centring each call on its own smallest distance makes a pair's value a
  # property of the block it was evaluated in rather than of the pair.
  centred <- function(l, r) {
    d <- weighted_l1(l, r)
    d - min(d)
  }
  left <- callback_units(20, 211)
  right <- callback_units(60, 212)
  expect_error(
    match_couples(left, right, vars = c("x", "y", "z"), distance = centred,
                  memory_mode = "implicit"),
    "must not depend on the other units in the call"
  )
})
