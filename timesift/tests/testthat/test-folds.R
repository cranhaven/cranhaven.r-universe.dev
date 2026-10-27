test_that("every unit lands in exactly one fold", {
  y <- sim_response(sim_series(n_unit = 97L, days = 2L))
  f <- fold_map(y, v = 10L)
  expect_length(f, 97L)
  expect_equal(names(f), rownames(y))
  expect_setequal(unique(f), 1:10)
})

test_that("folds are the same size to within one unit", {
  y <- sim_response(sim_series(n_unit = 100L, days = 2L))
  n <- table(fold_map(y, v = 10L))
  expect_lte(max(n) - min(n), 2L)
})

test_that("the same seed gives the same map and a different one does not", {
  y <- sim_response(sim_series(n_unit = 60L, days = 2L))
  expect_identical(unclass(fold_map(y, seed = 1L)), unclass(fold_map(y, seed = 1L)))
  expect_false(identical(unclass(fold_map(y, seed = 1L)), unclass(fold_map(y, seed = 2L))))
})

test_that("stratification spreads the stratifying value across the folds", {
  set.seed(11)
  y <- matrix(0, nrow = 200, ncol = 4, dimnames = list(sprintf("p%03d", 1:200), paste0("v", 1:4)))
  richness <- sample(0:4, 200, replace = TRUE)
  for (i in 1:200) if (richness[i] > 0) y[i, seq_len(richness[i])] <- 1

  spread <- function(f) {
    stats::sd(tapply(rowSums(y), f, mean))
  }
  expect_lt(spread(fold_map(y, v = 10L, strata = 5L)),
            spread(fold_map(y, v = 10L, strata = 1L)))
})

test_that("a map can be stratified on something other than richness", {
  y <- sim_response(sim_series(n_unit = 80L, days = 2L))
  elevation <- seq_len(80)
  f <- fold_map(y, v = 8L, by = elevation)
  expect_lt(stats::sd(tapply(elevation, f, mean)), 5)
})

test_that("fold_map leaves the session's random stream where it found it", {
  y <- sim_response(sim_series(n_unit = 40L, days = 2L))
  set.seed(99)
  before <- stats::runif(1L)
  set.seed(99)
  invisible(fold_map(y))
  expect_equal(stats::runif(1L), before)
})

test_that("a fold map arrives named, bare or as a table, and disagreement is an error", {
  units <- sprintf("p%02d", 1:10)
  named <- stats::setNames(rep(1:5, 2), units)
  expect_equal(.as_folds(named, units), rep(1:5, 2))
  expect_equal(.as_folds(unname(named), units), rep(1:5, 2))
  expect_equal(.as_folds(data.frame(id = units, fold = rep(1:5, 2)), units), rep(1:5, 2))
  expect_equal(.as_folds(named[10:1], units), rep(1:5, 2))
  expect_error(.as_folds(named[-1], units), "no fold")
  expect_error(.as_folds(unname(named)[-1], units), "one entry per unit")
})

test_that("a fold count the sample cannot carry is refused", {
  y <- sim_response(sim_series(n_unit = 6L, days = 2L))
  expect_error(fold_map(y, v = 20L), "between 2 and")
  expect_error(fold_map(y, v = 1L), "between 2 and")
})

test_that("a stratum holding a single unit does not take the whole map with it", {
  y <- matrix(rbinom(60, 1L, 0.3), nrow = 60,
              dimnames = list(sprintf("u%02d", 1:60), "v1"))
  by <- c(rep(1, 30), rep(2, 29), 3)
  f <- fold_map(y, v = 3L, strata = 5L, by = by, seed = 1L)
  expect_equal(sort(unique(unclass(f))), 1:3)
  expect_lt(diff(range(table(unclass(f)))), 3L)
})

test_that("every fold holds units across many responses and fold counts", {
  for (seed in 1:25) {
    set.seed(seed)
    y <- matrix(rbinom(133 * 4, 1L, 0.1), nrow = 133,
                dimnames = list(sprintf("u%03d", 1:133), paste0("v", 1:4)))
    for (v in c(2L, 3L, 5L)) {
      f <- fold_map(y, v = v, seed = seed)
      expect_equal(length(unique(unclass(f))), v)
      expect_lt(diff(range(table(unclass(f)))), 0.2 * length(f))
    }
  }
})

test_that("grouping is every row on its own where none is given", {
  y <- sim_response(sim_series(n_unit = 40L, days = 2L))
  expect_identical(unclass(fold_map(y, v = 5L)), unclass(fold_map(y, v = 5L, group = NULL)))
})

test_that("rows sharing a group land in one fold", {
  y <- sim_response(sim_series(n_unit = 60L, days = 2L))
  group <- rep(sprintf("s%02d", 1:20), each = 3L)
  f <- fold_map(y, v = 5L, group = group)
  expect_equal(length(f), 60L)
  expect_true(all(tapply(unclass(f), group, function(v) length(unique(v))) == 1L))
  expect_setequal(unique(unclass(f)), 1:5)
})

test_that("a grouped map counts its folds against the groups, not the rows", {
  y <- sim_response(sim_series(n_unit = 30L, days = 2L))
  group <- rep(1:3, each = 10L)
  expect_error(fold_map(y, v = 5L, group = group), "between 2 and the 3 groups")
  expect_setequal(unique(unclass(fold_map(y, v = 3L, group = group, strata = 1L))), 1:3)
  expect_error(fold_map(y, v = 3L, group = group[-1L]), "one value per unit")
})

test_that("a grouped map still balances what it stratifies on", {
  set.seed(4)
  y <- matrix(rbinom(200 * 3, 1L, 0.3), nrow = 200,
              dimnames = list(sprintf("u%03d", 1:200), paste0("v", 1:3)))
  group <- rep(sprintf("g%03d", 1:50), each = 4L)
  f <- fold_map(y, v = 5L, strata = 5L, group = group)
  expect_lt(diff(range(table(unclass(f)))), 0.25 * length(f))
})

test_that("a resampling spec says how the folds are drawn", {
  spec <- cv(v = 4L, seed = 7L, strata = 3L)
  expect_s3_class(spec, "timesift_resampling")
  expect_equal(spec$method, "cv")
  expect_equal(spec$v, 4L)
  expect_null(spec$group)

  grouped <- grouped_cv("site", v = 4L)
  expect_equal(grouped$method, "grouped_cv")
  expect_equal(grouped$group, "site")

  expect_error(cv(v = 1L), "2 or more")
  expect_error(grouped_cv(), "needs the grouping")
  expect_output(print.timesift_resampling(cv()), "cv in 10 folds")
  expect_output(print.timesift_resampling(grouped_cv("site")), "grouped by: site")
})

test_that("a resampling reaches the fitting path as one fold map, however it was asked for", {
  y <- sim_response(sim_series(n_unit = 24L, days = 2L))
  tf <- list(label = rownames(y), order = seq_len(nrow(y)))
  targets <- data.frame(plot = rownames(y), site = rep(sprintf("s%02d", 1:8), each = 3L),
                        stringsAsFactors = FALSE)

  drawn <- .as_fold_map(cv(v = 4L), y, targets, tf)
  expect_s3_class(drawn, "timesift_folds")
  expect_equal(names(drawn), rownames(y))
  expect_equal(length(unique(unclass(drawn))), 4L)

  by_column <- .as_fold_map(grouped_cv("site", v = 4L), y, targets, tf)
  expect_true(all(tapply(unclass(by_column), targets$site,
                         function(v) length(unique(v))) == 1L))
  by_vector <- .as_fold_map(grouped_cv(targets$site, v = 4L), y, targets, tf)
  expect_equal(unclass(by_vector), unclass(by_column))

  given <- stats::setNames(rep(1:4, 6L), rownames(y))
  expect_equal(as.integer(.as_fold_map(given, y, targets, tf)), unname(given))
  expect_equal(as.integer(.as_fold_map(fold_map(y, v = 3L), y, targets, tf)),
               as.integer(fold_map(y, v = 3L)))
})

test_that("a grouping the targets do not carry is refused by size", {
  y <- sim_response(sim_series(n_unit = 12L, days = 2L))
  tf <- list(label = rownames(y), order = seq_len(nrow(y)))
  targets <- data.frame(plot = rownames(y), stringsAsFactors = FALSE)
  expect_error(.as_fold_map(grouped_cv("site", v = 3L), y, targets, tf), "no column of")
  expect_error(.as_fold_map(grouped_cv(rep(1:3, 3L), v = 3L), y, targets, tf), "9 grouping")
})

test_that("a grouping written against the targets follows them into their fitting order", {
  y <- sim_response(sim_series(n_unit = 9L, days = 2L))
  order <- c(3L, 1L, 2L, 9L, 8L, 7L, 4L, 5L, 6L)
  tf <- list(label = rownames(y), order = order)
  targets <- data.frame(plot = rownames(y), stringsAsFactors = FALSE)
  group <- c("a", "a", "a", "b", "b", "b", "c", "c", "c")
  f <- .as_fold_map(grouped_cv(group, v = 3L), y, targets, tf)
  expect_true(all(tapply(unclass(f), group[order], function(v) length(unique(v))) == 1L))
})

test_that("a split written against the targets follows them into their fitting order", {
  y <- sim_response(sim_series(n_unit = 12L, days = 2L))
  order <- rev(seq_len(nrow(y)))
  tf <- list(label = rownames(y)[order], order = order)
  targets <- data.frame(plot = rownames(y), stringsAsFactors = FALSE)

  given <- rep(1:3, length.out = 12L)
  by_row <- .as_fold_map(given, y, targets, tf)
  expect_equal(as.integer(by_row), given[order])
  expect_equal(names(by_row), tf$label)

  # The same split written with its units on it is joined on them, and lands on the same folds.
  named <- stats::setNames(given, rownames(y))
  expect_equal(unclass(.as_fold_map(named, y, targets, tf)), unclass(by_row))
  as_table <- data.frame(plot = rownames(y), fold = given, stringsAsFactors = FALSE)
  expect_equal(unclass(.as_fold_map(as_table, y, targets, tf)), unclass(by_row))
})

test_that("the deal is balanced whatever `v` is, down to one unit per fold", {
  y <- sim_response(sim_series(n_unit = 30L, days = 2L))
  # Six units per stratum and ten folds: a deal restarted in every stratum would use six of the
  # ten labels per stratum and leave the fold sizes anywhere from two to five.
  n <- table(fold_map(y, v = 10L))
  expect_length(n, 10L)
  expect_lte(max(n) - min(n), 1L)
  # Leave one out is every unit in a fold of its own, and the map says how many folds it holds.
  loo <- fold_map(y, v = 30L)
  expect_equal(sort(as.integer(loo)), 1:30)
  expect_equal(attr(loo, "v"), 30L)
  # The strata still each reach every fold in turn, so a fold is not one stratum's.
  f <- fold_map(y, v = 5L, strata = 5L)
  value <- rowSums(y)
  spread <- tapply(value, f, mean)
  expect_lt(max(spread) - min(spread), diff(range(value)))
})

test_that("a fold map holding one fold is refused, and a caller's map keeps what it knows", {
  y <- sim_response(sim_series(n_unit = 24L, days = 2L))
  tf <- list(label = rownames(y), order = seq_len(nrow(y)))
  targets <- data.frame(plot = rownames(y), site = rep(sprintf("s%02d", 1:8), each = 3L),
                        stringsAsFactors = FALSE)
  expect_error(.as_fold_map(rep(1L, 24L), y, targets, tf), "holds one fold")
  grouped <- fold_map(y, v = 4L, group = targets$site, seed = 7L)
  kept <- .as_fold_map(grouped, y, targets, tf)
  expect_true(attr(kept, "grouped"))
  expect_equal(attr(kept, "seed"), 7L)
  expect_equal(attr(kept, "v"), 4L)
  expect_equal(as.integer(kept), as.integer(grouped))
})

test_that("a fold map carries its grouping, and the inner folds a fit draws keep it whole", {
  y <- sim_response(sim_series(n_unit = 24L, days = 2L))
  group <- rep(sprintf("s%02d", 1:8), each = 3L)
  f <- fold_map(y, v = 4L, group = group)
  expect_equal(attr(f, "group", exact = TRUE), group)
  expect_null(attr(fold_map(y, v = 4L), "group", exact = TRUE))
  # Read back in another row order, the grouping follows the units.
  shuffled <- rev(rownames(y))
  expect_equal(unname(.fold_group(f, shuffled)), rev(group))
  inner <- .inner_folds(y[, 1L], 3L, 5L, group)
  expect_true(all(tapply(inner, group, function(v) length(unique(v))) == 1L))
  expect_setequal(unique(inner), 1:3)
  # The selection's inner splitter deals the outer training units by the same grouping.
  split <- .inner_splitter(3L, group)
  train <- 1:18
  inner_map <- split(y[train, , drop = FALSE], 2L, train)
  expect_true(all(tapply(as.integer(inner_map), group[train],
                         function(v) length(unique(v))) == 1L))
})
