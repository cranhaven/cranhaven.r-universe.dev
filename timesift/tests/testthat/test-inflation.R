test_that("a threshold chosen on the scored units inflates the level it reports", {
  set.seed(41)
  y <- matrix(stats::rbinom(1500, 1L, 0.15), nrow = 250,
              dimnames = list(sprintf("p%03d", 1:250), paste0("sp", 1:6)))
  f <- fold_map(y, v = 10L, seed = 3L)
  out <- tss_inflation(y, f, skill = c(0.6, 0.9), replicates = 60L, seed = 8L)

  expect_equal(nrow(out), 2L)
  expect_true(all(out$inflation > 0))
  # thin presences are where the inflation is generated, so a high truth leaves less room for it
  expect_gt(out$inflation[out$skill == 0.6], out$inflation[out$skill == 0.9])
  expect_true(all(out$lower <= out$reported & out$reported <= out$upper))
})

test_that("the inflation is larger where a fold holds fewer presences", {
  units <- sprintf("p%03d", 1:400)
  thin <- matrix(0, nrow = 400, ncol = 2, dimnames = list(units, c("sp1", "sp2")))
  thin[seq(1, 400, by = 20), ] <- 1                      # 20 presences over 400 units
  thick <- matrix(0, nrow = 400, ncol = 2, dimnames = list(units, c("sp1", "sp2")))
  thick[seq(1, 400, by = 2), ] <- 1                      # 200 presences over 400 units

  f_thin <- fold_map(thin, v = 10L, seed = 2L)
  f_thick <- fold_map(thick, v = 10L, seed = 2L)
  a <- tss_inflation(thin, f_thin, skill = 0.6, replicates = 60L, seed = 9L)
  b <- tss_inflation(thick, f_thick, skill = 0.6, replicates = 60L, seed = 9L)
  expect_gt(a$inflation, b$inflation)
})

test_that("with no scorable cell there is no level to inflate", {
  units <- sprintf("p%02d", 1:20)
  y <- matrix(0, nrow = 20, ncol = 1, dimnames = list(units, "sp1"))
  expect_error(tss_inflation(y, fold_map(y, v = 5L)), "no cell of this design is scorable")
})

test_that("measuring the inflation leaves the session's random stream alone", {
  set.seed(51)
  y <- matrix(stats::rbinom(400, 1L, 0.3), nrow = 100,
              dimnames = list(sprintf("p%03d", 1:100), paste0("sp", 1:4)))
  f <- fold_map(y, v = 5L)
  set.seed(77)
  before <- stats::runif(1L)
  set.seed(77)
  invisible(tss_inflation(y, f, skill = 0.6, replicates = 5L))
  expect_equal(stats::runif(1L), before)
})

test_that("inverting the map recovers the skill it was planted from", {
  set.seed(61)
  y <- matrix(stats::rbinom(1200, 1L, 0.2), nrow = 200,
              dimnames = list(sprintf("p%03d", 1:200), paste0("sp", 1:6)))
  f <- fold_map(y, v = 5L, seed = 4L)
  forward <- tss_inflation(y, f, skill = 0.6, replicates = 60L, seed = 5L)
  back <- implied_skill(y, f, observed = forward$reported,
                        grid = seq(0.3, 0.9, by = 0.1), replicates = 60L, seed = 5L)
  expect_equal(back$skill, 0.6, tolerance = 0.05)
  expect_true(back$within_grid)
})

test_that("a level below anything the design can report is flagged rather than extrapolated", {
  set.seed(62)
  y <- matrix(stats::rbinom(600, 1L, 0.25), nrow = 100,
              dimnames = list(sprintf("p%03d", 1:100), paste0("sp", 1:6)))
  f <- fold_map(y, v = 5L, seed = 4L)
  out <- implied_skill(y, f, observed = c(0.05, 0.99), grid = seq(0.3, 0.9, by = 0.2),
                       replicates = 30L, seed = 5L)
  expect_false(any(out$within_grid))
  expect_true(all(is.finite(out$skill)))
})
