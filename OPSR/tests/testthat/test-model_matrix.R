test_that("returns n x 1 matrix", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | 1, dat, printLevel = 0)
  mm <- model.matrix(fit)
  test <- unlist(lapply(mm$X, function(x) {
    is.matrix(x) && dim(x)[2] == 1
  }))
  expect_true(all(test))
})

test_that("data of dim 1 x n produces expected output", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  group <- 1
  newdat <- dat[dat$ys == group, ][1, ]
  mm <- model.matrix(fit, data = newdat)
  test <- dim(mm$X[[group]])
  expect_equal(dim(mm$X[[group]]), c(1, 3))
  expect_null(mm$W[[2]])
  expect_null(mm$X[[2]])
  expect_null(mm$W[[3]])
  expect_null(mm$X[[3]])
})

test_that("data of dim 2 x n produces NULL for selection outcome not observed", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  newdat <- dat[dat$ys %in% c(1, 2), ]  # leave out group 3
  mm <- model.matrix(fit, data = newdat)
  expect_null(mm$W[[3]])
  expect_null(mm$X[[3]])
})
