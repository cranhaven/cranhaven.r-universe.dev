test_that("finds data if no new data is passed", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  mf1 <- model.frame(fit, data = dat)
  expect_no_error(model.frame(fit))
  mf2 <- model.frame(fit)
  expect_identical(mf1, mf2)
})

test_that("produces reasonable model frame even if onyl one new obs is passed", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  fit <- opsr(ys | yo ~ xs1 + xs2 | xo1 + xo2, dat, printLevel = 0)
  newdat <- dat[1, ]
  mf <- model.frame(fit, data = newdat)
  expect_s3_class(mf, "data.frame")
  expect_equal(dim(mf), c(1, 6))
})

test_that("finds variables if data was not passed in original 'opsr' call", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  ys <- dat$ys
  yo <- dat$yo
  xs1 <- dat$xs1
  xs2 <- dat$xs2
  xo1 <- dat$xo1
  xo2 <- dat$xo2
  rm(dat)
  f <- ys | yo ~ xs1 + xs2 | xo1 + xo2
  fit1 <- opsr(f, printLevel = 0)
  mf1 <- model.frame(fit1)
  dat <- sim_dat$data
  fit2 <- opsr(f, dat, printLevel = 0)
  mf2 <- model.frame(fit2)
  expect_identical(mf1, mf2)
})

test_that("finds variables for 'opsr.null' if data was not passed in original 'opsr' call", {
  sim_dat <- load_sim_dat()
  dat <- sim_dat$data
  ys <- dat$ys
  yo <- dat$yo
  xs1 <- dat$xs1
  xs2 <- dat$xs2
  xo1 <- dat$xo1
  xo2 <- dat$xo2
  rm(dat)
  f <- ys | yo ~ xs1 + xs2 | xo1 + xo2
  fit <- opsr(f, printLevel = 0)
  fit_null <- opsr_null_model(fit, printLevel = 0)
  expect_no_error(mf_null <- model.frame(fit_null))
  mf <- model.frame(fit)
  expect_identical(as.matrix(mf), as.matrix(mf_null))
})
