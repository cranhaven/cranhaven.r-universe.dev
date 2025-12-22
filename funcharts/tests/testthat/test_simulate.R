set.seed(0)
test_that("simulation functions work", {
  dat <- sim_funcharts(nobs1 = 10, nobs_tun = 10, nobs2 = 9)
  expect_is(dat, "list")
  dat <- simulate_mfd(nobs = 10, save_beta = TRUE)
  expect_is(dat, "list")
  dat <- simulate_mfd(nobs = 10,
                      shift_type_x = c("A", "B", "C"),
                      shift_type_y = "A",
                      R2 = 0.86,
                      d_y = 1,
                      d_x = c(1, 1, 1))
  expect_is(dat, "list")

})
