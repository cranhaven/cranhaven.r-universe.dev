N <- 10
l_grid <- 10
p <- 2
grid <- seq(0, 1, l = l_grid)

set.seed(123)
Xall_tra <- funcharts::simulate_mfd(
  nobs = N,
  p = p,
  ngrid = l_grid,
  correlation_type_x = c("Bessel", "Gaussian")
)
X_tra <-
  data.frame(
    x = c(Xall_tra$X_list[[1]], Xall_tra$X_list[[2]]),
    timeindex = rep(rep(1:l_grid, each = (N)), p),
    curve = rep(1:(N), l_grid * p),
    var = rep(1:p, each = l_grid * N)
  )

Xall_II <- funcharts::simulate_mfd(
  nobs = N,
  p = p,
  ngrid = l_grid,
  shift_type_x = list("A", "B"),
  d_x = c(10, 10),
  correlation_type_x = c("Bessel", "Gaussian")
)

X_II <-
  data.frame(
    x = c(Xall_II$X_list[[1]], Xall_II$X_list[[2]]),
    timeindex = rep(rep(1:l_grid, each = (N)), p),
    curve = rep(1:(N), l_grid * p),
    var = rep(1:p, each = l_grid * N)
  )

mod_phaseI_AMFCC <- AMFCC_PhaseI(
  data_tra = X_tra,
  data_tun =
    NULL,
  grid = grid,
  ncores = 1
)

mod_phaseII_AMFCC <- AMFCC_PhaseII(data = X_II,
                                   mod_Phase_I = mod_phaseI_AMFCC,
                                   ncores = 1)

test_that("AMFCC_PhaseI works", {
  expect_is(mod_phaseI_AMFCC, "AMFCC_PhaseI")
})

test_that("AMFCC_PhaseII works", {
  expect_is(mod_phaseII_AMFCC, "AMFCC_PhaseII")
})

test_that("plotting AMFCC objects works", {
  plot(mod_phaseII_AMFCC)
  testthat::expect(ok = TRUE, failure_message = "plotting problem")
  plot(mod_phaseII_AMFCC,type='cont',ind_obs=1)
  testthat::expect(ok = TRUE, failure_message = "plotting problem")
})
