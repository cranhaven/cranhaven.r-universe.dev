test_that("simulate_data_fmrcc works", {
  data <- simulate_data_fmrcc(n_obs = 1200, delta_1 = 1, delta_2 = 0.5)
  expect_is(data, "list")
})



test_that("FMRCC works", {
  library(funcharts)
  dat <- simulate_data_fmrcc(delta_1 = 1, delta_2 = 0)
  X_train_mfd <- get_mfd_list(list(X = t(dat$X)))
  Y_train_mfd <- get_mfd_list(list(X = t(dat$Y)))
  Y_tun_mfd <- Y_train_mfd
  X_tun_mfd <- X_train_mfd
  phaseI_results <- FMRCC_PhaseI(
    Y_train = Y_train_mfd,
    X_train = X_train_mfd,
    Y_tun = Y_tun_mfd,
    X_tun = X_tun_mfd,
    FVEy = 0.95,
    FVEx = 0.90,
    alpha = 0.01,
    groups = 1:3,
    sigma_par = c('VVV', 'EEE')
  )
  expect_is(phaseI_results, "list")

  X_train_matrix <- t(dat$X[1:2, ])
  X_tun_matrix <- X_train_matrix

  # Example with scalar covariates
  phaseI_scalar <- FMRCC_PhaseI(
    Y_train = Y_train_mfd,
    X_train = X_train_matrix,  # matrix of scalar covariates
    Y_tun = Y_tun_mfd,
    X_tun = X_tun_matrix,
    FVEy = 0.95,
    FVEx = NULL,
    alpha = 0.05,
    groups = 1:3
  )
  expect_is(phaseI_scalar, "list")

  # Assuming phaseI_results was obtained from FMRCC_PhaseI()
  phaseII_results <- FMRCC_PhaseII(
    Y_test = Y_train_mfd,
    X_test = X_train_mfd,
    phaseI = phaseI_results
  )
  expect_is(phaseII_results, "list")


})

