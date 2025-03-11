test_df <- data.frame(
  entities = rep(1:3, 5),
  times = rep(seq(1960, 2000, 10), each = 3),
  dep_var = 101:115,
  a = 201:215,
  b = 301:315
)

test_that(paste("SEM_dep_var_matrix creates a correct matrix"), {
  m_expected_data <- c(
    104, 107, 110, 113,
    105, 108, 111, 114,
    106, 109, 112, 115
  )
  m_expected <- matrix(m_expected_data, nrow = 3, byrow = TRUE)

  m <- SEM_dep_var_matrix(df = test_df, timestamp_col = times,
                          entity_col = entities, dep_var_col = dep_var)

  expect_equal(m, m_expected, ignore_attr = TRUE)
})

test_that(paste("SEM_regressors_matrix uses all regressors if",
                "regressors_subset argument is not given"), {
  m_expected_data <- c(
    207, 307, 210, 310, 213, 313,
    208, 308, 211, 311, 214, 314,
    209, 309, 212, 312, 215, 315
  )
  m_expected <- matrix(m_expected_data, nrow = 3, byrow = TRUE)

  m <- SEM_regressors_matrix(df = test_df, timestamp_col = times,
                             entity_col = entities, dep_var_col = dep_var)

  expect_equal(m, m_expected, ignore_attr = TRUE)
})

test_that("SEM_B_matrix computes proper matrix", {
  periods_n <- 4
  B <- SEM_B_matrix(3, periods_n, 4:6)
  B11_expected_data <- c(
    1, 0, 0, 0,
    -3, 1, 0, 0,
    0, -3, 1, 0,
    0, 0, -3, 1
  )
  B12_expected_data <- c(
    0, -4, 0, 0,
    0, -5, 0, 0,
    0, -6, 0, 0,
    0, 0, -4, 0,
    0, 0, -5, 0,
    0, 0, -6, 0,
    0, 0, 0, -4,
    0, 0, 0, -5,
    0, 0, 0, -6
  )
  B11_expected <- matrix(B11_expected_data, nrow = periods_n, byrow = TRUE)
  B12_expected <- matrix(B12_expected_data, nrow = periods_n)
  expect_equal(B[[1]], B11_expected, ignore_attr = TRUE)
  expect_equal(as.matrix(B[[2]]), B12_expected, ignore_attr = TRUE)
})

test_that("SEM_C_matrix computes proper matrix", {
  alpha <- 9
  phi_0 <- 19
  beta <- 11:15
  phi_1 <- 21:25
  periods_n <- 4
  C <- as.matrix(SEM_C_matrix(alpha, phi_0, periods_n, beta, phi_1))

  C_expected_data <- c(
    alpha + phi_0, rep(phi_0, periods_n-1),
    beta[1] + phi_1[1], rep(phi_1[1], periods_n-1),
    beta[2] + phi_1[2], rep(phi_1[2], periods_n-1),
    beta[3] + phi_1[3], rep(phi_1[3], periods_n-1),
    beta[4] + phi_1[4], rep(phi_1[4], periods_n-1),
    beta[5] + phi_1[5], rep(phi_1[5], periods_n-1)
  )
  C_expected <- matrix(C_expected_data, periods_n, 1 + length(beta))
  expect_equal(C, C_expected, ignore_attr = TRUE)
})

test_that("SEM_psi_matrix computes proper matrix", {
  psis <- c(101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112)
  timestamps_n <- 3
  feature_n <- 4

  psi_m <- SEM_psi_matrix(psis = psis, timestamps_n = timestamps_n,
                          features_n = feature_n)

  psi_m_expected_data <- c(
    psis[1], psis[2], psis[3], psis[4], psis[5], psis[6], psis[7], psis[8],
    0, 0, 0, 0, psis[9], psis[10], psis[11], psis[12],
    0, 0, 0, 0, 0, 0, 0, 0
  )
  psi_m_expected <- matrix(psi_m_expected_data, timestamps_n, byrow = TRUE)
  expect_equal(psi_m, psi_m_expected)
})

test_that("SEM_sigma_matrix computes proper matrix", {
  err_var <- 1
  dep_vars <- c(2, 2, 2, 2)
  phis <- c(10, 10, 20, 20, 30, 30)
  psis <- c(101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112)
  sigma <- as.matrix(SEM_sigma_matrix(err_var, dep_vars, phis, psis))

  err_var_sq <- err_var^2
  dep_vars_sq <- dep_vars^2

  sigma_11_expected_data <- c(
    err_var_sq + dep_vars_sq[1], err_var_sq, err_var_sq, err_var_sq,
    err_var_sq, err_var_sq + dep_vars_sq[2], err_var_sq, err_var_sq,
    err_var_sq, err_var_sq, err_var_sq + dep_vars_sq[3], err_var_sq,
    err_var_sq, err_var_sq, err_var_sq, err_var_sq + dep_vars_sq[4]
  )

  sigma_12_expected_data <- c(
    phis[1] + psis[1], phis[2] + psis[2], phis[3] + psis[3], phis[4] + psis[4],
    phis[5] + psis[5], phis[6] + psis[6],
    phis[1], phis[2], phis[3] + psis[7], phis[4] + psis[8],
    phis[5] + psis[9], phis[6] + psis[10],
    phis[1], phis[2], phis[3], phis[4],
    phis[5] + psis[11], phis[6] + psis[12],
    phis[1], phis[2], phis[3], phis[4], phis[5], phis[6]
  )

  sigma_11_expected <- matrix(sigma_11_expected_data, nrow = 4, byrow = TRUE)
  sigma_12_expected <- matrix(sigma_12_expected_data, nrow = 4, byrow = TRUE)
  expect_equal(sigma[[1]], sigma_11_expected, ignore_attr = TRUE)
  expect_equal(sigma[[2]], sigma_12_expected, ignore_attr = TRUE)
})
