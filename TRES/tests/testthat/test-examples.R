context("Test reproducible examples in the paper.")

testthat::skip('skip')
RNGkind("L'Ecuyer-CMRG")

test_that("Section 3.1", {
  # set.seed(1)
  data("bat")
  fit_ols1 <- TRR.fit(bat$x, bat$y, method = 'standard')
  fit_fg1 <- TRR.fit(bat$x, bat$y, u = c(14,14), method = "FG")
  fit_1d1 <- TRR.fit(bat$x, bat$y, u = c(14,14), method = "1D")
  fit_ecd1 <- TRR.fit(bat$x, bat$y, u = c(14,14), method = "ECD")
  fit_pls1 <- TRR.fit(bat$x, bat$y, u = c(14,14), method = 'PLS')
  dist_ols1 <- rTensor::fnorm(coef(fit_ols1) - bat$coefficients)
  dist_fg1 <- rTensor::fnorm(coef(fit_fg1) - bat$coefficients)
  dist_1d1 <- rTensor::fnorm(coef(fit_1d1) - bat$coefficients)
  dist_ecd1 <- rTensor::fnorm(coef(fit_ecd1) - bat$coefficients)
  dist_pls1 <- rTensor::fnorm(coef(fit_pls1) - bat$coefficient)
  Pdist_fg1 <- rep(NA_real_, 2)
  Pdist_1d1 <- rep(NA_real_, 2)
  Pdist_ecd1 <- rep(NA_real_, 2)
  Pdist_pls1 <- rep(NA_real_, 2)
  for (i in 1:2){
    Pdist_fg1[i] <- subspace(bat$Gamma[[i]], fit_fg1$Gamma[[i]])
    Pdist_1d1[i] <- subspace(bat$Gamma[[i]], fit_1d1$Gamma[[i]])
    Pdist_ecd1[i] <- subspace(bat$Gamma[[i]], fit_ecd1$Gamma[[i]])
    Pdist_pls1[i] <- subspace(bat$Gamma[[i]], fit_pls1$Gamma[[i]])
  }
  Pdist_fg1 <- sum(Pdist_fg1)
  Pdist_1d1 <- sum(Pdist_1d1)
  Pdist_ecd1 <- sum(Pdist_ecd1)
  Pdist_pls1 <- sum(Pdist_pls1)

  expect_equal(fit_ols1$coefficients@data[1:6], c(-0.23776572,0.00374166,0.07267266,0.07700538,-0.02571225,-0.02067925), tol = 1e-8)
  expect_equal(fit_fg1$coefficients@data[1:6], c(-0.00003057,-0.00003878,0.00000770,0.00001446,0.00002130,0.00001392), tol = 1e-8)
  expect_equal(fit_1d1$coefficients@data[1:6], c(-0.00003080,-0.00003882,0.00000861,0.00001522,0.00002113,0.00001237), tol = 1e-8)
  expect_equal(fit_ecd1$coefficients@data[1:6], c(-0.00005015,-0.00004211,0.00000448,0.00000875,0.00001045,0.00000893), tol = 1e-8)
  expect_equal(fit_pls1$coefficients@data[1:6], c(-0.24471205,-0.00664075,0.06630047,0.06801656,-0.02625363,-0.02765261), tol = 1e-8)
  expect_equal(c(dist_ols1, dist_fg1, dist_1d1, dist_ecd1, dist_pls1), c(0.97204003,0.04395745,0.04428458,0.04452115,1.19191181), tol = 1e-8)
  expect_equal(c(Pdist_fg1, Pdist_1d1, Pdist_ecd1, Pdist_pls1), c(0.11138601,0.11434039,0.12347292,1.46626921), tol = 1e-8)
})

test_that("Section 3.2", {
  set.seed(1)
  data("square")
  fit_ols2 <- TPR.fit(square$x, square$y, method = "standard")
  fit_fg2 <- TPR.fit(square$x, square$y, u = c(2, 2), method = "FG")
  fit_1d2 <- TPR.fit(square$x, square$y, u = c(2, 2), method = "1D")
  fit_ecd2 <- TPR.fit(square$x, square$y, u = c(2, 2), method = "ECD")
  fit_pls2 <- TPR.fit(square$x, square$y, u = c(2, 2), method = "PLS")
  dist_ols2 <- rTensor::fnorm(coef(fit_ols2) - square$coefficients)
  dist_fg2 <- rTensor::fnorm(coef(fit_fg2) - square$coefficients)
  dist_1d2 <- rTensor::fnorm(coef(fit_1d2) - square$coefficients)
  dist_ecd2 <- rTensor::fnorm(coef(fit_ecd2) - square$coefficients)
  dist_pls2 <- rTensor::fnorm(coef(fit_pls2) - square$coefficients)
  Pdist_fg2 <- rep(NA_real_, 2)
  Pdist_1d2 <- rep(NA_real_, 2)
  Pdist_ecd2 <- rep(NA_real_, 2)
  Pdist_pls2 <- rep(NA_real_, 2)
  for (i in 1:2){
    Pdist_fg2[i] <- subspace(square$Gamma[[i]],fit_fg2$Gamma[[i]])
    Pdist_1d2[i] <- subspace(square$Gamma[[i]],fit_1d2$Gamma[[i]])
    Pdist_ecd2[i] <- subspace(square$Gamma[[i]],fit_ecd2$Gamma[[i]])
    Pdist_pls2[i] <- subspace(square$Gamma[[i]],fit_pls2$Gamma[[i]])
  }
  Pdist_fg2 <- sum(Pdist_fg2)
  Pdist_1d2 <- sum(Pdist_1d2)
  Pdist_ecd2 <- sum(Pdist_ecd2)
  Pdist_pls2 <- sum(Pdist_pls2)
  expect_equal(fit_ols2$coefficients@data[1:6], c(-49.13646247,1.41063173,-1.46466660,-24.39282877,9.15014540,-89.74182422), tol = 1e-8)
  expect_equal(fit_fg2$coefficients@data[1:6], c(0.00679122,0.00658411,0.00562682,0.00738316,0.00589002,0.00626680), tol = 1e-8)
  expect_equal(fit_1d2$coefficients@data[1:6], c(0.00680319,0.00659523,0.00564799,0.00738843,0.00589805,0.00627677), tol = 1e-8)
  expect_equal(fit_ecd2$coefficients@data[1:6], c(0.00679506,0.00658858,0.00564158,0.00737992,0.00589150,0.00626948), tol = 1e-8)
  expect_equal(fit_pls2$coefficients@data[1:6], c(0.04441180,0.03804863,0.04182236,0.04462787,0.04009233,0.03539482), tol = 1e-8)
  expect_equal(c(dist_ols2, dist_1d2, dist_ecd2, dist_pls2, dist_ols2, Pdist_fg2, Pdist_1d2, Pdist_ecd2, Pdist_pls2), c(2225.63779503,5.79782627,5.79782493,5.59056824,2225.63779503,1.41425438,1.41425440,1.41425440,0.15631640), tol = 1e-8)
})

test_that("Section 3.5", {
  set.seed(1)
  data("EEG")
  u_eeg <- c(1,1)
  fit_eeg_ols <- TRR.fit(EEG$x, EEG$y, method = "standard")
  fit_eeg_1d <- TRR.fit(EEG$x, EEG$y, u_eeg, method = "1D")
  fit_eeg_pls <- TRR.fit(EEG$x, EEG$y, u_eeg, method = "PLS")
  testthat::expect_equal(fit_eeg_ols$coefficients@data[8:12,8,1], c(-0.08669501,-0.10257079,-0.03880391,0.02420581,0.08765343), tol = 1e-8)
  testthat::expect_equal(fit_eeg_1d$coefficients@data[8:12,25,1], c(1.12230059,1.09961505,0.48797670,0.50786339,0.60505694), tolerance = 1e-8)
  testthat::expect_equal(fit_eeg_pls$coefficients@data[8:12,12,1], c(-0.05072670,0.02281158,-0.19007863,-0.09430375,-0.00219999), tolerance = 1e-8)
})

test_that("Section 4.3", {
  set.seed(1)
  p <- 20
  u <- 5
  # Model M1
  data <- MenvU_sim(p, u, jitter = 1e-5)
  Gamma <- data$Gamma
  M <- data$M
  U <- data$U

  G1 <- simplsMU(M, U, u)
  G2 <- ECD(M, U, u)
  G3 <- manifold1D(M, U, u)
  G4 <- OptM1D(M, U, u)
  G5 <- manifoldFG(M, U, u)
  G6 <- OptMFG(M, U, u)
  d1 <- subspace(G1, Gamma)
  d2 <- subspace(G2, Gamma)
  d3 <- subspace(G3, Gamma)
  d4 <- subspace(G4, Gamma)
  d5 <- subspace(G5, Gamma)
  d6 <- subspace(G6, Gamma)

  # The randomly generated matrix
  A <- matrix(runif(p*u), p, u)
  G7 <- manifoldFG(M, U, u, Gamma_init = A)
  G8 <- OptMFG(M, U, u, Gamma_init = A)
  d7 <- subspace(G7, Gamma)
  d8 <- subspace(G8, Gamma)
  expect_equal(c(d1,d2,d3,d4,d5,d6,d7,d8), c(0.00000001,0.00000000,0.00000014,0.00000003,0.00000013,0.00000002,0.63245553,0.72155627), tol = 1e-8)
})


test_that("Section 4.4", {
  set.seed(1)
  p <- 50
  u <- 5
  n0 <- c(50, 70, 100, 200, 400, 800)
  uhat3 <- rep(NA_integer_, length(n0))
  for (i in seq_along(n0)){
    n <- n0[i]
    data <- MenvU_sim(p, u, jitter = 1e-5, wishart = TRUE, n = n)
    M <- data$M
    U <- data$U
    output <- oneD_bic(M, U, n, maxdim = p/2)
    uhat3[i] <- output$u
  }
  expect_equal(uhat3, c(11,9,5,5,5,5))
})


