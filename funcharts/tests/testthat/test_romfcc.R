data("air")
mfdobj <- get_mfd_list(air,
                       grid = 1:24,
                       n_basis = 5,
                       lambda = 1e-2)

test_that("rpca_mfd works", {
  rpca <- rpca_mfd(mfdobj, nharm = 2, method = "ROBPCA")
  expect_is(rpca, "pca_mfd")
  rpca <- rpca_mfd(mfdobj, nharm = 2, method = "Locantore")
  expect_is(rpca, "pca_mfd")
  rpca <- rpca_mfd(mfdobj, nharm = 2, method = "Proj")
  expect_is(rpca, "pca_mfd")
  rpca <- rpca_mfd(mfdobj, nharm = 10, method = "normal")
  expect_is(rpca, "pca_mfd")
  rpca <- rpca_mfd(mfdobj, nharm = 10,center = TRUE, scale = TRUE, method = "normal")
  expect_is(rpca, "pca_mfd")
})


test_that("functional_filter works", {
  filter_output <- functional_filter(mfdobj[, 1:3])
  expect_is(filter_output, "list")
})

test_that("RoMFDI works", {
  mfdobj_missing <- mfdobj
  mfdobj_missing$coefs[, 1, 1] <- NA
  imputation_output <- RoMFDI(mfdobj_missing)
  expect_is(imputation_output, "list")
})

test_that("RoMFCC works", {
  mfdobj <- mfdobj[, 1:3]
  nobs <- dim(mfdobj$coefs)[2]
  set.seed(0)
  ids <- sample(1:nobs)
  mfdobj1 <- mfdobj[ids[1:100]]
  mfdobj_tuning <- mfdobj[ids[101:300]]
  mfdobj2 <- mfdobj[ids[-(1:300)]]
  mod_phase1 <- RoMFCC_PhaseI(mfdobj = mfdobj1,
                              mfdobj_tuning = mfdobj_tuning,
                              functional_filter_par = list(bivariate = FALSE),
                              imputation_par = list(update = FALSE))
  expect_is(mod_phase1, "list")
  phase2 <- RoMFCC_PhaseII(mfdobj_new = mfdobj2,
                           mod_phase1 = mod_phase1)
  expect_is(phase2, "data.frame")
})


