context("testing transform-parameters.R")

test_that("direct_to_standardized and standardized_to_direct gives the correct result", {
  # set.seed(1); dput(round(runif(10, -1, 1), 3))
  smp <- c(-0.469, -0.256, 0.146, 0.816, -0.597, 0.797, 0.889, 0.322,
           0.258, -0.876)
  res <- standardized_to_direct(smp, 2L, jacobian = TRUE)
  back_val <- direct_to_standardized(res, 2L)

  expect_equal(smp, back_val)
  # dput(numDeriv::jacobian(standardized_to_direct, smp, n_scales = 2L))
  derivs <- structure(c(1.6464459252225, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.64644592520001, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.64644592522541, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.64644592520129, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.64644592522401, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.64644592520642, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.64644592520585, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.64644592521728, 0, 0, -0.184350089082795, -0.100626061409167, 0.0573883006481508, 0.32074557078296, -0.234663119792735, 0.31327723027261, 0.349439721086678, 0.126568717862633, 0.999999999951322, -5.77674799655253e-11, -0.0593134805207386, -0.0323758017355837, 0.0184643244260343, 0.10319786802051, -0.0755013813861174, 0.100794976486794, 0.112430030239388, 0.0407226881216811, -3.88939719538216e-11, 0.999999999969285), .Dim = c(10L, 10L))
  expect_equal(attr(res, "jacobian"), derivs, tolerance = 1e-6)

  res <- standardized_to_direct(smp, 10L, jacobian = TRUE)
  back_val <- direct_to_standardized(res, 10L)
  expect_equal(smp, back_val)
  expect_equal(attr(res, "jacobian"), diag(10))

  res <- standardized_to_direct(smp, 1L, jacobian = TRUE)
  back_val <- direct_to_standardized(res, 1L)
  expect_equal(smp, back_val)
  # dput(numDeriv::jacobian(standardized_to_direct, smp, n_scales = 1L))
  derivs <- structure(c(1.19014510292217, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.19014510291688, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.19014510293703, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.1901451029178, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.19014510294344, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.19014510291865, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.19014510292927, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.19014510293666, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1.19014510292598, 0, -0.0820542285910926, -0.0447886620850686, 0.0255435338464995, 0.142763860400935, -0.104448559632096, 0.139439701881636, 0.155535627314127, 0.0563357390307716, 0.0451385735102886, 0.999999999977775), .Dim = c(10L, 10L))
  expect_equal(attr(res, "jacobian"), derivs)
})
