data("air")
mfdobj <- get_mfd_list(air,
                       grid = 1:24,
                       n_basis = 5,
                       lambda = 1e-2)

test_that("AMFEWMA works", {
  mod <- AMFEWMA_PhaseI(mfdobj = mfdobj, mfdobj_tuning = mfdobj,
                        optimization_pars = list(
                          lambda_grid = 0.1,
                          k_grid = 1
                        ))
  cc <- AMFEWMA_PhaseII(mfdobj_2 = mfdobj, mod_1 = mod)
  expect_is(cc, "list")
  p <- plot_control_charts(cc$cc, nobsI = 100)
  expect_is(p, "ggplot")
})
