#
#
# test_that("dRGetMetrics returns matrix",{
#   rawDevEggs <- matrix(c(10, 0.031, 10, 0.039, 15, 0.047, 15, 0.059, 15.5, 0.066,
#      13, 0.072, 16, 0.083, 16, 0.100, 17, 0.100, 20, 0.100, 20, 0.143, 25, 0.171,
#      25, 0.200, 30, 0.200, 30, 0.180, 35, 0.001), ncol = 2, byrow = TRUE)
#   mEggs <- devRateModel(eq = taylor_81, temp = rawDevEggs[,1], devRate = rawDevEggs[,2],
#      startValues = list(Rm = 0.05, Tm = 30, To = 5))
#   myMetrics <- dRGetMetrics(nlsDR = mEggs, printOut = TRUE)
#   expect_equal(
#     object = class(myMetrics),
#     expected = "matrix"
#   )
# })
#
#
# test_that("dRGetMetrics returns matrix",{
#   res <- dRGetMetricsInfo(eq = taylor_81)
#   expect_equal(
#     object = class(res),
#     expected = "matrix"
#   )
# })
