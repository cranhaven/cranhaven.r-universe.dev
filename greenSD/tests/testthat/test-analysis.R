testthat::test_that("runs correctly", {
  pwgf <- greenSD::compute_exposure()
  ndvi_seg <- greenSD::ndvi_to_sem()
  morpho <- greenSD::compute_morphology()
  testthat::expect_type(pwgf, "NULL")
  testthat::expect_type(ndvi_seg, "NULL")
  testthat::expect_type(morpho, "NULL")
})
