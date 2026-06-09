testthat::test_that("runs correctly", {

  out <- dsmSearch::get_dsm_30(bbox = c(-83.783557,42.241833,-83.696525,42.310420))

  testthat::expect_type(out, "S4")
})
