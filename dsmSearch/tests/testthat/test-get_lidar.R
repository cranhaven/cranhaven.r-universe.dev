testthat::test_that("runs correctly", {

  las <- dsmSearch::get_lidar(x = -83.741289,
                              y = 42.270146,
                              r = 1000,
                              epsg = 2253)
  if (is.character(las)) {
    testthat::expect_type(las, "character")
  } else {
    testthat::expect_type(las, "S4")
  }
})
