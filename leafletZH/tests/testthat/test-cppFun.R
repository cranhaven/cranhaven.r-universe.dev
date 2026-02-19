test_that("converts geographical coordinates works", {
  expect_equal(
    convertCoordinates(39.90105, 116.42079, from = "WGS-84", to = "GCJ-02"),
    c(39.902446, 116.427022)
  )
})
