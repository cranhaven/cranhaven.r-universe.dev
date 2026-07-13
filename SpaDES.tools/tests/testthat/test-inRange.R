test_that("numerical-comparisons: inRange handles various inputs", {
  # inputs for x
  expect_equal(inRange(0.5, 0, 1),   TRUE)
  expect_equal(inRange(-0.5, 0, 1),  FALSE)
  expect_equal(inRange(NA_real_),    NA)
  expect_equal(inRange(NA_integer_), NA)
  expect_equal(inRange(NULL),        NULL)
  expect_error(inRange())
  expect_error(inRange("non-numeric"), "x must be numeric.")

  f <- system.file("ex/test.grd", package = "terra")

  testInit("terra")
  rastDF <- needTerraAndRaster()

  for (ii in seq_len(NROW(rastDF))) {
    pkg <- rastDF$pkg[ii]

    r <- switch(pkg,
                raster = raster::raster(f),
                terra = terra::rast(f))

    ids <- which(inRange(r, 850, 875))
    if (requireNamespace("raster", quietly = TRUE))
      if (packageVersion("raster") < "3.3.3" && pkg == "raster") {
        expect_equal(ids, c(708L, 1502L, 2853L, 3553L, 3638L, 3950L, 5708L, 6333L))
      } else {
        expect_equal(ids, c(1741L, 2774L, 3091L, 3092L, 3171L, 3645L, 3873L, 3878L, 3951L,
                            3952L, 4031L, 7486L, 7646L))
      }

    # inputs for a & b
    expect_error(inRange(0.5, 1, 0))
    expect_error(inRange(-0.5, NA_integer_, 1))
    expect_error(inRange(-0.5, NA_real_, 1))
    expect_error(inRange(-0.5, 0, NA_integer_))
    expect_error(inRange(-0.5, 0, NA_real_))
    expect_error(inRange(-0.5, NULL, 1))
    expect_error(inRange(-0.5, 0, NULL))
  }
})
