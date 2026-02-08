test_that("Falkland Islands geometry starts with correct WKT", {
  skip_if_offline()

  fk_wkt <- wkls$fk$wkt()
  expect_type(fk_wkt, "character")
  expect_true(startsWith(
    fk_wkt,
    "MULTIPOLYGON (((-59.1909483 -52.9191095, -59.1765549 -52.8936952, -59.1879201 -52.8916291,"
  ))
})

test_that("Falkland Islands has 25 cities", {
  skip_if_offline()

  fk_cities <- wkls$fk$cities()
  expect_equal(nrow(fk_cities), 25)
})

test_that("Stoney Ridge geometry starts with correct WKT", {
  skip_if_offline()

  stoneyridge_wkt <- wkls$fk$stoneyridge$wkt()
  expect_type(stoneyridge_wkt, "character")
  expect_true(startsWith(
    stoneyridge_wkt,
    "POLYGON ((-60.4887893 -52.014296, -60.4914225 -52.0133724, -60.4941385 -52.0139674, -60.491533 -52.0158203,"
  ))
})

test_that("dependencies function returns 105 dependencies", {
  skip_if_offline()

  deps <- wkls$dependencies()
  expect_equal(nrow(deps), 53)
})
