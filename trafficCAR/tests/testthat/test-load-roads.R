
test_that("load_roads returns sf object unchanged", {
  data(roads_small, package = "trafficCAR")
  out <- trafficCAR:::load_roads(roads_small)
  expect_s3_class(out, "sf")
})


test_that("load_roads reads rda file", {
  data(roads_small, package = "trafficCAR")
  tmp <- tempfile(fileext = ".rda")
  save(roads_small, file = tmp)

  out <- trafficCAR:::load_roads(tmp)
  expect_s3_class(out, "sf")
})


test_that("load_roads errors on unsupported input", {
  expect_error(
    trafficCAR:::load_roads(123),
    "sf object or a file path"
  )
})
