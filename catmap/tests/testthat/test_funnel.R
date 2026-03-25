test_that("catmap.funnel works", {
  expect_equal(data(catmapdata), "catmapdata")
  catmapobject <- catmap(catmapdata, 0.95, TRUE)
  expect_equal(catmap.funnel(catmapobject, TRUE)$x, catmapobject$logOR)
  expect_equal(catmap.funnel(catmapobject, TRUE)$y, sqrt(catmapobject$comvarlogOR))
})
