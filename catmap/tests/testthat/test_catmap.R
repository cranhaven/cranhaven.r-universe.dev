test_that("catmap works", {
  expect_equal(data(catmapdata), "catmapdata")
  catmapobject <- catmap(catmapdata, 0.95, TRUE)
  expect_equal(round(catmapobject$combinedLogOR, 6), 1.110256)
  expect_equal(round(catmapobject$chisq.dsl, 6), 4.118104)
})
