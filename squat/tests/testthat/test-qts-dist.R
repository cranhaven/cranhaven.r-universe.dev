test_that("Function dist.default() works", {
  x <- c(0, 0, 1, 1, 1, 1)
  y <- c(1, 0, 1, 1, 0, 1)
  D <- dist(rbind(x, y), method = "binary")
  expect_snapshot(round(D, 5))
})

test_that("Function dist.qts_sample() works via fdacluster pkg", {
  D <- dist(vespa64$igp[1:5])
  expect_snapshot(round(D, 5))
})

test_that("Function dist.qts_sample() works via dtw pkg", {
  D <- dist(vespa64$igp[1:5], metric = "dtw")
  expect_snapshot(round(D, 5))
})
