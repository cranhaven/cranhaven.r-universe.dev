test_that("Check for threshold not > max of data", {

  counts = matrix(rpois(1440*5, lambda = 5), ncol = 1440)
  wear = c(rep(0,300),rep(1,1020),rep(0,120))
  max_counts = max(counts)
  expect_error(fragmentation(counts, wear, thresh = max_counts + 1,bout.length = 1, metrics = "mean_bout"))

})
