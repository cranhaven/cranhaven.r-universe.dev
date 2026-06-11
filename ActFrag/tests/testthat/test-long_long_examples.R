testthat::context("Checking long-running fragmentation long code")

data(example_activity_data)
count = example_activity_data$count
wear = example_activity_data$wear

testthat::test_that("Running all metrics", {

  res = sapply(c("mean_bout","TP","Gini","power","hazard", "all"), function(x) {
    fragmentation_long(
      count.data = count,
      weartime = wear,thresh = 100,
      bout.length = 1,
      metrics = x,by = "day")
  })
  testthat::expect_equal(res$all$mean_r, res$mean_bout$mean_r)
  res = sapply(c("mean_bout","TP","Gini","power","hazard", "all"), function(x) {
    fragmentation_long(
      count.data = count,
      weartime = wear,thresh = 100,
      bout.length = 1,
      metrics = x,by = "subject")
  })
  testthat::expect_equal(res$all$mean_r, res$mean_bout$mean_r)
})
