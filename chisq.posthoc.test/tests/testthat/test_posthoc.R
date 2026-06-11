context("Test that calculations are performed correctly")

M <- as.table(rbind(c(762, 327, 468), c(484, 239, 477)))
dimnames(M) <- list(
  gender = c("F", "M"),
  party = c("Democrat", "Independent", "Republican")
)
post_hoc_results <- chisq.posthoc.test(M)

test_that("Post hoc test is performed correctly", {
  expect_equal(post_hoc_results$Democrat,
               c(4.502054, 0.000040, -4.502054, 0.000040),
               tolerance = 0.000001)
})
