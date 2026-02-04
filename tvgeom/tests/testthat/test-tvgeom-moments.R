context("moment functions")

test_that(
  "Check that moment functions are correct and work", {
    probs <- c(rep(0.01, 14))
    expect_equal(
      round(tvgeom_mean(probs), digits = 0),
      round(mean(rtvgeom(n = 1e8, prob = probs)), digits = 0)
    )
    expect_equal(
      round(tvgeom_var(probs), digits = 0),
      round(var(rtvgeom(n = 1e8, prob = probs)), digits = 0)
    )
  }
)
