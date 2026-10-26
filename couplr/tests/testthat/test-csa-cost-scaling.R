# Cost scaling on real-valued costs.
#
# `"csa"` reads the costs as supplied: its last refine ends on an
# epsilon-optimal assignment and the repair step makes it optimal, so neither
# an offset far from the origin nor a range spanning many orders of magnitude
# changes the answer.

test_that("csa agrees with the reference solver on ordinary continuous costs", {
  set.seed(101)
  for (rep in seq_len(10)) {
    cost <- matrix(stats::runif(2500), 50, 50)
    ref <- assignment(cost, method = "jv")
    got <- assignment(cost, method = "csa")
    expect_equal(got$total_cost, ref$total_cost, tolerance = 1e-9)
  }
})

test_that("csa is unaffected by an offset far from the origin", {
  # The span is one unit wide and sits at up to 1e9, where a double resolves
  # steps of about 1e-7.
  set.seed(102)
  base <- matrix(stats::runif(2500), 50, 50)
  for (offset in c(0, 1e6, 1e9)) {
    cost <- base + offset
    ref <- assignment(cost, method = "jv")
    got <- assignment(cost, method = "csa")
    expect_equal(got$total_cost, ref$total_cost, tolerance = 1e-6,
                 info = paste("offset", offset))
  }
})

test_that("csa certifies optimal on a cost range spanning many orders of magnitude", {
  # Lognormal costs at this width put the smallest entries a billionth of the
  # largest. The cheapest pairs, which the optimum is made of, are ordered by
  # differences far below the span.
  set.seed(103)
  cost <- matrix(exp(stats::rnorm(3600, 0, 6)), 60, 60)

  got <- assignment(cost, method = "csa")
  ref <- assignment(cost, method = "jv")
  expect_true(verify_assignment(got, cost)$certified_optimal)
  expect_equal(got$total_cost, ref$total_cost, tolerance = 1e-9)
})

test_that("csa certifies optimal across lognormal widths and both directions", {
  set.seed(104)
  for (sigma in c(0.5, 1, 2, 6)) {
    cost <- matrix(exp(stats::rnorm(900, 0, sigma)), 30, 30)
    for (maximize in c(FALSE, TRUE)) {
      got <- assignment(cost, method = "csa", maximize = maximize)
      cert <- verify_assignment(got, cost, maximize = maximize)
      expect_true(cert$certified_optimal,
                  info = paste("sigma", sigma, "maximize", maximize))
    }
  }
})

test_that("csa certifies optimal on integer costs", {
  set.seed(105)
  cost <- matrix(sample.int(500L, 900, replace = TRUE), 30, 30)
  storage.mode(cost) <- "double"

  ref <- assignment(cost, method = "jv")
  got <- assignment(cost, method = "csa")

  expect_equal(got$total_cost, ref$total_cost)
  expect_true(verify_assignment(got, cost)$certified_optimal)
})

test_that("csa handles a constant cost matrix", {
  cost <- matrix(3.5, 12, 12)
  got <- assignment(cost, method = "csa")
  expect_equal(got$total_cost, 12 * 3.5)
})
