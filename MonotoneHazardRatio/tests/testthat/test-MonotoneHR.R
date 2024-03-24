test_that("function works", {
  library(survival)
  library(fdrtool)
  library(KernSmooth)
  library(twostageTE)

  data("chernoff_realizations")

  f.data <- survData[survData$group == 'S',]
  g.data <- survData[survData$group == 'T',]

  ### Evaluation grid
  t.grid <- seq(0, 10, 1)

  ### Estimation and inference
  theta <- monotoneHR(t.grid, f.data, g.data)
})
