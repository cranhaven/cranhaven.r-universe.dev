test_that("AICc_permanova2 function returns a data frame", {
  library(vegan)
  data(dune)
  data(dune.env)
  Model <- adonis2(dune ~ Management*A1, data = dune.env)
  expect_s3_class(AICc_permanova2(Model), "data.frame")
})

test_that("AICc_permanova2 function returns the correct output", {
  library(vegan)
  data(dune)
  data(dune.env)
  Model <- adonis2(dune ~ Management*A1, data = dune.env)
  output <- AICc_permanova2(Model)
  expect_equal(output$k, 8)
  expect_equal(output$N, 20)
})
