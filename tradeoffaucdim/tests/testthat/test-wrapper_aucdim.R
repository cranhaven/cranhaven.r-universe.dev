test_that("output", {
  data1 = bananaquality[sample(1:nrow(bananaquality),
                               replace = FALSE,
                               size = 200),]
  expect_no_error(wrapper_aucdim(data = data1,
                                 outcome = "Quality",
                                 indep_vars = c("Size", "Weight", "Sweetness",
                                                "Softness", "HarvestTime",
                                                "Ripeness", "Acidity"  )))

})
