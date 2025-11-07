library(testthat)

test_that("output", {
  data1 = bananaquality[sample(1:nrow(bananaquality),
                               replace = FALSE,
                               size = 200),]
  expect_no_error(bootstrap_data(data = data1 ))
  expect_error(bootstrap_data(data = data1, outcome = "benfica" ))
  expect_error(bootstrap_data(data = data1 %>%
                                mutate(Quality = 1:200) ))
  expect_no_error(bootstrap_data(data = data1 %>%
                                mutate(cat = sample(c("A", "B", "C"),
                                                    replace = TRUE,
                                                    size = 200))))

  expect_error(bootstrap_data(data = data1, outcome = "test"))
  expect_error(bootstrap_data(data = mtcars %>%
                                dplyr::mutate(vs = 1:nrow(mtcars))))
  expect_type(bootstrap_data(data = data1), type = "list")

})
