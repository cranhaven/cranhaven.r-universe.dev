library(incidental)

#' train and validation split tests
test_that('train val split sum to original', {
  reported = c(10, 9, 8, 6, 0, 1, 2, 3)
  split_res = train_val_split(reported, frac_train=.75)
  expect_equal(split_res$reported_val+split_res$reported_train, reported)
})
test_that('frac train is close', {
  reported = c(10, 9, 8, 6, 0, 1, 2, 3)
  split_res = train_val_split(reported, frac_train=.75)
  expect_equal(sum(split_res$reported_train)/sum(reported), .7435897,
               tolerance=1E-5)
})
