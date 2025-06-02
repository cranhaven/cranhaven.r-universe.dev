test_that("Test m5_prepare", {

  # Saving the data
  # save(calendar, file=test_path("data", "calendar.rda"))
  # test <- sales_test[item_id == 'HOBBIES_1_001']
  # save(test, file=test_path("data", "sales_test.rda"))
  # train <- sales_train[item_id == 'HOBBIES_1_001']
  # save(train, file=test_path("data", "sales_train.rda"))
  # prices <- sell_prices[item_id == 'HOBBIES_1_001']
  # save(prices, file=test_path("data", "sell_prices.rda"))

  load(test_path("data", "calendar.rda"))
  load(test_path("data", "sales_test.rda"))
  load(test_path("data", "sales_train.rda"))
  load(test_path("data", "sell_prices.rda"))

  result <-
    m5_prepare(train, test, calendar, prices)

  expect_equal(length(unique(result$item_id)), 1)
  expect_equal(length(colnames(result)), 18)

})
