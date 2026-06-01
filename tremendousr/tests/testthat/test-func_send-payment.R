test_that("trem_send_reward errors - no client", {

  skip_on_cran()
  skip_on_travis()

    expect_error(trem_send_reward(name = "jdt",
                                  email = "jdt@jdtrat.com",
                                  reward_amount = 0.36,
                                  currency_code = "USD",
                                  delivery_method = "EMAIL",
                                  payment_description_id = paste0("sent-from-test_no-client", format(Sys.time(), "%H:%M_%m-%d-%y")),
                                  funding_source_id = Sys.getenv("TREM_FUND_ID"),
                                  reward_types = c(virtual_visa_id, amazon_id),
                                  parse = TRUE
    ),
    regexp = "Tremendous API Client required.")

})

test_that("trem_send_reward works - with client", {

  skip_on_cran()
  skip_on_travis()


  test_client <- trem_client_new(api_key = NULL, # Uses system env API key
                                 sandbox = TRUE)


  vcr::use_cassette("trem-send-payment_client", {
    testPaymentClient <- trem_send_reward(client = test_client,
                                      name = "jdt",
                                      email = "jdt@jdtrat.com",
                                      reward_amount = 0.36,
                                      currency_code = "USD",
                                      delivery_method = "EMAIL",
                                      payment_description_id = paste0("sent-from-test_client", format(Sys.time(), "%H:%M_%m-%d-%y")),
                                      funding_source_id = Sys.getenv("TREM_FUND_ID"),
                                      reward_types = c(virtual_visa_id, amazon_id),
                                      parse = TRUE
                                      )
  })

  expect_type(testPaymentClient, "list")
  expect_named(testPaymentClient, "order")

  expect_type(testPaymentClient$order, "list")
  expect_named(testPaymentClient$order, c("id", "external_id", "created_at",
                                    "status", "payment", "rewards"))

  expect_type(testPaymentClient$order$id, "character")
  expect_type(testPaymentClient$order$external_id, "character")
  expect_type(testPaymentClient$order$created_at, "character")
  expect_type(testPaymentClient$order$status, "character")
  expect_type(testPaymentClient$order$payment, "list")
  expect_s3_class(testPaymentClient$order$rewards, "data.frame")

})




