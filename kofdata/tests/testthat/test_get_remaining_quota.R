context("get_remaining_quota")

test_that("get_remaining_quota works", {
  apikey = Sys.getenv("KOFDATA_TEST_SECRET")

  skip_if(nchar(apikey) == 0)

  # testuser has unlimited access so as not to throw off this test
  # with others that consume quota
  # TODO: It does not seem possible to distinguish between 0 "out of quota" and 0 "unlimited quota"
  expect_equal(get_remaining_quota(apikey), 0)
})

test_that("get_remaining_quota errors with invalid api key", {
  expect_error(get_remaining_quota("some_nonexistent_key"), "Invalid API key")
})
