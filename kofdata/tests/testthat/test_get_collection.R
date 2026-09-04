context("get_collection")

test_that("get_collection works for public collection", {
  baros <- get_collection("baro_vintages_monthly")

  expect_is(baros, "list")
  expect_equal(sort(names(baros))[1:3], c("baro_2014m04", "baro_2014m05", "baro_2014m06"))
  expect_is(baros[[1]], "ts")
})

test_that("get_collection works for private collection", {
  apikey <- Sys.getenv("KOFDATA_TEST_SECRET")
  skip_if(nchar(apikey) == 0)

  baros <- get_collection("kofdata_test_collection", apikey)

  expect_is(baros, "list")
  expect_equal(names(baros), c("ch.kof.barometer", "ch.kof.globalbaro.leading"))
  expect_is(baros[[1]], "ts")
})

test_that("get_collection errors with invalid set", {
  expect_error(get_collection("some_nonexistent_set"), "The API responded")
})

test_that("get_collection errors with invalid api key", {
  expect_error(get_collection("set_doesnt_matter", "GandalfTheGray"), "Invalid API key")
})
