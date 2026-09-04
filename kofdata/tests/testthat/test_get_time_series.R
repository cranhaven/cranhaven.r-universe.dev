context("get_time_series")

test_that("get_time_series works for public series", {
  baro <- get_time_series("kofbarometer", show_progress = F)

  expect_is(baro, "list")
  expect_equal(names(baro), "kofbarometer")
  expect_is(baro$kofbarometer, "ts")
})

test_that("get_time_series works for non-public series", {
  apikey <- Sys.getenv("KOFDATA_TEST_SECRET")
  skip_if(nchar(apikey) == 0)

  key <- "ch.kof.inu.ng08.fx.sector_3d.289.q_ql_exp_chg_price_sales_n3m.share_eq"
  tsl <- get_time_series(key, api_key = apikey, show_progress = F)

  expect_is(tsl, "list")
  expect_equal(names(tsl), key)
  expect_is(tsl[[key]], "ts")
})

test_that("get_time_series errors on invalid key", {
  expect_error(get_time_series("some_nonexistent_key", show_progress = F), "The API responded")
})

test_that("get_time_series errors on invalid api key", {
  expect_error(get_time_series("key_doesnt_matter", "GandalfTheGray", show_progress = F), "Invalid API key")
})
