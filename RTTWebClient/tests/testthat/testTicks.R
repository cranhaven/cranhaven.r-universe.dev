reqTime <- as.POSIXct("2030-01-01", tz = "GMT")
reqTimeInMS <- round(as.double(reqTime)) * 1000
# ttPublicClient <- InitPublicWebClient(server = "ttlivewebapi.fxopen.com")

# test_that("Is Last 10 Ticks right format", {
#   vcr::use_cassette("Public_client_last10Ticks", {
#     ticks <- ttPublicClient$GetTicksRawMethod("EURUSD", reqTimeInMS, count = -10)
#   })
#   expect_equal(typeof(ticks), "list")
#   ticksColNames <- colnames(ticks)
#   expect_identical(ticksColNames, c("Timestamp", "BidPrice", "BidVolume", "BidType","AskPrice","AskVolume", "AskType"))
#
# })
#
# test_that("Is Last 10 Bars right format", {
#   vcr::use_cassette("Public_client_last10M1Bars", {
#     bars <- ttPublicClient$GetBarRawMethod("EURUSD", "Bid", "M1", reqTimeInMS, count = -10)
#   })
#   expect_equal(typeof(bars), "list")
#   expect_true(all(bars$Low <= bars$Open &&
#                     bars$Low <= bars$Close &&
#                     bars$Low <= bars$High &&
#                     bars$High >= bars$Open &&
#                     bars$High >= bars$Close))
#   barsColNames <- colnames(bars)
#   expect_identical(barsColNames, c("Volume", "Close", "Low", "High", "Open", "Timestamp"))
# })

rhost <- InitRTTWebApiHost(server = "ttlivewebapi.fxopen.com")
test_that("Is Last 10 ticks right format", {
  vcr::use_cassette("Public_client_last10Ticks", {
    ticks <- rhost$GetTickHistory("EURUSD", reqTime, reqTime, count = -10)
  })
  expect_equal(typeof(ticks), "list")
  ticksColNames <- colnames(ticks)
  expect_identical(ticksColNames, c("Timestamp", "BidPrice", "BidVolume", "BidType","AskPrice","AskVolume", "AskType"))
})

test_that("Is Last 10 Bars right format", {
  vcr::use_cassette("Public_client_last10M1Bars", {
    bars <- rhost$GetBarsHistory("EURUSD", "Bid", "M1", reqTime, reqTime, count = -10)
  })
  expect_equal(typeof(bars), "list")
  expect_true(all(bars$Low <= bars$Open &
                    bars$Low <= bars$Close &
                    bars$Low <= bars$High &
                    bars$High >= bars$Open &
                    bars$High >= bars$Close))
  barsColNames <- colnames(bars)
  expect_identical(barsColNames, c("Timestamp", "Open", "Low", "High","Close", "Volume"))
})
