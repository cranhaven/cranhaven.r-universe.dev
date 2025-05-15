test_that("Arguments are passed correctly between _process and _read", {
  y <- system.file("airdas_sample.das", package = "swfscAirDAS")
  
  y.read <- airdas_read(y, skip = 3, tz = "GMT")
  y.proc <- airdas_process(y.read, days.gap.part = 0.1/24)
  
  y.proc2 <- airdas_process(y, skip = 3, tz = "GMT", days.gap.part = 0.1/24, tmp = NA)
  
  expect_equal(y.proc, y.proc2)
})
