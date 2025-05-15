test_that("equallength with big value and section method match", {
  y.proc <- airdas_process(system.file("airdas_sample.das", package = "swfscAirDAS"))
  
  # Using "equallength" method
  y1 <- suppressMessages(airdas_effort(
    y.proc, method = "equallength", sp.codes = c("mn", "bm"),
    seg.km = 300, num.cores = 1
  ))

  # Using "section" method
  y2 <- airdas_effort(
    y.proc, method = "section", sp.codes = c("mn", "bm"),
    num.cores = 1
  )
  
  expect_equal(y1, y2)
})
