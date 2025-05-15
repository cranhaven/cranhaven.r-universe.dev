y.read <- airdas_read(system.file("airdas_sample.das", package = "swfscAirDAS"))
y.proc <- airdas_process(y.read)
y.read.noc <- y.proc[y.proc$Event != "C", ]


test_that("comments paste together correctly", {
  # TODO
  expect_equal(2 * 2, 4)
})


test_that("comment processing returns objects with expected classes", {
  y.comm.df <- airdas_comments_process(as.data.frame(y.read))
  y.comm.read <- airdas_comments_process(y.read)
  y.comm.proc <- airdas_comments_process(y.proc)
  
  expect_identical(c("airdas_dfr", "data.frame"), class(y.comm.df))
  expect_identical(c("airdas_dfr", "data.frame"), class(y.comm.read))
  expect_identical(c("airdas_df", "data.frame"), class(y.comm.proc))
})


test_that("processing properly handles data with no data comments", {
  y.comm.df0 <- suppressMessages(airdas_comments_process(y.read.noc))
  y.comm.read0 <- suppressMessages(airdas_comments_process(as_airdas_dfr(y.read.noc)))
  y.comm.proc0 <- suppressMessages(airdas_comments_process(as_airdas_df(y.read.noc)))
  
  expect_equal(0, nrow(y.comm.df0))
  expect_equal(0, nrow(y.comm.read0))
  expect_equal(0, nrow(y.comm.proc0))
  
  expect_identical(c("airdas_dfr", "data.frame"), class(y.comm.df0))
  expect_identical(c("airdas_dfr", "data.frame"), class(y.comm.read0))
  expect_identical(c("airdas_df", "data.frame"), class(y.comm.proc0))
})
