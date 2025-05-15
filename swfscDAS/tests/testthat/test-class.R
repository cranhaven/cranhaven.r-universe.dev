test_that("functions return expected das_ classes", {
  y.read <- das_read(system.file("das_sample.das", package = "swfscDAS"))
  y.proc <- das_process(y.read)
  y.sight <- das_sight(y.proc)

  expect_identical(c("das_dfr", "data.frame"), class(y.read))
  expect_identical(c("das_df", "data.frame"), class(y.proc))
  expect_identical("data.frame", class(y.sight))
})


test_that("as_das_ functions work as expected", {
  y.read <- das_read(system.file("das_sample.das", package = "swfscDAS"))
  y.proc <- das_process(y.read)

  y.read1 <- y.read2 <- y.read3 <- y.read
  y.read1$ttt <- 2
  y.read2$EffortDot <- "a"
  y.read3$EffortDot <- TRUE

  expect_identical("data.frame", class(as.data.frame(y.read)))
  expect_identical(c("das_dfr", "data.frame"), class(as_das_dfr(as.data.frame(y.read))))
  expect_identical(c("das_dfr", "data.frame"), class(as_das_dfr(y.read1)))
  expect_error(as_das_dfr(y.read[, 1:10]))
  expect_error(as_das_dfr(y.read2))
  expect_identical(c("das_dfr", "data.frame"), class(as_das_dfr(y.read3)))


  y.proc1 <- y.proc2 <- y.proc3 <- y.proc
  y.proc1$ttt <- 2
  y.proc2$Lat <- "a"
  y.proc3$OnEffort <- TRUE

  expect_identical("data.frame", class(as.data.frame(y.proc)))
  expect_identical(c("das_df", "data.frame"), class(as_das_df(as.data.frame(y.proc))))
  expect_identical(c("das_df", "data.frame"), class(as_das_df(y.proc1)))
  expect_error(as_das_df(y.proc[, 1:10]))
  expect_error(as_das_df(y.proc2))
  expect_identical(c("das_df", "data.frame"), class(as_das_df(y.proc3)))
})
