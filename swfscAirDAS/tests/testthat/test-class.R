y.read <- airdas_read(system.file("airdas_sample.das", package = "swfscAirDAS"))
y.proc <- airdas_process(y.read)


test_that("functions return expected airdas_ classes", {
  y.sight <- airdas_sight(y.proc)
  
  expect_identical(c("airdas_dfr", "data.frame"), class(y.read))
  expect_identical(c("airdas_df", "data.frame"), class(y.proc))
  expect_identical("data.frame", class(y.sight))
})


test_that("as_airdas_ functions work as expected", {
  y.read1 <- y.read2 <- y.read3 <- y.read
  y.read1$ttt <- 2
  y.read2$EffortDot <- "a"
  y.read3$EffortDot <- TRUE
  
  expect_identical("data.frame", class(as.data.frame(y.read)))
  expect_identical(c("airdas_dfr", "data.frame"), class(as_airdas_dfr(as.data.frame(y.read))))
  expect_identical(c("airdas_dfr", "data.frame"), class(as_airdas_dfr(y.read1)))
  expect_error(as_airdas_dfr(y.read[, 1:10]))
  expect_error(as_airdas_dfr(y.read2))
  expect_identical(c("airdas_dfr", "data.frame"), class(as_airdas_dfr(y.read3)))
  
  
  y.proc1 <- y.proc2 <- y.proc3 <- y.proc
  y.proc1$ttt <- 2
  y.proc2$Lat <- "a"
  y.proc3$OnEffort <- TRUE
  
  expect_identical("data.frame", class(as.data.frame(y.proc)))
  expect_identical(c("airdas_df", "data.frame"), class(as_airdas_df(as.data.frame(y.proc))))
  expect_identical(c("airdas_df", "data.frame"), class(as_airdas_df(y.proc1)))
  expect_error(as_airdas_df(y.proc[, 1:10]))
  expect_error(as_airdas_df(y.proc2))
  expect_identical(c("airdas_df", "data.frame"), class(as_airdas_df(y.proc3)))
})


test_that("can create airdas_dfr and airdas_df objects with zero rows", {
  y.read0 <- y.read[integer(0), ]
  y.proc0 <- y.proc[integer(0), ]
  
  y.proc0b <- y.proc[integer(0), 1:4]
  
  expect_identical(c("airdas_dfr", "data.frame"), class(as_airdas_dfr(y.read0)))
  expect_identical(c("airdas_dfr", "data.frame"), class(as_airdas_dfr(y.proc0)))
  expect_identical(c("airdas_df", "data.frame"), class(as_airdas_df(y.proc0)))
  
  expect_error(as_airdas_dfr(y.proc0b))
  expect_error(as_airdas_df(y.proc0b))
})
