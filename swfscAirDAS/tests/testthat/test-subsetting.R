test_that("subsetting airdas_ objects returns a data frame", {
  y.read <- airdas_read(system.file("airdas_sample.das", package = "swfscAirDAS"))
  y.proc <- airdas_process(y.read)
  
  expect_identical("data.frame", class(y.read[, 1:10]))
  expect_identical("data.frame", class(y.read[1:10, ]))
  expect_identical("data.frame", class(y.read[1:10, 1:10]))
  
  y.read1 <- y.read2 <- y.read3 <- y.read
  y.read1$Event <- 1
  y.read2[, "Event"] <- "a"
  y.read3[["Event"]] <- 1
  expect_identical("data.frame", class(y.read1))
  expect_identical("data.frame", class(y.read2))
  expect_identical("data.frame", class(y.read3))
  
  
  expect_identical("data.frame", class(y.proc[, 1:10]))
  expect_identical("data.frame", class(y.proc[1:10, ]))
  expect_identical("data.frame", class(y.proc[1:10, 1:10]))
  
  y.proc1 <- y.proc2 <- y.proc3 <- y.proc
  y.proc1$Event <- 1
  y.proc2[, "Event"] <- "a"
  y.proc3[["Event"]] <- 1
  expect_identical("data.frame", class(y.proc1))
  expect_identical("data.frame", class(y.proc2))
  expect_identical("data.frame", class(y.proc3))
})
