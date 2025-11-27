context("Testing the tsce function\n")

con <- list()
con$t <- matrix(100)
con$Nnu0 <- matrix(1e-2)
con$alpha <- matrix(1)
con$gamma <- matrix(0.1)
con$nu1 <- matrix(1e-6)

test_that("test with constant parameters", {
  result <- tsce(con$t,con)
  expect_equal(result$hazard, 0.0006881582)
  expect_equal(result$lnSurvival, -0.01164061)
})

spi<-list()
spi$t<-matrix(1:100,nrow = 1)
spi$Nnu0 <- matrix(1e-2,ncol = 100)
spi$alpha <- matrix(1,ncol = 100)
spi$gamma <- matrix(0.1,ncol = 100)
spi$nu1 <- matrix(1e-6,ncol = 100)
spi$Nnu0[20] <- 0.1
spi$alpha[40] <- 10
spi$gamma[60] <- 0.99
spi$nu1[80] <- 1e-5

test_that("test with spikes in all parameters", {
  result <- tsce(spi$t,spi)
  expect_equal(result$hazard, 0.0008900014)
  expect_equal(result$lnSurvival, -0.02253617)
})