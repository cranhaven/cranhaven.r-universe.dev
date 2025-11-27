context("Testing the msce function\n")

con <- list()
con$t <- matrix(100)
con$Nnu0 <- matrix(1e-2)
con$alpha1 <- matrix(1)
con$gamma1 <- matrix(0.1)
con$nu1 <- matrix(1e-6)

test_that("test msce as tsce with constant parameters", {
  result <- msce_numerical(con$t,con)
  expect_equal(result$hazard, 0.0006787898) #similar but not equal to tsce
  expect_equal(result$lnSurvival, -0.01142311)
})

con$alpha2 <-con$alpha1
con$gamma2 <- con$gamma1
con$alpha1 <- NULL
con$gamma1 <- NULL
con$nu1 <- matrix(1e-2)
con$nu2 <- matrix(2e-2)
con$nu3 <- matrix(3e-2)

test_that("test msce with 4 stages with constant parameters", {
  result <- msce_numerical(con$t,con)
  expect_equal(result$hazard, 0.0013566)
  expect_equal(result$lnSurvival, -0.05714932)
})

spi<-list()
spi$t<-matrix(1:100,nrow = 1)
spi$Nnu0 <- matrix(1e-2,ncol = 100)
spi$alpha1 <- matrix(1,ncol = 100)
spi$gamma1 <- matrix(0.1,ncol = 100)
spi$nu1 <- matrix(1e-6,ncol = 100)
spi$Nnu0[20] <- 0.1
spi$alpha1[40] <- 10
spi$gamma1[60] <- 0.99
spi$nu1[80] <- 1e-5

test_that("test msce as tsce with spikes in all parameters", {
  result <- msce_numerical(spi$t,spi)
  expect_equal(result$hazard, 0.0008892011) # similar but not equal to tsce
  expect_equal(result$lnSurvival, -0.02252228) # similar but not equal to tsce
})

spi$alpha2 <-spi$alpha1
spi$gamma2 <- spi$gamma1
spi$alpha1 <- NULL
spi$gamma1 <- NULL
spi$nu1 <- matrix(1e-2,ncol = 100)
spi$nu2 <- matrix(2e-2,ncol = 100)
spi$nu3 <- matrix(3e-2,ncol = 100)
spi$nu1[10] <- 0.1
spi$nu2[30] <- 0.2
spi$nu3[50] <- 0.3

test_that("test msce with 4 stages with spikes in all parameters", {
  result <- msce_numerical(spi$t,spi)
  expect_equal(result$hazard, 0.001480846)
  expect_equal(result$lnSurvival, -0.07082396)
})

spi$alpha1 <- 0.5*spi$alpha2
spi$gamma1 <- 0.5*spi$gamma2

test_that("test msce with 4 stages and 2 expansions with spikes in all parameters", {
  result <- msce_numerical(spi$t,spi)
  expect_equal(result$hazard, 0.001241301)
  expect_equal(result$lnSurvival, -0.08328728)
})