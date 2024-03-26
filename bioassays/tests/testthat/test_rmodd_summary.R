
# data set x
x <- c(1.01,0.98,0.6,0.54,0.6,0.6,0.4,3)

## testing examples
eg1 <- bioassays::rmodd_summary(x, rm = "FALSE", strict= "FALSE", cutoff=80, n=3)
exp1<- c(mean=0.9662500,median = 0.6000000,n = 8.0000000,sd = 0.8487796,cv = 87.8426480)

eg2<- bioassays::rmodd_summary(x, rm = "TRUE", strict= "FALSE", cutoff=20, n=5)
exp2<- c(mean=0.6757143,median = 0.6000000,n = 7.0000000,sd = 0.2294818,cv = 33.9613684)

eg3 <- bioassays::rmodd_summary(x, rm = "TRUE", strict= "TRUE", cutoff=20, n=5)
exp3<- c(mean=0.7216667,median = 0.6000000,n = 6.0000000, sd = 0.2132057,cv = 29.5435138)

context("rmodd_summary")

test_that("examples are working", {
  expect_that(eg1, equals(exp1))
  expect_that(eg2, equals(exp2))
  expect_that(eg3, equals(exp3))
})
