context("test-rBED")

test_that("Whether the function gives us the correct specified length", {

  Res=rBED(n=100,Betax=1,Betay=1,rho=0.85)
  expect_length(Res[,2],100)})
