context("test-pBED")

test_that("Whether the function gives us the correct specified length", {

  Res=pBED(rho=0.85,Betax=1,Betay=1,x=c(0.6,0.4),y=c(0.8,0.6))
  expect_length(Res,2)})
