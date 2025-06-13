context("test-CondBED")

test_that("Whether the function gives us the correct specified length", {

  Data=rBED(n=100,Betax=1,Betay=1,rho=0.85)
  Resp=CondBED(rho=0.85,Betax=1,Betay=1,x=Data[,1])
  expect_length(Resp$Conditional_Statistics$x,100)})
