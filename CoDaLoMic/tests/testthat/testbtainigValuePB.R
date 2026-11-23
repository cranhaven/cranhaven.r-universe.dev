#context("ObtainigValuePB")

set.seed(314)
esp2=t(gtools::rdirichlet(n=6, c(1,1,5,1,1)))

Num2<-list(3,c(3,5),1,c(3,5,4))
Dem2<-list(5,4,2,c(1,2))
T=6

test_that("ObtainigValuePB", {
  expect_equal(dim(ObtainigValuePB(Num2,Dem2,esp2,6))[1]   , 4   , tolerance=1e-5)
  expect_equal(dim(ObtainigValuePB(Num2,Dem2,esp2,6))[2]   , 6   , tolerance=1e-5)
  expect_equal(ObtainigValuePB(Num2,Dem2,esp2,6)[1,]   , PBalance(6,3,5, esp2)   , tolerance=1e-5)
  expect_equal(ObtainigValuePB(Num2,Dem2,esp2,6)[2,]   , PBalance(6,c(3,5),4, esp2)   , tolerance=1e-5)
  expect_equal(ObtainigValuePB(Num2,Dem2,esp2,6)[3,]   , PBalance(6,1,2, esp2)   , tolerance=1e-5)
  expect_equal(ObtainigValuePB(Num2,Dem2,esp2,6)[4,]   , PBalance(6,c(3,5,4),c(1,2), esp2)   , tolerance=1e-5)

})
