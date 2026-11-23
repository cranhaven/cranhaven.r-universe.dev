#context("ObtainigValueSPBal")

set.seed(314)
esp2=t(gtools::rdirichlet(n=6, c(1,1,5,1,1)))

Num2<-list(3,c(3,5),1,c(3,5,4))
Dem2<-list(5,4,2,c(1,2))
Tt=6

test_that("ObtainigValueSPBal", {
  expect_equal(dim(ObtainigValueSPBal(Num2,Dem2,esp2,6)$MatrixSPBal)[1]   , 3   , tolerance=1e-5)
  expect_equal(dim(ObtainigValueSPBal(Num2,Dem2,esp2,6)$MatrixSPBal)[2]   , 6   , tolerance=1e-5)
  expect_equal(ObtainigValueSPBal(Num2,Dem2,esp2,6)$MatrixSPBal[3,]   , PBalance(6,1,2, esp2)   , tolerance=1e-5)
  expect_equal(ObtainigValueSPBal(Num2,Dem2,esp2,6)$MatrixSPBal[2,]   , PBalance(6,c(3,5,4),c(1,2), esp2)   , tolerance=1e-5)
  expect_equal(ObtainigValueSPBal(Num2,Dem2,esp2,6)$PercenVarianceSPBal   , c(66.173736,19.729434 )  , tolerance=1e-5)

})
