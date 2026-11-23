#context("EstParmFunc")

especie1=cbind(c(0.5,0.3,0.2), c(0.1,0.3,0.6))
tau1=0.4
parms1= cbind(c(0.1,0.2),c(-0.2,0.1),c(0.3,0.2))
parms11=c(tau1,as.vector( parms1))



test_that("EstParmFunc", {
  expect_equal(EstParmFunc(parms11,especie1),-1.584698,tolerance=1e-5)

})
