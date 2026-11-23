#context("EstParmFunc_FBM")

especie1=cbind(c(0.5,0.3,0.2), c(0.1,0.3,0.6))
especiemodi=especie1[,-1]
tau1=0.4
parms1= cbind(c(0.1,0.2),c(-0.2,0.1),c(0.3,0.2))
parms11=c(as.vector( t(parms1)),tau1)



test_that("EstParmFunc_FBM", {
  expect_equal(EstParmFunc_FBM(parms11,especie1,3 ,3  , 2,especiemodi),-1.631432,tolerance=1e-5)

})
