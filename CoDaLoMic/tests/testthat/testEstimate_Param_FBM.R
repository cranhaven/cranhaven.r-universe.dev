#context("Estimate_Param_FBM")
#Here we test if the dimensions of the outputs are right just to observe is the function is being executed

set.seed(123)
especie=t(gtools::rdirichlet(10,c(1,3,1,2,4)))
Tt=10
E=5
EspecieMaxima=5
ridge.final=ridgeregression(Tt,especie, E, EspecieMaxima)
tau=20
Iter.EstParmFunc=80


test_that("Estimate_Param_FBM", {
  expect_equal(dim(Estimate_Param_FBM(tau,ridge.final,Iter.EstParmFunc, especie,EspecieMaxima,Tt,E,714)$All.iter),c(Iter.EstParmFunc,1+length(ridge.final$coef)))
  expect_equal(length(Estimate_Param_FBM(tau,ridge.final,Iter.EstParmFunc, especie,EspecieMaxima,Tt,E,714)$Param.Estimates),1+length(ridge.final$coef))
  expect_equal(length(Estimate_Param_FBM(tau,ridge.final,Iter.EstParmFunc, especie,EspecieMaxima,Tt,E,714)$AIC),1)
})
