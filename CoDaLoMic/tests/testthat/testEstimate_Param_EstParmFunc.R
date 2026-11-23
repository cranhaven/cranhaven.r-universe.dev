#context("Estimate_Param_EstParmFunc")
#Here we test if the dimensions of the outputs are right just to observe is the function is being executed

especie=cbind(c(0.5,0.3,0.2),c(0.1,0.3,0.6))
paramini=c(100,2,3,4,5,6,7)


test_that("Estimate_Param_EstParmFunc", {
  expect_equal(dim(Estimate_Param_EstParmFunc(20, paramini , especie, 714)$All.iter),c(20,length(paramini)))
  expect_equal(length(Estimate_Param_EstParmFunc(20, paramini , especie,714)$Param.Estimates),length(paramini))
})
