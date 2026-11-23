#context("TauAndParameters_EstParmFunc_FBM")
#Here we test if the dimensions of the outputs are right just to observe is the function is being executed

set.seed(123)
especie=t(gtools::rdirichlet(10,c(1,3,1,2,4)))
Tt=10
E=5
EspecieMaxima=5
ridge.final=ridgeregression(Tt,especie, E, EspecieMaxima)
ttau=3
Iter.EstParmFunc=5


func=TauAndParameters_EstParmFunc_FBM(ttau,ridge.final,Iter.EstParmFunc, especie,EspecieMaxima,Tt,E,714)

test_that("TauAndParameters_EstParmFunc_FBM", {
  expect_equal( dim(func$All.iter),   c(Iter.EstParmFunc, 3*(E-1)+1)    )
  expect_equal(length(func$EstimateParameters),  3*(E-1)+1)
  expect_equal(length(func$AIC),1)
})
