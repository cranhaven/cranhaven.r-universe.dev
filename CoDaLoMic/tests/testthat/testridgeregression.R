#context("ridgeregression")
#We test the length of the output of ridgeregression to observe if the function can be executed

set.seed(123)
especie=t(gtools::rdirichlet(10,c(1,3,1,2,4)))
Tt=10
E=5
EspecieMaxima=5


set.seed(123)
especie1=t(gtools::rdirichlet(20,c(1,3,1,2,4,1,1,1,1,1,1,1,1,1,7)))
T1=20
E1=15
EspecieMaxima1=15

test_that("ridgeregression", {
  expect_equal(length(ridgeregression(Tt,especie, E, EspecieMaxima,558562316)$coef),3*(E-1),tolerance=1e-5)
  expect_equal(length(ridgeregression(T1,especie1, E1, EspecieMaxima1,558562316)$coef),3*(E1-1),tolerance=1e-5)

})
