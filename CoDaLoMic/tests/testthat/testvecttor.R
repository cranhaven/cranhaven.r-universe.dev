set.seed(123)
especie=t(gtools::rdirichlet(5,c(1,3,1)))
Tt=5
E=3
EspecieMaxima=3

f=c(log(0.2915338/0.3176401),   log(0.48500536/0.08519937),  log(0.009179465/0.069391171) ,   log(0.0878459/0.4656159))



test_that("vecttor", {
  expect_equal(vecttor(1,especie,Tt,EspecieMaxima),f,tolerance=1e-5)
})
