#context("B1MODImatrizP")

Tt=2
especie1=cbind(c(0.5,0.3,0.2), c(0.1,0.3,0.6))
especieII=1
E=3
EspecieMaxima=3


fin=cbind(c(1,1),c(0.9162907,-1.791759),c(0.4054651,-0.6931472))
colnames(fin)<-c("uno", "CocienteLogaridmos", "VectorBalances")

test_that("B1MODImatrizP", {
  expect_equal(B1MODImatrizP(Tt, especieII,especie1, E, EspecieMaxima),fin,tolerance=1e-5)

})
