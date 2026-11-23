#context("PBmodel")

A=rbind(c(1,2,3),c(4,5,6),c(7,8,9))
MatrizPBmodelo=cbind(c(1,2,3),c(4,5,6),c(7,8,9))
E=3
Tt=3
matr=rbind(c(14,32,50),c(32,77,122),c(50,122,194))

test_that("PBmodel", {
  expect_equal( PBmodel(A,MatrizPBmodelo, E,Tt) ,   matr)
})
