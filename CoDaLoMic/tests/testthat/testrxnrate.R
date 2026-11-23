#context("rxnrate")


cinit1<-c(x1<-0.7,x2<-0.2,x3<-0.1)
parms1= cbind(c(0.1,0.2,-0.1),c(-0.2,0.1,-0.1),c(0.3,0.2,0.3),c(0.1,0.22,0.2))

cinit2<-c(x1<-0.6,x2<-0.2,x3<-0.1, x4<-0.1) #No es necesario darles nombre
parms2=cbind(c(0.1,0.2,-0.1,0.3),c(-0.2,0.1,-0.1,0.4),c(0.3,0.2,0.3,0.1),c(0.1,0.22,0.2,0.3),c(0.5,0.6,0.1,-0.4))



test_that("rxnrate", {
  expect_equal(rxnrate(cinit1,parms1), matrix(cbind(c(0.021,0.0664,-0.009))),tolerance=1e-3)
  expect_equal(rxnrate(cinit2,parms2), matrix(cbind(c(0.06,0.0764,-0.007,0.055))),tolerance=1e26)
})
