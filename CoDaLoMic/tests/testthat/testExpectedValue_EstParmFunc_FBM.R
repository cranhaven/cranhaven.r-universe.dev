#context("ExpectedValues_EstParmFunc_FBM")


set.seed(123)
especie=t(gtools::rdirichlet(2,c(1,1,3)))
Tt=2
E=3
tau=5
EspecieMaxima=3
Iter.EstParmFunc=5
parms11=c(0.1,0.2,0.3,0.4,0.5,0.6,tau)



alpha=cbind(c(1.726793,1.892901,1.380306),
            c(1.64845, 1.060321, 2.291229))

Expected=cbind(c(alpha[1,1]/tau, alpha[2,1]/tau, alpha[3,1]/tau  ),
               c(alpha[1,2]/tau,alpha[2,2]/tau,alpha[3,2]/tau))

Variance=cbind(c(0.03768101, 0.03920954, 0.03330857 ),
               c( 0.03683242,0.02784883, 0.0413761 ))

Expected.final=Expected[,-2]
Variance.final=Variance[,-2]

list.F<-list(alpha, matrix(Expected.final,3,1),matrix(Variance.final,3,1))
names(list.F)<-c("Dirichlet.Param", "Expected.Value","Variance.Value")


test_that("ExpectedValues_EstParmFunc_FBM", {
  expect_equal(ExpectedValues_EstParmFunc_FBM(parms11 , especie,E,EspecieMaxima,Tt),  list.F,tolerance=1e-5)

})
