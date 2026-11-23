#context("ExpectedValuess_BPBM")
Tt=3
E=3
mtx=rbind(c(0.1,0.2,0.3),c(0.4,0.5,0.6),c(0.7,0.8,0.9))
Estimated.Param=c(0.1 ,0.4, 0.7, 0.2 ,0.5, 0.8 ,0.3, 0.6, 0.9,0.1, 0.4 ,0.7, 0.2, 0.5, 0.8, 0.3 ,0.6, 0.9)
MatrizPBmodelo=rbind(c(1,1,1),c(0.3,0.6,-0.1),c(0.2,-0.4,0.3))



alpha=rbind(c( 1.246077,1.105171, 1.185305),
            c( 1.954237 ,1.584074 ,1.698932),
            c( 3.064854 ,2.270500 ,2.435130))

tau=apply(alpha, 2, sum)

Expected=cbind(c(alpha[1,1]/tau[1], alpha[2,1]/tau[1], alpha[3,1]/tau[1]  ),
               c(alpha[1,2]/tau[2],alpha[2,2]/tau[2],alpha[3,2]/tau[2]),
               c(alpha[1,3]/tau[3],alpha[2,3]/tau[3],alpha[3,3]/tau[3]))

Variance=cbind(  c(0.02193102,0.02954182,0.03439468),
                 c(0.02905758, 0.03647449, 0.04164909),
                 c(0.02740398, 0.0343988,0.03927893))



Expected.final=Expected[,-3]
Variance.final=Variance[,-3]

list.F<-list(alpha, matrix(Expected.final,3,2),matrix(Variance.final,3,2))
names(list.F)<-c("Dirichlet.Param", "Expected.Value","Variance.Value")


test_that("ExpectedValuess_BPBM", {
  expect_equal(ExpectedValuess_BPBM(Estimated.Param,MatrizPBmodelo,E,Tt),  list.F,tolerance=1e-5)

})
