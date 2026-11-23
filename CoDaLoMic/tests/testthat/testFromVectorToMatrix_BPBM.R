

set.seed(314)
especie=t(gtools::rdirichlet(n=6, c(6,6,1,6,6)))
E=5
Tt=6
MatrizPBmodelo=rbind(c(1,1,1,1,1,1),c(-0.3,0.4,0.3,-0.7,-0.4,-0.6),c(0.3,0.5,-0.3,0.1,0.4,0.1))
est=Estimating_BPBM(especie, Tt,E,MatrizPBmodelo, nn.chain=3,nn.burnin=1000,nn.sample=5000,nn.thin=10)
mm=StudyingParam(est$R2jagsOutput$BUGSoutput$summary,est$SamplesAllChains)

mm.final=cbind(c(1.819875   ,   1.520661,     0.000000 ,    1.399484  ,   1.604760 ),
               c(0.000000   ,  0.000000 ,    0.000000  ,   0.000000   ,  0.000000 ),
               c(0.000000    , 1.285797   ,   0.000000   ,  0.000000    , 0.000000 ))





test_that("FromVectorToMatrix_BPBM", {
  expect_equal(FromVectorToMatrix_BPBM(mm$Param.Summary[,"mean"],MatrizPBmodelo,5),mm.final,tolerance=1e-5)
  })
