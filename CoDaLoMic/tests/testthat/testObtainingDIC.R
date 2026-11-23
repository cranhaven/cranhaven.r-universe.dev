set.seed(314)
especie=t(gtools::rdirichlet(n=2, c(1,2,3)))
E=3
Tt=2
MatrizPBmodelo=rbind(c(1,1),c(-0.3,0.4),c(0.3,0.5))
set.seed(314)
est=Estimating_BPBM(especie, Tt,E,MatrizPBmodelo, nn.chain=3,nn.burnin=1000,nn.sample=5000,nn.thin=10)
cadenas=est$SamplesAllChains
especiemodi=especie[,-1]


DIC=est$R2jagsOutput$BUGSoutput$DIC
pD=est$R2jagsOutput$BUGSoutput$pV


test_that("ObtainingDIC", {
  expect_equal(as.numeric(ObtainingDIC(cadenas,MatrizPBmodelo,E,Tt,especiemodi)$X2[2]), round( pD,2),tolerance=1e-2)
  expect_equal(as.numeric(ObtainingDIC(cadenas,MatrizPBmodelo,E,Tt,especiemodi)$X3[2]), round( DIC,2),tolerance=1e-2)

})
