#context("EstParmFunc")

set.seed(314)
especie=t(gtools::rdirichlet(n=2, c(1,2,3)))
E=3
Tt=2
MatrizPBmodelo=rbind(c(1,1),c(-0.3,0.4),c(0.3,0.5))
set.seed(314)
est=Estimating_BPBM(especie, Tt,E,MatrizPBmodelo, nn.chain=3,nn.burnin=1000,nn.sample=5000,nn.thin=10)

param=est$R2jagsOutput$BUGSoutput$summary[,1]
#DIC=est$R2jagsOutput$BUGSoutput$DIC
#pD=est$R2jagsOutput$BUGSoutput$pD

especiemodi=especie[,-1]



test_that("LogVeroFuncBUENA", {
  expect_equal(LogVeroFuncBUENA(param,MatrizPBmodelo,E,Tt,especiemodi),2.14896,tolerance=1e-5)

})
