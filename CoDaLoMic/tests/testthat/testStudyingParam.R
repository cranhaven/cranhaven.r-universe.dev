set.seed(314)
especie=t(gtools::rdirichlet(n=6, c(6,6,1,6,6)))
E=5
Tt=6
MatrizPBmodelo=rbind(c(1,1,1,1,1,1),c(-0.3,0.4,0.3,-0.7,-0.4,-0.6),c(0.3,0.5,-0.3,0.1,0.4,0.1))


est1=Estimating_BPBM(especie, Tt,E,MatrizPBmodelo, nn.chain=3,nn.burnin=1000,nn.sample=2000,nn.thin=10)
est=Estimating_BPBM(especie, Tt,E,MatrizPBmodelo, nn.chain=3,nn.burnin=1000,nn.sample=50000,nn.thin=10)

ssum=est$R2jagsOutput$BUGSoutput$summary
mcmc.chainsss=est$SamplesAllChains
Sssum=StudyingParam(ssum,mcmc.chainsss)

ssum2=ssum
ssum2[,"2.5%"]=rep(-0.5, length(ssum[,"2.5%"]))
ssum2[,"97.5%"]=rep(+0.5, length(ssum[,"97.5%"]))
Sssum2=StudyingParam(ssum2,mcmc.chainsss)

ff=Sssum$AllChainsJoined[,c(which(Sssum$Param.Summary[,"mean"]==0))]
dimnames(ff)<-NULL
test_that("StudyingParam", {
  expect_equal(as.numeric(Sssum2$Param.Summary[,"mean"]),rep(0,length(Sssum2$Param.Summary[,"mean"])),tolerance=1e-5)
  expect_error(StudyingParam(est1$R2jagsOutput$BUGSoutput$summary,est1$SamplesAllChains))
  expect_equal(dim(StudyingParam(est$R2jagsOutput$BUGSoutput$summary,est$SamplesAllChains)$Param.Summary)[1],dim(est$R2jagsOutput$BUGSoutput$summary)[1]  ,tolerance=1e-5)
  expect_equal(ff,matrix(0, dim(Sssum$AllChainsJoined[,c(which(Sssum$Param.Summary[,"mean"]==0))])[1],dim(Sssum$AllChainsJoined[,c(which(Sssum$Param.Summary[,"mean"]==0))])[2] ) )
  })
