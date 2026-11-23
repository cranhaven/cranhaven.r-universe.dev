#context("Estimating_BPBM")

set.seed(314)
especie=t(gtools::rdirichlet(n=6, c(6,6,1,6,6)))
E=5
Tt=6
MatrizPBmodelo=rbind(c(1,1,1,1,1,1),c(-0.3,0.4,0.3,-0.7,-0.4,-0.6),c(0.3,0.5,-0.3,0.1,0.4,0.1))



test_that("Estimating_BPBM", {
  expect_equal(dim(Estimating_BPBM(especie, Tt,E,MatrizPBmodelo, nn.chain=3,nn.burnin=1000,nn.sample=2000,nn.thin=10,714)$SamplesAllChains)[1], 3*(2000-1000)/10)
  #fn <- "InfoEstimationBPBM_test.txt"
  #file.remove(fn)
  })
