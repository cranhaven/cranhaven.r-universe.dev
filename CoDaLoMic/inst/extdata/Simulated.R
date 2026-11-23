library(devtools)
#install_github("hallucigenia-sparsa/seqtime")
library(seqtime)
library(coda.base)

set.seed(150)
N = 5
S = 10
A=generateA(N, "klemm", pep=64, d=-1, max.strength=1, c=0.04, clique.size=4)
dataset = generateDataSet(S, A)
dataset = seqtime::normalize(dataset)
datos.sim<-t(dataset)
t=seq(1,S, by=1)
Simulated=cbind(t,datos.sim)


#save(Simulated, file = "data/Simulated.RData",version = 2)
