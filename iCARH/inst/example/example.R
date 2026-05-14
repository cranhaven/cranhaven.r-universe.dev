## ----setup, include=FALSE------------------------------------------------
library(iCARH)
library(abind)

Tp=4 # timepoints
N=10 # number of samples
J=14 # number of metabolites
K=2  # number of bacteria species
P=8  # number of pathways

set.seed(12473)

## ----real data, eval=F---------------------------------------------------
#  keggid = list("Unk1", "C03299","Unk2","Unk3",
#   c("C08363", "C00712"),  # allowing multiple ids per metabolite
#   )
#   pathways = iCARH.GetPathwaysMat(keggid, "rno")

## ----pathways------------------------------------------------------------
# Manually choose pathways
path.names = c("path:map00564","path:map00590","path:map00061","path:map00591",
               "path:map00592","path:map00600","path:map01040","path:map00563")
data.sim = iCARH.simulate(Tp, N, J, P, K, path.names=path.names, Zgroupeff=c(0,4),
                          beta.val=c(1,-1,0.5, -0.5))

XX = data.sim$XX
Y = data.sim$Y
Z = data.sim$Z
pathways = data.sim$pathways

## ------------------------------------------------------------------------
pathways.bin = lapply(pathways, function(x) { y=1/(x+1); diag(y)=0; y})
adjmat = rowSums(abind::abind(pathways.bin, along = 3), dims=2)
cor.thresh = 0.7
# check number of metabolites in same pathway but not correlated
for(i in 1:Tp) print(sum(abs(cor(XX[i,,])[which(adjmat>0)])<cor.thresh))

## ------------------------------------------------------------------------
rstan::rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
fit.sim = iCARH.model(XX, Y, Z, pathways, control = list(adapt_delta = 0.99, max_treedepth=10),
                     iter = 500, chains = 1, pars=c("YY","Xmis","Ymis"), include=F)
# Not run
# fit.sim = iCARH.model(XX, Y, Z, pathways, control = list(adapt_delta = 0.99, max_treedepth=10),
#                      iter = 1100, chains = 2, pars=c("YY","Xmis","Ymis"), include=F)

## ------------------------------------------------------------------------
if(!is.null(fit.sim)){
  gplot = iCARH.plotBeta(fit.sim)
  gplot
}

## ------------------------------------------------------------------------
if(!is.null(fit.sim)){
  gplot = iCARH.plotTreatmentEffect(fit.sim)
  gplot
}

## ------------------------------------------------------------------------
if(!is.null(fit.sim)){
  gplot = iCARH.plotPathwayPerturbation(fit.sim, names(data.sim$pathways))
  gplot
}

## ------------------------------------------------------------------------
if(!is.null(fit.sim)){
  par(mfrow=c(1,2))
  iCARH.checkNormality(fit.sim)
}

## ------------------------------------------------------------------------
if(!is.null(fit.sim)){
  waic = iCARH.waic(fit.sim)
  waic
}

## ------------------------------------------------------------------------
if(!is.null(fit.sim)){
  mad = iCARH.mad(fit.sim, XX)
  quantile(mad)
}

