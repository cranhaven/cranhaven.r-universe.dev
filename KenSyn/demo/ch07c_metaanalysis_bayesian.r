# kensyn Package. Knowledge synthesis in Agriculture: from experimental network to meta-analyisis.
# ch07. Meta-analysis with bayesian approach: comparing organic vs conventional cropping system (with MCMCglmm)
# David Makowski (INRA) 2017-12-04
library(nlme)
library(metafor)
library(MCMCglmm)
library(coda)
library(KenSyn)

TAB<-organic

Data<-groupedData(lnR~1|Study, data=TAB)

######################################################################################### 
#Study random effect, weighting, package metafor

Fit<-rma(yi=Data$lnR, vi=Data$Var_lnR, method="REML")
summary(Fit)
R<-exp(Fit$b)
R_lb<-exp(Fit$ci.lb)
R_ub<-exp(Fit$ci.ub)
Result[5,]<-c(R,R_lb,R_ub)

######################################################################################### 
#MCMCglmm

# definition of a priori, B for mu, R for residual variance and G for inter-study variance
prior1<-list(B=list(mu=0,V=10^8), R=list(V=1,nu=1),G=list(G1=list(V=1,nu=1)))

# fitting the model  with 50000 MCMC iterations, a burn-in period of 10000 iterations, 
# and 90percent elimination of iterations to reduce auto-correlations
Mod_mcmc<-MCMCglmm(lnR~1,random=~Study, mev=Data$Var_lnR, data=Data, verbose=F,
nitt=50000, thin=10, burnin=10000, prior=prior1,pr=TRUE)

# vizualization of the fiting
summary(Mod_mcmc)

# plot of the trace of values of mu parameter and posterior distribution
plot(Mod_mcmc)

# moment of posterior distribution of the yield ratio (the exponential of mu), 
# mean and quantiles
mean(exp(Mod_mcmc$Sol[,1]))
quantile(exp(Mod_mcmc$Sol[,1]), 0.025)
quantile(exp(Mod_mcmc$Sol[,1]), 0.975)

###########################################################################################
# Verification of the convergence of the algorithm
# generate several value chains (3)
Mod_mcmc_1<-MCMCglmm(lnR~1,random=~Study, mev=Data$Var_lnR, data=Data, verbose=F, nitt=50000, prior=prior1)
Mod_mcmc_2<-MCMCglmm(lnR~1,random=~Study, mev=Data$Var_lnR, data=Data, verbose=F, nitt=50000, prior=prior1)
Mod_mcmc_3<-MCMCglmm(lnR~1,random=~Study, mev=Data$Var_lnR, data=Data, verbose=F, nitt=50000, prior=prior1)
ChainList<-mcmc.list(Mod_mcmc_1$Sol,Mod_mcmc_2$Sol,Mod_mcmc_3$Sol)

#compare intra-string variance and inter-string variance
# graph of the Gelman and Rubin factor to diagnose chain convergence. 
# a value less than 1.02 is satisfactory.
gelman.plot(ChainList)

###########################################################################################
# TODO TO CHECK results <> chapter pbl on indice 65 ! TODO
SolRank<-apply(Mod_mcmc$Sol[,2:65],1,rank)

ResultSolRank<-matrix(nrow=64,ncol=3)

for ( i in 1:64) {
	ResultSolRank[i,]<-c(median(SolRank[i,]), quantile(SolRank[i,],0.25), quantile(SolRank[i,], 0.75))
}

TABrank<-data.frame(row.names(SolRank),ResultSolRank)

TABrank<-TABrank[order(TABrank[,2]),]
dotchart(TABrank[,2],labels=TABrank[,1], xlim=c(0,65),xlab="Ranking of experimental studies", pch=19)

for (i in 1:64) {
	
	lines(c(TABrank[i,3],TABrank[i,4]),c(i,i))
}

# end of file