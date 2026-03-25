# kensyn Package. Knowledge synthesis in Agriculture: from experimental network to meta-analyisis.
# ch04b. Network of experiments: Wheat varieties on on year. Heterogeneous variances
# Francois Piraux (Arvalis) 2017-11-30
# chargement des packages
library(lme4) # version 1.1-14
library(emmeans)   # required : pbkrtest, lmerTest, multcompView
library(car) # version 2.1-6
library(outliers) # Version: 0.14
library(metafor) # Version : 2.0-0
library(KenSyn)

#### loading data
DF <- wheat_var
DF$experimentation <- as.factor(DF$experimentation)
DF$bloc <- as.factor(DF$bloc)

str(DF)
summary(DF)

## compute mean data per experiment
DFmoy <- aggregate(DF$rendement, list(variete=DF$variete,experimentation=DF$experimentation), mean)
names(DFmoy)[3] <- "rendement"

#################################################################################
# 4.2. Experiments with heterogeneous variances
# Analysis of the wheat example with homogeneous variances between experiments
V1 <- diag(Vmoy, nrow(DFmoy))
res_metafor <- rma.mv(rendement~ -1 + variete, V1, random=list(~ 1|experimentation, ~ 1|variete:experimentation),rho=0, data=DFmoy) 
res_metafor$sigma2
res_metafor$tau2
res_metafor$b

# contrast
var1 <- c(1,0,0,0,0,0,0,0,0,0)    
var10 <- c(0,1,0,0,0,0,0,0,0,0)   
var1moinsvar10 <- var1 - var10 
covb <- vcov(res_metafor)
estimate <- var1moinsvar10%*%res_metafor$b
stderr.est <- sqrt(t(var1moinsvar10)%*%covb%*%var1moinsvar10)    
estimate
stderr.est

# Analysis of the wheat example with heterogeneous variances between experiments
Vmoyj <- c(2.365, 1.886, 1.586, 2.424, 1.911)
V2 <- diag(rep(Vmoyj, each=10))
res_metafor2 <- rma.mv(rendement ~ -1 + variete, V2, random=list(~ 1|experimentation, ~ 1|variete:experimentation),rho=0, data=DFmoy) 
res_metafor2

plot(res_metafor$b, res_metafor2$b, xlab="homogeneous variances", ylab="heterogeneous variances", cex.lab=1.5)
abline(0,1)

# end of file