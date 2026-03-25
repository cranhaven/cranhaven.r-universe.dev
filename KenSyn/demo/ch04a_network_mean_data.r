# kensyn Package. Knowledge synthesis in Agriculture: from experimental network to meta-analyisis.
# Ch04a. Network of experiments: Wheat varieties on on year. Heterogeneous variances
# Francois Piraux (Arvalis) 2017-11-30
# chargement des packages
library(lme4) # version 1.1-14
library(emmeans)   # # version 1.0 required : pbkrtest, lmerTest, multcompView
library(car) # version 2.1-6
library(outliers) # Version: 0.14
library(metafor) # Version : 2.0-0
library(KenSyn)

#################################################################################
# 4.1. Analysis of means of data
#### loading data
DF <- wheat_var
DF$experimentation <- as.factor(DF$experimentation)
DF$bloc <- as.factor(DF$bloc)

str(DF)
summary(DF)

## compute mean data per experiment
DFmoy <- aggregate(DF$rendement, list(variete=DF$variete,experimentation=DF$experimentation), mean)
names(DFmoy)[3] <- "rendement"

## fitting the model                                             
res.lmer <- lmer(rendement ~ variete + (1|experimentation), data=DFmoy, na.action=na.exclude)
res.lmer

## validation
plot(fitted(res.lmer),residuals(res.lmer), abline(h=0))
hist(residuals(res.lmer))

DFmoy$residus <- residuals(res.lmer)
grubbs.test(DFmoy$residus)
DFmoy[which.max(DFmoy$residus),]

## anova
Anova(res.lmer, test.statistic="F")

## adjusted means
moy_var <- lsmeans(res.lmer, ~variete)
moy_var

# comparisons 2 by 2
pairs(moy_var, adjust="tukey")
cld(moy_var, Letters=c(LETTERS))

# comparisons to the general mean
contrast(moy_var, method="eff", adjust="sidak")
confint(contrast(moy_var, method="eff", adjust="sidak"))

# comparisons to a control
# ? Dunnett Test  ?TODO : check
contrast(moy_var, method="trt.vs.ctrl", ref=2)
confint(contrast(moy_var,  method="trt.vs.ctrl", ref=2))

## random effects
ranef(res.lmer)

#### variance of means and variance of interaction
# calculation of Vmoy from individual data
MSE <- NULL
for (j in 1:5){
 res_lm.j <-  lm(rendement ~ variete + bloc, data=DF, subset=experimentation==j, na.action=na.exclude)
 MSE[j] <- summary(res_lm.j)$sigma^2   # variance par essai
} 
Vmoyj <- MSE/3 # 3 = number of block per experiment
Vmoy <- mean(Vmoyj) 
Vmoy

# calculation of variance of interaction
var_intera <- summary(res.lmer)$sigma^2 - Vmoy
var_intera

# end of file