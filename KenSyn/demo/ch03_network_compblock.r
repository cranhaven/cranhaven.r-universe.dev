# kensyn Package. Knowledge synthesis in Agriculture: from experimental network to meta-analyisis.
# ch03. Network of experiments: complete blocks. Wheat varieties on a single year
# Francois Piraux (Arvalis) 2017-11-01
# R version 3.1.3
# loading packages
library(lme4) # version 1.1-10
#library(lsmeans) # version 2.20-23
library(emmeans) # required : pbkrtest, lmerTest, multcompViewlibrary(car) # version 2.0-25
library(outliers) # Version: 0.14
library(KenSyn)

#################################################################################
#### data on evaluation of wheat varieties
DF <-wheat_var
DF$experimentation <- as.factor(DF$experimentation)
DF$bloc <- as.factor(DF$bloc)
DF$variete <- factor(DF$variete, levels=unique(DF$variete))
str(DF)
summary(DF)

#################################################################################
#### model with experiment as random effect - individual data
## fitting the model
res.lmer <- lmer(rendement ~ variete + (1|experimentation) +(1|experimentation:variete)  +(1|experimentation:bloc), data=DF, na.action=na.exclude)
res.lmer

## validation
plot(fitted(res.lmer),residuals(res.lmer), abline(h=0), xlab="predicted values", ylab="residuals",cex.lab=2)
hist(residuals(res.lmer), main="", xlab="residuals", ylab="frequency",cex.lab=2)
shapiro.test(residuals(res.lmer))

DF$residus <- residuals(res.lmer)
grubbs.test(DF$residus)
DF[which.max(DF$residus),]

## component of the variance
print(VarCorr(res.lmer), comp="Variance")

## anova
Anova(res.lmer, test.statistic="F")


## adjusted means
moy_var <- emmeans(res.lmer, ~variete)
moy_var

# comparisons 2 by 2
pairs(moy_var, adjust="tukey")
cld(moy_var, Letters=c(LETTERS))

# to the general mean
contrast(moy_var, method="eff", adjust="sidak")
confint(contrast(moy_var, method="eff", adjust="sidak"))

# comparisons to a control
# ? Dunnett Test  ?TODO : check
contrast(moy_var, method="trt.vs.ctrl", ref=2)  
confint(contrast(moy_var, method="trt.vs.ctrl", ref=2))


## random effects
ranef(res.lmer)

# end of script