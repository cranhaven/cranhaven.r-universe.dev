# kensyn Package. Knowledge synthesis in Agriculture: from experimental network to meta-analyisis.
# Ch04. Network of experiments: Wheat varieties on several years
# Francois Piraux (Arvalis) 2017-11-30
# chargement des packages
library(lme4) # version 1.1-14
library(emmeans)   # required : pbkrtest, lmerTest, multcompView
library(car) # version 2.1-6
library(outliers) # Version: 0.14
library(metafor) # Version : 2.0-0
library(KenSyn)

################## 4.4. factors place and year
#### loading data
DFmoy <- wheat_var_years
DFmoy$annee <- as.factor(DFmoy$annee)
str(DFmoy)
summary(DFmoy)


#### fitting the model                                            
res.lmer <- lmer(rendement ~ variete + (1|annee) + (1|lieu) + 
  (1|annee:lieu) + (1|variete:annee) + (1|variete:lieu), data=DFmoy,
  na.action=na.exclude)
res.lmer

# validation
plot(fitted(res.lmer),residuals(res.lmer), abline(h=0), xlab="predicted values", ylab="residuals", cex.lab=1.5)
hist(residuals(res.lmer), main="", xlab="residuals",ylab="frequency", cex.lab=1.5)

DFmoy$residus <- residuals(res.lmer)
grubbs.test(DFmoy$residus)
DFmoy[which.min(DFmoy$residus),]

par(mfrow=c(2,2))
hist(ranef(res.lmer)$`lieu`[,], main="", xlab="place effects",ylab="frequency", cex.lab=1.5)
hist(ranef(res.lmer)$`annee:lieu`[,], main="", xlab="year:place effects",ylab="frequency", cex.lab=1.5)
hist(ranef(res.lmer)$`variete:annee`[,], main="", xlab="variety:year effects",ylab="frequency", cex.lab=1.5)
hist(ranef(res.lmer)$`variete:lieu`[,], main="", xlab="variety:place effects",ylab="frequency", cex.lab=1.5)
par(mfrow=c(1,1))

# anova table
Anova(res.lmer, test.statistic="F")

# adjusted means
moy_var <- lsmeans(res.lmer, ~variete)
moy_var

# comparisons to the general mean
contrast(moy_var, method="eff", adjust="sidak")

# comparisons to a control
contrast(moy_var, method="trt.vs.ctrl", ref=2)  

# random effects
ranef(res.lmer)

# variance of means and variance of interaction
Vmoy <- mean(by(DFmoy$Vmoyj, DFmoy$experimentation, unique))
Vmoy

var_intera <- summary(res.lmer)$sigma^2 - Vmoy
var_intera

# end of file