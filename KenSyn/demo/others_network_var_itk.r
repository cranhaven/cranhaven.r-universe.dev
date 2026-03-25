# kensyn Package. Knowledge synthesis in Agriculture : from experimental network to meta-analyisis.
# Others. Network of experiments: Wheat varieties with different crop managements
# Francois Piraux (Arvalis) 2017-11-01
# R version 3.1.3
library(lme4) # version 1.1-10
library(emmeans)
library(car) # version 2.0-25
# required : pbkrtest, lmerTest, multcompView

# loading data
DF <- wheat_var_itk
DF$variete <- factor(DF$variete, levels=unique(DF$variete))
DF$experimentation <- as.factor(DF$experimentation)

str(DF)
summary(DF)

table(DF$variete, DF$conduite)

## fiting the model 
options(contrasts=c("contr.sum","contr.poly"))
res.lmer <- lmer(rendement ~ conduite*variete + (1|experimentation) + (1|experimentation:conduite) + (1|experimentation:variete), data=DF, na.action=na.exclude)

res.lmer

## validation of the model
plot(fitted(res.lmer),residuals(res.lmer), abline(h=0))
hist(residuals(res.lmer))

## anova type III
Anova(res.lmer, test="F", type="III") 

# ajusted mean and contrasts
# mean per crop management
itk.lsm <- emmeans(res.lmer,  ~ conduite)
itk.lsm
confint(contrast(itk.lsm, method="tukey"))

# mean per variety
variete.lsm <- emmeans(res.lmer,  ~ variete)
variete.lsm

# mean per variety per crop management
intera.lsm <- emmeans(res.lmer,  ~ variete|conduite)
intera.lsm

# end of file