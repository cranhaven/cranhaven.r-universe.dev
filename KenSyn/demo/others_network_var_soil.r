# kensyn Package. Knowledge synthesis in Agriculture : from experimental network to meta-analyisis.
# Others. Network of experiments : Wheat varieties on several soils
# Francois Piraux (Arvalis) 2017-11-01
# R version 3.1.3
library(lme4) # version 1.1-10
library(emmeans)
library(car) # version 2.0-25
library(lattice) # version 0.20-33
# required : pbkrtest, lmerTest, multcompView

# loading data
DF <- wheat_var_soil

str(DF)
summary(DF)

# fiting the model
options(contrasts=c("contr.sum","contr.poly"))
res.lmer <- lmer(rendement ~ variete*sol + (1|sol:experimentation), data=DF, na.action=na.exclude)

res.lmer

# validation of the model
plot(fitted(res.lmer),residuals(res.lmer), abline(h=0))
hist(residuals(res.lmer))

# table of anova
Anova(res.lmer, type="III", test.statistic="F")

# adjusted means
moy_var <- lsmeans(res.lmer, ~ variete)
moy_var

moy_sol <- lsmeans(res.lmer, ~ sol)
moy_sol

moy_intera <- emmeans(res.lmer, ~ variete|sol)
moy_intera

# comparisons 2 by 2
pairs(moy_intera, adjust="tukey")
cld(moy_intera, Letters=c(LETTERS))

# comparisons to a control
contrast(moy_intera, method="trt.vs.ctrl", ref=2)

# plot of interaction
moy_intera2 <- summary(emmeans(res.lmer, ~variete:sol))
xyplot(emmean ~ variete, groups=sol, data=moy_intera2, col=c("black","blue"), pch=19, type="b",
auto.key = list(columns=2, points=F, lines=F, title='sol', cex=1, col=c("black","blue"),font=2))

# end of file