# kensyn Package. Knowledge synthesis in Agriculture : from experimental network to meta-analyisis
# example on worker and machine TODO
# Francois Piraux (01-11-2017)
## version de R : R 3.1.3
library(lme4) # version 1.1-10
library(emmeans) # version 2.20-23
# required : pbkrtest, lmerTest

### two possibility of space of inference
## Fiting and analysis of fixed model
res_lm <- lm(score2 ~ Machine*Worker, data=machines)  
summary(res_lm)
#moy_lm <- lsmeans(res_lm, ~ Machine)  # depreaceted TODO
moy_lm <- emmeans(res_lm, ~ Machine)  
confint(contrast(moy_lm, method="tukey"))  

############################################################################################

## Fiting and analysis of Mixted model
res_lmer <- lmer(score2 ~ Machine + (1|Worker) + (1|Machine:Worker), data=machines)  
res_lmer
#moy_lmer <- lsmeans(res_lmer, ~ Machine) # depreaceted TODO
moy_lmer <- emmeans(res_lmer, ~ Machine)
confint(contrast(moy_lmer, method="tukey"))   
                   

# end of file