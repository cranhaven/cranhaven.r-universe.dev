# kensyn Package. Knowledge synthesis in Agriculture: from experimental network to meta-analyisis.
# ch01. Simple mixed-effects model example on wheat yield with nlme and lme4
# David Makowski (INRA) 2017-09-12
library(nlme)
library(KenSyn)
# Load wheat yield data
#data(wheatyield, envir = environment())
TAB<-wheatyield
TAB
plot(TAB,pch=19,xlab="Site",ylab="Yield (t ha-1)")
axis(1,at=seq(1,nrow(TAB),by=1))

# model with site effects as random effects
Mod<-lme(Rdt~1, random=~1|Site, data=TAB)
summary(Mod)

Mod$coefficients
predict(Mod)

# add prediction on the same plot
for (i in 1:nrow(TAB)) {
  points(TAB$Site[i], predict(Mod)[i],pch=24, col="red",cex=1.4)
}

abline(h=Mod$coefficients$fixed)

# model without any site effects
Mod0<-lm(Rdt~1,data=TAB)
summary(Mod0)
mean(TAB$Rdt)

# Comparison of residuals
par(mfrow=c(1,2))
plot(TAB$Site,Mod0$residuals,xlab="Site",ylab="Residuals (t ha-1)", pch=19)
axis(1,at=seq(1,nrow(TAB),by=1))
abline(0,0)
title("without site effects")
plot(TAB$Site,Mod$residuals[,2],xlab="Site",ylab="Residuals (t ha-1)", pch=19)
axis(1,at=seq(1,nrow(TAB),by=1))
abline(0,0)
title("with site effects")

# comparison with lme4 package
library(lme4)
Mod_bis<-lmer(Rdt~1+(1|Site), data=TAB)
summary(Mod_bis)

# end of file
