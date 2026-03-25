# kensyn Package. Knowledge synthesis in Agriculture: from experimental network to meta-analyisis.
# ch06. Meta-analysis: code for illustrating the main principles of meta-regression with metafor
# version with Metafor
# David Makowski (INRA) 2019-01-30
library(metafor)
library(KenSyn)

#Data
X1<-c(-101,-52, -75, 10, 55, 82, 75, 98)
X2<-c(0,1,0,1,0,0,1,1)
Ratio<-c(0.55, 0.61, 0.59,0.81, 0.78, 0.91, 0.95, 0.99)
sdRatio<-c(0.07, 0.09, 0.05, 0.02, 0.06,0.08, 0.07, 0.09)
VRatio<-sdRatio^2

precision<-0.1/sdRatio

par(mfrow=c(2,2))
plot(X1, Ratio, cex=precision,xlab="X1", ylab="Yield ratio",ylim=c(0.5,1), pch=19, col="blue")
plot(X2, Ratio, xlab="X2", ylab="Yield ratio", cex=precision, xlim=c(-0.1,1.2),pch=19, col="pink")

par(mfrow=c(1,1))
Mod_0<-rma(yi=Ratio,vi=VRatio)
summary(Mod_0)
forest(Mod_0)

Mod_1<-rma(yi=Ratio,vi=VRatio, mods=~X1,method="FE")
summary(Mod_1)
forest(Mod_1)

precision<-0.1/sdRatio
plot(X1, Ratio, cex=precision,xlab="X1", ylab="Yield ratio",ylim=c(0.5,1), pch=19, col="blue")

Fit<-predict(Mod_1,newmods=-100:100)
lines(-100:100,Fit$pred, col="red", lwd=3)
lines(-100:100,Fit$ci.lb,lty=2, col="red")
lines(-100:100,Fit$ci.ub,lty=2, col="red")

Mod_2<-rma(yi=Ratio,vi=VRatio, mods=~X2, method="FE")
summary(Mod_2)
forest(Mod_2)

Mod_3<-rma(yi=Ratio,vi=VRatio, mods=~X1+X2, method="FE")
summary(Mod_3)
forest(Mod_3)

Mod_4<-rma(yi=Ratio,vi=VRatio, mods=~X1*X2)
summary(Mod_4)

Mod_5<-rma(yi=Ratio,vi=VRatio, method="FE", mods=~X1*X2)
summary(Mod_5)

# end of file