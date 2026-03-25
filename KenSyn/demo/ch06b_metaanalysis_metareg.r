# kensyn Package. Knowledge synthesis in Agriculture: from experimental network to meta-analyisis.
# ch06. Meta-analysis: code for illustrating the main principles of meta-regression with nlme
# David Makowski (INRA) 2019-01-30
library(nlme)
library(KenSyn)

# fictive dataset
TAB<-data.frame(
X1=c(-101,-52, -75, 10, 55, 82, 75, 98),
X2=c(0,1,0,1,0,0,1,1),
Ratio=c(0.55, 0.61, 0.59,0.81, 0.78, 0.91, 0.95, 0.99),
sdRatio=c(0.07, 0.09, 0.05, 0.02, 0.06,0.08, 0.07, 0.09))
TAB<-TAB[order(TAB$Ratio),]
TAB$Studies=1:8
TAB

#Graphical presentation
par(mfrow=c(2,2))
plot(TAB$Ratio, 1:8, ylab=" ", xlab="Individual effect size", axes=F,xlim=c(0.2,1.25), pch=19)
for (i in 1:8) {
lines(c(TAB$Ratio[i]-1.96*TAB$sdRatio[i], TAB$Ratio[i]+1.96*TAB$sdRatio[i]), c(i,i))		
}
axis(1,at=seq(0.2, 1.3, by=0.1))
title("A.", adj=0)
precision<-0.1/TAB$sdRatio
plot(TAB$X1, TAB$Ratio, cex=precision,xlab="X1", ylab="Individual effect size",ylim=c(0.5,1))
title("B.", adj=0)
plot(TAB$X2, TAB$Ratio, cex=precision,xlab="X2", ylab="Individual effect size", xlim=c(-0.5,1.5), axes=F, col=(TAB$X2+1))
axis(1, at=c(0,1), labels=c(0,1))
axis(2, at=seq(0.5,1,by=0.1), labels=seq(0.5,1,by=0.1))
title("C.", adj=0)
plot(TAB$X1, TAB$Ratio, cex=precision,xlab="X1", ylab="Individual effect size",col=(TAB$X2+1),ylim=c(0.5,1))
title("D.", adj=0)

# Heterogeneity

V<-TAB$sdRatio^2
#W is a vector including weights associated with Y data (inverse of variance)
W<-1/V
Y<-TAB$Ratio
Q<-sum(W*Y^2)-((sum(W*Y))^2)/sum(W)
1-pchisq(Q,length(Y)-1)

# Fixed-effects models
X1<-TAB$X1
X2<-TAB$X2

#Adjustment of fixed effect models with the glm function
mod1<-glm(Y~X1,weights=W)
summary(mod1)

# shown in chapter
mod2<-glm(Y~X2,weights=W)
summary(mod2)

mod3<-glm(Y~X1+X2,weights=W)
summary(mod3)

# ploting the regression
par(mfrow=c(2,2))
plot(TAB$X1, TAB$Ratio, cex=precision,xlab="X1", ylab="Individual effect size",ylim=c(0.5,1),col=(TAB$X2+1))
lines(-100:100, coef(mod1)[1]+coef(mod1)[2]*(-100:100))
title("A.", adj=0)
plot(TAB$X2, TAB$Ratio, cex=precision,xlab="X2", ylab="Individual effect size", xlim=c(-0.5,1.5), axes=F, col=(TAB$X2+1))
axis(1, at=c(0,1), labels=c(0,1))
axis(2, at=seq(0.5,1,by=0.1), labels=seq(0.5,1,by=0.1))
title("B.", adj=0)
points(0,coef(mod2)[1], pch=19, col="blue")
points(1, coef(mod2)[1]+coef(mod2)[2], pch=19, col="blue")
print(coef(mod2)[1])
print(coef(mod2)[1]+coef(mod2)[2])
plot(TAB$X1, TAB$Ratio, cex=precision,xlab="X1", ylab="Individual effect size",col=(TAB$X2+1),ylim=c(0.5,1))
title("C.", adj=0)
lines(-100:100, coef(mod3)[1]+coef(mod3)[2]*(-100:100))
lines(-100:100, coef(mod3)[1]+coef(mod3)[2]*(-100:100)+coef(mod3)[3],col="red")

# Test of heterogeneity for the three models
Qres<-sum(W*mod1$residuals^2)
1-pchisq(Qres,length(Y)-2)
Qres<-sum(W*mod2$residuals^2)
1-pchisq(Qres,length(Y)-2)
Qres<-sum(W*mod3$residuals^2)
1-pchisq(Qres,length(Y)-3)

########################################################################################
# Random-effects models
# Adjust random effects models with the lme function of the package nlme
Data<-groupedData(Y~X1+X2|Studies, data=TAB)

mod1RE<-lme(Y~X1, random=~1,weights=varFixed(~V), data=Data)
summary(mod1RE)

# shown in chapter
mod2RE<-lme(Y~X2, random=~1,weights=varFixed(~V), data=Data)
summary(mod2RE)

mod3RE<-lme(Y~X1+X2, random=~1,weights=varFixed(~V), data=Data)
summary(mod3RE)

# ploting the regression
plot(TAB$X2, TAB$Ratio, cex=precision,xlab="X2", ylab="Individual effect size", xlim=c(-0.5,1.5), axes=F, col=(TAB$X2+1))
axis(1, at=c(0,1), labels=c(0,1))
axis(2, at=seq(0.5,1,by=0.1), labels=seq(0.5,1,by=0.1))
title("D.", adj=0)
points(0,mod2RE$coefficients$fixed[1], pch=19, col="blue")
points(1, mod2RE$coefficients$fixed[1]+mod2RE$coefficients$fixed[2], pch=19, col="blue")
print(mod2RE$coefficients$fixed[1])
print(mod2RE$coefficients$fixed[1]+mod2RE$coefficients$fixed[2])


# end of file