# kensyn Package. Knowledge synthesis in Agriculture: from experimental network to meta-analyisis.
# ch06. Meta-analysis: mean size effect - comparing organic vs conventional cropping system with nlme and metafor
# David Makowski (INRA) 2017-11-01
library(nlme)
library(metafor)
library(KenSyn)

TAB<-organic

######################################################################################### 
# Calculation of Var_lnR
Var_logRatio<-(TAB$SD_conv^2)/(TAB$N_conv*TAB$Yield_conv^2) + (TAB$SD_org^2)/(TAB$N_org*TAB$Yield_org^2)
TAB$Var_lnR

# Dot chart - Logarithms of the ratios biological yield / conventional yield
dotchart(TAB$lnR,labels=TAB$Study,cex=0.8, pch=19, col="red",xlim=c(-3,1.5), xlab="log ratio" )
for (i in 1:length(TAB$lnR)) {
	lb_i<-TAB$lnR[i]-1.96*sqrt(TAB$Var_lnR[i])
	ub_i<-TAB$lnR[i]+1.96*sqrt(TAB$Var_lnR[i])
	lines(c(lb_i,ub_i),c(i,i))
}
abline(v=0)

######################################################################################### 
# Heterogeneity test
# Calculating vectors of weights and log ratios
w<-1/TAB$Var_lnR
L<-TAB$lnR

# Calculation of the Q statistic
Q<-sum(w*(L)^2)-((sum(w*L))^2)/sum(w)
# Calculation of the p-value
1-pchisq(Q,length(L)-1)

######################################################################################### 
# Calculation of the mean effect size (MES)
# Matrix storing results
Result<-matrix(nrow=6,ncol=3)

######################################################################################### 
# fixed effect model
# Calculation of weights from the inverse of log ratio variances of the TAB table
w<-1/TAB$Var_lnR
# Creation of a vector including log ratios
L<-TAB$lnR
# Calculation of the weighted average of log ratios
MEF<-sum(w*L)/sum(w)
# Calculation of the standard deviation of the mean
SE<-sqrt(1/(sum(w)))
print(MEF)
print(SE)
# Ratio estimated
R<-exp(MEF)
print(R)
# Calculation of the lower and upper limits of the 95percent confidence intervals
R_lb<-exp(MEF-1.96*SE)
R_ub<-exp(MEF+1.96*SE)
print(R_lb)
print(R_ub)
Result[1,]<-c(exp(MEF),R_lb,R_ub)

######################################################################################### 
# random effect on study - weighting
# Method of Dersimonian and Laird
# Estimation of inter-study variance
Vb<-(Q-(length(L)-1))/(sum(w)-(sum(w^2)/sum(w)))
# Calculation of weights
wT<-1/(TAB$Var_lnR+Vb)
# Estimation of the average effect size
MEF<-sum(wT*L)/sum(wT)
# Estimation of the standard deviation of the estimated effect size
SE<-sqrt(1/(sum(wT)))
print(MEF)
print(SE)
# Ratio estimated
R<-exp(MEF)
print(R)

# bounds below and above the confidence interval (95percent)
R_lb<-exp(MEF-1.96*SE)
R_ub<-exp(MEF+1.96*SE)
print(R_lb)
print(R_ub)
# Ratio estimated and confidence interval (95percent)
Result[2,]<-c(exp(MEF),R_lb,R_ub)

# Definition of the data structure with the groupedData function
#The observed variable is lnR and the data is grouped according to the variable Study
Data<-groupedData(lnR~1|Study, data=TAB)


# Using rma function of the package metafor - random effect, weighting
Fit<-rma(yi=Data$lnR, vi=Data$Var_lnR, method="DL")
summary(Fit)
R<-exp(Fit$b)
R_lb<-exp(Fit$ci.lb)
R_ub<-exp(Fit$ci.ub)
Result[3,]<- c(R,R_lb,R_ub)

########################################################################################
# Random effect on study, weighting
# Data format
Data<-groupedData(lnR~1|Study, data=TAB)

# with the lme function of the package nlme
# Parameter estimation with lme - random effect, weighting
Fit<-lme(lnR~1, random = ~ 1, data=Data, weight= varFixed(~Var_lnR),
method="REML")
#control=lmeControl(sigma=1))summary(Fit)
# Estimated ratio and confidence interval (95percent)
R<-exp(Fit$coefficients$fixed)
R_lb<-exp(Fit$coefficients$fixed-1.96*sqrt(Fit$varFix))
R_ub<-exp(Fit$coefficients$fixed+1.96*sqrt(Fit$varFix))
Result[4,]<-c(R,R_lb,R_ub)


# Random effect on study, weighting - package metafor 

Fit<-rma(yi=Data$lnR, vi=Data$Var_lnR, method="REML")
summary(Fit)
R<-exp(Fit$b)
R_lb<-exp(Fit$ci.lb)
R_ub<-exp(Fit$ci.ub)
Result[5,]<-c(R,R_lb,R_ub)

########################################################################################
# Random effect on study, no weighting - package nlme
Fit<-lme(lnR~1, random = ~ 1, data=Data, method="REML")
summary(Fit)
# Estimated ratio
R<-exp(Fit$coefficients$fixed)
R_lb<-exp(Fit$coefficients$fixed-1.96*sqrt(Fit$varFix))
R_ub<-exp(Fit$coefficients$fixed+1.96*sqrt(Fit$varFix))
Result[6,]<-c(R,R_lb,R_ub)




# Results
Result<-as.data.frame(Result)
names(Result)<-c("Estimate","LowerBound", "UpperBound")
Result

plot(Result$Estimate[1:6], 1:6, xlab="Ratio", ylab="Method", xlim=c(0.6,1))
lines(c(Result$LowerBound[1],Result$UpperBound[1]),c(1,1))
lines(c(Result$LowerBound[2],Result$UpperBound[2]),c(2,2))
lines(c(Result$LowerBound[3],Result$UpperBound[3]),c(3,3))
lines(c(Result$LowerBound[4],Result$UpperBound[4]),c(4,4))
lines(c(Result$LowerBound[5],Result$UpperBound[5]),c(5,5))
lines(c(Result$LowerBound[6],Result$UpperBound[6]),c(6,6))
text(c(0.65,0.65,0.65,0.65,0.65,0.65), c(1,2,3,4,5,6),
c("Fixed effect", "RE DL" ,"RE DL metafor","RE REML lme",
"RE REML metafor","RE REML lme no weight"),
cex=0.5)

#########################################################################################
# Evaluation of results
# Funnel plot
LogRatio<-TAB$lnR
Precision<-1/sqrt(TAB$Var_lnR)
plot(LogRatio,Precision, xlab="log ratio", ylab="Precision")
abline(v=Fit$coefficients$fixed)
LRnorm<-LogRatio*Precision
summary(lm(LRnorm~Precision))

# Forest plot with metafor
Fit<-rma(yi=Data$lnR, vi=Data$Var_lnR, method="REML")
forest(Fit)

######################################################################################### 
# Analysis for each continent separately
# Asia, Europe, North-Am, Lat Am, Oceania, Africa

TAB_C<-TAB[TAB$Continent=="AA_Europe",]

# Dot chart
dotchart(TAB_C$lnR,labels=TAB_C$Study,cex=0.6, pch=19, col="red",xlim=c(-3,1.5), xlab="log ratio")

for (i in 1:length(TAB_C$lnR)) {
	lb_i<-TAB_C$lnR[i]-1.96*sqrt(TAB_C$Var_lnR[i])
	ub_i<-TAB_C$lnR[i]+1.96*sqrt(TAB_C$Var_lnR[i])
	lines(c(lb_i,ub_i),c(i,i))
}
abline(v=0)

# DerSimonian and Laird
Result<-matrix(nrow=2,ncol=3)

w<-1/TAB_C$Var_lnR
L<-TAB_C$lnR

Q<-sum(w*(L)^2)-((sum(w*L))^2)/sum(w)
1-pchisq(Q,length(L)-1)

Vb<-(Q-(length(L)-1))/(sum(w)-(sum(w^2)/sum(w)))

wT<-1/(TAB_C$Var_lnR+Vb)
MEF<-sum(wT*L)/sum(wT)
SE<-sqrt(1/(sum(wT)))
print(MEF)
print(SE)
print(exp(MEF))
R_lb<-exp(MEF-1.96*SE)
R_ub<-exp(MEF+1.96*SE)
print(R_lb)
print(R_ub)

Result[1,]<-c(exp(MEF),R_lb,R_ub)

######################################################################################### 
# Random effect on study, weighting
# Data format

Data<-groupedData(lnR~1|Study, data=TAB_C)

Fit<-lme(lnR~1, random = ~ 1, data=Data, weight= varFixed(~Var_lnR), method="REML")
summary(Fit)
# Estimated ratio
R<-exp(Fit$coefficients$fixed)
R_lb<-exp(Fit$coefficients$fixed-1.96*sqrt(Fit$varFix))
R_ub<-exp(Fit$coefficients$fixed+1.96*sqrt(Fit$varFix))
Result[2,]<-c(R,R_lb,R_ub)

# Results
Result<-as.data.frame(Result)
names(Result)<-c("Estimate","LowerBound", "UpperBound")
rownames(Result) = c("DerSimonian and Laird", "Random effect on study, weighting")
Result

plot(Result$Estimate[1:2], 1:2, xlab="Ratio", ylab="Method", xlim=c(0.5,1), yaxt="n")
lines(c(Result$LowerBound[1],Result$UpperBound[1]),c(1,1))
lines(c(Result$LowerBound[2],Result$UpperBound[2]),c(2,2))
text(0.9,1:2,rownames(Result))
# end of file