# kensyn Package. Knowledge synthesis in Agriculture : from experimental network to meta-analyisis.
# ch07. Meta-analysis to estimate the response of N2O emissions to the applied N fertilizer rate (with nlme)
# David Makowski (INRA) 2017-11-01

# Package for non linear mixed-effects models
library(nlme)
library(KenSyn)

# Data loading
TAB<-N2O

# add Rice binary variable
TAB$Rice=as.numeric(TAB$crop_type=="Wetland_rice")
TAB$Rice[is.na(TAB$Rice)]=0

TAB<-TAB[order(TAB$Ref_num),]

summary(TAB)

# plotting all the data of emission N2O depending on N fertilizer rate
plot(TAB$N_rate,TAB$N2O, xlab="N fertilizer rate (kg ha-1)",
ylab="N2O emission (kg ha-1)", pch=19)

# Defining the structure of the dataset for lme
groupedTAB <- groupedData(N2O ~ N_rate+Rice | Ref_num, data = TAB)

# Examples of N2O emission measurements obtained on 9 experiments for different doses of N fertilizer appliedpar(mfrow=c(1,1))
par(mfrow=c(3,3), mar=c(4.1,4.1,1,1))
ListNum<-c(12,363,312,232,158,226,87,21,17)
null<-sapply(ListNum, function(Num){plot(groupedTAB$N_rate[groupedTAB$Ref_num==Num],groupedTAB$N2O[groupedTAB$Ref_num==Num], xlab="N fertilizer rate (kg/ha)", ylab="N2O emission (kg/ha/an)", pch=19, xlim=c(0,350))})


# Model NL-N-RR-Rice # described in chapter
modele<-nlme(N20~exp(theta0+theta1*N_rate+theta2*Rice),
data=groupedTAB,
fixed=theta0+theta1+theta2~1,random=pdDiag(theta0+theta1~1),
start=c(theta0= 1.46, theta1= 0.002, theta2=0))
Coef<-coef(modele)
Coef

Param<-modele$coefficients$fixed
Param
# end of file