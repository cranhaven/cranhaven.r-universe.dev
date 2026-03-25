# kensyn Package. Knowledge synthesis in Agriculture: from experimental network to meta-analyisis.
# ch07. Meta-analysis to estimation the effectiveness of a fungicidal treatment to control Phyllosticta citricarpa, a citrus fungus (with lme4)
# David Makowski (INRA) 2017-11-01
library(lme4)
library(KenSyn)

DataSetT<-citrus

P_C<-DataSetT$NbDiseasedF[DataSetT$Fung_Gp==0]/DataSetT$NbFruits[DataSetT$Fung_Gp==0]
P_E<-DataSetT$NbDiseasedF[DataSetT$Fung_Gp==1]/DataSetT$NbFruits[DataSetT$Fung_Gp==1]
P_C<-P_C[order(P_E)]
P_E<-P_E[order(P_E)]

###############################################################################################
par(mfrow=c(2,2))
dotchart(P_E, xlab="Proportion of disease fruits",xlim=c(0,1),pch=19)
points(P_C,1:length(P_C))
title("A.", adj=0)

Odds_C<-P_C/(1-P_C)
Odds_E<-P_E/(1-P_E)
Odds_ratio<-Odds_E/Odds_C
dotchart(Odds_C, xlab="Odds in controls")
title("B.", adj=0)
abline(v=1)
dotchart(Odds_E, pch=19, xlab="Odds in experimental treatments")
title("C.", adj=0)
abline(v=1)
dotchart(Odds_ratio, pch=19, xlab="Odds ratio")
title("D.", adj=0)
abline(v=1)

###############################################################################################
summary(DataSetT)

###############################################################################################
Mod<-glmer(cbind(NbDiseasedF, NbFruits-NbDiseasedF)~Fung_Gp+(1+Fung_Gp|Code), family=binomial, data=DataSetT)
summary(Mod)

#Odds ratio
exp(fixef(Mod)[2]-1.96*sqrt(vcov(Mod)[4]))
exp(fixef(Mod)[2]+1.96*sqrt(vcov(Mod)[4]))

##Incidence

##untreated case
#estimated incidence 
exp(fixef(Mod)[1])/(1+exp(fixef(Mod)[1]))
#lower bound of IC
exp(fixef(Mod)[1]-1.96*sqrt(vcov(Mod)[1]))/(1+exp(fixef(Mod)[1]-1.96*sqrt(vcov(Mod)[1])))
#upper bound of IC
exp(fixef(Mod)[1]+1.96*sqrt(vcov(Mod)[1]))/(1+exp(fixef(Mod)[1]+1.96*sqrt(vcov(Mod)[1])))

##treated case
#Variance  of estimated expected value of logit
VAR<-vcov(Mod)[1]+vcov(Mod)[4]+2*vcov(Mod)[2]
#Incidence estimated
exp(fixef(Mod)[1]+fixef(Mod)[2])/(1+exp(fixef(Mod)[1]+fixef(Mod)[2]))
#lower bound of IC
exp(fixef(Mod)[1]+fixef(Mod)[2]-1.96*sqrt(VAR))/(1+exp(fixef(Mod)[1]+fixef(Mod)[2]-1.96*sqrt(VAR)))
#upper bound of IC
exp(fixef(Mod)[1]+fixef(Mod)[2]+1.96*sqrt(VAR))/(1+exp(fixef(Mod)[1]+fixef(Mod)[2]+1.96*sqrt(VAR)))

# end of file