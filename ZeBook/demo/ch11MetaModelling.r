################################################################################
# "Working with dynamic models for agriculture" Edition 3
# R script for practical work
# Daniel Wallach (INRA), David Makowski (INRA), James W. Jones (U.of Florida),
# Francois Brun (ACTA)
# version : 2018-11-08 by David Makowski
# Demo CH11 : Metamodeling
# Final Biomass (g/m2) meta-models

DataSet<-maize.data_MetaModelling

head(DataSet)

#Model without interaction
Mod0<-glm(B~T1+T2+T3+RAD1+RAD2+RAD3, data=DataSet)
summary(Mod0)

#Model with interaction + stepwise
Mod1<-glm(B~T1*T2*T3*RAD1*RAD2*RAD3, data=DataSet)
summary(Mod1)
ModSel<-step(Mod1, k=log(length(DataSet$B)))

RMSE_0<-sqrt(mean((DataSet$B-predict(Mod0))^2))
RMSE_sel<-sqrt(mean((DataSet$B-predict(ModSel))^2))
RMSE_0
RMSE_sel

#Graphics
par(mfrow=c(1,2))
plot(predict(Mod0),DataSet$B, xlab="Meta-model output", ylab="Maize model output")
abline(0,1)
title("A.                                        ")
text(2400,3100,paste("RMSE= ",round(RMSE_0, digits=2)))
plot(predict(ModSel),DataSet$B, xlab="Meta-model output", ylab="Maize model output")
abline(0,1)
title("B.                                        ")
text(2400,3100,paste("RMSE= ", round(RMSE_sel, digits=2)))

dev.new()
plot(DataSet$RAD3,residuals(ModSel))
abline(h=0)


#Cross-validation
B_pred_0<-rep(NA,length(DataSet$B))
B_pred_sel<-rep(NA,length(DataSet$B))

List_year<-unique(DataSet$Year)

for (i in 1:length(List_year)) 
{
Training_i<-DataSet[DataSet$Year!=List_year[i],]
Test_i<-DataSet[DataSet$Year==List_year[i],]	
Mod0<-glm(B~T1+T2+T3+RAD1+RAD2+RAD3, data=Training_i)
B0_i<-predict(Mod0, newdata=Test_i)
B_pred_0[DataSet$Year==List_year[i]]<-B0_i
Mod1<-glm(B~T1*T2*T3*RAD1*RAD2*RAD3, data=Training_i)
ModSel<-step(Mod1, k=log(length(Training_i$B)))
Bsel_i<-predict(ModSel, newdata=Test_i)
B_pred_sel[DataSet$Year==List_year[i]]<-Bsel_i	
}

RMSEP_0<-sqrt(mean((DataSet$B-B_pred_0)^2))
RMSEP_sel<-sqrt(mean((DataSet$B-B_pred_sel)^2))
RMSEP_0
RMSEP_sel

dev.new()
par(mfrow=c(1,2))
plot(B_pred_0,DataSet$B, xlab="Meta-model output", ylab="Maize model output")
abline(0,1)
title("A.                                        ")
text(2400,3100,paste("RMSEP= ", round(RMSEP_0, digits=2)))
plot(B_pred_sel,DataSet$B, xlab="Meta-model output", ylab="Maize model output")
abline(0,1)
title("B.                                        ")
text(2400,3100,paste("RMSEP= ", round(RMSEP_sel, digits=2)))

##GAM
library(mgcv)
Mod_gam<-gam(B~s(T1)+s(T2)+s(T3)+s(RAD1)+s(RAD2)+s(RAD3), data=DataSet)
summary(Mod_gam)
dev.new()
par(mfrow=c(2,3))
plot(Mod_gam)

RMSE_gam<-sqrt(mean((DataSet$B-predict(Mod_gam))^2))
RMSE_gam

#Cross-validation
B_pred_gam<-rep(NA,length(DataSet$B))

List_year<-unique(DataSet$Year)

for (i in 1:length(List_year)) 
{
Training_i<-DataSet[DataSet$Year!=List_year[i],]
Test_i<-DataSet[DataSet$Year==List_year[i],]	
Mod_i<-gam(B~s(T1)+s(T2)+s(T3)+s(RAD1)+s(RAD2)+s(RAD3), data=Training_i)
B_gam_i<-predict(Mod_i, newdata=Test_i)
B_pred_gam[DataSet$Year==List_year[i]]<-B_gam_i	
}

RMSEP_gam<-sqrt(mean((DataSet$B-B_pred_gam)^2))
RMSEP_gam

dev.new()
par(mfrow=c(1,2))
plot(predict(Mod_gam),DataSet$B, xlab="Meta-model output", ylab="Maize model output")
abline(0,1)
title("A.                                        ")
text(2400,3100,paste("RMSE= ", round(RMSE_gam, digits=2)))
plot(B_pred_gam,DataSet$B, xlab="Meta-model output", ylab="Maize model output")
abline(0,1)
title("B.                                        ")
text(2400,3100,paste("RMSEP= ", round(RMSEP_gam, digits=2)))

####Regression tree
library(rpart)
library(rpart.plot)

Mod_tree<-rpart(B~T1+T2+T3+RAD1+RAD2+RAD3,data=DataSet)
print(Mod_tree)
dev.new()
par(mfrow=c(1,1))
rpart.plot(Mod_tree)
#text(Mod_tree)

###Random forest
library(randomForest)
Mod_RF<-randomForest(B~T1+T2+T3+RAD1+RAD2+RAD3,data=DataSet,ntree=500, mtry=6)
Mod_RF
dev.new()
par(mfrow=c(1,1))
plot(Mod_RF)
dev.new()
par(mfrow=c(1,1))
varImpPlot(Mod_RF,type=2)
RMSE_rf<-sqrt(mean((DataSet$B-predict(Mod_RF))^2))
RMSE_rf

#Cross-validation
B_pred_rf<-rep(NA,length(DataSet$B))

List_year<-unique(DataSet$Year)

for (i in 1:length(List_year)) 
{
Training_i<-DataSet[DataSet$Year!=List_year[i],]
Test_i<-DataSet[DataSet$Year==List_year[i],]	
Mod_i<-randomForest(B~T1+T2+T3+RAD1+RAD2+RAD3,data=Training_i, ntree=200)
B_rf_i<-predict(Mod_i, newdata=Test_i)
B_pred_rf[DataSet$Year==List_year[i]]<-B_rf_i	
}

RMSEP_rf<-sqrt(mean((DataSet$B-B_pred_rf)^2))
RMSEP_rf

dev.new()
par(mfrow=c(1,2))
plot(predict(Mod_RF),DataSet$B, xlab="Meta-model output", ylab="Maize model output")
abline(0,1)
title("A.                                        ")
text(2500,3100,paste("RMSE= ", round(RMSE_rf, digits=2)))
plot(B_pred_rf,DataSet$B, xlab="Meta-model output", ylab="Maize model output")
abline(0,1)
title("B.                                        ")
text(2600,3100,paste("RMSEP= ", round(RMSEP_rf, digits=2)))

# End of file