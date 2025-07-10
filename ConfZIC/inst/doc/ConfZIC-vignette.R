## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------

library(ConfZIC)

## -----------------------------------------------------------------------------
library("ConfZIC")
data(Concrete)
x=Concrete
Y=x[,9] #dependent variable
#independent variables
X1=x[,1];X2=x[,2];X3=x[,3];X4=x[,4];
X5=x[,5];X6=x[,6];X7=x[,7];X8=x[,8];
mydata=cbind(Y,X1,X2,X3,X4,X5,X6,X7,X8) #data matrix
RankReg(mydata,0.95,"BIC")

## -----------------------------------------------------------------------------
x=Concrete
Y=x[,9] #dependent variable
model1=lm(Y~X1)
model2=lm(Y~X1+X2)
regZIC.test(model1,model2,model_ZIC="BIC",data=mydata,alpha=0.05)



## -----------------------------------------------------------------------------
library("ConfZIC")
data(Sunspots)
x=Sunspots
RankTS(x,max.p=13,max.q=13,0.95,"AICc")


## -----------------------------------------------------------------------------
model1=try(arima(x,order=c(1,0,1),method="ML",include.mean=FALSE),silent = TRUE)
model2=try(arima(x,order=c(1,0,0),method="ML",include.mean=FALSE),silent = TRUE)
tsZIC.test(x,model1,model2,model_ZIC="AIC",alpha=0.05)

