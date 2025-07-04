## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----tailor made functions, include=FALSE-------------------------------------
t2c <- function(x){
  "
  Function to transform an upper run-off triangle into a half-square.
    
  This function takes an upper run-off triangle as input.
  
  It returns a half square.
  "
  I= dim(x)[1]
  J= dim(x)[2]
  
  mx=matrix(NA,nrow=I,ncol=J)
  for(i in 1:(I)){
    for(j in 1:(J)){
      if(i+j<=J+1){
        mx[j,(i+j-1)]=x[i,j]
      }
    }
  }
  return(mx)
}


c2t <- function(x){
  "
  Function to transform a square into an upper run-off triangle.
  
  This function takes a half square as input.
  
  It returns an upper run-off triangle. 
  "
  I= dim(x)[1]
  J= dim(x)[2]
  
  mx=matrix(NA,nrow=I,ncol=J)
  for(i in 1:(I)){
    for(j in 1:(J)){
      if(i+j<=J+1){
        mx[i,j]=x[j,(i+j-1)]
      }
    }
  }
  return(mx)
}



## ----sand box, message=FALSE, warning=FALSE-----------------------------------
library(ChainLadder)

data("AutoBI")
dataset=AutoBI$AutoBIPaid 
dataset

colnames(dataset)=c(0:(dim(dataset)[1]-1))
rownames(dataset)=c(0:(dim(dataset)[1]-1))


## ----life representation, echo=FALSE------------------------------------------
t2c(dataset)

## ----rtt data, include=FALSE--------------------------------------------------
library(clmplus)
rtt <- AggregateDataPP(cumulative.payments.triangle = dataset)


## ----amodel, message=FALSE, warning=FALSE-------------------------------------
a.model.fit=clmplus(AggregateDataPP =  rtt, 
             hazard.model = "a")


## ----amodeloutput1, message=FALSE---------------------------------------------

a.model.fit$fitted_development_factors


## ----amodeloutput2, message=FALSE---------------------------------------------

a.model.fit$fitted_effects


## ----amodelpredict, message=FALSE---------------------------------------------

a.model <- predict(a.model.fit)


## ----dfpredicted, message=FALSE-----------------------------------------------

a.model$development_factors_predicted


## ----ltpredicted, message=FALSE-----------------------------------------------

a.model$lower_triangle


## ----ftpredicted, message=FALSE-----------------------------------------------

a.model$full_triangle


## ----predictionsoneyear, message=FALSE----------------------------------------

a.model.2 <- predict(a.model.fit,
                     forecasting_horizon=1)


## ----mack, message=FALSE, warning=FALSE---------------------------------------
mck.chl <- MackChainLadder(dataset)
ultimate.chl=mck.chl$FullTriangle[,dim(mck.chl$FullTriangle)[2]]
diagonal=rev(t2c(mck.chl$FullTriangle)[,dim(mck.chl$FullTriangle)[2]])

## ----clm replicated-----------------------------------------------------------
data.frame(ultimate.cost.mack=ultimate.chl,
           ultimate.cost.clmplus=a.model$ultimate_cost,
           reserve.mack=ultimate.chl-diagonal,
           reserve.clmplus=a.model$reserve
           )

cat('\n Total reserve:',
    sum(a.model$reserve))


## ----apc clm------------------------------------------------------------------
library(apc)

ds.apc = apc.data.list(cum2incr(dataset),
                       data.format = "CL")

ac.model.apc = apc.fit.model(ds.apc,
                         model.family = "od.poisson.response",
                         model.design = "AC")


## ----show comparison----------------------------------------------------------

ac.model.apc$coefficients.canonical[,'Estimate']

ac.fcst.apc = apc.forecast.ac(ac.model.apc)

data.frame(reserve.mack=ultimate.chl-diagonal,
           reserve.apc=c(0,ac.fcst.apc$response.forecast.coh[,'forecast']),
           reserve.clmplus=a.model$reserve
           
           )



## ----fitted ax amodel---------------------------------------------------------
a.model.fit$fitted_effects


## ----plot effects ax, message=FALSE, warning=FALSE----------------------------
plot(a.model)

## ----amodel residuals---------------------------------------------------------
#make it triangular
plot(a.model.fit)

## ----message=FALSE, warning=FALSE---------------------------------------------
ac.model.fit <- clmplus(rtt, 
                    hazard.model="ac")

ac.model <- predict(ac.model.fit,
                    gk.fc.model='a')
plot(ac.model.fit)

## ----message=FALSE, warning=FALSE---------------------------------------------

plot(ac.model)


## ----apapc models, message=FALSE, warning=FALSE-------------------------------
ap.model.fit = clmplus(rtt,
                   hazard.model = "ap")

ap.model<-predict(ap.model.fit, 
                   ckj.fc.model='a',
                   ckj.order = c(0,1,0))

apc.model.fit = clmplus(rtt,hazard.model = "apc")

apc.model<-predict(apc.model.fit, 
                   gk.fc.model='a', 
                   ckj.fc.model='a',
                   gk.order = c(1,1,0),
                   ckj.order = c(0,1,0))

## ----residuals apmodel--------------------------------------------------------
plot(ap.model.fit)

## ----residuals apcmodel-------------------------------------------------------
plot(apc.model.fit)

## ----apc effects, message=FALSE, warning=FALSE--------------------------------
plot(apc.model)

