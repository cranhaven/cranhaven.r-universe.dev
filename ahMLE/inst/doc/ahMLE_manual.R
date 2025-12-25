## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages("ahMLE")

## ---- message=FALSE, warning=FALSE--------------------------------------------
library(ahMLE)

## ---- message=FALSE, warning=FALSE--------------------------------------------

require(survival)
require(invGauss)
library(ahMLE)

data(d.oropha.rec)

SData = data.frame(time = d.oropha.rec$time, 
                   constant = 1,
                   sex = d.oropha.rec$sex, 
                   treatm = d.oropha.rec$treatm, 
                   grade = d.oropha.rec$grade, 
                   age = d.oropha.rec$age, 
                   cond = d.oropha.rec$cond, 
                   tstage = d.oropha.rec$tstage, 
                   nstage = d.oropha.rec$nstage,
                   event = d.oropha.rec$status)

# Data rescaled
SData$sex = SData$sex - 1
SData$treatm = SData$treatm -1
SData$grade = SData$grade -1
SData$age = (SData$age -60)/10
SData$cond = SData$cond -1
SData$tstage = SData$tstage - 1


## ---- message=FALSE, warning=FALSE, results="hide"----------------------------
formula_input = Surv(time= time, event = event) ~ sex + treatm +grade + age +cond + tstage + nstage

# Use Aalen's OLS method to compute the cumulative beta

beta_aalen = ah(formula_input, data = SData, method = "aalen")
Cbeta_aalen = beta_aalen$cumbeta


# Use (default) maximum likelihood method to compute the cumulative beta

beta_mle= ah(formula_input, data = SData, progbar = TRUE)
Cbeta_mle = beta_mle$cumbeta


## ---- message=FALSE, warning=FALSE, fig.height = 3, fig.width = 6, fig.align = "center"----

old.par <- par(no.readonly = TRUE)
par(mfrow=c(1,1))
par(mar=c(5, 6, 4, 2))

time_grid = beta_aalen$cumbeta[,1]
plot(time_grid,Cbeta_mle[,2],type="l", lwd = 2, col=colors()[258], ylim=c(-0.1,1.1),  ylab = "Cumulative beta",xlab = "Days from diagnosis", cex.lab=1)
lines(time_grid,Cbeta_aalen[,2], type="l", lty = 5, lwd =2, col="red")
legend("topright", legend=c("MLE","OLS"), col=c(colors()[258],"red"),lty=c(1,5), cex=1,lwd = 1)
par(old.par)


