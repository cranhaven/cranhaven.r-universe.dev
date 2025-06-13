### R code from vignette source 'Identification.Rnw'

###################################################
### code chunk number 1: Identification.Rnw:182-191
###################################################
# attach apc library
library(apc)
# get data from precoded function
data <- data.Belgian.lung.cancer()
# Estimate APC model
model.family <- "poisson.dose.response"
model.design <- "APC"
fit <- apc.fit.model(data,model.family,model.design)
c.c <- fit$coefficients.canonical


###################################################
### code chunk number 2: Identification.Rnw:210-212
###################################################
id <- apc.identify(fit)
c.ssdd <- id$coefficients.ssdd


###################################################
### code chunk number 3: Identification.Rnw:275-276
###################################################
c.detrend <- id$coefficients.detrend


###################################################
### code chunk number 4: Identification.Rnw:334-340
###################################################
# fit AC model
model.design <- "AC"
fit.ac <- apc.fit.model(data,model.family,model.design)
# identify to get sums of difference parameters
id.ac <- apc.identify(fit.ac)
c.demean <- id.ac$coefficients.demean


###################################################
### code chunk number 5: Identification.Rnw:347-349
###################################################
# get difference parameters
c.dif <- id.ac$coefficients.dif


###################################################
### code chunk number 6: Identification.Rnw:362-369
###################################################
# Coefficients
fit$coefficients
# Arrange linear predictors as matrix in original format
# Create matrix of original dimension
m.fit <- fit$response       
m.fit[fit$index.data] <-fit$linear.predictors
m.fit


###################################################
### code chunk number 7: Identification.Rnw:375-379
###################################################
# Canonical paramters
c.c
# Check canonical coefficients are the same as the standard coefficients
sum(abs(c.c[,1]-fit$coefficients))


###################################################
### code chunk number 8: Identification.Rnw:385-397
###################################################
# get design matrix
m.design <- apc.get.design(fit)$design
# create matrix of original dimension
m.fit.canonical.no.dose <- fit$response     
m.fit.canonical.no.dose[fit$index.data] <- m.design %*% c.c[,1]
if(is.null(data$dose)==TRUE)
 m.fit.canonical <- m.fit.canonical.no.dose
if(is.null(data$dose)==FALSE)
 m.fit.canonical <- m.fit.canonical.no.dose + log(data$dose)
m.fit.canonical
# Check canonical coefficients give same fit as standard fit
sum(abs(m.fit-m.fit.canonical),na.rm=TRUE)


###################################################
### code chunk number 9: Identification.Rnw:407-408
###################################################
c.ssdd


###################################################
### code chunk number 10: Identification.Rnw:430-453
###################################################
age <- fit$index.trap[,1]
coh <- fit$index.trap[,2]
# From this we get the period. Need to correct for lowest period value.
per.zero <- fit$per.zero
per <- age+coh-1-per.zero
U <- fit$U
# Then we can compute the prediction as a vector
if(model.design=="APC")
{
 prediction <- c.ssdd[1,1] +
  + c.ssdd[2,1]*(age-U) +
  + c.ssdd[3,1]*(coh-U) +
  + c.ssdd[id$index.age.max[age],1] +
  + c.ssdd[id$index.per.max[per],1] +
  + c.ssdd[id$index.coh.max[coh],1]
 # Then we embed it into a matrix 
 m.fit.ssdd <- fit$response      
 m.fit.ssdd[fit$index.data] <- prediction
 # Add dose
 m.fit.ssdd  <- m.fit.ssdd + log(data$dose)
 # Check fit is correct
 sum(abs(m.fit.canonical-m.fit.ssdd),na.rm=TRUE)
} 


###################################################
### code chunk number 11: Identification.Rnw:467-498
###################################################
# We need two further variables
slopes <- fit$slopes
difdif <- fit$difdif
# Compute the prediction as a vector
prediction <- c.ssdd[1,1]
# Add the age double differences and age slope
if(difdif[1]) # TRUE if age double differences
 prediction <- prediction + c.ssdd[id$index.age.max[age],1]
if(difdif[2]) # TRUE if period double differences
 prediction <- prediction + c.ssdd[id$index.per.max[per],1]
if(difdif[3]) # TRUE if cohort double differences
 prediction <- prediction + c.ssdd[id$index.coh.max[coh],1]
if(slopes[1]) # TRUE if age linear trend
{ prediction <- prediction + c.ssdd[2,1]*(age-U)
  if(slopes[3])
  prediction <- prediction + c.ssdd[3,1]*(coh-U)
}  
if(slopes[1]==FALSE)
{ if(slopes[2])
   prediction <- prediction + c.ssdd[2,1]*(per-1)
  if(slopes[3])
   prediction <- prediction + c.ssdd[2,1]*(coh-U)
}
# Then we embed it into a matrix 
m.fit.ssdd <- fit$response      
m.fit.ssdd[fit$index.data] <- prediction
# Add dose 
if(is.null(data$dose)==FALSE)
 m.fit.ssdd  <- m.fit.ssdd + log(data$dose)
# Check fit is correct 
sum(abs(m.fit.canonical-m.fit.ssdd),na.rm=TRUE)


###################################################
### code chunk number 12: Identification.Rnw:504-506
###################################################
apc.plot.fit(fit,type="sum.sum")
m.fit.canonical.no.dose


###################################################
### code chunk number 13: Identification.Rnw:533-562
###################################################
# We use age, coh, per, per.zero, id, slopes, difdif defined above.
# Compute the prediction as a vector
prediction <- c.detrend[1,1]
# Add the age double differences and age slope
if(difdif[1]) # TRUE if age double differences
 prediction <- prediction + c.detrend[id$index.age.max[age],1]
if(difdif[2]) # TRUE if period double differences
 prediction <- prediction + c.detrend[id$index.per.max[per],1]
if(difdif[3]) # TRUE if cohort double differences
 prediction <- prediction + c.detrend[id$index.coh.max[coh],1]
if(slopes[1]) # TRUE if age linear trend
{ prediction <- prediction + c.detrend[2,1]*(age-1)
  if(slopes[3])
  prediction <- prediction + c.detrend[3,1]*(coh-1)
}  
if(slopes[1]==FALSE)
{ if(slopes[2])
   prediction <- prediction + c.detrend[2,1]*(per-1)
  if(slopes[3])
   prediction <- prediction + c.detrend[2,1]*(coh-1)
}
# Then we embed it into a matrix 
m.fit.detrend <- fit$response      
m.fit.detrend[fit$index.data] <- prediction
# Add dose 
if(is.null(data$dose)==FALSE)
 m.fit.detrend  <- m.fit.detrend + log(data$dose)
# Check fit is correct 
sum(abs(m.fit.canonical-m.fit.detrend),na.rm=TRUE)


###################################################
### code chunk number 14: Identification.Rnw:568-570
###################################################
apc.plot.fit(fit)
m.fit.canonical.no.dose


###################################################
### code chunk number 15: Identification.Rnw:591-611
###################################################
if(model.design=="AC")
{
 ################################### 
 # Get fit of canonical parameters
 # get the canonical parameters
 c.c.ac <- fit.ac$coefficients.canonical
 # Get design matrix
 m.design.ac <- apc.get.design(fit.ac)$design
 # Create matrix of original dimension
 m.fit.canonical.ac <- fit.ac$response
 m.fit.canonical.ac[fit.ac$index.data] <- m.design.ac %*% c.c.ac[,1]
 #####################################
 # Get fit of sum of difference parameters
 prediction <- c.demean[1,1] +
  + c.demean[id.ac$index.age.sub[age],1] +       
  + c.demean[id.ac$index.coh.sub[coh],1]
 # Create matrix of original dimension
 m.fit.demean.ac <- fit.ac$response
 m.fit.demean.ac[fit.ac$index.data] <- prediction 
}


###################################################
### code chunk number 16: Identification.Rnw:617-623
###################################################
# ALL CHECKS
sum(abs(c.c[,1]-fit$coefficients))
sum(abs(m.fit-m.fit.canonical),na.rm=TRUE)
sum(abs(m.fit.canonical-m.fit.ssdd),na.rm=TRUE)
sum(abs(m.fit.canonical-m.fit.detrend),na.rm=TRUE)
sum(abs(m.fit.canonical.ac-m.fit.demean.ac),na.rm=TRUE)


