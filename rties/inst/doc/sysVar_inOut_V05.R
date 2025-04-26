## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- fig.show="hide", results="hide"-----------------------------------------
library(rties)
data1 <- rties_ExampleDataFull

data2 <- dataPrep(basedata=data1, dyadId="couple", personId="person", obs_name="dial", dist_name="female", time_name="time", time_lag="absMaxCC") 

ic <- indivInertCoord(prepData=data2, whichModel="inertCoord")

lpaData <- inspectProfiles(whichModel="inertCoord", prepData=data2, paramEst=ic$params, n_profiles=2) 

fullData <- makeFullData(basedata=data1, dyadId="couple", personId="person", dist_name="female", lpaData=lpaData, params=ic$params)

## -----------------------------------------------------------------------------
sysOut <- sysVarOut(fullData=fullData, sysVar_name="dyadInfluence", sysVarType="dyadic", dist0name="Men", dist1name="Women")
sysVarOutResults(sysOut$models$base, sysOut$models$profile)

## ---- fig.width=5-------------------------------------------------------------
sysVarOutPlots(fullData=fullData, sysVar_name="dyadSup", sysVarType="dyadic", testModel=sysOut$models$profile, dist0name=NULL, dist1name=NULL)

## -----------------------------------------------------------------------------
sysOut <- sysVarOut(fullData=fullData, sysVar_name="ambivB", sysVarType="indiv", dist0name="Men", dist1name="Women", family="binomial")
sysVarOutResults(sysOut$models$base, sysOut$models$profile, Gaussian=F)
sysVarOutResults(sysOut$models$base, sysOut$models$profilePlusDist, Gaussian=F)
sysVarOutResults(sysOut$models$base, sysOut$models$profileByDist, Gaussian=F)

## ---- fig.width=5-------------------------------------------------------------
sysVarOutPlots(fullData=fullData, sysVar_name="ambivB", sysVarType="indiv", testModel=sysOut$models$profileByDist, dist0name=NULL, dist1name=NULL, binomial=T)

## -----------------------------------------------------------------------------
sysIn <- sysVarIn(fullData=fullData, sysVar_name="dyadInfluence", n_profiles=2, sysVarType="dyadic")
sysVarInResults(sysIn$models$base, sysIn$models$sysVarMain, n_profiles=2)

## ---- fig.width=5-------------------------------------------------------------
inPlots <- sysVarInPlots(fullData=fullData, sysVar_name="dyadInfluence", sysVarType="dyadic", n_profiles=2)

## -----------------------------------------------------------------------------
sysIn <- sysVarIn(fullData=fullData, sysVar_name="conflictCat", n_profiles=2, sysVarType="indiv")
sysVarInResults(sysIn$models$base, sysIn$models$sysVarMain, n_profiles=2)
sysVarInResults(sysIn$models$base, sysIn$models$sysVarInteract, n_profiles=2)

## ---- fig.width=5-------------------------------------------------------------
inPlots <- sysVarInPlots(fullData=fullData, sysVar_name="conflictCat", sysVarType="indiv", n_profiles=2, testModel= sysIn$models$sysVarInteract, dist0name="men", dist1name="women")

