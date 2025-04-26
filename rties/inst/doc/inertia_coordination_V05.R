## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(rties)
data1 <- rties_ExampleDataFull

## ---- fig.height=5, fig.width=5-----------------------------------------------
temp <- subset(data1, couple %in% c(2,5,27,31,47,60,103,110))
autoCorPlots(basedata=temp, dyadId="couple", personId="person", obs_name="dial", time_name="time")
crossCorPlots(basedata=temp, dyadId="couple", personId="person", obs_name="dial", time_name= "time")

## -----------------------------------------------------------------------------
data2 <- dataPrep(basedata=data1, dyadId="couple", personId="person", obs_name="dial", dist_name="female",
                  time_name="time", time_lag="absMaxCC") 

## -----------------------------------------------------------------------------
compare <- indivInertCoordCompare(data2)
summary(compare$R2inert)
summary(compare$R2coord)
summary(compare$R2inertCoord)
summary(compare$R2dif_IC_I)

## ---- eval=FALSE--------------------------------------------------------------
#  figures <- indivInertCoordPlots(data2, "inertCoord", dist0name="Men", dist1name="Women", plot_obs_name="Dial", minMax=c(.05,.95), printPlots=F)
#  modelPlots <- gridExtra::marrangeGrob(grobs= figures, ncol=2, nrow=3)
#  ggsave(filename="modelPlots.pdf", plot=modelPlots, device="pdf")

## ---- eval=FALSE--------------------------------------------------------------
#  residPlots <- inertCoordResids(prepData=data2, whichModel="inertCoord", printPlots=F)
#  residPlots <- suppressMessages(gridExtra::marrangeGrob(grobs= residPlots, ncol=2, nrow=3))
#  ggsave(filename="residPlots.pdf", plot=residPlots, device="pdf")

## -----------------------------------------------------------------------------
ic <- indivInertCoord(prepData=data2, whichModel="inertCoord")
head(ic$params)

## ---- warning=F, fig.width=4.5------------------------------------------------
lpaData <- inspectProfiles(whichModel="inertCoord", prepData=data2, paramEst=ic$params, n_profiles=3, seed=21, numPlots=1)

## ---- fig.width=4-------------------------------------------------------------
plots <- inertCoordPlotTraj(prepData=data2, paramEst=ic$params, n_profiles=3, dist0name = "w", dist1name = "m", plot_obs_name = "EE", time_length=100, minMax = c(.025,.975), seed=6, numPlots=1)

## -----------------------------------------------------------------------------
fullData <- makeFullData(basedata=data1, dyadId="couple", personId="person", dist_name="female", 
                         lpaData=lpaData, params=ic$params)
head(fullData)

## ---- eval=FALSE--------------------------------------------------------------
#  # display plots on screen
#  profilePlots <- plotDataByProfile(prepData=data2, fullData=fullData, n_profiles= 3, dist0name="men", dist1name="women", plot_obs_name="Dial", printPlots=T)
#  # save plots to pdf
#  pdf("dataByProfile.pdf")
#  plotDataByProfile(prepData=data2, fullData=fullData, n_profiles= 2, dist0name="men", dist1name="women", plot_obs_name="Dial")
#  dev.off()

