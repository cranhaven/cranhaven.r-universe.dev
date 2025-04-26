## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(rties)
data1 <- rties_ExampleDataFull
data2 <- dataPrep(basedata=data1, dyadId="couple", personId="person", obs_name="dial", 
                  dist_name="female", time_name="time") 

## -----------------------------------------------------------------------------
taus <- c(4,5)
embeds <- c(3,4,5)
delta <- 1

derivs <- estDerivs(prepData=data2, taus=taus, embeds=embeds, delta=delta, idConvention=500)

## -----------------------------------------------------------------------------
head(derivs$fitTable, 15)

## -----------------------------------------------------------------------------
summary(derivs$fitTable[ ,4])

## -----------------------------------------------------------------------------
summary(derivs$fitTable[ ,5])
summary(derivs$fitTable[ ,6])

## -----------------------------------------------------------------------------
compare <- indivCloCompare(derivData = derivs$data)
summary(compare$R2uncouple)
summary(compare$R2couple)
summary(compare$R2dif)

## ---- eval=FALSE--------------------------------------------------------------
#  plots <- indivCloPlots(derivData=derivs$data, whichModel="coupled", idConvention=500, plot_obs_name="dial", printPlots=F)
#  cloPlots <- gridExtra::marrangeGrob(grobs= plots, ncol=2, nrow=3)
#  ggsave(filename="cloPlots.pdf", plot=cloPlots, device="pdf")

## ---- eval=FALSE--------------------------------------------------------------
#  residPlots <- cloResids(derivData=derivs$data, whichModel="coupled", printPlots=F)
#  residPlots <- suppressMessages(gridExtra::marrangeGrob(grobs= residPlots, ncol=2, nrow=3))
#  ggsave(filename="residPlots.pdf", plot=residPlots, device="pdf")

## -----------------------------------------------------------------------------
clo <- indivClo(derivData=derivs$data, whichModel="coupled")
head(clo$params)

## ---- warning=F, fig.width=5--------------------------------------------------
lpaData <- inspectProfiles(whichModel="clo", prepData=data2, paramEst=clo$params, 
                           n_profiles=2, minMax = c(.1, .9), dist0name="men", dist1name="women")

## ---- fig.width=5-------------------------------------------------------------
plots <- cloPlotTraj(prepData=data2, paramEst=clo$params, n_profiles=2, time_length=500, 
                     minMax=c(.1, .9), dist0name="men", dist1name="women")

## -----------------------------------------------------------------------------
fullData <- makeFullData(basedata=data1, personId="person", dyadId="couple", dist_name="female", 
                         lpaData=lpaData, params=clo$params)
head(fullData)

## ---- eval=FALSE--------------------------------------------------------------
#  # display plots on screen
#  profilePlots <- plotDataByProfile(prepData=data2, fullData=fullData, n_profiles= 3, dist0name="men", dist1name="women", plot_obs_name="Dial", printPlots=T)
#  # save plots to pdf
#  pdf("dataByProfile.pdf")
#  plotDataByProfile(prepData=data2, fullData=fullData, n_profiles= 2, dist0name="men", dist1name="women", plot_obs_name="Dial")
#  dev.off()

