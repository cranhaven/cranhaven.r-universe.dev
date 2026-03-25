################################################################################
# "Working with dynamic models for agriculture"
# R script for pratical work
# Daniel Wallach (INRA), David Makowski (INRA), James W. Jones (U.of Florida),
# Francois Brun (ACTA)
# version : 2018-02-25
# Model described in the book, Appendix. Models used as illustrative examples: description and R code
# Simulation with the Maize model, with harvest index and yield.
# Reproduce the simulation from Muchow RC, Sinclair TR, and Bennett JM (1990). Temperature and Solar Radiation
# Effects on Potential Maize Yield across Locations AGRONOMY JOURNAL, VOL. 82, MARCH-APRIL 1990
############################## MAIN PROGRAM ####################################
library(ZeBook)

#weather_GNS1983 = weather_GNS[weather_GNS$year==1983,]

########################
# Gainesville experiment in 1982
# observation
dataobs=data.frame(idsite="Gainesville_FL_USA",year=1982, cultivar="McCurdy84AA",DAS=c(60,75,80,90,95,100,105,115,120),
           Bobs=c(300,850,1100,1350,1400,1600,1900,2250,2200),
           YIELDobs=c(NA,NA,NA,NA,200,450,750,1038,1038))
# simulation
weather_GNS1982 = weather_GNS[weather_GNS$year==1982,]
AM=750 #area of the largest leaf (cm2)
Population=7.2 # number of plant per square meter (-)
TLN=17 #total number of leaves initiated (-)
sdate=as.numeric(format(as.Date("1982-02-26"),'%j'))
res_GNS1982 = maize.muchow.model(AM=AM,TLN=TLN,Population=Population,sdate=sdate,weather=weather_GNS1982)
maize.muchow.graph(res_GNS1982)
res_GNS1982$sim$DAS = res_GNS1982$sim$day-sdate
par(mfrow=c(1,1))
plot(res_GNS1982$sim$DAS, res_GNS1982$sim$B, type="l", xlab="Days after Sowing", ylab="Biomass and Yield (g.m-2)",lwd=2, ylim=c(0,2400))
lines(res_GNS1982$sim$DAS, res_GNS1982$sim$YIELD, lty=2,lwd=2)
points(dataobs$DAS,dataobs$Bobs,pch=15)
points(dataobs$DAS,dataobs$YIELDobs,pch=16)

res_GNS1982$daysilking-sdate
res_GNS1982$daymaturity-sdate
res_GNS1982$FinalYield
#res_GNS1982$tab_leaf

TT = res_GNS1982$sim$TT1-87
#plot(TT,pmin(round(2.5*exp((TT)*0.00225)), 17) )
res_GNS1982$sim

#end of file