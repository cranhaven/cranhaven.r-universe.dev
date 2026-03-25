################################################################################
# "Working with dynamic models for agriculture"
# R script for practical work
# Daniel Wallach (INRA), David Makowski (INRA), James W. Jones (U.of Florida),
# Francois Brun (ACTA)
# version : 2018-03-04
# Model described in the book, Appendix. Models used as illustrative examples: description and R code
# model for Course SMACH - January 2014
################################ MAIN PROGRAM #####################################
library(ZeBook)
weather=subset(weather_FranceWest, WEYR==1997 & idsite==39)
################################################################################
# reproduce simulation from Zadoks 1971 (Phytopathology 61:441-598, 19 May 1971)
weather$TMIN = 10
weather$TMAX = 10
# unlimited number of Site
SITE0= 10^30
sdate = 1
ldate = 150
out=zakoks.original.model(nlpd=16*10,nipd=30*10,dmfr=16,SITE0 = SITE0, weather, sdate = sdate, ldate = ldate )
################################################################################
# Latent period varies from 8-14 days in the field with temperatures of 10-25 C.
# With temperatures of 5-10 C latent periods of several weeks are common.
# typical infectious period for leaf rust about 30 days
par(mfrow=c(3,1), mar=c(4,4,2,4))
graph_epid_s(zakoks.original.model(nlpd=8*10,nipd=30*10,dmfr=16,SITE0 = SITE0, weather, sdate = sdate, ldate = ldate, XLAT0=SITE0*1e-14),typel="l")
graph_epid_s(zakoks.original.model(nlpd=16*10,nipd=30*10,dmfr=16,SITE0 = SITE0, weather, sdate = sdate, ldate = ldate, XLAT0=SITE0*1e-14),typel="l")
graph_epid_s(zakoks.original.model(nlpd=24*10,nipd=30*10,dmfr=16,SITE0 = SITE0, weather, sdate = sdate, ldate = ldate, XLAT0=SITE0*1e-14),typel="l")

################################################################################
# Figure 7.
par(mfrow=c(3,1), mar=c(3,3,0,0))
graph_epid(zakoks.original.model(nlpd=4*10,nipd=1*10,dmfr=16,SITE0 = SITE0, weather, sdate = sdate, ldate = sdate+55 ))
graph_epid(zakoks.original.model(nlpd=4*10,nipd=2*10,dmfr=16,SITE0 = SITE0, weather, sdate = sdate, ldate = sdate+55 ),typel="l")
graph_epid(zakoks.original.model(nlpd=4*10,nipd=4*10,dmfr=16,SITE0 = SITE0, weather, sdate = sdate, ldate = sdate+55 ),typel="l")

################################################################################
# Figure 8.
par(mfcol=c(2,2), mar=c(3,3,1,0))
#A)Various latent period
graph_epid(zakoks.original.model(nlpd=4*10,nipd=10*10,dmfr=16,SITE0 = SITE0, weather, sdate = sdate, ldate = sdate+55 ),typel="l", all=FALSE, param=FALSE)
title("Various latent period",cex.main=0.75)
for (nlpd in c(4,8,12,16)*10){
  sim=zakoks.original.model(nlpd=nlpd,nipd=10*10,dmfr=16,SITE0 = SITE0, weather, sdate = sdate, ldate = sdate+55 )$sim
  lines(sim$DACE,sim$XSEV,lty=1, lwd=3)
  text(53, sim$XSEV[sim$DACE==50], nlpd,cex=0.8, col="darkgrey")
}
#B)Various infectious period
graph_epid(zakoks.original.model(nlpd=4*10,nipd=1*10,dmfr=16,SITE0 = SITE0, weather, sdate = sdate, ldate = sdate+55 ),typel="s", all=FALSE, param=FALSE)
title("Various infectious period",cex.main=0.75)
for (nipd in c(2,3,4,16)*10){
  sim=zakoks.original.model(nlpd=4*10,nipd=nipd,dmfr=16,SITE0 = SITE0, weather, sdate = sdate, ldate = sdate+55 )$sim
  lines(sim$DACE,sim$XSEV,lty=1, lwd=3)
  text(50, sim$XSEV[sim$DACE==50], nipd,cex=0.8, col="darkgrey")
}

#C)Various infectious period on  cumulative total of removal
graph_epid(zakoks.original.model(nlpd=4*10,nipd=10*10,dmfr=16,SITE0 = SITE0, weather, sdate = sdate, ldate = sdate+55 ),typel="l", all=FALSE, param=FALSE)
title("Various infectious period on XCTR",cex.main=0.75)
for (nipd in c(4,8,12,16)*10){
  sim=zakoks.original.model(nlpd=4*10,nipd=nipd,dmfr=16,SITE0 = SITE0, weather, sdate = sdate, ldate = sdate+55 )$sim
  lines(sim$DACE,sim$XCTR,lty=1, lwd=1, col="grey")
  text(50, sim$XCTR[sim$DACE==50], nipd,cex=0.8, col="darkgrey")
}
#D)Various multiplication factor
graph_epid(zakoks.original.model(nlpd=4*10,nipd=8*10,dmfr=16,SITE0 = SITE0, weather, sdate = sdate, ldate = sdate+55 ),typel="l", all=FALSE, param=FALSE)
title("Various infectious period",cex.main=0.75)
for (dmfr in c(4,8,12,16)){
  sim=zakoks.original.model(nlpd=4*10,nipd=8*10,dmfr=dmfr,SITE0 = SITE0, weather, sdate = sdate, ldate = sdate+55 )$sim
  lines(sim$DACE,sim$XSEV,lty=1, lwd=3)
  text(50, sim$XSEV[sim$DACE==50], dmfr,cex=0.8, col="darkgrey")
}



# Figure 9B
#SITE0= 5*10^9 # from Zadoks 1971


# end of file