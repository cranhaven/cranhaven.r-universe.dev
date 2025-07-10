## ---- include = FALSE---------------------------------------------------------
# see https://r-pkgs.org/vignettes.html
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=6.5,fig.height=4,fig.align='center'
)

## ----eval=FALSE---------------------------------------------------------------
#  setwd(".../NADA2")

## ----warning=F,message=F,eval=F,results="hide"--------------------------------
#  check.packages <- function(pkg){
#    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#    if (length(new.pkg))
#      install.packages(new.pkg, dependencies = TRUE)
#    sapply(pkg, require, character.only = TRUE)
#  }
#  
#  pkg <- c("bestglm","car","cenGAM","EnvStats","fitdistrplus","Kendall",
#           "mgcv","multcomp","NADA","nlme","perm","rms","survminer",
#           "vegan","NADA2","bestglm")
#  check.packages(pkg)

## ----setup,warning=F,message=F,results="hide"---------------------------------
# Load Package
library(cenGAM)
library(EnvStats)
library(fitdistrplus)
library(Kendall)
library(mgcv)
library(multcomp)
library(NADA)
library(perm)
library(rms)
library(survminer)
library(vegan)
library(NADA2)
library(bestglm)
library(car)
library(nlme)
library(rms)


## ----load data example--------------------------------------------------------
data(Golden); # From NADA package

head(Golden,5L)

## ----eval=F-------------------------------------------------------------------
#  dat<-load(".../path/to/data/data.rda")

## ----eval=F-------------------------------------------------------------------
#  library(readxl)
#  dat <- read_excel(".../path/to/data/data.xlsx",sheet=4)
#  
#  # or
#  
#  library(openxlsx)
#  dat <- read.xlsx(".../path/to/data/data.xlsx",sheet=4)
#  

## ----eval=F-------------------------------------------------------------------
#  dat <- read.csv(".../path/to/data/data.csv")

## ----eval=F-------------------------------------------------------------------
#  dat <- read.table(".../path/to/data/data.txt")

## ----bxplot1,fig.width=6.5,fig.height=4,fig.align='center'--------------------
data(CuZn); # Data from the NADA package

cboxplot(CuZn$Zn,CuZn$ZnCen,CuZn$Zone,minmax = TRUE,Xlab="Zone",Ylab="Zn")

## ----bxplot2,fig.width=6.5,fig.height=4,fig.align='center'--------------------

cboxplot(CuZn$Zn,CuZn$ZnCen,CuZn$Zone,LOG=TRUE,Xlab="Zone",Ylab="Zn")

## ----bxplot3,fig.width=6.5,fig.height=4,fig.align='center'--------------------

cboxplot(CuZn$Zn,CuZn$ZnCen,CuZn$Zone,LOG=TRUE,show = TRUE, minmax = TRUE,
         Xlab="Zone",Ylab="Zn")

## ----scatplot1,fig.width=6.5,fig.height=4,fig.align='center'------------------
data(TCEReg); # Data from the NADA package

cenxyplot(TCEReg$PopDensity, 1-TCEReg$PopAbv1, TCEReg$TCEConc, TCEReg$TCECen)

## ----scatplot2,fig.width=6.5,fig.height=4,fig.align='center'------------------
cenxyplot(TCEReg$PopDensity, 1-TCEReg$PopAbv1, TCEReg$TCEConc, TCEReg$TCECen,
          xlab="Population Denisty",ylab="TCE Concentration, in ug/L")

## ----scatplot3,fig.width=6.5,fig.height=4,fig.align='center'------------------
cenxyplot(TCEReg$PopDensity, 1-TCEReg$PopAbv1, TCEReg$TCEConc, TCEReg$TCECen,
          xlab="Population Denisty",ylab="TCE Concentration, in ug/L", 
          main = "Your Title Here", log ="y")

## ----cdf1,fig.width=6.5,fig.height=4,fig.align='center'-----------------------
# Data already loaded

cen_ecdf(CuZn$Zn, CuZn$ZnCen)

## ----cdf2,fig.width=6.5,fig.height=4,fig.align='center'-----------------------
cen_ecdf(CuZn$Zn, CuZn$ZnCen,CuZn$Zone,
         Ylab="Zinc Concentration, in ug/L")

## ----cdf comp1,fig.width=6.5,fig.height=4,fig.align='center'------------------
data(ShePyrene); # From the NADA package

cenCompareCdfs(ShePyrene$Pyrene,ShePyrene$PyreneCen)

## ----cdf comp2,fig.width=6.5,fig.height=4,fig.align='center'------------------
cenCompareCdfs(ShePyrene$Pyrene,ShePyrene$PyreneCen,dist3 = "weibull")

## ----QQ1,fig.width=6.5,fig.height=4,fig.align='center'------------------------
cenQQ(ShePyrene$Pyrene,ShePyrene$PyreneCen)

## ----QQ2,fig.width=7,fig.height=6,fig.align='center'--------------------------
cenCompareQQ(ShePyrene$Pyrene,ShePyrene$PyreneCen,Yname="Pyrene",cex=0.75)

## ----sum----------------------------------------------------------------------
censummary(ShePyrene$Pyrene,ShePyrene$PyreneCen)

## -----------------------------------------------------------------------------
Pyr.mle.nada <- with(ShePyrene,
                     cenmle(Pyrene,PyreneCen))
Pyr.mle.nada

## -----------------------------------------------------------------------------
Pyr.mle <- with(ShePyrene,
                elnormAltCensored(Pyrene, PyreneCen, 
                             ci=TRUE, ci.method ="bootstrap",
                             n.bootstraps = 5000))
print(Pyr.mle)

## -----------------------------------------------------------------------------
Pyr.km.nada <- with(ShePyrene,
                    cenfit(Pyrene, PyreneCen))
Pyr.km.nada

## -----------------------------------------------------------------------------
Pyr.km <- with(ShePyrene,
                enparCensored(Pyrene, PyreneCen, 
                             ci=TRUE, ci.method ="bootstrap",
                             n.bootstraps = 5000))
print(Pyr.km)

## ---- fig.width=6.5,fig.height=4,fig.align='center',fig.cap="Lognormal probability of pyrene data"----
Pyr.ROS.nada <- with(ShePyrene,
                     cenros(Pyrene, PyreneCen))
mean(Pyr.ROS.nada)

sd(Pyr.ROS.nada)

quantile(Pyr.ROS.nada)

plot(Pyr.ROS.nada)

## -----------------------------------------------------------------------------
Pyr.ROS <- with(ShePyrene,
                elnormAltCensored(Pyrene, PyreneCen, method="rROS",
                             ci=TRUE, ci.method ="bootstrap",
                             n.bootstraps = 5000))

print(Pyr.ROS)

## -----------------------------------------------------------------------------
with(ShePyrene,censtats(Pyrene, PyreneCen))

## -----------------------------------------------------------------------------
## from above
print(Pyr.km)

## -----------------------------------------------------------------------------
Pyr.km2 <- with(ShePyrene,enparCensored(Pyrene,PyreneCen, ci=TRUE))

print(Pyr.km2)

## -----------------------------------------------------------------------------
pymle <- with(ShePyrene,cenmle(Pyrene, PyreneCen,conf.int=0.95))

mean(pymle)

## -----------------------------------------------------------------------------
pymlenorm <- with(ShePyrene,cenmle(Pyrene, PyreneCen, dist="gaussian"))

mean(pymlenorm)

## -----------------------------------------------------------------------------
pyr.lnorm <- with(ShePyrene,
                  elnormAltCensored(Pyrene, PyreneCen, 
                                    ci=TRUE, ci.method ="bootstrap", 
                                    n.bootstraps = 5000))

print(pyr.lnorm)

## -----------------------------------------------------------------------------
# from above
print(Pyr.ROS)

## -----------------------------------------------------------------------------
with(ShePyrene,cenPredInt(Pyrene, PyreneCen))

## -----------------------------------------------------------------------------
with(ShePyrene,cenPredInt(Pyrene, PyreneCen,newobs =2, method = "rROS"))

## ---- fig.width=6.5,fig.height=4,fig.align='center'---------------------------
with(ShePyrene,cenTolInt(Pyrene, PyreneCen, cover=0.9))

## -----------------------------------------------------------------------------
example <- with(ShePyrene,
            eqlnormCensored (Pyrene, PyreneCen, p=0.9, 
                             ci=TRUE, ci.type ="upper"))
print(example)

## -----------------------------------------------------------------------------

dat.gamma <- ShePyrene$Pyrene^(1/3)

obj.gamma <- eqnormCensored(dat.gamma, ShePyrene$PyreneCen, p=0.9, 
                            ci=TRUE, ci.type ="upper")
pct.gamma <- obj.gamma$quantiles^3 # the 90th percentile in orig units
pct.gamma

ti.gamma <- (obj.gamma$interval$limits[2])^3 # the upper tol limit in orig units
ti.gamma


## ---- fig.width=6.5,fig.height=5,fig.align='center'---------------------------
data(Example1) # From NADA2 package

head(Example1,5L)

with(Example1,cen_paired(Arsenic, NDisTRUE, 10, alt = "greater"))

## ---- fig.width=6.5,fig.height=5,fig.align='center'---------------------------
data(Atra); # From NADA package

head(Atra,5L)

with(Atra,cen_paired(June, JuneCen, Sept, SeptCen))

## -----------------------------------------------------------------------------
# test for the median difference = 0 using the sign test.
with(Atra,cen_signtest(June, JuneCen, Sept, SeptCen))

## -----------------------------------------------------------------------------
# test for a difference in the cdfs of the two months using the signed-rank
with(Atra,cen_signedranktest(June, JuneCen, Sept, SeptCen))

## ----fig.width=6.5,fig.height=4,fig.align='center'----------------------------

with(Example1,
     cboxplot(Arsenic, NDisTRUE, Ylab="Arsenic Conc", show = TRUE))

## ----fig.width=6.5,fig.height=4,fig.align='center'----------------------------
with(Example1,
     cenCompareCdfs (Arsenic, NDisTRUE, Yname = "Arsenic concentration in ug/L"))

## -----------------------------------------------------------------------------
egam <- with(Example1,
             egammaAltCensored(Arsenic, NDisTRUE, 
                               ci=TRUE, ci.type = "upper",
                               ci.method = "normal.approx"))
print(egam)

## -----------------------------------------------------------------------------
arsenic.out <- with(Example1,
                    enparCensored(Arsenic, NDisTRUE, 
                                  ci=TRUE, ci.method="bootstrap", ci.type="upper",
                                  n.bootstraps=5000))
print(arsenic.out)

## -----------------------------------------------------------------------------
data(Example2)

## -----------------------------------------------------------------------------
mibk.ucl95 <- with(Example2, 
                   elnormAltCensored (MIBK, MIBKcen, method = "rROS", 
                                      ci=TRUE, ci.method = "bootstrap", 
                                      ci.type = "upper", n.bootstraps = 5000))
print(mibk.ucl95)

## -----------------------------------------------------------------------------
mibk2.out <- with(Example2, 
                  elnormAltCensored (MIBK2, MIBK2cen, method = "rROS", 
                                     ci=TRUE, ci.method = "bootstrap", 
                                     ci.type = "upper", n.bootstraps = 5000))
print(mibk2.out)

## -----------------------------------------------------------------------------
data(Example3)

## -----------------------------------------------------------------------------
binom.test(0,14,alternative="less")

## ----fig.width=6.5,fig.height=4,fig.align='center'----------------------------
with(CuZn,cen2means(Zn,ZnCen,Zone,LOG=FALSE))

## ----fig.width=6.5,fig.height=4,fig.align='center'----------------------------
with(CuZn,cenperm2(Zn,ZnCen,Zone))

## ----fig.width=6.5,fig.height=4,fig.align='center'----------------------------
with(CuZn,cen2means(Zn,ZnCen,Zone))

## ----fig.width=6.5,fig.height=4,fig.align='center'----------------------------
with(CuZn,cen1way(Zn,ZnCen,Zone))

## ----fig.width=6.5,fig.height=4,fig.align='center'----------------------------
with(CuZn,cen_ecdf (Zn,ZnCen,Zone,
                    Ylab="Zinc Concentration, in ug/L"))

## ----fig.width=6.5,fig.height=4,fig.align='center'----------------------------
with(CuZn,cboxplot(Zn,ZnCen,Zone,
                    Ylab="Zinc Concentration, in ug/L"))

## -----------------------------------------------------------------------------
data(TCE2)

with(TCE2,ftable(Density~Below5Cens))

tab=with(TCE2,xtabs(~Below5Cens+Density))
chisq.test(tab)

## -----------------------------------------------------------------------------
TCE2$Below5[TCE2$Below5Cens== 1] <- -1      # all <5s are now a -1

wilcox.test(Below5~Density,TCE2)

## -----------------------------------------------------------------------------
t.test(Half.DL~Density,TCE2)

## ----fig.width=6.5,fig.height=4,fig.align='center'----------------------------
data(Golden)

with(Golden,cboxplot(Liver, LiverCen, Dosage,
                     Ylab = "Lead concentrations in liver, in ppb"))

## -----------------------------------------------------------------------------
Golden$Below04 <- Golden$Liver
Golden$Below04[Golden$Liver<0.04] <- -1
Golden$Below04[Golden$LiverCen==TRUE] <- -1


## -----------------------------------------------------------------------------
kruskal.test(Below04~Dosage,Golden)

## -----------------------------------------------------------------------------
with(Golden,cen1way(Liver,LiverCen,Dosage))

## ----fig.width=6.5,fig.height=4,fig.align='center'----------------------------
with(Golden,cen_ecdf(Liver,LiverCen,Dosage))

## ----fig.width=6.5,fig.height=4,fig.align='center'----------------------------
Golden$lnLiver=log(Golden$Liver)

with(Golden,cen_ecdf(lnLiver,LiverCen,Dosage,
                     xlim = c(min(lnLiver), max(lnLiver)), 
                     Ylab = "Natural Logs of Lead Concentrations in Liver"))

## -----------------------------------------------------------------------------
with(Golden,cenanova(Liver,LiverCen,Dosage))

## -----------------------------------------------------------------------------
with(Golden,cenpermanova(Liver,LiverCen,Dosage))

## -----------------------------------------------------------------------------
with(Golden,cenanova(Liver,LiverCen,Dosage,LOG=FALSE))

## -----------------------------------------------------------------------------
# Load data
data(Gales_Creek)
Gales_Creek <- as.data.frame(Gales_Creek)

# set Year to factor by making a new variable.
Gales_Creek$Yr.f <- as.factor(Gales_Creek$Yr)

with(Gales_Creek,cen2way(TCr, CrND, Yr.f, Season))

## -----------------------------------------------------------------------------
data(Recon)

## -----------------------------------------------------------------------------
vif(lm(AtraConc ~ Area + Applic + PctCorn + SoilGp + Temp + Precip + Dyplant + Pctl,Recon))

## -----------------------------------------------------------------------------
recon.8 <- with(Recon,data.frame(Area, Applic, PctCorn, SoilGp, Temp, Precip, Dyplant, Pctl))
reg.recon.8 <- with(Recon,cencorreg(AtraConc, AtraCen, recon.8))

summary(reg.recon.8)

## ----fig.height=10------------------------------------------------------------
layout(matrix(1:8,4,2))
with(Recon,partplots(AtraConc, AtraCen, recon.8,multiplot = F))

## -----------------------------------------------------------------------------
Recon$cbrtPctCorn <- recon.8$PctCorn^(1/3)

recon.8onecube <- cbind(recon.8[, -3], Recon$cbrtPctCorn)

reg.recon.8onecube = with(Recon,cencorreg(AtraConc, AtraCen, recon.8onecube))

## ----fig.height=10------------------------------------------------------------
layout(matrix(1:8,4,2))
with(Recon,partplots(AtraConc, AtraCen, recon.8onecube,multiplot = F))


## -----------------------------------------------------------------------------
summary(reg.recon.8onecube)

## -----------------------------------------------------------------------------
# an alternative way to make a data.frame without using data.frame(...)
recon.7 <- Recon[,c("Area", "Applic", "cbrtPctCorn", "Temp", "Precip", "Dyplant", "Pctl")]

reg.recon.7 <- with(Recon,cencorreg(AtraConc, AtraCen, recon.7))

## -----------------------------------------------------------------------------
recon.6 <- Recon[,c("Area", "Applic", "cbrtPctCorn", "Temp","Dyplant", "Pctl")]

reg.recon.6 <- with(Recon,cencorreg(AtraConc, AtraCen, recon.6))

summary(reg.recon.6)

## -----------------------------------------------------------------------------
recon.5 <- Recon[,c("Applic", "cbrtPctCorn", "Temp","Dyplant", "Pctl")]

reg.recon.5 <- with(Recon,cencorreg(AtraConc, AtraCen, recon.5))

summary(reg.recon.5)

## -----------------------------------------------------------------------------
recon.4 <- Recon[,c( "cbrtPctCorn", "Temp","Dyplant", "Pctl")]

reg.recon.4 <- with(Recon,cencorreg(AtraConc, AtraCen, recon.4))

summary(reg.recon.4)

## -----------------------------------------------------------------------------
with(Recon,bestaic(AtraConc, AtraCen, recon.8onecube))

## -----------------------------------------------------------------------------
reg.recon.cbrtPctCorn  <- with(Recon,cencorreg(AtraConc, AtraCen, cbrtPctCorn))

## -----------------------------------------------------------------------------
reg.recon.Temp   <- with(Recon,cencorreg(AtraConc, AtraCen, Temp))

## -----------------------------------------------------------------------------
reg.recon.Dyplant   <- with(Recon,cencorreg(AtraConc, AtraCen, Dyplant ))

## -----------------------------------------------------------------------------
reg.recon.Pctl  <- with(Recon,cencorreg(AtraConc, AtraCen, Pctl))

## -----------------------------------------------------------------------------
summary(reg.recon.Dyplant)

## -----------------------------------------------------------------------------
with(Recon,cencorreg(AtraConc, AtraCen, Dyplant,pred.plot=TRUE))

## -----------------------------------------------------------------------------
recon.5.alt <- Recon[,c("Dyplant","Applic", "cbrtPctCorn", "Temp", "Pctl")]

plot.reg.recon.5 <- with(Recon,cencorreg(AtraConc, AtraCen, recon.5.alt,pred.plot = TRUE))


## -----------------------------------------------------------------------------
with(Recon,ATS(AtraConc, AtraCen, Dyplant))

## -----------------------------------------------------------------------------
with(Recon,ATS(AtraConc, AtraCen, Dyplant,retrans=TRUE))

## -----------------------------------------------------------------------------
with(Recon,ATS(AtraConc,AtraCen, Pctl,retrans=TRUE))

## -----------------------------------------------------------------------------
data(Gales_Creek)

## -----------------------------------------------------------------------------
with(Gales_Creek,ATS(TCr,CrND,dectime,LOG=FALSE))

## -----------------------------------------------------------------------------
with(Gales_Creek,centrend(TCr,CrND,discharge,dectime))

## -----------------------------------------------------------------------------
with(Gales_Creek,censeaken(dectime,TCr,CrND,Season,seaplots=TRUE))

## -----------------------------------------------------------------------------
with(Gales_Creek,centrendsea(TCr,CrND,discharge,dectime,Season))

## -----------------------------------------------------------------------------
with(Gales_Creek,cencorreg(TCr,CrND,dectime))

## -----------------------------------------------------------------------------
timeflow <- with(Gales_Creek,data.frame(dectime, discharge))
with(Gales_Creek,cencorreg(TCr,CrND,timeflow))


## -----------------------------------------------------------------------------
sinT <- with(Gales_Creek, sin(2*pi*dectime))
cosT <- with(Gales_Creek, cos(2*pi*dectime))
timeflowseas <- with(Gales_Creek,data.frame(dectime, discharge))
timeflowseas <- cbind(timeflowseas,sinT,cosT)
with(Gales_Creek, cencorreg(TCr, CrND, timeflowseas))


## -----------------------------------------------------------------------------
data(ReconLogistic)

head(ReconLogistic,3)


## -----------------------------------------------------------------------------
glm.1 <- glm(GT_1~
               APPLIC+
               CORNpct+
               SOILGP+
               PRECIP+
               DYPLANT+
               FPCTL,
             ReconLogistic,family=binomial(logit))
vif(glm.1)

## -----------------------------------------------------------------------------
summary(glm.1)

## -----------------------------------------------------------------------------
glm.null <- glm(GT_1~1,ReconLogistic,family=binomial(logit))

anova(glm.null,glm.1,test="Chisq")

## -----------------------------------------------------------------------------
residualPlots(glm.1,type="deviance")


## -----------------------------------------------------------------------------
glm.3 <- glm(GT_1~
               CORNpct+
               SOILGP+
               PRECIP+
               DYPLANT+
               FPCTL,
             ReconLogistic,family=binomial(logit))
summary(glm.3)

## -----------------------------------------------------------------------------
glm.4 <- glm(GT_1~
               CORNpct+
               PRECIP+
               DYPLANT+
               FPCTL,
             ReconLogistic,family=binomial(logit))
summary(glm.4)

## ----warning=FALSE------------------------------------------------------------
bestglm(ReconLogistic,family = binomial(logit), IC = "AIC")


## -----------------------------------------------------------------------------
anova(glm.4, glm.1, test="Chisq")

## -----------------------------------------------------------------------------
Recon.frame = with(ReconLogistic, datadist(CORNpct, DYPLANT, FPCTL, TEMP, GT_1))
options(datadist = "Recon.frame")
lrm4 <- lrm(GT_1 ~ CORNpct + DYPLANT + FPCTL + TEMP, data = ReconLogistic)
lrm4

## ----message = FALSE,results="hide"-------------------------------------------
data(Markers)
head(Markers,3)

Mdat <- Markers[, -15] # remove the Site Name column

M.usc <- uscoresi(Mdat)    # uscoresi drops rows with NAs  (row 13 here)
M.euclid <- dist(M.usc)
Site <- Markers$Site_Name[-13]  # delete the site entry for row 13 with NAs
M.anosim <- anosim(M.euclid, Site)
M.anosim


anosimPlot(M.anosim)

## ----message = FALSE,results="hide"-------------------------------------------
uMDS(M.usc, group = Site, 
     legend.pos = "topright", 
     title = "NMDS of rank(uscores) for markers + entero")

## ----message = FALSE,results="hide"-------------------------------------------
M.nmds <- metaMDS(M.euclid)
Site <- as.factor(Site)
gp.color <- as.integer(Site)
Mplot <- ordiplot(M.nmds, type="none", display = "sites", 
                  main="NMDS of rank(uscores) for markers + entero")
points(Mplot, "sites", pch=19, col=gp.color)
text(Mplot, "sites", pos=4, cex = 0.8)
leg.col <- c(1: length(levels(Site)))
legend("topright", legend=levels(Site), bty="n", col = leg.col, text.col = leg.col, pch = 19)


## -----------------------------------------------------------------------------
Mdata <- Markers[-13,]  # delete row with NAs
M6 <- Mdata[, -(13:15)]  # only keep columns for the 6 markers
M6.usc <- uscoresi(M6)
M6.euclid <- dist(M6.usc)  # matrix for the 6 MST markers
ent <- Mdata[, 13:14]   # the entero1A data
ent.usc<-uscoresi(ent)
ent.euclid<-dist(ent.usc)  # matrix for the entero1A data
M6.Ktau <- mantel(ent.euclid, M6.euclid, method="kendall", permutations = 9999) 
M6.Ktau


## -----------------------------------------------------------------------------
Site <-  as.factor(Markers$Site_Name)
gp.color <- as.numeric(Site)  # assigns numbers to group names in Site_Name
plot(ent.euclid, M6.euclid, pch = 19, 
     col = gp.color, 
     main = "Correlation of distance matrix of rank(uscores)")
lws <- lowess(ent.euclid, M6.euclid)
lines(lws)
legend("bottomright", legend=levels(Site), bty="n", col = 1:nlevels(Site), pch = 19)


## -----------------------------------------------------------------------------
bioenv(ent.euclid, M6.usc, method = "kendall")


## -----------------------------------------------------------------------------
plot(ent.usc, M6.usc[,4], pch = 19)

## -----------------------------------------------------------------------------
plot(ent.usc, M6.usc[,5], ylab = "HPyv rank of uscores")

## ----eval=F-------------------------------------------------------------------
#  enormCensored(Data, Cen,ci=TRUE, ci.type="upper", ci.method="normal.approx")
#  elnormAltCensored(Data, Cen, ci=TRUE, ci.type="upper", ci.method="bootstrap")
#  egammaAltCensored(Data, Cen, ci=TRUE, ci.type="upper", ci.method="bootstrap")

## ----eval=F-------------------------------------------------------------------
#  enparCensored(Data, Cen, ci=TRUE, ci.method="bootstrap", ci.type="upper", n.bootstraps=5000)

