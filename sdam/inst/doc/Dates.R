## ----setup, echo=FALSE, message=FALSE-----------------------------------------
knitr::opts_chunk$set(echo=TRUE,error=TRUE)
knitr::opts_chunk$set(comment = "")
library("sdam")

## ----set-options, echo=FALSE, cache=FALSE-----------------------------------------------------
options(width = 96)

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------
#  install.packages("sdam") # from CRAN
#  devtools::install_github("sdam-au/sdam") # development version
#  devtools::install_github("mplex/cedhar", subdir="pkg/sdam") # legacy version R 3.6.x

## ---------------------------------------------------------------------------------------------
# load and check versions
library(sdam)
packageVersion("sdam")

## ---------------------------------------------------------------------------------------------
# load shipwrecks external dataset
sw <- system.file("extdata","StraussShipwrecks.csv",package="sdam") |> 
  read.csv(sep=";", check.names=FALSE)

## ---------------------------------------------------------------------------------------------
# variables in shipwrecks dataset
colnames(sw)

## ---- echo=TRUE, eval=TRUE, fig.width=4, fig.height=4, fig.align="center", fig.cap="Range of timespans in Shipwrecks dataset"----
# shipwrecks dates with Wreck ID
plot.dates(sw, id="Wreck ID", type="rg", taq="Earliest date", tpq="Latest date", col=4)

## ---------------------------------------------------------------------------------------------
# add mid points and range to shipwrecks data
prex(sw[c(1,7,15:16)], type="mp", vars=c("Earliest date", "Latest date"), keep=TRUE) |> 
  tail()

## ---------------------------------------------------------------------------------------------
# aoristic sum shipwrecks
prex(sw[c(1,7,15:16)], vars=c("Earliest date", "Latest date"))

## ---------------------------------------------------------------------------------------------
# aoristic sum shipwrecks 8 bin
prex(sw[c(1,7,15:16)], vars=c("Earliest date", "Latest date"), cp="bin8")

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------
#  # silhouette of Italian peninsula
#  plot.map(x="Ita", date=TRUE)
#  ## not run

## ---------------------------------------------------------------------------------------------
# 59 provinces dates, colors, and shapes
data("rpmcd")

# province acronyms as in EDH
names(rpmcd)

## ---------------------------------------------------------------------------------------------
# pipe dataset for dates in second component
rpmcd |> 
  lapply(function (x) x[[2]]) |> 
  head()

## ---- echo=-5---------------------------------------------------------------------------------
# second component in dataset
est <- rpmcd |> 
  lapply(function (x) x[[2]]) |> 
  unlist(use.names=FALSE)
est

## ---------------------------------------------------------------------------------------------
# clean levels are 0-9
cln(est, level=9)

## ---------------------------------------------------------------------------------------------
# update object with establishment dates
est <- est |> 
  cln(level=9) |> 
  dts()

## ---------------------------------------------------------------------------------------------
est

## ---------------------------------------------------------------------------------------------
# Roman province dates of establishement (strings still strings)
rpde <- cbind(names(rpmcd),dts(est)) |>
  as.data.frame(stringsAsFactors=FALSE)

## ---------------------------------------------------------------------------------------------
rownames(rpde) <- NULL
head(rpde)

## ---------------------------------------------------------------------------------------------
# order of affiliation of provinces
rpde[order(as.numeric(rpde$V2)),1]

## ---- echo=TRUE, eval=TRUE--------------------------------------------------------------------
# list with 45 early and late influence dates provinces
data("rpcp")

## ---------------------------------------------------------------------------------------------
# look at data internal structure
str(rpcp)

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------
#  # early influence dates are in first list of 'rpcp'
#  plot.dates(x=rpcp[[1]], taq="EarInf", tpq="OffPrv", main="Early period", ylab="province")

## ---- echo=FALSE, eval=TRUE, fig.width=4, fig.height=4, fig.align="center"--------------------
plot.dates(x=rpcp[[1]], taq="EarInf", tpq="OffPrv", main="Early period", ylab="province", yaxt="n")

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------
#  # late influence dates are in second list of 'rpcp'
#  plot.dates(x=rpcp[[2]], type="mp", taq="LateInf", tpq="Fall", lwd=5, col="red",
#             main="Late period", ylab="province")

## ---- echo=FALSE, eval=TRUE, fig.width=4, fig.height=4, fig.align="center"--------------------
plot.dates(x=rpcp[[2]], type="mp", taq="LateInf", tpq="Fall", lwd=5, col="red", main="Late period", ylab="province", yaxt="n")

## ---- echo=TRUE, eval=TRUE--------------------------------------------------------------------
# Roman provinces dates from EDH
data("rpd")

## ---------------------------------------------------------------------------------------------
# Rome
summary(rpd$Rom)

## ---------------------------------------------------------------------------------------------
# Aegyptus
summary(rpd$Aeg)

## ---------------------------------------------------------------------------------------------
# Armenia
rpd$Arm

## ---------------------------------------------------------------------------------------------
# list with arguments
formals(edhwpd)

## ---------------------------------------------------------------------------------------------
# characteristics of inscriptions
vars = c("findspot_ancient", "type_of_inscription", "type_of_monument", "language")

## ---- echo=TRUE, eval=TRUE--------------------------------------------------------------------
# Armenia: restricted imputation of dates
edhwpd(vars=vars, province="Arm") |> 
  rmids()

