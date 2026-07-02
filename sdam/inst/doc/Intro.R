## ----setup, echo=FALSE, message=FALSE---------------------------------------------------------
knitr::opts_chunk$set(echo=TRUE,error=TRUE)
knitr::opts_chunk$set(comment = "")
library("sdam")

## ----set-options, echo=FALSE, cache=FALSE-----------------------------------------------------
options(width = 96)

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------
#  install.packages("sdam") # from CRAN
#  devtools::install_github("sdam-au/sdam") # development version
#  devtools::install_github("mplex/cedhar", subdir="pkg/sdam") # a legacy version R 3.6.x

## ---------------------------------------------------------------------------------------------
# load and check version
library(sdam)
packageVersion("sdam")

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------
#  # pop-up a new window
#  data(package="sdam")

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
print(data(package="sdam"))

## ---------------------------------------------------------------------------------------------
# Data sets in package 'sdam':
# 
# rp        Roman province names and acronyms as in EDH
# rpcp      Roman provinces chronological periods
# rpd       Roman provinces dates from EDH
# rpmcd     Caption maps and affiliation dates of Roman provinces

## ---------------------------------------------------------------------------------------------
# Additional built-in datasets in 'sdam':
#
# EDH       Epigraphic Database Heidelberg Dataset
# rpmp      Maps of ancient Roman provinces and Italian regions
# retn      Roman Empire transport network and Mediterranean sea

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------
#  # Epigraphic Database Heidelberg Dataset help
#  ?EDH

## ---- echo=TRUE, eval=TRUE--------------------------------------------------------------------
# load dataset
data("rp")

# obtain object structure
str(rp)

## ---------------------------------------------------------------------------------------------
# Armenian records in 'EDH'
edhw(province="Arm")[1]

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------
#  # record HD015521
#  edhw(id="15521", as="df")

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------
#  # record HD015521 with explicit variables
#  edhw(id="15521", vars="people", as="df")

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
suppressWarnings(edhw(id="15521", vars="people", as="df"))

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------
#  # record HD015521 with more explicit variables
#  edhw(id="15521", vars=c("people", "province_label"), as="df")

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
suppressWarnings(edhw(id="15521", vars=c("people", "province_label"), as="df"))

## ---- echo=FALSE, eval=TRUE, out.width="25%", fig.align="center", fig.cap="Roman province of Armenia (ca 117 AD)."----
plot.map("Arm", cap=TRUE, name=FALSE)

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------
#  # people in Armenia
#  edhw(province="Arm") |>
#    edhw(vars="people")

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
edhw(province="Arm") |> 
  suppressWarnings() |> 
  edhw(vars="people") |> 
  suppressWarnings()

## ---------------------------------------------------------------------------------------------
# land contour around Mediterranean
plot.map(type="plain")

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------
#  # display settlements and shipping routes
#  plot.map(type="plain", settl=TRUE, shipr=TRUE)

## ---- echo=TRUE, eval=TRUE--------------------------------------------------------------------
# dates from EDH
data("rpd")

# three provinces in object structure
str(rpd[1:3])

## ---- echo=TRUE, eval=TRUE--------------------------------------------------------------------
# periods for Roman provinces
data("rpcp")

# object structure
str(rpcp)

