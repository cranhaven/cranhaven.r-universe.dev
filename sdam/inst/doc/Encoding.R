## ----setup, echo=FALSE, message=FALSE---------------------------------------------------------
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

## ---- echo=FALSE, eval=TRUE, out.width="25%", fig.align="center", fig.cap="Roman province of Achaia (ca 117 AD)."----
plot.map("Ach", cap=TRUE, name=FALSE)

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------
#  # select two people variables from Achaia
#  Ach <- edhw(province="Ach") |>
#    edhw(vars="people", select=c("cognomen","nomen"))

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
Ach <- edhw(province="Ach") |> 
  suppressWarnings() |> 
  edhw(vars="people", select=c("cognomen","nomen")) |> 
  suppressWarnings()

## ---------------------------------------------------------------------------------------------
# number of people entries in Achaia
nrow(Ach)

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------
#  # also remove NAs
#  Ach <- edhw(province="Ach") |>
#    edhw(vars="people", select=c("cognomen","nomen"), na.rm=TRUE)
#  
#  nrow(Ach)

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
Ach <- edhw(province="Ach") |> 
  suppressWarnings() |> 
  edhw(vars="people", select=c("cognomen","nomen"), na.rm=TRUE) |> 
  suppressWarnings()

nrow(Ach)

## ---------------------------------------------------------------------------------------------
# some people entries in Achaia
head(Ach)

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------
#  # re-encode in Ach cognomen
#  Ach$cognomen |>
#    head() |>
#    cln()

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(head(Ach$cognomen,6))[1])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(head(Ach$cognomen,6))[2])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(head(Ach$cognomen,6))[3])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(head(Ach$cognomen,6))[4])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(head(Ach$cognomen,6))[5])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(head(Ach$cognomen,6))[6])

## ---- echo=FALSE, eval=FALSE------------------------------------------------------------------
#  detach("package:sdam", unload=TRUE)
#  sdam::cln(tail(Ach))

## ---------------------------------------------------------------------------------------------
# last entries
tail(Ach)

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------
#  # clean last entries of cognomen
#  Ach$cognomen |>
#    tail() |>
#    cln()

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Ach$cognomen,6))[1])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Ach$cognomen,6))[2])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Ach$cognomen,6))[3])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Ach$cognomen,6))[4])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Ach$cognomen,6))[5])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Ach$cognomen,6))[6])

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------
#  # clean last entries of nomen
#  Ach$nomen |>
#    tail() |>
#    cln()

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Ach$nomen,6))[1])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Ach$nomen,6))[2])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Ach$nomen,6))[3])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Ach$nomen,6))[4])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Ach$nomen,6))[5])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Ach$nomen,6))[6])

## ---- echo=FALSE, eval=TRUE, out.width="25%", fig.align="center", fig.cap="Roman province of Aegyptus (ca 117 AD)."----
plot.map("Aeg", cap=TRUE, name=FALSE)

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------
#  # Aegyptus people
#  Aeg <- edhw(province="Aeg") |>
#    edhw(vars="people")

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
Aeg <- edhw(province="Aeg") |> 
  suppressWarnings() |> 
  edhw(vars="people") |>
  suppressWarnings()

## ---------------------------------------------------------------------------------------------
# three variables of the last eight records
Aeg[ , c(3,5:6)] |> 
  tail(8)

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------
#  # re-encode three variables from last entries
#  Aeg[ ,c(3,5:6)] |>
#    tail() |>
#    cln()

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Aeg$cognomen,8))[1])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Aeg$cognomen,8))[2])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Aeg$cognomen,8))[3])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Aeg$cognomen,8))[4])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Aeg$cognomen,8))[5])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Aeg$cognomen,8))[6])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Aeg$cognomen,8))[7])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Aeg$cognomen,8))[8])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Aeg$name,8))[1])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Aeg$name,8))[2])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Aeg$name,8))[3])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Aeg$name,8))[4])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Aeg$name,8))[5])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Aeg$name,8))[6])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Aeg$name,8))[7])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Aeg$name,8))[8])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Aeg$nomen,8))[1])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Aeg$nomen,8))[2])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Aeg$nomen,8))[3])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Aeg$nomen,8))[4])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Aeg$nomen,8))[5])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Aeg$nomen,8))[6])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Aeg$nomen,8))[7])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(tail(Aeg$nomen,8))[8])

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------
#  # default cleaning level 1
#  Aeg$nomen |>
#    cln() |>
#    table() |>
#    sort(decreasing=TRUE)

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(sort(unique(Aeg$nomen)),level=1)[32])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
as.vector(sort(table(Aeg$nomen),decreasing=TRUE))[1]

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(sort(unique(Aeg$nomen)),level=1)[22])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
as.vector(sort(table(Aeg$nomen),decreasing=TRUE))[2]

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(sort(unique(Aeg$nomen)),level=1)[23])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
as.vector(sort(table(Aeg$nomen),decreasing=TRUE))[3]

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(cln(sort(unique(Aeg$nomen)),level=1)[1])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
as.vector(sort(table(Aeg$nomen),decreasing=TRUE))[4]

## ---- echo=TRUE, eval=FALSE-------------------------------------------------------------------
#  # raise cleaning level and remove NAs
#  Aeg$nomen |>
#    cln(level=2, na.rm=TRUE) |>
#    table() |>
#    sort(decreasing=TRUE)

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(names(sort(table(cln(x=Aeg$nomen, level=2, na.rm=TRUE)), decreasing=TRUE))[1])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
as.vector(sort(table(cln(x=Aeg$nomen, level=2, na.rm=TRUE)), decreasing=TRUE))[1]

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(names(sort(table(cln(x=Aeg$nomen, level=2, na.rm=TRUE)), decreasing=TRUE))[2])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
as.vector(sort(table(cln(x=Aeg$nomen, level=2, na.rm=TRUE)), decreasing=TRUE))[2]

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(names(sort(table(cln(x=Aeg$nomen, level=2, na.rm=TRUE)), decreasing=TRUE))[3])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
as.vector(sort(table(cln(x=Aeg$nomen, level=2, na.rm=TRUE)), decreasing=TRUE))[3]

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
knitr::asis_output(names(sort(table(cln(x=Aeg$nomen, level=2, na.rm=TRUE)), decreasing=TRUE))[4])

## ---- echo=FALSE, eval=TRUE-------------------------------------------------------------------
as.vector(sort(table(cln(x=Aeg$nomen, level=2, na.rm=TRUE)), decreasing=TRUE))[4]

