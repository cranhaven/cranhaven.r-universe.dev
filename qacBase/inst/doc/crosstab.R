## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(qacBase)

## -----------------------------------------------------------------------------
crosstab(cars74, cyl, gear)

crosstab(cars74, cyl, gear, plot=TRUE)

## -----------------------------------------------------------------------------
crosstab(cars74, cyl, gear, type="percent")

crosstab(cars74, cyl, gear, type="percent", plot=TRUE)

## -----------------------------------------------------------------------------
crosstab(cars74, cyl, gear, type = "rowpercent")

crosstab(cars74, cyl, gear, type = "rowpercent", plot=TRUE)

## -----------------------------------------------------------------------------
crosstab(cars74, cyl, gear, type = "colpercent")

crosstab(cars74, cyl, gear, type = "colpercent", plot=TRUE)

## -----------------------------------------------------------------------------
crosstab(cars74, cyl, gear, type = "colpercent", chisquare=TRUE)

crosstab(cars74, cyl, gear, type = "colpercent", plot=TRUE, 
         chisquare = TRUE)

