## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup--------------------------------------------------------------------
#  library(vprr)

## ---- eval = TRUE-------------------------------------------------------------
csv <- read.csv('station_names_COR2019002.csv')

head(csv)


## ---- eval =  TRUE------------------------------------------------------------

aid <- read.table(file = system.file("extdata/COR2019002/autoid/bad_image_blurry/aid/sep20_2svmaid.d222.h04", package = 'vprr', mustWork = TRUE))

head(aid)

aidmeas <- readLines(system.file("extdata/COR2019002/autoid/bad_image_blurry/aidmea/sep20_2svmaid.mea.d222.h04", package = 'vprr', mustWork = TRUE))

head(aidmeas)

