## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE--------------------------------------------------------------
#  # Install zoomGroupStats from CRAN
#  install.packages("zoomGroupStats")
#  
#  # Install the development version of zoomGroupStats
#  devtools::install_github("andrewpknight/zoomGroupStats")

## ----setup--------------------------------------------------------------------
library(zoomGroupStats)

## ---- eval=FALSE--------------------------------------------------------------
#  batchOut = batchProcessZoomOutput(batchInput="./myMeetingsBatch.xlsx")

## ----error=FALSE, message=FALSE, warning=FALSE, include=FALSE, results='hide'----
batchOut = invisible(batchProcessZoomOutput(batchInput=system.file('extdata', 'myMeetingsBatch.xlsx', package = 'zoomGroupStats')))

## -----------------------------------------------------------------------------
str(batchOut$batchInfo)

## -----------------------------------------------------------------------------
str(batchOut$meetInfo)

## -----------------------------------------------------------------------------
str(batchOut$partInfo)

## -----------------------------------------------------------------------------
str(batchOut$transcript)

## -----------------------------------------------------------------------------
str(batchOut$chat)

## -----------------------------------------------------------------------------
str(batchOut$rosetta)

## ---- eval=FALSE--------------------------------------------------------------
#  batchOut = batchProcessZoomOutput(batchInput="./myMeetingsBatch.xlsx", exportZoomRosetta="./myMeetings_rosetta_original.xlsx")

## ---- eval=FALSE--------------------------------------------------------------
#  batchOutIds = importZoomRosetta(zoomOutput=batchOut, zoomRosetta="./myEditedRosetta.xlsx",
#  meetingId="batchMeetingId")

