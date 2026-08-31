## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE--------------------------------------------------------------
#  batchOut = batchProcessZoomOutput(batchInput="./myMeetingsBatch.xlsx")

## ---- eval=FALSE, error=FALSE, message=FALSE, warning=FALSE, include=FALSE, results='hide'----
#  batchOut = invisible(batchProcessZoomOutput(batchInput=system.file('extdata', 'myMeetingsBatch.xlsx', package = 'zoomGroupStats')))

## ---- eval=FALSE--------------------------------------------------------------
#  batchGrabVideoStills(batchInfo=batchOut$batchInfo, imageDir="~/Documents/myMeetings/videoImages", sampleWindow=60)

## ---- eval=FALSE--------------------------------------------------------------
#  vidOut = batchVideoFaceAnalysis(batchInfo=batchOut$batchInfo, imageDir="~/Documents/meetingImages", sampleWindow=60, facesCollectionID="group-r")

