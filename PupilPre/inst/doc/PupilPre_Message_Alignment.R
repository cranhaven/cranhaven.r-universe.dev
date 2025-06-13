## ----global_options, include=FALSE---------------------------------------
knitr::opts_chunk$set(fig.width=6, fig.height=4, warning=FALSE)

## ---- echo=FALSE, eval=TRUE, message=FALSE-------------------------------
library(PupilPre)
data(Pupildat)

## ---- eval= TRUE, echo=FALSE, results='hide', message=FALSE--------------
library(PupilPre)
data(Pupildat)
aligndat <- ppl_prep_data(data = Pupildat, Subject = "RECORDING_SESSION_LABEL", Item = "item")

## ---- eval= TRUE, echo=TRUE, results='asis'------------------------------
check_all_msgs(data = aligndat)

## ---- eval= TRUE, echo=TRUE, results='asis'------------------------------
check_msg_time(data = aligndat, Msg = "TIMER_1")

## ---- eval= TRUE, echo=TRUE, results='asis'------------------------------
aligned1 <- align_msg(data = aligndat, Msg = "TIMER_1")

## ---- eval= TRUE, echo=TRUE, results='asis'------------------------------
check_msg_time(data = aligned1, Msg = "TIMER_1")

## ---- eval= FALSE, echo=TRUE, results='asis'-----------------------------
#  MSGTime <- check_msg_time(data = aligned1, Msg = "TIMER_1", ReturnData = TRUE)

## ---- eval= TRUE, echo=TRUE, results='asis'------------------------------
aligned2 <- create_time_series(data = aligned1, Adjust = 0)

## ---- eval= TRUE, echo=TRUE, results='asis'------------------------------
check_time_series(data = aligned2)

## ---- eval= TRUE, echo=TRUE, results='asis'------------------------------
check_msg_time(data = aligned2, Msg = "TIMER_1")

