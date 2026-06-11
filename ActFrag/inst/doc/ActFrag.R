## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# devtools::install_github("junruidi/ActFrag")

## -----------------------------------------------------------------------------
library(ActFrag)

## ----eval=FALSE---------------------------------------------------------------
# data(example_activity_data)
# count = example_activity_data$count
# weartime = wear_flag(count.data = count, start = "06:00", end = "23:00")

## ----eval=FALSE---------------------------------------------------------------
# data(example_activity_data)
# count1 = c(t(example_activity_data$count[1,-c(1,2)]))
# wear1 = c(t(example_activity_data$wear[1,-c(1,2)]))
# mb = fragmentation(x = count1, w = wear1, thresh = 100, metrics = "mean_bout",bout.length = 1)
# tp = fragmentation(x = count1, w = wear1, thresh = 100, metrics = "TP",bout.length = 1)

## ----eval=FALSE---------------------------------------------------------------
# data(example_activity_data)
# count = example_activity_data$count
# wear = example_activity_data$wear
# frag_by_subject = fragmentation_long(count.data = count, weartime = wear,thresh = 100, metrics = "all",by = "subject",bout.length = 1)
# frag_by_day = fragmentation_long(count.data = count, weartime = wear,thresh = 100, metrics = "all",by = "day",bout.length = 1)

