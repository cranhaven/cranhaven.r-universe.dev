## ----env, include = FALSE-----------------------------------------------------

options(scipen=999)

options(rmarkdown.html_vignette.check_title = FALSE)

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")


## ----hypnogram, echo=FALSE, message=FALSE, warning=FALSE----------------------
library(rsleep)

if(!file.exists("15012016HD.csv")){
  download.file(
  url = "https://rsleep.org/data/15012016HD.csv",
  destfile = "15012016HD.csv",
  method="curl")}

events <- read_events_noxturnal("15012016HD.csv")

events = hypnogram(events)

plot_hypnogram(events)


