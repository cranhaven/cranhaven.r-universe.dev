## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
knitr::opts_knit$set(root.dir = tempdir())

## ----setup--------------------------------------------------------------------
library(ptspotter)

## -----------------------------------------------------------------------------
log_file_ops(dir_path = "logs", logfile_nm = "logfile")

list.files("logs")

## ---- error=TRUE--------------------------------------------------------------
log_file_ops(dir_path = "logs", logfile_nm = "logfile")


## -----------------------------------------------------------------------------
log_enable(logfile_loc = "logs/logfile.txt")

## -----------------------------------------------------------------------------
log4r::info(logger = my_logger, message = "Some info")
log4r::warn(logger = my_logger, message = "Some warning")
log4r::error(logger = my_logger, message = "Some error")

# Check the messages are being logged
readLines("logs/logfile.txt")

## ---- include=FALSE-----------------------------------------------------------
unlink("logs", recursive = TRUE)

