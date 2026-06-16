## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE---------------------------------------------------
library(loggit)

## ----handlers_0, eval = FALSE-------------------------------------------------
#  library(loggit)
#  
#  set_logfile("/path/to/my/log/directory/loggit.log") # loggit enforces no specific file extension

## ----handlers-----------------------------------------------------------------
message("This is a message")
warning("This is a warning")
# stop("This is a critical error, so I'm not actually going to run it in this vignette")

## ----loggit_func--------------------------------------------------------------
loggit("INFO", "This is also a message")
loggit("WARN", "This is also a warning")
loggit("ERROR", "This is an error, but it won't stop your code from running like `stop()` does")

## ----custom_fields------------------------------------------------------------
loggit(
  "INFO",
  "This is a message",
  but_maybe = "you want more fields?",
  sure = "why not?",
  like = 2,
  or = 10,
  what = "ever"
)

## ----read_logs----------------------------------------------------------------
read_logs()

