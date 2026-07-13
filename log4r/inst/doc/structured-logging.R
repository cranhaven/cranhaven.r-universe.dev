## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(log4r)

# Dummy Plumber request & response.
req <- list(
  REQUEST_METHOD = "POST", PATH_INFO = "/upload",
  QUERY_STRING = "", HTTP_USER_AGENT = "curl/7.58.0",
  REMOTE_ADDR = "124.133.52.161"
)
res <- list(status = 401)

# Dummy CSV-parsing stuff:
filename <- "catpics_01.csv"
entries <- data.frame(x = 1:4124)
start <- Sys.time()

## ----json---------------------------------------------------------------------
logger <- logger(appenders = console_appender(json_log_layout()))

## ----json-example-------------------------------------------------------------
# Here "req" and "res" are slightly fake request & response objects.
info(
  logger, message = "authentication failed",
  method = req$REQUEST_METHOD,
  path = req$PATH_INFO,
  params = sub("^\\?", "", req$QUERY_STRING),
  user_agent = req$HTTP_USER_AGENT,
  remote_addr = req$REMOTE_ADDR,
  status = res$status
)

## ----logfmt-------------------------------------------------------------------
logger <- logger(appenders = console_appender(logfmt_log_layout()))

## ----logfmt-example-----------------------------------------------------------
info(
  logger, message = "processed entries", file = filename,
  entries = nrow(entries),
  elapsed = unclass(difftime(Sys.time(), start, units = "secs"))
)

