## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(log4r)

## ----rsyslog, eval=requireNamespace("rsyslog", quietly = TRUE)----------------
logger <- logger(appenders = syslog_appender("my-R-script"))

## ----http-1-------------------------------------------------------------------
logger <- logger(appenders = http_appender("http://logging.example.local"))

## ----http-2-------------------------------------------------------------------
logger <- logger(
  appenders = http_appender("http://logging.example.local", method = "GET")
)

## ----http-3-------------------------------------------------------------------
logger <- logger(
  appenders = http_appender(
    "http://logging.example.local",
    method = "GET",
    layout = default_log_layout(),
    httr::add_headers(`X-Custom-Header` = 1),
    httr::user_agent("my-r-script/1.0.0")
  )
)

## ----tcp, eval = FALSE--------------------------------------------------------
#  logger <- logger(
#    appenders = tcp_appender("tcp://logging.example.local", port = 9551)
#  )

