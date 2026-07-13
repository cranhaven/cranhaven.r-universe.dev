## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----cat-setup----------------------------------------------------------------
cat_debug <- function() {
  cat() # Print nothing.
}

cat_info <- function() cat(
  "INFO  [", format(Sys.time(), "%Y-%m-%d %H:%M:%S", usetz = FALSE),
  "] Info message.", sep = ""
)

## ----log4r-setup, results = "hide"--------------------------------------------
log4r_logger <- log4r::logger(threshold = "INFO")

log4r_info <- function() {
  log4r::info(log4r_logger, "Info message.")
}

log4r_debug <- function() {
  log4r::debug(log4r_logger, "Debug message.")
}

## ----fl-setup, results = "hide"-----------------------------------------------
requireNamespace("futile.logger")

futile.logger::flog.logger()

fl_info <- function() {
  futile.logger::flog.info("Info message.")
}

fl_debug <- function() {
  futile.logger::flog.debug("Debug message.")
}

## ----logging-setup, results = "hide"------------------------------------------
requireNamespace("logging")

logging::basicConfig()

logging_info <- function() {
  logging::loginfo("Info message.")
}

logging_debug <- function() {
  logging::logdebug("Debug message.")
}

## ----logger-setup, results = "hide"-------------------------------------------
requireNamespace("logger")

# Match the behaviour of other logging packages and write to the console.
logger::log_appender(logger::appender_stdout)

logger_info <- function() {
  logger::log_info("Info message.")
}

logger_debug <- function() {
  logger::log_debug("Debug message.")
}

## ----lgr-setup, results = "hide"----------------------------------------------
requireNamespace("lgr")

lgr_logger <- lgr::get_logger("perf-test")
lgr_logger$set_appenders(list(cons = lgr::AppenderConsole$new()))
lgr_logger$set_propagate(FALSE)

lgr_info <- function() {
  lgr_logger$info("Info message.")
}

lgr_debug <- function() {
  lgr_logger$debug("Debug message.")
}

## ----loggit-setup, results = "hide"-------------------------------------------
requireNamespace("loggit")

if (.Platform$OS.type == "unix") {
  loggit::set_logfile("/dev/null")
} else {
  loggit::set_logfile("nul")
}

loggit_info <- function() {
  loggit::loggit("INFO", "Info message.")
}

## ----rlog-setup, results = "hide"---------------------------------------------
requireNamespace("rlog")

rlog_info <- function() {
  rlog::log_info("Info message.")
}

rlog_debug <- function() {
  rlog::log_debug("Debug message.")
}

## ----test-loggers-debug-------------------------------------------------------
log4r_debug()
cat_debug()
logging_debug()
fl_debug()
logger_debug()
lgr_debug()
rlog_debug()

## ----test-loggers, collapse=TRUE----------------------------------------------
log4r_info()
cat_info()
logging_info()
fl_info()
logger_info()
lgr_info()
loggit_info()
rlog_info()

## ----benchmark, results = "hide", echo = TRUE---------------------------------
info_bench <- microbenchmark::microbenchmark(
  cat = cat_info(),
  log4r = log4r_info(),
  futile.logger = fl_info(),
  logging = logging_info(),
  logger = logger_info(),
  lgr = lgr_info(),
  loggit = loggit_info(),
  rlog = rlog_info(),
  times = 500,
  control = list(warmups = 50)
)

debug_bench <- microbenchmark::microbenchmark(
  cat = cat_debug(),
  log4r = log4r_debug(),
  futile.logger = fl_debug(),
  logging = logging_debug(),
  logger = logger_debug(),
  lgr = lgr_debug(),
  rlog = rlog_debug(),
  times = 500,
  control = list(warmups = 50)
)

## ----print-benchmark-1--------------------------------------------------------
print(info_bench, order = "median")

## ----print-benchmark-2--------------------------------------------------------
print(debug_bench, order = "median")

