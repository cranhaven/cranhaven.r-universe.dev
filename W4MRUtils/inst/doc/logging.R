## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
logger <- W4MRUtils::get_logger("LoggerTest1")

## -----------------------------------------------------------------------------
## provide a path at logger creation
logfile <- tempfile()
logger <- W4MRUtils::get_logger(
  "LoggerTest2",
  out_path = logfile
)

## -----------------------------------------------------------------------------
## add it after the logger's creation
logfile <- tempfile()
logger <- W4MRUtils::get_logger("LoggerTest3")
logger$set_out_paths(logfile)

## -----------------------------------------------------------------------------
## messages are printed to the terminal and sent to the log file.
logger$info("Info message")
logger$warning("Warning message")
logger$error("Error message")
logger$debug("Debug message")
## debug messages are deactivated by default
logger$verbose("Verbose message")
## verbose messages are deactivated by default
print(readLines(logfile))
file.remove(logfile)

## -----------------------------------------------------------------------------
W4MRUtils::get_logger(
  "Processing",
  format = "[{{ time }}-{{ name }}] - {{ message }}"
)

## -----------------------------------------------------------------------------
W4MRUtils::get_logger(
  "Processing",
  coloring = list(
    debug = "red",
    warning = "green",
    error = "purple",
    verbose = "blue",
    info = "orange",
    INTERNAL = "white"
  ),
  show_debug = TRUE
)$info("Infos are orange")$debug("Debug is red")

