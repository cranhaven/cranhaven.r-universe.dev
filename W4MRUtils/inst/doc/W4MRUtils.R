## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- include = FALSE---------------------------------------------------------

input <- file.path(tempdir(), "./input.csv")
commandArgs <- function() { #nolint
  list(
    "--input", input,
    "--output", file.path(tempdir(), "./output.csv"),
    "--threshold", "2"
  )
}
write.csv(
  data.frame(x = c(10, 2, 1, 4, 7, 9), y = c("a", "b", "c", "d", "e", "f")),
  input,
  row.names = FALSE
)

## ----setup--------------------------------------------------------------------
library(W4MRUtils)

TOOL_NAME <- "A Test Tool" #nolint

## ----create a logger----------------------------------------------------------
logger <- get_logger(TOOL_NAME)

## ----read parameters----------------------------------------------------------
logger$info("Parsing parameters...")
args <- optparse_parameters(
  input = optparse_character(),
  output = optparse_character(),
  threshold = optparse_numeric(default = 10),
  args = commandArgs()
)
logger$info("Parsing parameters OK.")
show_galaxy_header(
  TOOL_NAME,
  "1.2.0",
  args = args,
  logger = logger,
  show_sys = FALSE
)

## ----check parameters---------------------------------------------------------
check_parameters <- function(args, logger) {
  logger$info("Checking parameters...")
  check_one_character(args$input)
  check_one_character(args$output)
  check_one_numeric(args$threshold)
  if (args$threshold < 1) {
    logger$errorf(
      "The threshold is too low (%s). Cannot continue", args$threshold
    )
    stopf("Threshold = %s is tool low.", args$threshold)
  }
  if (args$threshold < 3) {
    logger$warningf(
      paste(
        "The threshold is very low (%s).",
        "This may lead to erroneous results."
      ),
      args$threshold
    )
  }
  logger$info("Parameters OK.")
}

read_input <- function(args, logger) {
  logger$infof("Reading file in %s ...", args$input)
  return(read.csv(args$input))
}

process_input <- function(args, logger, table) {
  filter <- table[1, ] > args$threshold
  logger$infof(
    "%s values filtered (%s%%)",
    length(filter),
    round(length(filter) / nrow(table) * 100, 2)
  )
  if (length(filter) == nrow(table)) {
    logger$warning("All values have been filtered.")
  }
  return(table[which(table[, 1] > args$threshold), ])
}

write_output <- function(args, logger, output) {
  logger$info("Writing output file...")
  write.table(output, args$output)
}

# check_parameters(args, logger$sublogger("Param Checking"))
# input <- read_input(args, logger$sublogger("Inputs Reader"))
# print(input)
# output <- process_input(args, logger$sublogger("Inputs Processing"), input)
# print(output)
# write_output(args, logger$sublogger("Output Writter"), output)
# show_galaxy_footer(TOOL_NAME, "1.2.0", logger = logger, show_packages = FALSE)

