
## Multiline match is just completely f***** up
## so, I've written a version that works.
multiline_match <- function(x, regex, fun = testthat::expect_match) {
  lapply(seq_along(x), function(i) fun(x[[i]], regex = regex[[i]]))
}

base_regex <- "\\[\\s*%s\\-test\\-\\d{2}(:\\d{2}){2}\\] - %s"
regexps <- list(
  info = info_regex <- sprintf(base_regex, "info", "%s"),
  warning = warning_regex <- sprintf(base_regex, "warning", "%s"),
  error = error_regex <- sprintf(base_regex, "error", "%s"),
  verbose = verbose_regex <- sprintf(base_regex, "verbose", "%s"),
  debug = debug_regex <- sprintf(base_regex, "debug", "%s"),
  internal = internal_regex <- sprintf(base_regex, "INTERNAL", "%s")
)

test_that("Testing get_logger failproof", {
  err_base <- "The 'name' (first) parameter for get_logger must be"
  testthat::expect_error(
    get_logger(1),
    regex = paste(err_base, "a character, not a numeric."),
    fixed = TRUE
  )
  testthat::expect_error(
    get_logger(NULL),
    regex = paste(err_base, "a character, not a NULL"),
    fixed = TRUE
  )
  testthat::expect_error(
    get_logger(NA),
    regex = paste(err_base, "a character, not a logical"),
    fixed = TRUE
  )
  testthat::expect_error(
    get_logger(data.frame()),
    regex = paste(err_base, "a character, not a data.frame."),
    fixed = TRUE
  )
  testthat::expect_error(
    get_logger(character(0)),
    regex = paste(err_base, "1 element long, not 0."),
    fixed = TRUE
  )
  testthat::expect_error(
    get_logger(c("black", "lives", "matter")),
    regex = paste(err_base, "1 element long, not 3."),
    fixed = TRUE
  )
})

test_that("Testing simple get_logger()", {
  testthat::expect_warning(get_logger("test"), regex = NA)
  testthat::expect_error(get_logger("test"), regex = NA)
})

test_that("Testing logger$set_* failproof", {
  logger <- get_logger("test")
  err_base <- paste(
    "The 'value' (second) parameter for",
    "W4MLogger$set_info must be"
  )
  testthat::expect_error(
    logger$set_info("blahaj! UwU"),
    regex = paste(err_base, "a logical, not a character."),
    fixed = TRUE
  )
  testthat::expect_error(
    ## The flag is important to test Unicode!!
    ## Do not remove or everything will crash!!
    ## - your local IT trans engineer
    logger$set_info(c("support", "trans", "rights 🏳️‍⚧️")),
    regex = paste(err_base, "a logical, not a character."),
    fixed = TRUE
  )
  testthat::expect_error(
    logger$set_info(c(TRUE, FALSE, !NA)),
    regex = paste(err_base, "1 element long, not 3."),
    fixed = TRUE
  )
})

test_that("Testing W4MLogger base", {
  logger <- get_logger("test")
  msg <- paste(
    "Les belettes sont des petits animaux nocturnes de la",
    "famille des zbeulideae..."
  )
  testthat::expect_message(
    logger$info(msg),
    regex = sprintf(info_regex, msg)
  )
  testthat::expect_message(
    logger$info(),
    regex = sprintf(info_regex, "")
  )
  testthat::expect_message(
    logger$warning(msg),
    regex = sprintf(warning_regex, msg)
  )
  testthat::expect_message(
    logger$error(msg),
    regex = sprintf(error_regex, msg)
  )
  testthat::expect_message(logger$debug(msg), regex = NA)
  testthat::expect_message(logger$verbose(msg), regex = NA)
})

test_that("Testing W4MLogger zero logger", {
  logger <- get_logger("test")
  logger$set_verbose(FALSE)
  logger$set_info(FALSE)
  logger$set_debug(FALSE)
  logger$set_warning(FALSE)
  logger$set_error(FALSE)
  for (kind in c("info", "warning", "error", "debug", "verbose")) {
    testthat::expect_message(logger$field(kind)("ACAB"), regex = NA)
  }
})

test_that("Testing W4MLogger all kind of loggers", {
  logger <- get_logger("test")
  setters <- list(
    verbose = logger$set_verbose,
    info = logger$set_info,
    debug = logger$set_debug,
    warning = logger$set_warning,
    error = logger$set_error
  )
  for (kind in names(setters)) {
    setters[[kind]](FALSE)
  }
  for (kind in names(setters)) {
    setters[[kind]](TRUE)
    testthat::expect_message(
      logger$field(kind)("ACAB"),
      regex = sprintf(regexps[[kind]], "ACAB")
    )
    setters[[kind]](FALSE)
  }
})

test_that("Testing W4MLogger logger outputs", {
  regex <- c(
    sprintf(info_regex, "Starting the processing of:"),
    sprintf(info_regex, "List of 3"),
    " \\$ a: int \\[1:5\\] 1 2 3 4 5",
    " \\$ b: int \\[1:6\\] 5 6 7 8 9 10",
    " \\$ c: int \\[1:7\\] 8 7 6 5 4 3 2"
  )
  result <- strsplit(
    capture.output(get_logger("test")$set_info()$info(
      "Starting the processing of:", list(a = 1:5, b = 5:10, c = 8:2)
    ), type = "message"),
    "\n"
  )[[1]]
  multiline_match(result, regex)
})

test_that("Testing W4MLogger logger no coloring outputs", {
  regex <- c(
    sprintf(info_regex, "Starting the processing of:"),
    sprintf(info_regex, "List of 3"),
    " \\$ a: int \\[1:5\\] 1 2 3 4 5",
    " \\$ b: int \\[1:6\\] 5 6 7 8 9 10",
    " \\$ c: int \\[1:7\\] 8 7 6 5 4 3 2"
  )
  result <- strsplit(
    capture.output(
      W4MLogger("test", do_coloring = FALSE)
      $set_info()
      $info(
        "Starting the processing of:",
        list(a = 1:5, b = 5:10, c = 8:2)
      ),
      type = "message"
    ),
    "\n"
  )[[1]]
  multiline_match(result, regex)
})

test_that("Testing W4MLogger output file", {
  regex <- c(
    sprintf(info_regex, "Starting the processing of:"),
    sprintf(info_regex, "List of 3"),
    " \\$ a: int \\[1:5\\] 1 2 3 4 5",
    " \\$ b: int \\[1:6\\] 5 6 7 8 9 10",
    " \\$ c: int \\[1:7\\] 8 7 6 5 4 3 2"
  )
  paths <-  file.path(tempdir(), c("test1", "test2"))
  capture.output(
    W4MLogger("test", out_path = paths)
    $set_info()
    $info(
      "Starting the processing of:",
      list(a = 1:5, b = 5:10, c = 8:2)
    ),
    type = "message"
  )
  results <- lapply(paths, readLines)
  multiline_match(results[[1]], regex)
  multiline_match(results[[2]], regex)
})
