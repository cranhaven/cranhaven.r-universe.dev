
w4m_colors__ <- list(
  red = 31,
  green = 32,
  orange = 33,
  blue = 34,
  purple = 35,
  cyan = 36,
  grey = 37,
  white = 0
)

w4m_default_coloring__ <- list(
  debug = "purple",
  warning = "orange",
  error = "red",
  verbose = "blue",
  info = "green",
  INTERNAL = "white"
)

w4m_loggers__ <- new.env()

#' get_logger
#' @title Instantiate a Logger
#' @description
#' Create a logger of the given name. You can call again `get_logger` and
#' provide the same name to get the same logger. It will not be recreated
#' unless recreate is TRUE.
#'
#' @param name the name of the logger to get or create. This name will
#' be used in logs to differentiate from which part of you program
#' comes which lines of log. See the example of usage bellow.
#' @param recreate logical=FALSE tells whether to recreate the logger
#' of the given name or not. Preferably, one should not recreate
#' a new logger each time.
#' @inheritDotParams W4MLogger
#' @return A new W4MLogger instance if it did not exist or if recreate is
#' TRUE. Otherwise, a new W4MLogger instance.
#'
#' @examples
#' ## let's say our program is divided in three big tasks:
#' ##   - the parsing
#' ##   - the processing
#' ##   - the output writing
#' parser_logger <- W4MRUtils::get_logger("Parser")
#' process_logger <- W4MRUtils::get_logger("Processing")
#' write_logger <- W4MRUtils::get_logger("Writer")
#' input_path <- "/home/anyone/input.csv"
#' parser_logger$info(paste("Parsing the input file at", input_path))
#' parser_logger$debug("Input extension detected: csv")
#' parser_logger$debug("The csv parser program will be used")
#' ## do the parsing...
#' input <- list(a=1:5, b=5:10, c=8:2)
#' parser_logger$info("Parsing succeed")
#' process_logger$info("Starting the processing of:", input)
#' process_logger$debug("The processing has started at...")
## do the processing...
#' result <- as.list(input)
#' process_logger$debug("The processing has finished at...")
#' process_logger$info("Processing finished in x seconds.")
#' outfile <- "/home/anyone/output.tsv"
#' write_logger$info(paste("Creating the output in", outfile))
#'
#' ## we detected that the input was csv and the out was tsv:
#' ## but it is not a blocking problem
#' write_logger$warning("The input and output file's extensions are different")
#' write_logger$debug("The output will be casted from csv to tsv")
#'
#' ## we try to write the file, but it fails
#' tryCatch({
#'   ## writing the output file failed with this error:
#'   stop(sprintf("I/O Error: %s is not writable.", outfile))
#' }, error = function(e) {
#'   write_logger$error(e$message)
#'   write_logger$error("Writing output file aborted.")
#'   ## quit(save = "no", status = 42)
#' })
#'
#' ## note that debug output were not written. To show debug logs
#' ## we have to active it (disabled by default):
#'
#' write_logger$set_debug()
#' write_logger$debug("The debug outputs are now visible!")
#'
#' @author L.Pavot
#' @export
get_logger <- function(name, recreate = FALSE, ...) {
  check_param_type_n_length(name, "character", nth = "first")
  check_param_type_n_length(recreate, "logical", nth = "second")
  if (!exists(name, envir = w4m_loggers__) || recreate) {
    logger <- W4MLogger(name, ...)
    assign(name, logger, envir = w4m_loggers__)
  }
  return(invisible(get(name, envir = w4m_loggers__)))
}

#' @name W4MLogger
#' @title The W4MLogger Class
#' @description
#' This is a simple logger used to make uniform outputs across W4M tools.
#'
#' See [get_logger] for example usages.
#'
#' @param name character vector of length 1 - The name of the logger.
#' Use different loggers with a name specific to each part of you program.
#' The name will appear in the log prefix and helps to determine which
#' part of the program did what
#' @param format character vector of length 1 - The format string for each
#' log line. The default is :
#' \code{"[{{ level }}-{{ name }}-{{ time }}] - {{ message }}"}
#' @param do_coloring logical vector of length 1 - By default, the logger
#' uses special control character to give some coloring to the text,
#' depending on the log level (info, warning, error, debug or verbose).
#' This coloring is deactivated in files and if
#' \code{W4MRUtils::in_galaxy_env()} returns TRUE. You can force or
#' deactivate the coloring with this  parameter.
#' @param show_debug logical vector of length 1 - Tells whether the
#' debug logs must be displayed/written or not. Default is FALSE
#' @param show_verbose logical vector of length 1 - Tells whether the
#' verbose logs must be displayed/written or not. Default is FALSE
#' @param show_info logical vector of length 1 - Tells whether the
#' info logs must be displayed/written or not Default is TRUE.
#' @param show_warning logical vector of length 1 - Tells whether the
#' warning logs must be displayed/written or not Default is TRUE.
#' @param show_error logical vector of length 1 - Tells whether the
#' error logs must be displayed/written or not Default is TRUE.
#' @param coloring named list - This lists maps a logging level to
#' its coloring. Like this: \code{list(debug = "purple", info = "green")}
#' Available colors can be found in \code{W4MRUtils::w4m_colors__}.
#' @param out_func function - the default function to print messages
#' in the terminal. The default is \code{base::message} .
#' @param out_path list of file paths - Provide a list of file path
#' where the logs will be written. It is not possible to separate
#' different levels of logs in different log files for the moment.
#' @return A W4MLogger instance
#' @seealso [W4MLogger$info], [W4MLogger$warning], [W4MLogger$error],
#' [W4MLogger$debug], [W4MLogger$verbose]
#'
#' @author L.Pavot
#' @export W4MLogger
NULL
W4MLogger <- setRefClass( ## nolint - This is a class name
  "W4MLogger",
  fields = list(
    name = "character",
    format = "character",
    do_coloring = "logical",
    coloring = "list",
    debug = "function",
    verbose = "function",
    info = "function",
    warning = "function",
    error = "function",
    debugf = "function",
    verbosef = "function",
    infof = "function",
    warningf = "function",
    errorf = "function",
    default = "function",
    out_func = "function",
    instantiated = "logical",
    out_file = "list",
    status = "list"
  )
)

#' @name W4MLogger_[info,warning,error,debug,verbose]
#' @title Log info/warning/error/debug/verbose messages
#' @aliases W4MLogger$info W4MLogger$warning W4MLogger$error
#' W4MLogger$debug W4MLogger$verbose
#' @description
#' Call one of the following function when you want a message to be printed
#' or written in a log file:
#'  - \code{your_logger$info("A info message")} ;
#'  - \code{your_logger$warning("A warning message")} ;
#'  - \code{your_logger$error("A error message")} ;
#'  - \code{your_logger$debug("A debug message")} ;
#'  - \code{your_logger$verbose.("A verbose. message")}
#'
#' If the corresponding level is activated (with your_logger$set_info(TRUE),
#' your_logger$set_debug(TRUE), etc...), these functions will print the
#' message provided in the terminal and in logs files, if there were some
#' provided at the creation of the logger.
#'
#' If the corresponding log level is deactivated, these function will
#' not do anything. So, do not hesitate to use them a lot, and activate them
#' when needed.
#'
#' See [get_logger] for example usages.
#' @author L.Pavot
NULL
W4MLogger$methods(initialize = function(
  name,
  format = "[{{ level }}-{{ name }}-{{ time }}] - {{ message }}",
  do_coloring = NULL,
  show_debug = FALSE,
  show_verbose = FALSE,
  show_info = TRUE,
  show_warning = TRUE,
  show_error = TRUE,
  coloring = w4m_default_coloring__,
  out_func = base::message,
  out_path = NULL
) {
  default_do_coloring <- do_coloring
  if (is.null(default_do_coloring)) {
    default_do_coloring <- ! in_galaxy_env()
  }
  not_ready <- function(...) {
    ## This is in case the logger is used before the set_info,
    ## set_debug, etc function are called. This should never happen.
    ## The only way I think it is possible, is if execution tree is
    ## modified at runtime. But who knows, perhaps I did a mistake!
    ## This function is not (will not be) even tested...
    stop(.self$.get_formated__("INTERNAL", "Logger called while not ready yet"))
  }
  callSuper(
    name = name,
    format = format,
    do_coloring = default_do_coloring,
    coloring = as.list(coloring),
    out_func = out_func,
    debug = not_ready,
    verbose = not_ready,
    info = not_ready,
    warning = not_ready,
    error = not_ready
  )
  .self$set_info(value = show_info, default = show_info)
  .self$set_warning(value = show_warning, default = !show_info && show_warning)
  .self$set_error(
    value = show_error,
    default = !show_info && !show_warning && show_error
  )
  .self$set_debug(value = show_debug)
  .self$set_verbose(value = show_verbose)
  if (!is.null(out_path)) {
    .self$set_out_paths(out_path)
  }
})

#' @name W4MLogger_set_out_paths
#' @title Defines in which file logs are duplicated
#' @description
#' W4MLogger can output logs in file. This function tells in which file
#' to put logs.
NULL
W4MLogger$methods(set_out_paths = function(out_paths) {
  .self$out_file <- list()
  .self$add_out_paths(out_paths)
  return(invisible(.self))
})

#' @name W4MLogger_add_out_paths
#' @title Adds a file where logs are duplicated
#' @description
#' W4MLogger can output logs in file. This function adds a file in which
#' to put logs.
NULL
W4MLogger$methods(add_out_paths = function(out_paths) {
  for (path in out_paths) {
    .self$out_file[[path]] <- file(path, open = "a")
  }
  return(invisible(.self))
})

#' This method activate or deactivate the logging of debugs messages
#' @name W4MLogger_set_debug
#' @title W4MLogger$set_debug
#'
#' @param value logical TRUE/FALSE to activate/deactivate debug logging
#' @param default logical set to TRUE to use debug by default
#' @return .self the current W4MLogger instance
NULL
W4MLogger$methods(set_debug = function(value = TRUE, default = FALSE) {
  return(.self$.set_value_for__("debug", value, default))
})

#' This method activate or deactivate the logging of info messages
#' @name W4MLogger_set_info
#' @title W4MLogger$set_info
#'
#' @param value logical TRUE/FALSE to activate/deactivate info logging
#' @param default logical set to TRUE to use info by default
#' @return .self the current W4MLogger instance
NULL
W4MLogger$methods(set_info = function(value = TRUE, default = FALSE) {
  return(.self$.set_value_for__("info", value, default))
})

#' This method activate or deactivate the logging of verbose messages
#' @name W4MLogger_set_verbose
#' @title W4MLogger$set_verbose
#'
#' @param value logical TRUE/FALSE to activate/deactivate verbose logging
#' @param default logical set to TRUE to use verbose by default
#' @return .self the current W4MLogger instance
NULL
W4MLogger$methods(set_verbose = function(value = TRUE, default = FALSE) {
  return(.self$.set_value_for__("verbose", value, default))
})

#' This method activate or deactivate the logging of warnings messages
#' @name W4MLogger_set_warning
#' @title W4MLogger$set_warning
#'
#' @param value logical TRUE/FALSE to activate/deactivate warning logging
#' @param default logical set to TRUE to use warning by default
#' @return .self the current W4MLogger instance
NULL
W4MLogger$methods(set_warning = function(value = TRUE, default = FALSE) {
  return(.self$.set_value_for__("warning", value, default))
})

#' This method activate or deactivate the logging of errors messages
#' @name W4MLogger_set_error
#' @title W4MLogger$set_error
#'
#' @param value logical TRUE/FALSE to activate/deactivate error logging
#' @param default logical set to TRUE to use error by default
#' @return .self the current W4MLogger instance
NULL
W4MLogger$methods(set_error = function(value = TRUE, default = FALSE) {
  return(.self$.set_value_for__("error", value, default))
})

#' @noRd
W4MLogger$methods(.set_value_for__ = function(level, value, default) {
  check_param_type_n_length(
    level,
    "character",
    nth = "first",
    func_name = paste0("W4MLogger$set_", level)
  )
  check_param_type_n_length(
    value,
    "logical",
    nth = "second",
    func_name = paste0("W4MLogger$set_", level)
  )
  check_param_type_n_length(
    default,
    "logical",
    nth = "third",
    func_name = paste0("W4MLogger$set_", level)
  )
  if (value) {
    func <- function(...) message(level, ...)
    assign("message", .self$.message__, envir = environment(func))
    .self$status[[level]] <- TRUE
  } else {
    .self$status[[level]] <- FALSE
    func <- function(...) invisible(.self)
  }
  .self$field(level, func)
  .self$field(paste0(level, "f"), function(...) func(..., format = TRUE))
  if (default) {
    .self$default <- func
  }
  return(invisible(.self))
})

#' @noRd
W4MLogger$methods(.internal_error__ = function(message) {
  .self$.write__(.self$.get_formated__("INTERNAL", message))
  return(invisible(.self))
})

#' @title W4MLogger_.message__
#' @name W4MLogger_.message__
#' @description
#' The function W4MLogger$.message__ is the function that gets
#' automatically called when  W4MLogger$info, W4MLogger$debug,
#' W4MLogger$warning, W4MLogger$error or W4MLogger$verbose are invoked.
#' This function is not truly internal, so it has to be considered
#' as external, but should not be exported:
#'
#' This means its has to do type checking of its inputs, and consider
#' parameters as unsafe.
#'
#' See [get_logger] for example usages.
#'
#' @param level is a string. By default its value should be either "info",
#' "debug", "warning", "debug", "verbose" or "INTERNAL".
#' But, if the logger was build with a different color naming, one of
#' the names provided in the "coloring" \code{named list} parameter must be
#' used, as it determines the color to use.
#'
#' @param ... anything, of any length. If this is not a character vector,
#' then, its displayable value will be obtained with
#' \code{capture.output(str(...))}
#' If the resulting character vector's length is greater than one,
#' then multiple messages will be printed.
#' @return this logger's instance ( \code{.self} )
#'
NULL
W4MLogger$methods(.message__ = function(level, ..., format = FALSE) {
  messages <- list(...)
  if (length(messages) > 1) {
    if (!format) {
      ## no formating. Messages are all sent one by one.
      for (message in messages) {
        .self$.one_message__(level, message)
      }
      return(invisible(.self))
    }
    formatings <- list(...)
    for (i in seq_along(formatings)) {
      if (!is.character(formatings[[i]])) {
        formatings[[i]] <- collapse_lines(
          capture.output(str(formatings[[i]]))
        )
      }
    }
    messages <- do.call(sprintf, formatings)
  } else if (length(messages) == 0) {
    messages <- ""
  } else {
    messages <- messages[[1]]
  }
  return(.self$.one_message__(level, messages))
})

#' @noRd
W4MLogger$methods(.one_message__ = function(level, ...) {
  message <- c(...)
  if (!is.character(message)) {
    message <- collapse_lines(capture.output(str(message)))
  }
  messages <- strsplit(message, "\n", fixed = TRUE)[[1]]
  if (length(messages) == 0) {
    messages <- ""
  }
  for (msg in messages) {
    text <- .self$.get_formated__(level, msg)
    if (.self$do_coloring) {
      .self$.write__(.self$.add_coloring__(text, level), no_color = text)
    } else {
      .self$.write__(text)
    }
  }
  return(invisible(.self))
})

#' @noRd
W4MLogger$methods(.add_coloring__ = function(message, level) {
  if (is.null(.coloring <- .self$coloring[[level]])) {
    stopf("Unknow logging level: %s.", level)
  }
  return(sprintf("\33[%sm%s\33[m", w4m_colors__[[.coloring]], message))
})

#' @noRd
W4MLogger$methods(.write__ = function(message, no_color = NULL) {
  .self$out_func(message)
  for (curent_file in .self$out_file) {
    if (is(curent_file, "connection") && isOpen(curent_file)) {
      if (is.null(no_color)) {
        no_color <- message
      }
      base::write(no_color, file = curent_file, append = TRUE)
    }
  }
  return(invisible(.self))
})

#' @noRd
W4MLogger$methods(.get_formated__ = function(level, message) {
  if (length(message) == 0) {
    return("")
  }
  content <- .self$format
  content <- gsub("{{ name }}", .self$name, content, fixed = TRUE)
  if (any(grepl("{{ time }}", content, fixed = TRUE))) {
    content <- gsub(
      "{{ time }}",
      base::format(Sys.time(), "%X"),
      content,
      fixed = TRUE
    )
  }
  content <- gsub("{{ level }}", sprintf("%7s", level), content, fixed = TRUE)
  content <- gsub("{{ message }}", message, content, fixed = TRUE)
  return(content)
})

#' @title W4MLogger_finalize
#' @name W4MLogger_finalize
#' @description
#' The function W4MLogger$finalize is the destructor function of this
#' class. It closes every files that was opened by the logger, or that
#' was provided during execution. It has to be considered internal.
#'
NULL
W4MLogger$methods(copy = function(new_name = NULL) {
  if (is.null(new_name)) {
    new_name <- .self$name
  }
  logger <- W4MLogger(
    name = new_name,
    format = .self$format,
    do_coloring = .self$do_coloring,
    coloring = .self$coloring,
    out_func = .self$out_func
  )
  logger$set_info(.self$status[["info"]])
  logger$set_debug(.self$status[["debug"]])
  logger$set_verbose(.self$status[["verbose"]])
  logger$set_warning(.self$status[["warning"]])
  logger$set_error(.self$status[["error"]])
  return(logger)
})

#' @title W4MLogger_finalize
#' @name W4MLogger_finalize
#' @description
#' The function W4MLogger$finalize is the destructor function of this
#' class. It closes every files that was opened by the logger, or that
#' was provided during execution. It has to be considered internal.
#'
NULL
W4MLogger$methods(sublogger = function(name, sep = "-") {
  logger <- .self$copy(name = paste(.self$name, name, sep = sep))
  return(logger)
})

#' @title W4MLogger_finalize
#' @name W4MLogger_finalize
#' @description
#' The function W4MLogger$finalize is the destructor function of this
#' class. It closes every files that was opened by the logger, or that
#' was provided during execution. It has to be considered internal.
#'
NULL
W4MLogger$methods(finalize = function() {
  .self$close_files()
})

#' @title W4MLogger_finalize
#' @name W4MLogger_finalize
#' @description
#' The function W4MLogger$finalize is the destructor function of this
#' class. It closes every files that was opened by the logger, or that
#' was provided during execution. It has to be considered internal.
#'
NULL
W4MLogger$methods(close_files = function() {
  for (path in names(.self$out_file)) {
    curent_file <- .self$out_file[[path]]
    if (is(curent_file, "connection") && isOpen(curent_file)) {
      close(curent_file)
    }
    .self$out_file[[path]] <- NULL
  }
})

#' @title W4MLogger_finalize
#' @name W4MLogger_finalize
#' @description
#' The function W4MLogger$finalize is the destructor function of this
#' class. It closes every files that was opened by the logger, or that
#' was provided during execution. It has to be considered internal.
#'
NULL
W4MLogger$methods(open_files = function() {
  for (path in names(.self$out_file)) {
    curent_file <- .self$out_file[[path]]
    if (is(curent_file, "connection") && isOpen(curent_file)) {
      close(curent_file)
    }
    .self$out_file[[path]] <- file(path, open = "a")
  }
})
