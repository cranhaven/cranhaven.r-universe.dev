#' @title
#' Set logger settings
#'
#' @description
#' Updates logger settings, including log level and location of the file.
#'
#' @param logger_file_path A path (including file name) to log the messages.
#' (Default: GPCERF.log)
#' @param logger_level The log level. Available levels include:
#'   - TRACE
#'   - DEBUG
#'   - INFO (Default)
#'   - SUCCESS
#'   - WARN
#'   - ERROR
#'   - FATAL
#'
#' @export
#'
#' @return
#' No return value. This function is called for side effects.
#'
#' @examples
#'
#' set_logger("mylogger.log", "INFO")
#'
set_logger <- function(logger_file_path = "GPCERF.log",
                       logger_level = "INFO") {

  available_levels <- c("TRACE", "DEBUG", "INFO", "SUCCESS", "WARN",
                        "ERROR", "FATAL")

  if (!is.element(logger_level, available_levels)) {
    stop(paste("logger_level: ", logger_level, " is not valid."))
  }

  logger::log_appender(appender = logger::appender_file(logger_file_path),
                       index = 1)

  set_options("logger_file_path", logger_file_path)
  set_options("logger_level", logger_level)

  logger::log_threshold(logger_level)
}

#' @title
#' Get logger settings
#'
#' @description
#' Returns current logger settings.
#'
#'
#' @return
#' Returns a list that includes **logger_file_path** and **logger_level**.
#'
#' @export
#'
#' @examples
#'
#' set_logger("mylogger.log", "INFO")
#' log_meta <- get_logger()
#'
get_logger <- function() {

  return(list(logger_file_path = get_options("logger_file_path"),
              logger_level = get_options("logger_level")))
}


# Keeping logger options
my_options <- new.env(parent = emptyenv())

get_options <- function(k, v) {
  my_options[[k]]
}

set_options <- function(k, v) {
  my_options[[k]] <- v
}

list_options <- function() {
  names(my_options)
}
