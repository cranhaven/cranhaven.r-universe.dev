#' @title Log Level
#'
#' @description S3 object to represent a typed & predefined log level.
#'
#' @param name name of the log level is the string representation.
#'
#' @param description description of the log level & limited info
#' on appropriate usage.
#'
#' @param severity log severity is used in determining if a
#' message should get displayed according to the currently set
#' evaluation threshold.
#'
#' @param log_style is a {crayon::style()} that will colorize
#' the log level.
#'
#' @param msg_style is a {crayon::style()} style that will gray scale the
#' log message, with typically inverted strength, according to the severity.
#'
#' @family Log Level
#' @return \code{log_level}
#' @export
new_log_level <- function(name,
                          description,
                          severity,
                          log_style = NULL,
                          msg_style = NULL) {

  if (!is.character(name) || nchar(name) == 0)
    stop("invalid log level name")

  if (!is.character(description) || nchar(description) == 0)
    stop("invalid log level description")

  if (!is.integer(severity) || severity < 0)
    stop("invalid severity level")

  new_level <- structure(
    list(),
    name = toupper(name),
    description = description,
    severity = severity,
    log_style = log_style,
    msg_style = msg_style,
    class = c(paste0("level_", name), "log_level")
  )

  log_levels(name, new_level)

  new_level
}

#' @title Log Levels
#'
#' @description
#' an active binding to keep track of log levels created
#' with \code{new_log_level}.
#'
#' @param name name associated with the log level
#' @param level log level to add if not already existing.
#'
#' @return defined log levels
#' @export
log_levels <- local({

  levels <- list()

  function(name = character(0), level = NULL) {
    lvl_names <- names(levels)

    if (!(missing(name) || identical(name, character(0)))) {
      name <- tolower(name)

      if (length(level) > 0 && is.na(level)) {
        # remove an existing level
        lvl_idx <- which(!is.na(match(lvl_names, name)), arr.ind = TRUE)
        if (!identical(lvl_idx, integer())) {
          levels <<- levels[-lvl_idx]
        }
      } else if (!is.null(level)) {
        # assign a new level
        levels[[name]] <<- level
      } else if (name %in% lvl_names) {
        # return an existing level
        return(levels[[name]])
      }
    } else {
      # get all levels
      invisible(lvl_names)
    }
  }
})

#' @title Get Level Style
#'
#' @description gets the style of the log level.
#'
#' @param obj log level
#' @param ... further arguments passed to or from other methods.
#' @return log level name
#' @export
style.log_level <- function(obj, ...) { # nolint (generic)
  return(list(
    level = attr(obj, "log_style"),
    message = attr(obj, "msg_style")))
}

#' @title Get Log Level Name
#'
#' @description gets the name of the log level though
#' casting to a character and forwarding the call
#' to get_level_name.
#'
#' @param x log level
#' @param ... ignored
#' @return log level name
#' @export
#' @method as.character log_level
as.character.log_level <- function(x, ...) {
  return(level_name(x))
}

#' @title Get Level Name
#'
#' @description gets the name of the log level.
#'
#' @param level log level
#'
#' @return log level name
#' @export
level_name <- function(level) {
  return(attr(level, "name"))
}

#' @title Log Level Description
#'
#' @description
#' Gets the description of a log level.
#'
#' @param level log level
#'
#' @return level description
#' @export
#' @examples
#' \dontrun{
#' level_description(LEVEL)
#' }
level_description <- function(level) {
  UseMethod("level_description", level)
}

#' @title Get Log Level Description
#'
#' @description
#' Gets the description of a log level.
#'
#' @param level log level
#'
#' @return level severity
#' @export
#'
#' @examples
#' \dontrun{
#' level_description(LEVEL)
#' }
level_description <- function(level) {
  return(attr(level, "description"))
}

#' @title get level severity
#'
#' @param level log level
#'
#' @return level severity
#' @export
#' @examples
#' \dontrun{
#' level_severity(LEVEL)
#' }
level_severity <- function(level) {
  UseMethod("level_severity", level)
}

#' @title Get Log Level Severity
#'
#' @description
#' Gets the severity of a log level.
#'
#' @param level log level
#'
#' @return level severity
#' @export
#'
#' @examples
#' \dontrun{
#' level_severity(LEVEL)
#' }
level_severity <- function(level) {
  return(attr(level, "severity"))
}

#' Gets the severity of a log level.
#'
#' @param x log level
#' @param ... ignored
#' @return log level
#' @export
#' @method as.integer log_level
as.integer.log_level <- function(x, ...) {
  return(level_severity(x))
}

#' @title Log Level Information
#'
#' @description
#' Gets log level information.
#'
#' @param level log level
#'
#' @return log level information
#'
#' @export
#'
#' @examples
#' \dontrun{
#' level_info(LEVEL)
#' }
level_info <- function(level) {

  if (identical(class(level), "character")) {
    level <- log_levels(level)
  }

  if (is.null(level) || !any(match(class(level), "log_level"))) {
    stop("level info must be called on a valid log level")
  }

  lvl_style <- style(level)
  style_info <- list()

  if (!is.null(lvl_style$level) || !is.null(lvl_style$message)) {
    lvl_fmt <- "{lvl_style$level(level_name(level))}"
    msg_fmt <- "{lvl_style$message(level_description(level))}"

    style_info <- list(
      level = lvl_style$level,
      message = lvl_style$message,
      example = glue::glue_col(paste(lvl_fmt, msg_fmt, sep = " - "))
    )
  }

  list(
    name = level_name(level),
    description = level_description(level),
    severity = level_severity(level),
    style = style_info
  )
}

#' @title Log Level Format
#'
#' @description
#' formats a message with the style of the log level.
#'
#' @param x log level
#' @param message message to format
#' @param ... further arguments passed to or from other methods.
#'
#' @return styled level information
#'
#' @export
#' @examples
#' \dontrun{
#' level_info(LEVEL)
#' }
format.log_level <- function(x,
                             message = character(0),
                             ...) {

  if (identical(message, character(0))) {
    style(x)$level(level_name(x))
  } else {
    style(x)$message(message)
  }
}

#' @title Level Severities
#'
#' @description
#' Gets the severity associated with
#' each log level.
#'
#' @return styled level information
#' @export
level_severities <- function() {
  sapply(log_levels(), function(level) {
    info <- level_info(toupper(level))
    name <- level_name(level)
    setNames(info$severity, name)
  })
}

#' @title Get Minimum Severity
#'
#' @description
#' Gets the log level with the highest threshold
#' which is used internally to log 'trace/info'
#' level messages.
#'
#' @return styled level information
get_minimum_severity <- function() {
  severities <- level_severities()

  names(which(severities == max(severities)))
}
