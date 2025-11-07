#' @title Format Layout
#'
#' @description
#' Base type for log format objects.
#' @param style [crayon] that the layout will use in log generation.
#'
#' @family Log Layout
#' @return new log format
#' @export
new_fmt_layout <- function(style) {

  stopifnot(class(style) == "crayon")

  structure(
    list(),
    style = style,
    class = c("fmt_layout")
  )
}

#' @title Formatted Log Level
#'
#' @description
#' Placeholder for the formatted log level in a log layout.
#'
#' @family Formats
#' @returns a \code{fmt_log_level}.
#' @export
new_fmt_log_level <- function() {
  structure(
    list(),
    style = crayon::black,
    class = c("fmt_log_level", "fmt_layout")
  )
}

#' @title Value
#'
#' @description
#' Generic override for getting the value for
#' log level information.
#'
#' @param obj object to extract value from.
#' @param lvl_context context to evaluate log level.
#' @param ... further arguments passed to or from other methods.
#'
#' @return object's value
#' @export
value.fmt_log_level <- function(obj, lvl_context, ...) { # nolint (generic)
  format(lvl_context$level, message = lvl_context$name)
}

#' @title
#' Formatted Messaged, based on log level
#'
#' @description
#' Placeholder for the log msg in a log layout.
#'
#' @family Log Layout
#' @returns log layout newline.
#' @export
new_fmt_log_msg <- function() {
  structure(
    list(),
    style = crayon::black,
    class = c("fmt_log_msg", "fmt_layout")
  )
}

#' @title Value
#'
#' @description
#' Generic override for getting the value of an
#' log format message.
#'
#' @param obj object to extract value from.
#' @param msg_context context to evaluate log message.
#' @param ... further arguments passed to or from other methods.
#'
#' @return object's value
#' @export
value.fmt_log_msg <- function(obj, msg_context, ...) { # nolint (generic)
  format(msg_context$level, message = msg_context$msg)
}

#' @title Formatted Metric
#'
#' @description
#' Inserts a formatted log metric.
#'
#' @param style that the layout will use in log generation
#' @param metric the metric to log.
#'
#' @seealso [LogDispatch]
#' @family Log Layout
#' @return a new formatted metric
#' @export
#'
#' @examples
#' \dontrun{
#' new_fmt_metric(bold $ green, "sysname")
#'
#' new_fmt_metric(bold $ red, "release")
#' }
new_fmt_metric <- function(style, metric) {

  stopifnot(class(style) == "crayon")

  if (!is.character(metric) || nchar(metric) == 0)
    stop("invalid log metric specified")

  valid_metric <- any(!is.na(match(names(sys_context()), metric)))

  if (!valid_metric) {
    stop(paste0("metric '", metric, "' is not a reconized system metric.",
                " See ?sys_context for more information."))
  }

  structure(
    list(),
    style = style,
    metric = metric,
    class = c("fmt_metric", "fmt_layout")
  )
}

#' @title Style
#'
#' @description
#' Gets the style of a format object.
#'
#' @param obj object to extract value from.
#' @param ... further arguments passed to or from other methods.
#'
#' @return object's value
#' @export
style.fmt_layout <- function(obj, ...) { # nolint (generic)
  attr(obj, "style")
}

#' @title Value
#'
#' @description
#' Generic override for getting the value of an
#' system info variable.
#'
#' @param obj object to extract value from.
#' @param sys_context context to evaluate the metric.
#' @param ... further arguments passed to or from other methods.
#'
#' @return object's value
#' @export
value.fmt_metric <- function(obj, sys_context, ...) { # nolint (generic)
  style(obj)(get(attr(obj, "metric"), sys_context))
}

#' @title Formatted Time stamp
#'
#' @description
#' Placeholder for a formatted time stamp in a log layout.
#'
#' @param style format style (crayon)
#' @param format time stamp format, defaults to: %x %H:%M:%S %z,
#' e.g., 12/04/21 14:31:25 -0500
#'
#' @family Log Layout
#' @returns log metric layout.
#' @export
#' @examples
#' \dontrun{
#' fmt_timestamp(red $ bold, "%Y-%m-%d %H:%M:%S")
#'
#' fmt_timestamp(blue $ italic, "%x %H:%M:%S %z")
#' }
new_fmt_timestamp <- function(style,
                              format = "[%x %H:%M:%S %z]") {
  structure(
    list(),
    style = style,
    format = format,
    value = rlang::as_function(~ format(Sys.time(), .)),
    class = c("fmt_timestamp", "fmt_layout")
  )
}

#' Gets the format of a format object.
#'
#' @param x object to extract value from.
#' @param ... further arguments passed to or from other methods.
#'
#' @return object's value
#' @export
format.fmt_timestamp <- function(x, ...) {
  attr(x, "format")
}

#' @title Value
#'
#' @description
#' Generic override for getting the value of a
#' formatted timestamp.
#'
#' @param obj object to extract value from.
#' @param ... further arguments passed to or from other methods.
#'
#' @return object's value
#' @export
value.fmt_timestamp <- function(obj, ...) { # nolint (generic)
  v <- attr(obj, "value")
  f <- attr(obj, "format")

  style(obj)(v(f))
}

#' @title Formatted Literal
#'
#' @description
#' Placeholder for a formatted literal in a log layout.
#'
#' @param style format style (crayon)
#' @param literal log value
#'
#' @family Log Layout
#' @returns log metric layout.
#' @export
#' @examples
#' \dontrun{
#' new_fmt_literal(red $ bold, "literal text")
#'
#' new_fmt_literal(blue $ italic, "literal text")
#' }
new_fmt_literal <- function(style, literal) {
  structure(
    list(),
    style = style,
    value = literal,
    class = c("fmt_literal", "fmt_layout")
  )
}

#' @title Value
#'
#' @description
#' Generic override for getting the value of a
#' literal log message.
#'
#' @param obj object to extract value from.
#' @param ... further arguments passed to or from other methods.
#'
#' @return object's value
#' @export
value.fmt_literal <- function(obj, ...) { # nolint (generic)
  style(obj)(attr(obj, "value"))
}

#' @title Formatted Line Break
#'
#' @description
#' Placeholder for a new line in a log layout.
#'
#' @family Log Layout
#' @returns log layout newline.
#' @export
new_fmt_line_break <- function() {
  structure(
    list(),
    style = crayon::black,
    value = "\n",
    class = c("fmt_newline", "fmt_layout")
  )
}

#' @title Value
#'
#' @description
#' Generic override for getting the value of a
#' new line placeholder.
#'
#' @param obj object to extract value from.
#' @param ... further arguments passed to or from other methods.
#'
#' @return object's value
#' @export
value.fmt_newline <- function(obj, ...) { # nolint (generic)
  attr(obj, "value")
}

#' @title
#' Formatted field from the calling class scope.
#'
#' @description
#' Placeholder for a container class field
#'
#' @param style {crayon::style()}
#' @param field field in the object to display
#'
#' @family Log Layout
#' @returns \code{new_fmt_cls_field}
#' @export
new_fmt_cls_field <- function(style, field) {

  stopifnot(class(style) == "crayon")

  if (!is.character(field) || nchar(field) == 0)
    stop("invalid cls field specified")

  structure(
    list(),
    style = style,
    field = field,
    class = c("fmt_cls_field", "fmt_layout")
  )
}

#' @title Value
#'
#' @description
#' Generic override for getting the value of an
#' enclosing class variable.
#'
#' @param obj object to extract value from.
#' @param cls_context class scope to evaluate with.
#' @param ... further arguments passed to or from other methods.
#'
#' @return object's value
#' @export
value.fmt_cls_field <- function(obj, cls_context, ...) { # nolint (generic)
  value <- get(attr(obj, "field"), cls_context)
  style(obj)(value)
}

#' @title
#' Formatted variable from the execution scope.
#'
#' @description
#' Placeholder for an execution scope variable.
#'
#' @param style {crayon::style()}
#' @param field execution scope field
#'
#' @family Log Layout
#' @returns \code{new_fmt_cls_field}
#' @export
new_fmt_exec_scope <- function(style, field) {

  stopifnot(class(style) == "crayon")

  if (!is.character(field) || nchar(field) == 0)
    stop("invalid execution scope field specified")

  structure(
    list(),
    style = style,
    field = field,
    class = c("fmt_exec_scope", "fmt_layout")
  )
}

#' @title Value
#'
#' @description
#' Generic override for getting the value of an
#' execution scope variable.
#'
#' @param obj object to extract value from.
#' @param env_context class scope to evaluate with.
#' @param ... further arguments passed to or from other methods.
#'
#' @return object's value
#' @export
value.fmt_exec_scope <- function(obj, env_context, ...) { # nolint (generic)
  value <- get(attr(obj, "field"), env_context)
  style(obj)(value)
}
