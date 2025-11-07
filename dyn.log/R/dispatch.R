#' @title Log Dispatch
#'
#' @description
#' R6 Class that dispatches log messages throughout the application.
#'
#' @details
#' This object is designed to a centralized logging dispatcher that
#' renders log messages with the appropriate context of the calling
#' object. The \code{log_layout()} object is used to generate log message
#' layouts (render formats), which are used by the \code{LogDispatcher}
#' to render highly-customizable and detailed log messages.
#'
#' @docType class
#' @family Logging
#' @importFrom R6 R6Class
#' @importFrom rlang caller_env
#' @importFrom glue glue
#' @export
LogDispatch <- R6::R6Class( #nolint
  classname = "LogDispatch",
  lock_objects = FALSE,
  lock_class = FALSE,
  cloneable = FALSE,
  portable = FALSE,

  public = list(

    #' @description
    #' Creates a new instance of a log config.
    #' @return A new `LogLayout` object.
    initialize = function() {

      if (is.null(private$public_bind_env)) {
        private$default_severitiy <- 0
        private$create_singleton(LogDispatch)
      } else {
        self <- private$instance

        private$set_bindings()
      }

      invisible(self)
    },

    #' @description
    #' Attaches a S3 \code{log_level} object to the log
    #' dispatcher by creating a new function wrapping the
    #' specified log level, and binding and instance
    #' of the dispatcher quote block with
    #' the context of the log level.
    #'
    #' @param log_level log level to attach
    attach_log_level = function(log_level) {

      fn_name <- tolower(level_name(log_level))
      dispatch <- private$create_level_dispatch(log_level)

      self[[fn_name]] <- dispatch

      lvl_sev <- level_severity(log_level)
      if (lvl_sev > private$default_severitiy) {
        self[["default"]] <- dispatch
        private$default_severitiy <- lvl_sev
      }

      invisible(self)
    }
  ),

  active = list(),

  private = list(

    default_severitiy = NULL,

    # overrides from base R6
    public_bind_env = NULL,
    private_bind_env = NULL,

    create_singleton = function(obj) {
      # nocov start
      private$public_bind_env <- base::dynGet("public_bind_env")
      private$private_bind_env <- base::dynGet("private_bind_env")

      obj$set("private",
              "public_bind_env",
              private$public_bind_env,
              overwrite = TRUE)

      obj$set("private",
              "private_bind_env",
              private$private_bind_env,
              overwrite = TRUE)
      # nocov end
    },

    set_bindings = function() {

      private$copy_env("public_bind_env",
                       private$public_bind_env)

      private$copy_env("private_bind_env",
                       private$private_bind_env)
    },

    copy_env = function(key, value) {

      n <- sys.nframe()

      for (idx in seq_len(n - 1)) {
        parent_env <- parent.frame(idx)
        parent_keys <- ls(parent_env)

        if (any(key %in% parent_keys))
          base::assign(key, value, envir = parent_env)
      }

      invisible()
    },

    create_level_dispatch = function(level) {
      rlang::new_function(
        args = rlang::pairlist2(msg = ,
                                level = level,
                                layout = "default"),
        body = dispatcher,
        env = rlang::caller_env()
      )
    }
  )
)

dispatcher <- quote({

  if (is.null(level)) return()

  if (level_severity(level) > active$threshold$severity) return()

  caller_env <- rlang::caller_env()
  parent_env <- parent.env(caller_env)

  has_calling_class <- ifelse(is.null(parent_env$self), FALSE, TRUE)
  calling_class <- NA

  log_msg <- glue::glue(msg, .envir = caller_env)
  log_layout <- log_layouts(layout)

  if (has_calling_class) {
    calling_class <- parent_env$self

    # match class name(s) against registered
    # log layouts, to find the "best" class
    # association given its hierarchy.
    cls_name <- class(calling_class)
    registered <- names(log_layouts())
    layout_idx <- max(c(0L, match(cls_name, registered)), na.rm = TRUE)

    if (layout_idx > 0) {
      log_layout <- log_layouts(registered[layout_idx])
    }
  }

  if (is.null(layout)) {
    stop("cannot log without an associated layout.")
  }

  detail <- log_layout_detail(log_layout)

  context <- list()

  context[["fmt_log_level"]] <- list(
    name = level_name(level),
    level = level
  )

  context[["fmt_log_msg"]] <- list(
    msg = log_msg,
    level = level
  )

  if (has_calling_class && any(!is.na(match(detail$types, "fmt_cls_field")))) {
    context[["fmt_cls_field"]] <- class_scope(calling_class)
  }

  if (any(!is.na(match(detail$types, "fmt_metric")))) {
    context[["fmt_metric"]] <- sys_context()
  }

  if (any(!is.na(match(detail$types, "fmt_exec_scope")))) {

    context[["fmt_exec_scope"]] <- exec_context(
      max_calls = active$callstack$max,
      call_subset = c(active$callstack$start,
                      active$callstack$stop))
  }

  evaluated <- evaluate_layout(detail, context)

  cat(evaluated,
      fill = TRUE,
      file = stdout())
})
