#' @title Get Configurations
#'
#' @description
#' Gets all available logging configurations
#' exposed by the package.
#'
#' @param pkgname package name to get configs for.
#'
#' @family Logging
#'
#' @export
get_configurations <- function(pkgname = "dyn.log") {

  config_files <- list.files(
    system.file("", package = pkgname),
    pattern = ".yaml",
    full.names = TRUE
  )

  configs <- list()

  sapply(config_files, function(fname) {
    cname <- tools::file_path_sans_ext(basename(fname))
    configs[[cname]] <<- fname
    invisible()
  })

  configs
}

#' @title Init Logger
#'
#' @description
#' Loads the configuration passed in,
#' or uses the default if nothing is specified,
#' and attaches a reference to the singleton
#' dispatcher to the global environment.
#'
#' @param file_path logging configuration to use.
#' @family Logging
#' @export
init_logger <- function(file_path = NULL) {
  tryCatch(
    expr = {

      config_file <- configs$default

      if (!is.null(file_path)) {
        if (!file.exists(file_path)) {
          stop(paste("Unable to load logging configuration: ", file_path))
        }
        config_file <- file_path
      }

      config <- yaml::read_yaml(config_file, eval.expr = TRUE)
      config_name <- tools::file_path_sans_ext(basename(config_file))

      log_levels <- create_log_levels(config$levels)
      apply_active_settings(config$settings)

      ensure_logger(config$variable)

      if (!identical(active$config, config_name)) {
        logger <- LogDispatch$new()

        sapply(log_levels, function(level) {
          logger$attach_log_level(level)
        })

        logger$default("dyn.log loaded '{config_name}' configuration successfully.")

        active$config <- config_name
      }

      load_log_layouts(config$layouts)

      invisible(active)
    },
    error = function(cond) {
      stop(paste0("Failed to load dyn.log:", cond))
    }
  )
}

#' @title Ensure Instance
#'
#' @description
#' Ensures there is an active dispatcher
#' attached to the specified environment.
#'
#' @param variable variable name.
#'
#' @family Configuration
#'
#' @returns None.
#' @export
ensure_logger <- function(variable) {

  if (is.null(variable) |
      nchar(variable) == 0 |
      make.names(variable) != variable) {
    stop("Config setting 'variable' must be supplied and valid R variable name.")
  }

  envir <- globalenv()
  idx <- which(ls(envir) == variable, arr.ind = TRUE)

  if (identical(idx, integer())) {
    logger <- LogDispatch$new()
    envir[[variable]] <- logger
    active$log_var <- variable
  }
}

#' @title Wipe the Logger Instance
#'
#' @description
#' Cleans up any dangling global instance
#' from a previous load.
#'
#' @family Configuration
#'
#' @returns None.
#' @export
wipe_logger <- function() {
  envir <- globalenv(); objs <- ls(envir)

  idx <- which(objs == active$log_var, arr.ind = TRUE)

  if (!identical(idx, integer(0))) {
    rm(list = objs[idx], envir = envir)
  }
}

#' @title Apply Active Logger Settings
#'
#' @description
#' Parses and loads the settings specified
#' in the logger configuration and ensures
#' they are active in the environment.
#'
#' @param settings defined in the configuration
#'
#' @family Configuration
apply_active_settings = function(settings) {

  threshold_level <- log_levels(settings$threshold)

  active$threshold <- list()
  active$threshold$name <- level_name(threshold_level)
  active$threshold$severity <- level_severity(threshold_level)

  active$callstack <- list()
  active$callstack$max <- settings$callstack$max
  active$callstack$start <- settings$callstack$start
  active$callstack$stop <- settings$callstack$stop
}

#' @title Active Logger Settings
#'
#' @description
#' Gets the active global settings
#' for the logger.
#'
#' @family Configuration
#' @export
get_active_settings = function() {
  as.list(active)
}

#' @title Attach Log Levels
#'
#' @description
#' Parses and loads the levels specified in the
#' logging configuration and registers them with the
#' dispatcher via the \code{log_levels} active
#' binding.
#'
#' @param definitions defined in the configuration
#' @family Configuration
create_log_levels = function(definitions) {
  sapply(definitions, function(level) {
    new_log_level(name = level$name,
                  description = level$description,
                  severity = as.integer(level$severity),
                  log_style = level$log_style,
                  msg_style = level$msg_style)
  })
}

#' @title Load Log Layouts
#'
#' @description
#' Parses and loads layouts specified in the logging
#' configuration and registers them with the log
#' dispatcher via the \code{log_layouts} active
#' binding.
#'
#' @param layouts defined in the configuration
#'
#' @family Configuration
#'
#' @returns None.
load_log_layouts <- function(layouts) {

  sapply(layouts, function(layout) {
    if (identical(class(layout$format), "character")) {

      parsed <- stringr::str_split(layout$formats,
                                   pattern = ",",
                                   simplify = TRUE)

      formats <- sapply(parsed, function(fmt) {
        eval(parse(text = stringr::str_trim(fmt)))
      })
    } else {
      formats <- layout$formats
    }

    new_layout <- new_log_layout(
      format = formats,
      seperator = layout$seperator,
      new_line = layout$new_line,
      association = layout$association
    )
  })

  invisible()
}

#' @title Display Log Levels
#'
#' @description
#' A utility function that dynamically displays the configured
#' log levels (loaded from config), and outputs them in a simple
#' layout with only the log level and msg formatted in their
#' crayon styles.
#'
#' @family Configuration
#'
#' @export
display_log_levels <- function() {

  sapply(log_levels(), function(level) {
    info <- level_info(level)
    logger <- LogDispatch$new()
    fn <- logger[[tolower(info$name)]]

    if (is.function(fn)) {
      fn(msg = info$description, layout = "level_msg")
      cat("\n")
    }
  })

  invisible()
}

#' @title Config Specification
#'
#' @description
#' Loads & attaches a logger with the specified
#' config.
#'
#' @family Configuration
#'
#' @return Nothing.
config_specification <- function() {

  config <- getOption("dyn.log.config")

  if (!is.null(config)) {

    if (any(!is.na(match(names(configs), config)))) {
      config <- configs[[config]]
    }

    if (file.exists(config)) {
      init_logger(file_path = config)
    } else {
      warning("Config options was specified but the file doesn't exist.")
    }
  }

  invisible()
}
