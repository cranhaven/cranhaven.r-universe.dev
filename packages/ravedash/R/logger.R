#' @name logger
#' @title Logger system used by 'RAVE'
#' @description Keep track of messages printed by modules
#' @param ...,.envir,.sep passed to \code{\link[raveio]{glue}}, if
#' \code{use_glue} is true
#' @param use_glue whether to use \code{\link[raveio]{glue}} to combine
#' \code{...}; default is false
#' @param calc_delta whether to calculate time difference between current
#' message and previous message; default is \code{'auto'}, which prints
#' time difference when \code{level} is \code{'debug'}.
#' This behavior can be changed by altering \code{calc_delta} by a logical
#' \code{TRUE} to enable or \code{FALSE} to disable.
#' @param level the level of message, choices are \code{'info'} (default),
#' \code{'warning'}, \code{'error'}, \code{'fatal'}, \code{'debug'},
#' \code{'trace'}
#' @param module_id 'RAVE' module identification string, or name-space; default
#' is \code{'ravedash'}
#' @param reset_timer whether to reset timer used by \code{calc_delta}
#' @param root_path root directory if you want log messages to be saved to
#' hard disks; if \code{root_path} is \code{NULL}, \code{""}, or
#' \code{\link{nullfile}}, then logger path will be unset.
#' @param max_bytes maximum file size for each logger partitions
#' @param max_files maximum number of partition files to hold the log; old
#' files will be deleted.
#' @param type which type of logging should be set; default is \code{'console'},
#' if file log is enabled through \code{set_logger_path}, \code{type} could be
#' \code{'file'} or \code{'both'}. Default log level is \code{'info'} on
#' console and \code{'debug'} on file.
#' @param cond condition to log
#' @param class,title,delay,autohide passed to \code{\link[shidashi]{show_notification}}
#' @param session shiny session
#' @param expr expression to evaluate
#' @param envir environment to evaluate \code{expr}
#' @param quoted whether \code{expr} is quoted; default is false
#' @return The message without time-stamps
#'
#' @examples
#' logger("This is a message")
#'
#' a <- 1
#' logger("A message with glue: a={a}")
#'
#' logger("A message without glue: a={a}", use_glue = FALSE)
#'
#'
#' logger("Message A", calc_delta = TRUE, reset_timer = TRUE)
#' logger("Seconds before logging another message", calc_delta = TRUE)
#'
#'
#' # by default, debug and trace messages won't be displayed
#' logger('debug message', level = 'debug')
#'
#' # adjust logger level, make sure `module_id` is a valid RAVE module ID
#' logger_threshold('debug', module_id = NULL)
#'
#' # Debug message will display
#' logger('debug message', level = 'debug')
#'
#' # Trace message will not display as it's lower than debug level
#' logger('trace message', level = 'trace')
#'
NULL

appender_ravedash <- function (
  file, append = TRUE, max_bytes = Inf,
  max_files = ifelse(is.finite(max_bytes), 10, 1))
{
  force(file)
  force(append)
  force(max_bytes)
  max_files <- as.integer(max_files)
  if(!isTRUE(max_files >= 1L)){
    max_files <- 1L
  }

  if(file == nullfile()){
    partition_path <- function(ii){
      file
    }

    ensure_file <- function(){}
  } else {
    if(!endsWith(tolower(file), ".log")) {
      file <- paste0(file, ".log")
    }
    partition_path <- function(ii){
      if(ii == 0){
        return(file)
      }
      sub(pattern = "\\.log$", x = file, replacement = sprintf(".%d.log", ii), ignore.case = TRUE)
    }
    ensure_file <- function(){
      if(!file.exists(file)){
        if(dir.exists(dirname(file))){
          file.create(file)
        }
      }
    }
  }

  ensure_file()




  if(as.logical(dipsaus::package_installed("crayon"))){
    remove_style <- function(x){
      crayon::strip_style(x)
    }
  } else {
    remove_style <- function(x) { x }
  }


  if(is.finite(max_bytes) && file != nullfile()) {
    structure(function(lines) {
      n_bytes <- ifelse(file.exists(file), file.info(file)$size, 0)
      if( n_bytes >= max_bytes ){
        # file size is too large, purge or create new
        for (ii in max_files:1) {
          partition <- partition_path(ii)
          if(ii == max_files){
            unlink(partition, recursive = FALSE, force = TRUE)
          }
          previous_partition <- partition_path(ii - 1L)
          if(file.exists(previous_partition)){
            file.rename(previous_partition, partition)
          }
        }
        ensure_file()
      }

      if(file.exists(file)){
        cat(remove_style(lines), sep = "\n", file = file, append = append)
      }

    }, generator = deparse(match.call()))

  } else if(file == nullfile()){
    structure(function(lines) { }, generator = deparse(match.call()))
  } else {
    structure(function(lines) {
      if(file.exists(file)){
        cat(remove_style(lines), sep = "\n", file = file, append = append)
      }
    }, generator = deparse(match.call()))
  }
}


abbr_module_id <- function(module_id, ...){
  paste(sapply(unlist(strsplit(module_id, "_")), abbreviate, minlength = 2), collapse = "_")
}

.logger_functions <- local({
  crayon_ns <- NULL
  logger_data <- NULL

  ensure_logger_data <- function(){
    if(!inherits(logger_data, "fastmap2")){
      logger_data <<- dipsaus::list_to_fastmap2(list(
        last_time = NULL,
        registered_namespaces = NULL,
        show_time_diff = FALSE,
        use_crayon_ = NA,
        session_id = ""
      ))
    }
  }


  use_crayon <- function(){
    if(is.na(logger_data$use_crayon_)){
      logger_data$use_crayon_ <- as.logical(dipsaus::package_installed("crayon"))
    }
    if(is.null(crayon_ns)){
      crayon_ns <<- asNamespace("crayon")
    }
    logger_data$use_crayon_ && crayon::has_color()
  }

  bold <- function(...){
    if(use_crayon()){
      crayon::bold(...)
    } else {
      paste(...)
    }
  }
  colorize <- function(msg, level){
    if(use_crayon()){
      logger::colorize_by_log_level(msg, level)
    } else {
      paste(msg, collapse = " ")
    }
  }

  format_color <- function(msg, fmt, color = "silver") {
    if(!missing(fmt)){
      msg <- format(msg, fmt)
    }
    if(use_crayon()){
      msg <- crayon_ns[[color]](msg)
    }
    msg
  }

  get_delta <- function(delta){
    if(length(delta) != 1){return(NULL)}
    if(delta < 0.3){
      delta_color <- "silver"
    } else if(delta < 0.5){
      delta_color <- "cyan"
    } else if (delta < 1){
      delta_color <- "green"
    } else if (delta < 5){
      delta_color <- "yellow"
    } else {
      delta_color <- "red"
    }
    format_color(msg = sprintf("(+%.2fs)", delta), color = delta_color)
  }

  rave_logger_layout_generator <- function(time_format = "%Y-%m-%d %H:%M:%S", session_len = 50, abbrev = FALSE){
    ensure_logger_data()
    force(time_format)
    force(session_len)
    force(abbrev)
    function (level, msg, namespace = NA_character_,
              .logcall = sys.call(),
              .topcall = sys.call(-1),
              .topenv = parent.frame()) {

      var <- logger::get_logger_meta_variables(
        log_level = level,
        namespace = namespace,
        .logcall = .logcall,
        .topcall = .topcall,
        .topenv = .topenv
      )

      now <- var$time
      delta <- NULL
      if(logger_data$show_time_diff){
        if(is.null(logger_data$last_time)){
          delta <- 0
        } else {
          delta <- dipsaus::time_delta(logger_data$last_time, now)
        }
        logger_data$last_time <- now
      }

      if(logger_data$session_id != ""){
        session_id <- logger_data$session_id
        nc <- nchar(session_id)
        if(nc > session_len){
          session_id <- substr(session_id, start = nc + 1 - session_len, stop = nc)
        }
        var$pid <- sprintf("%s:%s", var$pid, session_id)
      }
      ns <- trimws(var$ns)
      if(!is.na(ns) && !ns %in% c("", "global", "ravedash", "NA") ){
        if(abbrev) {
          ns <- abbr_module_id(ns)
        }
        ns_delta <- c(format_color(sprintf("[%s:%s]", var$pid, ns)), get_delta(delta))
      } else {
        ns_delta <- c(format_color(sprintf("[%s]", var$pid)), get_delta(delta))
      }

      paste(c(
        bold(colorize(msg = var$level, level = var$levelr)),
        format_color(var$time, time_format),
        ns_delta,
        colorize(msg = paste(msg, collapse = " "), level = var$levelr)
      ), sep = " ", collapse = " ")

    }
  }


  get_levelr <- function(level){
    switch (
      level,
      'info' = logger::INFO,
      "warning" = logger::WARN,
      "error" = logger::ERROR,
      "fatal" = logger::FATAL,
      "debug" = logger::DEBUG,
      {
        logger::TRACE
      }
    )
  }

  logger <- function(..., level = c("info", "warning", "error", "fatal", "debug", "trace"),
           calc_delta = 'auto', .envir = parent.frame(), .sep = "",
           use_glue = FALSE, reset_timer = FALSE){
    level <- match.arg(level)

    ensure_logger_data()

    module <- get_active_module_info()
    if(is.list(module)){
      namespace <- module$id
      rave_id <- paste(module$rave_id, collapse = "")
      if(is.na(rave_id)){
        rave_id <- ""
      }
    } else {
      namespace <- "ravedash"
      rave_id <- ""
    }

    if(reset_timer || !identical(rave_id, logger_data$session_id)){
      logger_data$last_time <- NULL
      logger_data$session_id <- rave_id
    }


    # get file appender
    has_file_appender <- tryCatch({
      call <- logger::log_appender(namespace = namespace, index = 2)
      identical(as.character(call[[1]]), 'appender_ravedash')
    }, error = function(e){ FALSE })

    if(!has_file_appender){
      root_path <- getOption("ravedash.logger.root_path",
                             Sys.getenv('RAVE_LOGGER_PATH', unset = ""))
      if(!is.na(root_path) && root_path != "" && root_path != nullfile() && root_path != "NA"){
        if(root_path == "." || dir.exists(root_path)){
          path <- file.path(root_path, sprintf("%s.log", namespace))

          max_files <- as.integer(getOption(
            "ravedash.logger.max_files",
            Sys.getenv('RAVE_LOGGER_FILE_LIMIT', unset = 3)
          ))

          max_bytes <-
            as.integer(getOption(
              "ravedash.logger.max_bytes",
              Sys.getenv('RAVE_LOGGER_BYTE_LIMIT', unset = 52428800)
            ))

          eval(bquote({
            logger::log_appender(
              appender = appender_ravedash(
                file = .(path), append = TRUE,
                max_bytes = .(max_bytes),
                max_files = .(max_files)
              ),
              namespace = .(namespace), index = 2
            )
          }))
          logger::log_threshold(level = get_levelr(Sys.getenv("RAVE_LOGGER_FILELEVEL", unset = "debug")),
                                namespace = namespace, index = 2)
        }
      }
    }




    if(identical(calc_delta, "auto") && level %in% c("debug", "trace")){
      calc_delta <- TRUE
    }
    logger_data$show_time_diff <- isTRUE(as.logical(calc_delta))

    loglevel <- switch (
      level,
      'info' = logger::INFO,
      "warning" = logger::WARN,
      "error" = logger::ERROR,
      "fatal" = logger::FATAL,
      "debug" = logger::DEBUG,
      {
        logger::TRACE
      }
    )

    registered <- namespace %in% logger_data$registered_namespaces
    if(!registered){
      logger_data$registered_namespaces <- c(logger_data$registered_namespaces, namespace)
      logger::log_formatter(logger::formatter_paste, namespace = namespace)
      logger::log_layout(layout = rave_logger_layout_generator(
        time_format = "%H:%M:%S", session_len = 4, abbrev = TRUE
      ), namespace = namespace, index = 1)
      logger::log_layout(layout = rave_logger_layout_generator(),
                         namespace = namespace, index = 2)
      logger::log_threshold(level = logger::TRACE,
                            namespace = namespace, index = 1)
      logger::log_appender(appender = logger::appender_console, namespace = namespace, index = 1)
      logger::log_appender(appender = logger::appender_file(nullfile()), namespace = namespace, index = 2)
    }

    if(use_glue) {
      msg <- raveio::glue(..., .envir = .envir, .sep = .sep)
    } else {
      msg <- paste(..., collapse = .sep, sep = .sep)
    }

    if(loglevel < logger::ERROR ||
       (
         loglevel == logger::ERROR &&
         raveio::raveio_getopt("traceback_on_error", default = FALSE)
       )){
      if(system.file(package = "rlang") == '') {
        msg <- utils::capture.output({
          traceback()
        })
      } else {
        trace_back <- rlang::trace_back(globalenv())
        msg <- c(msg, format(trace_back, simplify = "none"))
      }
      msg <- paste(msg, collapse = "\n")
    }

    logger::log_level(level = loglevel, namespace = namespace, msg)

    invisible(msg)

  }


  set_root_path <- function(root_path, max_bytes, max_files){
    if(missing(root_path)){
      root_path <- getOption("ravedash.logger.root_path",
                             Sys.getenv('RAVE_LOGGER_PATH', unset = ""))
    }

    options("ravedash.logger.root_path" = NA)
    Sys.unsetenv("RAVE_LOGGER_PATH")
    valid_root <- FALSE

    if(isTRUE(length(root_path) == 1 && root_path != nullfile())){
      if(!(
        is.na(root_path) ||
        trimws(root_path) == "" ||
        root_path == "NA" ||
        root_path == nullfile()
      )){
        dir.create(root_path, recursive = TRUE, showWarnings = FALSE)
        root_path <- normalizePath(root_path, mustWork = TRUE)
        valid_root <- TRUE
        options("ravedash.logger.root_path" = root_path)
        Sys.setenv("RAVE_LOGGER_PATH" = root_path)
      }
    }

    if(missing(max_bytes)){
      max_bytes <-
        as.integer(getOption(
          "ravedash.logger.max_bytes",
          Sys.getenv('RAVE_LOGGER_BYTE_LIMIT', unset = 52428800)
        ))
    } else {
      max_bytes <- as.integer(max_bytes)
      stopifnot(max_bytes >= 10240)
      if(valid_root){
        options("ravedash.logger.max_bytes" = max_bytes)
        Sys.setenv("RAVE_LOGGER_BYTE_LIMIT" = max_bytes)
      }

    }
    if(missing(max_files)){
      max_files <- as.integer(getOption(
        "ravedash.logger.max_files",
        Sys.getenv('RAVE_LOGGER_FILE_LIMIT', unset = 3)
      ))
    } else {
      max_files <- as.integer(max_files)
      stopifnot(max_files >= 1)
      if(valid_root){
        options("ravedash.logger.max_files" = max_files)
        Sys.setenv("RAVE_LOGGER_FILE_LIMIT" = max_files)
      }
    }

    filelevel <- get_levelr(Sys.getenv("RAVE_LOGGER_FILELEVEL", unset = "debug"))

    for(namespace in logger_data$registered_namespaces){
      if(valid_root) {
        path <- file.path(root_path, sprintf("%s.log", namespace))
      } else {
        path <- nullfile()
      }

      expr <- bquote({
        logger::log_appender(
          appender = appender_ravedash(
            file = .(path), append = TRUE,
            max_bytes = .(max_bytes),
            max_files = .(max_files)
          ),
          namespace = .(namespace), index = 2
        )
      })
      logger::log_threshold(level = filelevel,
                            namespace = namespace, index = 2)

      eval(expr)

    }
  }


  list(
    logger = logger,
    set_root_path = set_root_path
  )
})

#' @rdname logger
#' @export
logger <- function(..., level = c("info", "warning", "error", "fatal", "debug", "trace"),
                   calc_delta = 'auto', .envir = parent.frame(), .sep = "",
                   use_glue = FALSE, reset_timer = FALSE) {
  force(.envir)
  level <- match.arg(level)
  if(level == 'fatal'){
    .logger_functions$logger(..., level = level, calc_delta = calc_delta,
                             .envir = .envir, .sep = .sep, use_glue = use_glue,
                             reset_timer = reset_timer)
  } else {
    call <- match.call()
    tryCatch({
      .logger_functions$logger(..., level = level, calc_delta = calc_delta,
                               .envir = .envir, .sep = .sep, use_glue = use_glue,
                               reset_timer = reset_timer)
    }, error = function(e){
      cat("Cannot log the following call: \n")
      print(call)
      cat("\nReason: ", e$message, "\n")
    })
  }

}

#' @rdname logger
#' @export
set_logger_path <- .logger_functions$set_root_path


#' @rdname logger
#' @export
logger_threshold <- function(
  level = c("info", "warning", "error", "fatal", "debug", "trace"),
  module_id, type = c("console", "file", "both")){

  level <- match.arg(level)
  type <- match.arg(type)

  if(missing(module_id) || !length(module_id)){
    module <- get_active_module_info()
    if(is.list(module)){
      namespace <- module$id
    } else {
      namespace <- "ravedash"
    }
  } else {
    namespace <- module_id
  }

  loglevel <- switch (
    level,
    'info' = logger::INFO,
    "warning" = logger::WARN,
    "error" = logger::ERROR,
    "fatal" = logger::FATAL,
    "debug" = logger::DEBUG,
    {
      logger::TRACE
    }
  )

  if(type == "both") {
    logger::log_threshold(level = loglevel, namespace = namespace, index = 1)
    logger::log_threshold(level = loglevel, namespace = namespace, index = 2)
    Sys.setenv("RAVE_LOGGER_FILELEVEL" = level)
  } else if( type == 'file' ){
    logger::log_threshold(level = loglevel, namespace = namespace, index = 2)
    Sys.setenv("RAVE_LOGGER_FILELEVEL" = level)
  } else {
    logger::log_threshold(level = loglevel, namespace = namespace, index = 1)
  }

  set_logger_path()

  invisible()

}


#' @rdname logger
#' @export
logger_error_condition <- function(cond, level = "error"){
  tback <- tryCatch({
    paste(utils::capture.output(traceback(cond)), collapse = "\n")
  }, error = function(e){
    "(cannot obtain traceback info)"
  })
  if(is.null(cond$call)){
    logger("Error: ", cond$message, "\nTraceback:\n", tback, level = level)
  } else {
    logger("Error in ", cond$call, ": ", cond$message, "\nTraceback:\n", tback, level = level)
  }
}

#' @rdname logger
#' @export
error_notification <- function(
    cond, title = "Error found!", type = "danger",
    class = "error_notif", delay = 30000, autohide = TRUE,
    session = shiny::getDefaultReactiveDomain()
) {
  if(!inherits(cond, "condition")) {
    cond <- simpleError(message = cond$message)
  }
  logger_error_condition(cond = cond)
  if(!is.null(session)) {
    shidashi::show_notification(
      session = session,
      message = cond$message,
      title = title,
      type = type,
      close = TRUE,
      autohide = autohide, delay = delay,
      class = session$ns("error_notif"),
      collapse = "\n"
    )
  }
}

#' @rdname logger
#' @export
with_error_notification <- function(
  expr, envir = parent.frame(), quoted = FALSE, ...
) {
  if(!quoted) {
    expr <- substitute(expr)
  }

  args <- list(...)

  tryCatch({
    force(envir)
    eval(expr, envir = envir)
  }, error = function(e) {
    args$cond <- e
    do.call(error_notification, args)
    e
  })
}
