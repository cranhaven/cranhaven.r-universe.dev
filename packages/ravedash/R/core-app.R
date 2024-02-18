session_root <- function(ensure = FALSE){
  cache_path <- raveio::raveio_getopt("tensor_temp_path", default = file.path(tempdir(), "rave2-session"))
  if( ensure && !dir.exists(cache_path) ){
    raveio::dir_create2(cache_path)
  }
  normalizePath(cache_path, mustWork = FALSE)
}

ensure_template <- function(path, use_cache = TRUE){


  template_path <- file.path(R_user_dir("raveio", 'data'), 'rave-pipelines')

  if(!use_cache || !dir.exists(template_path)) {
    raveio::pipeline_install_github('dipterix/rave-pipelines', to = "default")
  }

  fs <- list.files(template_path, full.names = TRUE, recursive = FALSE)
  raveio::dir_create2(path)
  for(f in fs){
    file.copy(f, to = path, overwrite = TRUE, copy.date = TRUE, recursive = TRUE)
  }
  # remove <path>/modules
  module_path <- file.path(path, "modules")
  if(dir.exists(module_path)) {
    unlink(module_path, recursive = TRUE)
    raveio::dir_create2(module_path)
  }
  module_yaml_path <- file.path(path, "modules.yaml")
  if(file.exists(module_yaml_path)) {
    unlink(module_yaml_path)
  }
  normalizePath(path)
}

#' @name rave-session
#' @title Create, register, list, and remove 'RAVE' sessions
#' @param update logical, whether to update to latest 'RAVE' template
#' @param session,x session identification string, or session object; use
#' \code{list_session} to list all existing sessions
#' @param path root path to store the sessions; default is the
#' \code{"tensor_temp_path"} in \code{\link[raveio]{raveio_getopt}}
#' @param host host 'IP' address, default is 'localhost'
#' @param port port to listen
#' @param options additional options, including \code{jupyter},
#' \code{jupyter_port}, \code{as_job}, and \code{launch_browser}
#' @param jupyter logical, whether to launch 'jupyter' instance as well. It
#' requires additional setups to enable 'jupyter' lab
#' @param jupyter_port port used by 'jupyter' lab, can be set by
#' \code{'jupyter_port'} option in \code{\link[raveio]{raveio_setopt}}
#' @param as_job whether to launch the application as 'RStudio' job, default is
#' true if 'RStudio' is detected; when running without 'RStudio', this option
#' is always false
#' @param launch_browser whether to launch browser, default is true
#' @param single_session whether to enable single-session mode. Under this
#' mode, closing the main frame will terminate 'RAVE' run-time session,
#' otherwise the 'RAVE' instance will still open in the background
#' @param new whether to create a new session instead of using the most recent
#' one, default is false
#' @param order whether to order the session by date created; choices are
#' \code{'none'} (default), \code{'ascend'}, \code{'descend'}
#' @param keys vector of characters, one or more keys of which the values
#' should be obtained
#' @param default default value if key is missing
#' @param namespace namespace of the option; default is \code{'default'}
#' @param ...,.list named list of key-value pairs of session options. The
#' keys must be characters, and values must be simple data types (such as
#' numeric vectors, characters)
#' @param returnValue passed to \code{\link[shiny]{stopApp}}
#' @return
#' \describe{
#' \item{\code{new_session}}{returns a session object with character
#' \code{'session_id'} and a function \code{'launch_session'} to launch the
#' application from this session}
#' \item{\code{use_session}}{returns a session object, the same as
#' \code{new_session} under the condition that corresponding session exists,
#' or raise an error if the session is missing}
#' \item{\code{list_session}}{returns a list of all existing session objects
#' under the session root}
#' \item{\code{remove_session}}{returns a logical whether the corresponding
#' session has been found and removed}
#' }
#'
#' @examples
#'
#' if(interactive()){
#'
#'   sess <- new_session()
#'   sess$launch_session()
#'
#'   all_sessions <- list_session()
#'   print(all_sessions)
#'
#'   # Use existing session
#'   session_id <- all_sessions[[1]]$session_id
#'   sess <- use_session(session_id)
#'   sess$launch_session()
#'
#'   # Remove session
#'   remove_session(session_id)
#'   list_session()
#' }
#'
#' @export
new_session <- function(update = FALSE) {

  # o <- raveio::pipeline_root()
  # on.exit({
  #   raveio::pipeline_root(o)
  # })

  src_root <- R_user_dir("raveio", 'data')
  src_pipeline <- file.path(R_user_dir("raveio", 'data'), "pipelines")

  if(!dir.exists(src_pipeline)){
    stop("No pipeline found. TODO (dev: fixe this)")
  }
  pipelines <- raveio::pipeline_list(root_path = src_pipeline)
  if(!length(pipelines)){
    stop("No pipeline found. TODO (dev: fixe this)")
  }

  # create a temporary repository
  session_id <- paste0(strftime(Sys.time(), "session-%y%m%d-%H%M%S-%Z-"), toupper(rand_string(4)))
  cache_path <- session_root(ensure = TRUE)
  cache_path <- normalizePath(cache_path)
  app_path <- file.path(cache_path, session_id)

  # raveio::dir_create2(app_path)
  ensure_template(app_path, use_cache = !update)

  pipeline_path <- file.path(app_path, "_pipelines")

  for(pipeline in pipelines){
    p <- raveio::pipeline_find(pipeline)
    raveio::pipeline_fork(
      src = p, dest = file.path(pipeline_path, pipeline)
    )
  }

  # copy modules
  module_root_path <- file.path(R_user_dir("raveio", 'data'), "shidashi_modules")
  source_path <- file.path(module_root_path, "modules")
  target_path <- file.path(app_path, "modules")
  if(file.exists(target_path)){
    unlink(target_path, recursive = TRUE)
  }
  file.copy(
    from = source_path, to = app_path,
    recursive = TRUE, copy.date = TRUE
  )

  module_conf <- raveio::load_yaml(file.path(module_root_path, "modules.yaml"))
  groups <- lapply(module_conf$modules, function(item){
    if(length(item$group) == 1) {
      order <- c(item$order, 99999L)
      order <- min(order, na.rm = TRUE)
      return(data.frame(
        group = item$group,
        order = order
      ))
    }
  })
  groups <- do.call("rbind", dipsaus::drop_nulls(groups))
  groups <- lapply(split(groups, groups$group), function(item){
    list(
      name = item$group[[1]],
      order = min(item$order),
      open = FALSE
    )
  })
  names(groups) <- sapply(groups, '[[', 'name')
  module_conf$groups <- groups

  if(!is.list(module_conf$divider)) {
    module_conf$divider <- list()
  }
  module_conf$divider[["Preprocess"]] <- list(order = 0)
  module_conf$divider[["Built-ins"]] <- list(order = 9.99)
  module_conf$divider[["Add-ons"]] <- list(order = 99.99)

  raveio::save_yaml(module_conf, file = file.path(app_path, "modules.yaml"))
  # file.copy(
  #   from = file.path(module_root_path, "modules.yaml"),
  #   to = file.path(app_path, "modules.yaml"),
  #   overwrite = TRUE, copy.date = TRUE
  # )

  logger("A new RAVE session has been created [session ID: { session_id }]", level = "info", use_glue = TRUE)
  use_session(session_id)

}

#' @rdname rave-session
#' @export
use_session <- function(x) {
  UseMethod("use_session")
}

#' @export
use_session.default <- function(x) {
  force(x)

  if(length(x) != 1 || is.na(x) || !grepl("^session-[0-9]{6}-[0-9]{6}-[a-zA-Z]+-[A-Z0-9]{4}$", x)) {
    stop("Invalid session ID")
  }

  cache_path <- session_root()
  app_path <- file.path(cache_path, x)
  app_path <- normalizePath(app_path, mustWork = TRUE)

  x <- structure(
    list(
      app_path = app_path,
      session_id = x
    ),
    class = "rave-dash-session"
  )
  x$launch_session <- function(...) {
    launch_session(x, ...)
  }
  x
}

#' @rdname rave-session
#' @export
launch_session <- function(
    x, host = "127.0.0.1", port = NULL, options = list(
      jupyter = TRUE,
      jupyter_port = NULL,
      as_job = TRUE,
      launch_browser = TRUE,
      single_session = FALSE
    )) {
  sess <- use_session(x)

  default_options <- list(
    jupyter = TRUE,
    jupyter_port = NULL,
    as_job = TRUE,
    launch_browser = TRUE,
    single_session = FALSE
  )

  for(nm in names(options)) {
    default_options[[nm]] <- options[[nm]]
  }

  options <- default_options

  jupyter_port <- options$jupyter_port

  if(isTRUE(options$jupyter)) {
    if(length(jupyter_port) == 0) {
      jupyter_port <- raveio::raveio_getopt("jupyter_port", default = 17284L)
    } else {
      jupyter_port <- as.integer(jupyter_port)
      if(!(length(jupyter_port) == 1 && !is.na(jupyter_port) &&
           jupyter_port >= 1024 && jupyter_port <= 65535)) {
        stop("`launch_session`: options$jupyter_port must be an integer from 1024-65535.")
      }
    }
    jupyter_wd <- raveio::raveio_getopt('data_dir')
    rpymat::jupyter_check_launch(
      open_browser = FALSE, workdir = jupyter_wd, port = jupyter_port,
      host = host, async = TRUE)

    raveio::save_yaml(
      list(
        host = host,
        port = jupyter_port
      ),
      file = file.path(x$app_path, "jupyter.yaml")
    )
  }
  port <- as.integer(port)
  if(length(port)) {
    if(!(length(port) == 1 && !is.na(port) && port >= 1024 && port <= 65535)) {
      stop("`launch_session`: port must be an integer from 1024-65535.")
    }
  } else {
    port <- NULL
  }

  # Set up configuration file
  profile_path <- file.path(x$app_path, "config.R")
  s <- NULL
  if(file.exists(profile_path)) {
    # make sure of no syntax error
    tryCatch({
      s0 <- readLines(profile_path)
      parse(text = s0)
      s <- trimws(s0)
    }, error = function(e) {
      unlink(profile_path)
      logger("Detected syntax error in `config.R`... Reset succeed.")
    })
  }
  anchors <- c(
    "## <<<< Start: RAVE Global Settings",
    "## >>>> End: RAVE Global Settings"
  )
  start_idx <- which(s %in% anchors[[1]])
  end_idx <- which(s %in% anchors[[2]])

  need_insert <- TRUE
  if(length(start_idx) && length(end_idx)) {
    start_idx <- start_idx[[1]]
    end_idx <- end_idx[[1]]
    if(start_idx < end_idx) {
      need_insert <- FALSE
    }
  }
  if( need_insert ) {
    s <- c(
      s, anchors[[1]],
      deparse1(bquote(options(
        ravedash.logger.max_bytes = 52428800,
        ravedash.logger.max_files = 10
      ))),
      anchors[[2]]
    )
    writeLines(s, con = profile_path)
  }



  shidashi::render(
    root_path = x$app_path,
    port = port,
    host = host,
    launch_browser = options$launch_browser,
    as_job = options$as_job,
    test_mode = isTRUE(options$test_mode),
    prelaunch_quoted = TRUE,
    prelaunch = bquote({
      local({
        if(file.exists(.(profile_path))) {
          source(.(profile_path), local = TRUE)
        }
        Sys.setenv("RAVEDASH_SESSION_ID" = .(x$session_id))
        options("ravedash.single.session" = .(!isFALSE(options$single_session)))
        options("shiny.useragg" = FALSE)
        sess_info <- utils::capture.output({ print(utils::sessionInfo()) })
        ravedash <- asNamespace("ravedash")
        ravedash$set_logger_path(root_path = .(file.path(x$app_path, "logs")))
        ravedash$logger_threshold(level = "trace", type = "file")
        ravedash$logger(sprintf("Session path: %s", ravedash$current_session_path(.(x$app_path))))
        ravedash$logger(c(
          "Current session information: ", sess_info, ""
        ), .sep = "\n")
      })
    })
  )

}

current_session_path <- local({
  ravedash_path0 <- NULL
  function(v) {
    if(!missing(v)) {
      ravedash_path0 <<- normalizePath(v, winslash = "/")
    }
    ravedash_path0
  }
})

#' @title Create a random temporary file path for current session
#' @param persist persist level, choices are \code{'app-session'},
#' \code{'package-cache'}, and \code{'process'}; see 'Details'.
#' 'RAVE' application session, default), \code{'package-cache'} (package-level
#' cache directory)
#' @param check whether to create the temporary directory
#' @param pattern,fileext see \code{\link{tempfile}}
#' @returns A file or a directory path to persist temporary data cache
#' @details R default \code{\link{tempdir}} usually gets removed once the R
#' process ends. This behavior might not meet all the needs for 'RAVE' modules.
#' For example, some data are 'RAVE' session-based, like current or last visited
#' subject, project, or state data (like bookmarks, configurations). This
#' session-based information will be useful when launching the same 'RAVE'
#' instance next time, hence should not be removed when users close R.
#' Other data, such as subject-related, or package-related should last even
#' longer. These types of data may be cache of subject power,
#' package-generated color schemes, often irrelevant from R or 'RAVE' sessions,
#' and can be shared across different 'RAVE' instances.
#'
#' The default scheme is \code{persist='process'}. Under this mode, this
#' function behaves the same as \code{\link{tempfile}}. To store data in 'RAVE'
#' session-based manner, please use \code{persist='app-session'}.
#' The actual path will be inside of 'RAVE' session folder, hence this option
#' is valid only if 'RAVE' instance is running. When
#' 'RAVE' instance is not running, the result falls back to
#' \code{persist='process'}. When \code{persist='process'},
#' To cache larger and session-irrelevant data, use \code{'package-cache'}.
#'
#' The 'RAVE' session and package cache are not cleared even when R process
#' ends. Users need to clean the data by themselves. See
#' \code{\link{remove_session}} or \code{\link{remove_all_sessions}} about
#' removing session-based folders, or
#' \code{\link[raveio]{clear_cached_files}} to remove package-based cache.
#'
#' @examples
#'
#' temp_dir()
#' temp_dir(persist = "package-cache")
#'
#' @export
temp_file <- function(
    pattern = "file",
    fileext = "",
    persist = c("process", "app-session", "package-cache")) {
  persist <- match.arg(persist)
  tempfile(tmpdir = temp_dir(persist = persist, check = TRUE),
           pattern = pattern, fileext = fileext)
}

#' @rdname temp_file
#' @export
temp_dir <- function(
    check = FALSE,
    persist = c("process", "app-session", "package-cache")) {
  persist <- match.arg(persist)
  if(persist == "app-session") {
    root <- current_session_path()
    if(!length(root) || is.na(root) || !dir.exists(root)) {
      session_id <- Sys.getenv("RAVEDASH_SESSION_ID", unset = "")
      if(isTRUE(is.character(session_id)) && nchar(session_id) > 0) {
        try({
          sess <- use_session(session_id)
          root <- sess$app_path
        }, silent = TRUE)
      }
      if(!length(root) || is.na(root) || !dir.exists(root)) {
        persist <- "process"
      }
    } else {
      root <- file.path(root, "tmp")
    }
  }
  if(persist == "package-cache") {
    root <- file.path(raveio::cache_root(), "app_tmp")
  }
  if(persist == "process") {
    root <- tempdir()
  }
  if(check && !dir.exists(root)) {
    dir.create(root, showWarnings = FALSE, recursive = TRUE)
  }
  root
}

session_config_path <- function(namespace = "default") {
  namespace <- as.character(namespace)
  if(length(namespace) != 1 || is.na(namespace) || namespace %in% c("/", ".", "..", "")) {
    namespace <- "default"
  }
  fname <- sprintf("ravedash-session-config-%s.yaml", namespace)
  root <- current_session_path()
  if(length(root) == 1 && !is.na(root) && dir.exists(root)) {
    return(file.path(temp_dir(persist = "app-session"), fname))
  }
  session_id <- Sys.getenv("RAVEDASH_SESSION_ID", unset = "")
  if(isTRUE(is.character(session_id)) && nchar(session_id) > 0) {
    try({
      sess <- use_session(session_id)
      return(file.path(sess$app_path, "tmp", fname))
    }, silent = TRUE)
  }
  return(file.path(temp_dir(persist = "app-session"), fname))
}

#' @rdname rave-session
#' @export
session_getopt <- function(keys, default = NA, namespace = "default") {

  conf_path <- session_config_path(namespace = namespace)

  map <- dipsaus::fastmap2()
  if(file.exists(conf_path)) {
    try(silent = TRUE, expr = {
      raveio::load_yaml(conf_path, map = map)
    })
  }

  if(missing(keys)) {
    return(map)
  }
  if(length(keys) <= 1) {
    return(map$`@get`(keys, missing = default))
  }
  return(map$`@mget`(keys, missing = default))
}

#' @rdname rave-session
#' @export
session_setopt <- function(..., .list = NULL, namespace = "default") {

  conf_path <- session_config_path(namespace = namespace)

  new_map <- c(list(...), .list)

  nms <- names(new_map)

  if(!length(nms)) {
    return(invisible(FALSE))
  }
  nms <- nms[!nms %in% ""]
  if(!length(nms)) {
    return(invisible(FALSE))
  }

  map <- session_getopt(namespace = namespace)
  for(k in nms) {
    map[[k]] <- new_map[[k]]
  }

  tfile <- tempfile()
  on.exit({
    unlink(tfile, force = TRUE)
  })
  raveio::dir_create2(dirname(tfile))
  raveio::save_yaml(map, file = tfile)

  raveio::dir_create2(dirname(conf_path))

  file.copy(from = tfile, to = conf_path, overwrite = TRUE, recursive = FALSE)

  return(invisible(TRUE))
}

#' @export
`use_session.rave-dash-session` <- function(x) {
  x
}

#' @export
`print.rave-dash-session` <- function(x, ...){
  vname <- substitute(x)
  session_id <- x$session_id
  session_path <- file.path(session_root(), session_id)
  cat(sprintf("RAVE session <%s>\n", session_id))
  timestamp <- sub("\\-[a-zA-Z0-9]{4}$", "", session_id)
  timestamp <- strsplit(timestamp, "-")[[1]]
  tz <- timestamp[[4]]
  timestamp <- paste(timestamp[c(2,3)], collapse = "-")
  timestamp <- strptime(timestamp, "%y%m%d-%H%M%S")
  timestamp <- strftime(timestamp, usetz = FALSE)
  timestamp <- paste(timestamp, tz)

  if(dir.exists(session_path)){
    cat("  Path:", session_path, "\n")
  } else {
    cat("  Path:", session_path, "(invalid path)\n")
  }
  cat("  Date created:", timestamp, "\n\n")

  vname <- paste(deparse(vname), collapse = "\n")
  cat(sprintf("Please run `%s$launch_session()` to launch the session.\n", vname))

  invisible(x)
}

#' @rdname rave-session
#' @export
remove_session <- function(x){
  UseMethod("remove_session")
}

#' @export
remove_session.default <- function(x){
  if(grepl("^session-[0-9]{6}-[0-9]{6}-[a-zA-Z]+-[A-Z0-9]{4}$", x)){
    session_path <- file.path(session_root(), x)
    if(dir.exists(session_path)){
      unlink(session_path, recursive = TRUE)
      return(invisible(TRUE))
    }
  }
  return(invisible(FALSE))
}

#' @export
`remove_session.rave-dash-session` <- function(x) {
  if(grepl("^session-[0-9]{6}-[0-9]{6}-[a-zA-Z]+-[A-Z0-9]{4}$", x$session_id)){
    session_path <- file.path(session_root(), x$session_id)
    if(dir.exists(session_path)){
      unlink(session_path, recursive = TRUE)
      return(invisible(TRUE))
    }
  }
  return(invisible(FALSE))
}

#' @rdname rave-session
#' @export
remove_all_sessions <- function() {
  sess <- list_session()
  for(s in sess) {
    remove_session(s)
  }
  invisible()
}

#' @rdname rave-session
#' @export
list_session <- function(path = session_root(), order = c("none", "ascend", "descend")){
  order <- match.arg(order)
  dirs <- list.dirs(path = path, full.names = FALSE, recursive = FALSE)
  sel <- grepl("^session-[0-9]{6}-[0-9]{6}-[a-zA-Z]+-[A-Z0-9]{4}$", dirs)
  session_ids <- dirs[sel]
  if(order != "none") {

    timestamps <- sub("\\-[a-zA-Z]+\\-[a-zA-Z0-9]{4}$", "", session_ids)
    timestamps <- strptime(timestamps, "session-%y%m%d-%H%M%S")
    order <- order(timestamps, decreasing = order == "descend", na.last = TRUE)
    session_ids <- session_ids[order]

  }
  re <- lapply(session_ids, use_session)
  re
}

#' @rdname rave-session
#' @export
start_session <- function(session, new = NA, host = "127.0.0.1", port = NULL,
                          jupyter = NA, jupyter_port = NULL, as_job = TRUE,
                          launch_browser = TRUE, single_session = FALSE) {

  if(!missing(session) && length(session)) {
    if(isTRUE(new)) {
      stop("`start_session`: Please leave `session` blank or NULL if you want to create a new session (new=TRUE).")
    }
    if(!inherits(session, "rave-dash-session")) {
      if(length(session) != 1 || is.na(session) ||
         !grepl("^session-[0-9]{6}-[0-9]{6}-[a-zA-Z]+-[A-Z0-9]{4}$", session)) {
        stop("`start_session`: Invalid `session`.")
      }
      session <- tryCatch({
        use_session(x = session)
      }, error = function(e) {
        stop(sprintf("`start_session`: Session [%s] cannot be found.", session))
      })
    }
  } else if(isTRUE(new)) {
    session <- new_session()
  } else {
    # find existing session (most recent)
    all_sessions <- list_session(order = "descend")
    if(!length(all_sessions)) {
      # start a new session
      session <- new_session()
    } else {
      session <- all_sessions[[1]]

      if(is.na(new)) {
        # try to guess
        tryCatch({
          last_updated <- file.path(R_user_dir(package = "ravemanager", which = "config"), "last_updates", "rave-family")
          if(file.exists(last_updated)) {
            last_updated <- readLines(last_updated, n = 1L)
            last_updated <- as.POSIXlt(last_updated)
            session_created <- strptime(substr(session$session_id, start = 9, 21), "%y%m%d-%H%M%S")
            if( last_updated > session_created ) {
              # RAVE dash just got updated
              # start a new session
              session <- new_session()
            }
          }
        }, error = function(e){ NULL })
      }

    }

  }

  rs_available <- dipsaus::rs_avail(child_ok = TRUE, shiny_ok = TRUE)
  if(is.na(jupyter)) {
    jupyter <- rs_available
  } else if(isTRUE(jupyter) && !rs_available) {
    warning("RStudio is not available. Please manually launch Jupyter lab")
    jupyter <- FALSE
  }
  if(!rs_available) {
    as_job <- FALSE
  }

  if(as_job) {
    job_id <- launch_session(x = session, host = host, port = port, options = list(
      jupyter = jupyter,
      jupyter_port = jupyter_port,
      as_job = as_job,
      launch_browser = launch_browser,
      single_session = single_session
    ))
    if(as_job) {
      logger("RAVE application [{session$session_id}] has been launched. Detailed information has been printed out in the `jobs` panel.", level = "info", use_glue = TRUE, .trim = FALSE)
    }

    return(invisible(list(
      session = session,
      job_id = job_id
    )))
  } else {
    return(launch_session(x = session, host = host, port = port, options = list(
      jupyter = jupyter,
      jupyter_port = jupyter_port,
      as_job = as_job,
      launch_browser = launch_browser,
      single_session = single_session
    )))
  }
}

#' @rdname rave-session
#' @export
shutdown_session <- function(
    returnValue = invisible(),
    session = shiny::getDefaultReactiveDomain()
) {
  if(!is.null(session)) {
    session$sendCustomMessage("shidashi.shutdown_session", message = list())
  }
  shiny::stopApp(returnValue = returnValue)
}
