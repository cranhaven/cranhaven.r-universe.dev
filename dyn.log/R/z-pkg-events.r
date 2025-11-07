#' @title Active Settings
#'
#' @description
#' Package environment variable to hold
#' global level settings.
active <- new.env(parent = emptyenv())

#' @title Load Handler
#'
#' Package initialization routine.
#'
#' @param libname library name
#' @param pkgname package name
.onLoad <- function(libname, pkgname) { # nolint
  # nocov start
  assign("namespace", pkgname, envir = topenv())

  configs <- get_configurations(pkgname = pkgname)
  assign("configs", configs, envir = topenv())

  invisible()
  # nocov end
}

#' @title Attach Handler
#'
#' Package initialization routine.
#'
#' @param libname library name
#' @param pkgname package name
.onAttach <- function(libname, pkgname) { # nolint

  # nocov start
  config_specification()

  # Set a hook to reinitialize the logger
  # in the event the environment got wiped out.
  setHook(
    packageEvent(pkgname, "attach"),
    function(...) {
      config_specification()
    }
  )

  invisible()
  # nocov end
}

#' @title Detach Handler
#'
#' Package initialization routine.
#'
#' @param libpath libpath
.onDetach <- function(libpath) { # nolint

  # nocov start
  wipe_logger()

  # Set a hook to reinitialize the logger
  # in the event the environment got wiped out.
  setHook(
    packageEvent(libpath, "detach"),
    function(...) {
      wipe_logger()
    }
  )

  invisible()
  # nocov end
}
