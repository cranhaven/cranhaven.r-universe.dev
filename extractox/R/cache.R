#' Save an object to the package cache
#'
#' This function saves an R object to the cache directory of the `extractox` package
#' using the `.rds` file format. If a file with the same name already exists in the cache,
#' it will be overwritten.
#'
#' @param dat Any R object to be saved.
#' @param file_name A character string specifying the name of the file (without extension).
#' @param verbose A logical value indicating whether to print detailed messages. Default is FALSE.
#' @return Invisibly returns the full path of the saved file.
#' @details The cache directory is determined using [tools::R_user_dir()] with the `cache` subdirectory
#' for the `extractox` package. If the directory does not exist, it is created automatically.
#' The function will overwrite any existing file with the same name.
#' @keywords internal
#' @noRd
save_to_cache <- function(dat, file_name, verbose = FALSE) {
  # Sys.getenv("R_USER_CACHE_DIR")

  if (base::missing(dat)) {
    cli::cli_abort("The argument {.field {dat}} is required.")
  }

  if (base::missing(file_name)) {
    cli::cli_abort("The argument {.field {file_name}} is required.")
  }

  cache_dir <- tools::R_user_dir("extractox", which = "cache")
  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)

  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }

  file_path <- file.path(cache_dir, file_name)

  if (all(file.exists(file_path), verbose)) {
    cli::cli_alert_info("Overwriting cache.")
  } else {
    if (isTRUE(verbose)) {
      cli::cli_alert_info("Saving data in the cache {.path {file_path}}.")
    }
  }

  saveRDS(dat, file_path)

  invisible(file_path)
}

#' Read an object from the package cache
#'
#' This function reads an R object from the cache directory of the `extractox`
#' packageusing the `.rds` file format. If the file does not exist, it stops.
#'
#' @param file_name A character string specifying the name of the file
#'   (without extension).
#' @param verbose A logical value indicating whether to print detailed messages.
#'   Default is FALSE.
#' @return The R object read from the cache, or NULL if the file does not exist.
#' @details The cache directory is determined using
#' [tools::R_user_dir()] with the `cache` subdirectory for the `extractox`
#' package. If the file does not exist, a message is printed if verbose is TRUE.
#' @keywords internal
#' @noRd
read_from_cache <- function(file_name, verbose = FALSE) {
  if (base::missing(file_name)) {
    cli::cli_abort("The argument {.field {file_name}} is required.")
  }

  cache_dir <- tools::R_user_dir("extractox", which = "cache")
  cache_dir <- normalizePath(cache_dir, mustWork = FALSE)
  file_path <- file.path(cache_dir, file_name)

  if (file.exists(file_path)) {
    out <- readRDS(file_path)
    if (verbose) {
      cli::cli_alert_success("Successfully load. {.file {file_name}} from cache.")
    }
  } else {
    cli::cli_abort("File not found in cache.")
  }
  out
}

#' Execute Code in a Temporary Directory
#'
#' @description
#' Runs user-defined code inside a temporary directory, setting up a temporary
#' working environment. This function is intended for use in examples and tests
#' and ensures that no data is written to the user's file space.
#' Environment variables such as `HOME`, `APPDATA`, `R_USER_DATA_DIR`,
#' `XDG_DATA_HOME`, `LOCALAPPDATA`, and `USERPROFILE` are redirected to
#' temporary directories. This function was implemented by @luciorq in
#' `condathis` dev.
#'
#' @details
#' This function is not designed for direct use by package users. It is
#' primarily used to create an isolated environment during examples and
#' tests. The temporary directories are created automatically and cleaned
#' up after execution.
#'
#' @param code [expression]
#'   An expression containing the user-defined code to be executed in the
#'   temporary environment.
#'
#' @param .local_envir [environment]
#'  The environment to use for scoping.
#'
#' @return
#' Returns `NULL` invisibly.
#'
#' @examples
#' condathis::with_sandbox_dir(print(fs::path_home()))
#' condathis::with_sandbox_dir(print(tools::R_user_dir("condathis")))
#'
#' @export
with_sandbox_dir <- function(code, .local_envir = base::parent.frame()) {
  tmp_home_path <- withr::local_tempdir(
    pattern = "tmp-home",
    .local_envir = .local_envir
  )
  tmp_data_path <- withr::local_tempdir(
    pattern = "tmp-data",
    .local_envir = .local_envir
  )
  tmp_cache_path <- withr::local_tempdir(
    pattern = "tmp-cache",
    .local_envir = .local_envir
  )

  if (isFALSE(fs::dir_exists(tmp_home_path))) {
    fs::dir_create(tmp_home_path)
  }
  if (isFALSE(fs::dir_exists(tmp_data_path))) {
    fs::dir_create(tmp_data_path)
  }
  if (isFALSE(fs::dir_exists(tmp_cache_path))) {
    fs::dir_create(tmp_cache_path)
  }

  withr::local_envvar(
    .new = list(
      `HOME` = tmp_home_path,
      `USERPROFILE` = tmp_home_path,
      `LOCALAPPDATA` = tmp_data_path,
      `APPDATA` = tmp_data_path,
      `R_USER_DATA_DIR` = tmp_data_path,
      `XDG_DATA_HOME` = tmp_data_path,
      `XDG_CACHE_HOME` = tmp_cache_path,
      `R_USER_CACHE_DIR` = tmp_cache_path
    ),
    .local_envir = .local_envir
  )
  code <- base::substitute(expr = code)
  rlang::eval_bare(expr = code, env = .local_envir)
  return(invisible(NULL))
}
