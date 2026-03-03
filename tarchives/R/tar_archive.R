with_dir_archive <- function(
  package,
  pipeline,
  envir,
  code
) {
  withr::with_dir(
    rlang::eval_bare(
      rlang::call2(
        "system.file",
        "tarchives",
        pipeline,
        package = package,
        mustWork = TRUE
      ),
      env = envir
    ),
    code
  )
}

#' Path to the archived target script file
#'
#' @param package A scalar character of the package name.
#' @param pipeline A scalar character of the pipeline name.
#' @inheritParams targets::tar_make
#'
#' @return A scalar character of the path to the archived target script file.
#'
#' @export
tar_archive_script <- function(
  package,
  pipeline,
  envir = parent.frame(),
  script = targets::tar_config_get("script")
) {
  with_dir_archive(
    package = package,
    pipeline = pipeline,
    envir = envir,
    fs::path_wd(script)
  )
}

#' Path to the archived target store directory
#'
#' @param package A scalar character of the package name.
#' @param pipeline A scalar character of the pipeline name.
#' @inheritParams targets::tar_make
#'
#' @return A scalar character of the path to the archived target store
#' directory.
#'
#' @export
tar_archive_store <- function(
  package,
  pipeline,
  store = targets::tar_config_get("store")
) {
  fs::path(
    tools::R_user_dir(
      "tarchives",
      which = "cache"
    ),
    package,
    pipeline,
    store
  )
}

#' Function factory for archived targets
#'
#' @param f A function of targets package.
#' @param package A scalar character of the package name.
#' @param pipeline A scalar character of the pipeline name.
#' @inheritParams targets::tar_make
#'
#' @return A function.
#'
#' @export
tar_archive <- function(
  f,
  package,
  pipeline,
  envir = parent.frame(),
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store")
) {
  fmls_names <- rlang::fn_fmls_names(f)

  args <- list()
  if ("envir" %in% fmls_names) {
    args$envir <- envir
  }
  if ("script" %in% fmls_names) {
    args$script <- tar_archive_script(
      package = package,
      pipeline = pipeline,
      script = script
    )
  }
  if ("store" %in% fmls_names) {
    args$store <- tar_archive_store(
      package = package,
      pipeline = pipeline,
      store = store
    )
  }
  function(...) {
    with_dir_archive(
      package = package,
      pipeline = pipeline,
      envir = envir,
      rlang::eval_tidy(
        rlang::call2(
          "f",
          !!!args,
          !!!rlang::enexprs(...)
        ),
        data = list(f = f),
        env = envir
      )
    )
  }
}
