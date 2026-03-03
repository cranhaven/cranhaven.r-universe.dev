#' Run an archived pipeline of targets.
#'
#' @param package A scalar character of the package name.
#' @param pipeline A scalar character of the pipeline name.
#' @param reporter A scalar character of the reporter type. By default,
#' `"silent"`. See [targets::tar_make()] for more options.
#' @inheritParams targets::tar_make
#'
#' @inherit targets::tar_make return
#'
#' @export
tar_make_archive <- function(
  package,
  pipeline,
  names = NULL,
  shortcut = targets::tar_config_get("shortcut"),
  reporter = "silent",
  seconds_meta_append = targets::tar_config_get("seconds_meta_append"),
  seconds_meta_upload = targets::tar_config_get("seconds_meta_upload"),
  seconds_reporter = targets::tar_config_get("seconds_reporter"),
  seconds_interval = targets::tar_config_get("seconds_interval"),
  callr_function = callr::r,
  callr_arguments = targets::tar_callr_args_default(callr_function, reporter),
  envir = parent.frame(),
  script = targets::tar_config_get("script"),
  store = targets::tar_config_get("store"),
  garbage_collection = NULL,
  use_crew = targets::tar_config_get("use_crew"),
  terminate_controller = TRUE,
  as_job = targets::tar_config_get("as_job")
) {
  script <- tar_archive_script(
    package = package,
    pipeline = pipeline,
    envir = envir,
    script = script
  )
  store <- tar_archive_store(
    package = package,
    pipeline = pipeline,
    store = store
  )
  with_dir_archive(
    package = package,
    pipeline = pipeline,
    envir = envir,
    targets::tar_make(
      names = {{ names }},
      shortcut = shortcut,
      reporter = reporter,
      seconds_meta_append = seconds_meta_append,
      seconds_meta_upload = seconds_meta_upload,
      seconds_reporter = seconds_reporter,
      seconds_interval = seconds_interval,
      callr_function = callr_function,
      callr_arguments = callr_arguments,
      envir = envir,
      script = script,
      store = store,
      garbage_collection = garbage_collection,
      use_crew = use_crew,
      terminate_controller = terminate_controller,
      as_job = as_job
    )
  )
}
