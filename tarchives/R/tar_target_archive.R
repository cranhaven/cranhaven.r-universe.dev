#' Declare a target to read an archive.
#'
#' @param package A scalar character of the package name.
#' @param pipeline A scalar character of the pipeline name.
#' @param name_archive A scalar character of a name of archived target.
#' If `NULL`, the name of the target is used. By default, `NULL`.
#' @param ... Arguments to pass to [targets::tar_outdated], [targets::tar_make] or [tar_read_archive_raw].
#' @inheritParams targets::tar_target
#'
#' @inherit targets::tar_target return
#'
#' @export
tar_target_archive <- function(
  name,
  package,
  pipeline,
  name_archive = NULL,
  ...,
  pattern = NULL,
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  deps = NULL,
  string = NULL,
  format = targets::tar_option_get("format"),
  repository = targets::tar_option_get("repository"),
  iteration = targets::tar_option_get("iteration"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = isTRUE(targets::tar_option_get("garbage_collection")),
  deployment = targets::tar_option_get("deployment"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue"),
  description = targets::tar_option_get("description")
) {
  name <- targets::tar_deparse_language(substitute(name))
  name_archive <- targets::tar_deparse_language(substitute(name_archive)) %||%
    name
  tar_target_archive_raw(
    name = name,
    package = package,
    pipeline = pipeline,
    name_archive = name_archive,
    ...,
    pattern = pattern,
    packages = packages,
    library = library,
    deps = deps,
    string = string,
    format = format,
    repository = repository,
    iteration = iteration,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue,
    description = description
  )
}

#' @rdname tar_target_archive
#'
#' @export
tar_target_archive_raw <- function(
  name,
  package,
  pipeline,
  name_archive = name,
  ...,
  pattern = NULL,
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  deps = NULL,
  string = NULL,
  format = targets::tar_option_get("format"),
  repository = targets::tar_option_get("repository"),
  iteration = targets::tar_option_get("iteration"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  garbage_collection = isTRUE(targets::tar_option_get("garbage_collection")),
  deployment = targets::tar_option_get("deployment"),
  priority = targets::tar_option_get("priority"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue"),
  description = targets::tar_option_get("description")
) {
  args <- rlang::list2(...)

  tar_outdated_archive <- tar_archive(
    targets::tar_outdated,
    package = package,
    pipeline = pipeline
  )
  outdated <- rlang::exec(
    tar_outdated_archive,
    names = name_archive,
    !!!args[names(args) %in% rlang::fn_fmls_names(targets::tar_outdated)],
  )
  if (name %in% outdated) {
    cue$mode <- "always"

    rlang::exec(
      tarchives::tar_make_archive,
      package = package,
      pipeline = pipeline,
      names = name_archive,
      !!!args[names(args) %in% rlang::fn_fmls_names(targets::tar_make)]
    )
  }

  command <- rlang::call2(
    "tar_read_archive_raw",
    name = name_archive,
    package = package,
    pipeline = pipeline,
    !!!args[names(args) %in% rlang::fn_fmls_names(tar_read_archive_raw)],
    .ns = "tarchives"
  )
  targets::tar_target_raw(
    name = name,
    command = command,
    pattern = pattern,
    packages = packages,
    library = library,
    deps = deps,
    string = string,
    format = format,
    repository = repository,
    iteration = iteration,
    error = error,
    memory = memory,
    garbage_collection = garbage_collection,
    deployment = deployment,
    priority = priority,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue,
    description = description
  )
}
