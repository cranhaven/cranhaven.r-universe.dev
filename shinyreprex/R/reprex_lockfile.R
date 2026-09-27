#' Packages Required to Reproduce Reactives
#'
#' @description
#' Extract the set of non-base packages needed to reproduce one or more
#' `shiny::reactive` objects, with duplicates removed across every reactive
#' passed.
#'
#' Use this when building a custom UI around [reprex_lockfile()]: it gives you
#' the list of detected packages to present to the user for selection.
#'
#' Packages are found by reading the expression held in each reactive rather
#' than by generating its script, so every branch of an `if` or `switch`
#' contributes. The result may therefore be a superset of the `library()` calls
#' [reprex_reactive()] emits for the branch actually taken, on the basis that a
#' lockfile is safer holding a package that is not needed than missing one.
#'
#' @param ... One or more `shiny::reactive` objects to inspect. If none are
#' supplied, every reactive registered against `session` by [register_reactives()]
#' is used.
#' @param session The Shiny session to read registered reactives from. Only used
#' when `...` is empty. Defaults to the current reactive domain.
#'
#' @returns
#' A character vector of unique package names. Base packages are excluded, as
#' they require no installation.
#'
#' @seealso [reprex_lockfile()] to turn this set into an `renv` lockfile, and
#' [register_reactives()] to record reactives from within each module.
#'
#' @examplesIf rlang::is_installed("shiny")
#' library(shiny)
#'
#' numeric_iris <- reactive(purrr::keep(iris, is.numeric))
#' styled_code <- reactive(styler::style_text("1 + 1"))
#'
#' # Outside a running application, isolate supplies the reactive context
#' isolate(reprex_packages(numeric_iris, styled_code))
#'
#' @export
reprex_packages <- function(..., session = shiny::getDefaultReactiveDomain()) {
  reactives <- list(...)

  if (length(reactives) == 0L) {
    reactives <- registered_reactives(session)

    if (length(reactives) == 0L) {
      cli::cli_abort(c(
        "No reactive objects supplied or registered.",
        "i" = "Either pass reactives directly, or call {.fn register_reactives} in the modules that own them."
      ))
    }
  } else {
    assert_reactives(reactives, "reprex_packages")
  }

  reactives |>
    purrr::map(reactive_expression) |>
    purrr::map(\(reactive) walk_packages(reactive$body, reactive$env)) |>
    unlist(use.names = FALSE) |>
    unique()
}

#' Create a Lockfile to Reproduce Reactives
#'
#' @description
#' Capture the exact package versions, sources and R version needed to reproduce
#' one or more `shiny::reactive` objects, as an `renv` lockfile. Restoring the
#' lockfile with [renv::restore()] recreates the environment the reactives were
#' generated in, including the recursive dependency tree.
#'
#' A single lockfile covers every reactive passed, so an application can offer
#' one download that reproduces all of its outputs.
#'
#' ## Snapshot Scope
#' The snapshot is taken from the currently loaded library (`.libPaths()`), so
#' the versions recorded are exactly those the running Shiny session used to
#' produce the reactives. `renv` resolves and records the full recursive
#' dependency tree of `packages`, so only the top-level set needs to be supplied.
#'
#' ## Isolation
#' The snapshot runs against a throwaway temporary project, so it never writes
#' `renv` infrastructure into, or otherwise modifies, the application's own
#' directory.
#'
#' ## Output Path
#' A relative `lockfile` is resolved against the working directory, not against
#' the temporary project used for the snapshot. The default therefore writes
#' `renv.lock` into whichever directory the application is running from. Pass an
#' absolute path to control where it lands; inside a
#' [shiny::downloadHandler()] that is the `file` argument.
#'
#' @param ... One or more `shiny::reactive` objects to reproduce. If none are
#' supplied, every reactive registered by [register_reactives()] is used, so a
#' modular application can offer a whole-app lockfile without threading
#' reactives up to the top level.
#' @param packages Character vector of package names to snapshot. If `NULL`
#' (the default), every package detected across `...` is used. Supplying this
#' lets the user narrow the set, or add a package the detector could not find
#' (for example one attached only for an operator or method).
#' @param lockfile Path to write the lockfile to. Defaults to `"renv.lock"`.
#' @param exclude Character vector of package names to omit from the snapshot.
#' @param session The Shiny session to read registered reactives from. Only used
#' when `...` is empty and `packages` is `NULL`.
#'
#' @returns
#' The absolute path the lockfile was written to, invisibly.
#'
#' @seealso [reprex_packages()] to inspect the detected set without writing a
#' lockfile, and [register_reactives()] to record reactives from within each module.
#'
#' @examplesIf interactive()
#' library(shiny)
#'
#' numeric_iris <- reactive(purrr::keep(iris, is.numeric))
#'
#' isolate(reprex_lockfile(numeric_iris, lockfile = tempfile(fileext = ".lock")))
#'
#' @export
reprex_lockfile <- function(...,
                            packages = NULL,
                            lockfile = "renv.lock",
                            exclude = NULL,
                            session = shiny::getDefaultReactiveDomain()) {
  if (is.null(packages)) {
    packages <- reprex_packages(..., session = session)
    detect_term <- "detected" #nolint
  } else {
    detect_term <- "selected" #nolint
  }

  if (length(packages) == 0L) {
    cli::cli_warn("No non-base packages {detect_term}; the lockfile will record only the R version")
  }

  # Snapshot against a throwaway project so renv never writes infrastructure
  # into (or modifies the .Rbuildignore of) the running application's directory.
  scratch_project <- tempfile("shinyreprex-snapshot-")
  dir.create(scratch_project)
  on.exit(unlink(scratch_project, recursive = TRUE), add = TRUE)

  renv::snapshot(
    project = scratch_project,
    packages = packages,
    exclude = exclude,
    lockfile = lockfile,
    library = .libPaths(),
    prompt = FALSE
  )

  invisible(normalizePath(lockfile, winslash = "/", mustWork = FALSE))
}
