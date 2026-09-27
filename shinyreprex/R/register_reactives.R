#' Register Reactives for Reproduction
#'
#' @description
#' Record reactives against the current Shiny session, so that
#' [reprex_packages()] and [reprex_lockfile()] can be called with no arguments
#' and still cover the whole application.
#'
#' This avoids having to return reactives out of every module purely so a single
#' top-level call can see them. Each module registers what it owns, and the
#' session holds the collection.
#'
#' ## Namespacing
#' Registrations are namespaced by the calling module, so two modules may
#' register reactives of the same name without collision, and re-registering the
#' same name in the same module replaces the previous entry rather than adding a
#' duplicate.
#'
#' ## When to Register
#' Registering does not evaluate the reactive. Packages are resolved by reading
#' the expression held in the reactive, so it may be registered while still
#' gated behind [shiny::req()] or inputs that have yet to be set. Registering at
#' module setup is therefore both safe and preferred, as it does not depend on
#' the user having visited the output first.
#'
#' ## Session Scope
#' The collection lives on the session, so it is discarded when the session ends
#' and is never shared between concurrent users of the same application.
#'
#' @param ... One or more `shiny::reactive` objects to register. Names are taken
#' from the arguments, so `register_reactives(summary_tbl)` registers under
#' `"summary_tbl"`. Supply names explicitly to override.
#' @param session The Shiny session to register against. Defaults to the current
#' reactive domain, which inside a module is that module's session.
#'
#' @returns
#' The registered reactives, invisibly, as a named list.
#'
#' @examplesIf interactive()
#' library(shiny)
#'
#' summaryServer <- function(id) {
#'   moduleServer(id, function(input, output, session) {
#'     summary_tbl <- reactive(purrr::keep(iris, is.numeric))
#'
#'     register_reactives(summary_tbl)
#'
#'     summary_tbl
#'   })
#' }
#'
#' # Elsewhere in the application, with no reactives threaded through:
#' # reprex_lockfile()
#'
#' @seealso [reprex_packages()] and [reprex_lockfile()], which read the
#' registered collection when called with no reactives.
#'
#' @export
register_reactives <- function(..., session = shiny::getDefaultReactiveDomain()) {
  # Names must be captured before the dots are forced; once `list(...)` has run,
  # rlang labels the reactive values rather than the argument expressions.
  labels <- names(rlang::enexprs(..., .named = TRUE))
  reactives <- stats::setNames(list(...), labels)

  if (is.null(session)) {
    cli::cli_abort("`register_reactives` must be called from within a Shiny session")
  } else if (length(reactives) == 0L) {
    cli::cli_abort("At least one reactive object must be supplied to `register_reactives`")
  }

  assert_reactives(reactives, "register_reactives")


  store <- reprex_store(session)
  for (nm in names(reactives)) {
    store$reactives[[session$ns(nm)]] <- reactives[[nm]]
  }

  invisible(reactives)
}

#' Session Reactive Store
#'
#' @description
#' Fetch (creating on first use) the environment holding this session's
#' registered reactives. An environment is used so that registrations from
#' module servers mutate a single shared collection.
#'
#' @param session A Shiny session object
#'
#' @returns
#' An environment with a `reactives` element, a named list of reactives.
#'
#' @keywords internal
reprex_store <- function(session) {
  user_data <- session$userData

  if (is.null(user_data$.shinyreprex)) {
    store <- new.env(parent = emptyenv())
    store$reactives <- list()
    user_data$.shinyreprex <- store
  }

  user_data$.shinyreprex
}

#' Registered Reactives
#'
#' @description
#' The reactives registered against a session by [register_reactives()].
#'
#' @param session A Shiny session object, or `NULL` when called outside Shiny
#'
#' @returns
#' A named list of reactives, empty if none have been registered.
#'
#' @keywords internal
registered_reactives <- function(session) {
  if (is.null(session)) {
    return(list())
  }

  reprex_store(session)$reactives
}

#' Reactive Object Check
#'
#' @description
#' Confirm every object supplied is an unevaluated reactive, raising an error
#' that names the calling function so the message points at the user's call.
#'
#' @param reactives A list of objects to check
#' @param fn_name Name of the calling function, used in the error message
#'
#' @returns
#' `NULL`, invisibly. Called for its side effect of raising an error.
#'
#' @keywords internal
assert_reactives <- function(reactives, fn_name) {
  is_reactive <- vapply(reactives, inherits, logical(1L), what = "reactiveExpr")

  if (!all(is_reactive)) {
    cli::cli_abort("All objects passed to {.var {fn_name}} must be unevaluated reactive objects")
  }

  invisible(NULL)
}
