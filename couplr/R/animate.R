# ==============================================================================
# Animation Interface for Assignment Algorithms
# ==============================================================================
# This file defines lap_animate(), which produces an interactive D3-based
# visualisation of how a chosen assignment algorithm transforms a bipartite
# graph step-by-step. The actual algorithm-specific state traces live in
# R/trace_*.R files and are registered into an internal trace registry.
#
# Design:
#   - lap_animate(cost, method = "hungarian") mirrors assignment()'s signature
#   - Method strings match assignment()'s method strings (single source of truth)
#   - Trace functions are registered under their method string in .trace_registry
#   - Adding a new animated algorithm = one register_trace() call
# ==============================================================================

# Internal trace registry. Keys are method strings ("hungarian", "jv", ...);
# values are functions with signature function(cost, maximize, ...) -> trace list.
.trace_registry <- new.env(parent = emptyenv())

#' @keywords internal
register_trace <- function(method, fn) {
  assign(method, fn, envir = .trace_registry)
  invisible(NULL)
}

#' @keywords internal
get_trace_fn <- function(method) {
  if (!exists(method, envir = .trace_registry, inherits = FALSE)) {
    available <- sort(ls(.trace_registry))
    stop(
      "No animation trace registered for method '", method, "'.\n",
      "Available methods: ",
      if (length(available) == 0) "(none yet)" else paste(available, collapse = ", "),
      call. = FALSE
    )
  }
  get(method, envir = .trace_registry)
}

#' List methods that currently support animation
#'
#' Returns the character vector of method strings for which a trace function
#' has been registered. Use any of these with [lap_animate()].
#'
#' @return Character vector of registered method names.
#' @export
animated_methods <- function() {
  sort(ls(.trace_registry))
}

# ------------------------------------------------------------------------------
# Trace shape validation
# ------------------------------------------------------------------------------
# A trace is:
#   list(
#     meta = list(
#       algorithm   = character(1),     # e.g. "hungarian"
#       n_rows      = integer(1),
#       n_cols      = integer(1),
#       cost_matrix = numeric matrix,   # original costs (with NA/Inf preserved)
#       maximize    = logical(1),
#       total_cost  = numeric(1),       # final objective
#       description = character(1)      # short algorithm description
#     ),
#     frames = list(
#       list(
#         step         = integer(1),
#         phase        = character(1),  # e.g. "init", "augment", "scaling"
#         description  = character(1),  # short text for this frame
#         matching     = integer(n),    # row -> col (0 = unmatched)
#         dual_u       = numeric(n) or NULL,
#         dual_v       = numeric(m) or NULL,
#         active_edges = list of c(row, col) currently being explored,
#         path         = list of c(row, col) on current augmenting path
#       ),
#       ...
#     )
#   )

#' @keywords internal
validate_trace <- function(trace) {
  if (!is.list(trace)) stop("Trace must be a list, got ", typeof(trace))
  if (!all(c("meta", "frames") %in% names(trace))) {
    stop("Trace must have 'meta' and 'frames' elements")
  }
  meta <- trace$meta
  required_meta <- c("algorithm", "n_rows", "n_cols", "cost_matrix")
  missing_meta <- setdiff(required_meta, names(meta))
  if (length(missing_meta) > 0) {
    stop("Trace meta missing required fields: ", paste(missing_meta, collapse = ", "))
  }
  if (!is.list(trace$frames) || length(trace$frames) == 0) {
    stop("Trace must contain at least one frame")
  }
  for (i in seq_along(trace$frames)) {
    f <- trace$frames[[i]]
    if (is.null(f$matching)) {
      stop("Frame ", i, " missing required 'matching' field")
    }
  }
  invisible(TRUE)
}

# ------------------------------------------------------------------------------
# Trace serialisation: convert NA/Inf to JSON-safe sentinels
# ------------------------------------------------------------------------------
# JavaScript JSON does not have NaN/Inf; jsonlite encodes them as strings or
# nulls depending on options. We pre-normalise so the JS side has a stable
# contract: forbidden = null in JSON, optimal = number.

#' @keywords internal
normalise_for_json <- function(x) {
  if (is.matrix(x)) {
    x[!is.finite(x)] <- NA_real_
    return(x)
  }
  if (is.numeric(x)) {
    x[!is.finite(x)] <- NA_real_
    return(x)
  }
  x
}

# ==============================================================================
# Public API
# ==============================================================================

#' Animate an assignment algorithm step-by-step
#'
#' Produce an interactive bipartite-graph animation showing how a chosen
#' linear-assignment algorithm transforms the matching over time. The result
#' is an htmlwidget suitable for use in R Markdown, Quarto, pkgdown vignettes,
#' Shiny apps, or standalone HTML output.
#'
#' This is a *teaching* interface. It runs a slower R reference implementation
#' that emits a state trace at every step, then plays it back in the browser.
#' For production solving, use [assignment()] or [lap_solve()] which call the
#' fast C++ backends.
#'
#' @param cost Numeric cost matrix. Rows = workers/sources, columns = jobs/targets.
#'   `NA` or `Inf` entries mark forbidden assignments.
#' @param method Character; the algorithm to animate. Must match one of the
#'   methods registered for animation (see `animated_methods()` for the
#'   current list). Method names are the same strings used by [assignment()],
#'   e.g. `"hungarian"`, `"jv"`, `"auction"`, `"gabow_tarjan"`.
#' @param maximize Logical; if `TRUE`, animate the maximization variant.
#' @param width,height Optional explicit widget dimensions (pixels or CSS units).
#' @param elementId Optional DOM id for the widget container.
#' @param ... Algorithm-specific extra arguments forwarded to the trace
#'   function (e.g. `auction_eps`).
#'
#' @return An `htmlwidget` object.
#'
#' @section Animated methods:
#' Animation support is added incrementally. Call `animated_methods()` to see
#' which method strings currently have a registered trace.
#'
#' @seealso [assignment()] for production solving, [lap_solve()] for the tidy
#'   interface.
#'
#' @examples
#' \dontrun{
#' cost <- matrix(c(4, 2, 5,
#'                  3, 3, 6,
#'                  7, 5, 4), nrow = 3, byrow = TRUE)
#' lap_animate(cost, method = "hungarian")
#' }
#'
#' @export
lap_animate <- function(cost,
                        method = "hungarian",
                        maximize = FALSE,
                        width = NULL,
                        height = NULL,
                        elementId = NULL,
                        ...) {
  if (!requireNamespace("htmlwidgets", quietly = TRUE)) {
    stop("lap_animate() requires the 'htmlwidgets' package. ",
         "Install it with install.packages(\"htmlwidgets\").", call. = FALSE)
  }

  cost <- as.matrix(cost)
  n <- nrow(cost)
  m <- ncol(cost)

  if (n == 0 || m == 0) {
    stop("Cost matrix must have at least one row and one column.", call. = FALSE)
  }
  if (!is.numeric(cost)) {
    stop("`cost` must be a numeric matrix, got ", typeof(cost), call. = FALSE)
  }
  if (any(is.nan(cost))) {
    stop("NaN not allowed in `cost`", call. = FALSE)
  }

  trace_fn <- get_trace_fn(method)
  trace <- trace_fn(cost, maximize = maximize, ...)
  validate_trace(trace)

  trace$meta$cost_matrix <- normalise_for_json(trace$meta$cost_matrix)
  trace$frames <- lapply(trace$frames, function(f) {
    if (!is.null(f$dual_u)) f$dual_u <- normalise_for_json(f$dual_u)
    if (!is.null(f$dual_v)) f$dual_v <- normalise_for_json(f$dual_v)
    f
  })

  htmlwidgets::createWidget(
    name = "lap_animate",
    x = trace,
    width = width,
    height = height,
    package = "couplr",
    elementId = elementId,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = "100%",
      defaultHeight = 500,
      knitr.figure = FALSE,
      browser.fill = TRUE,
      viewer.fill = TRUE
    )
  )
}

