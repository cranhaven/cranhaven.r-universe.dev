#' @keywords internal
.jlme <- new.env(parent = emptyenv())
is_setup <- function() {
  # nocov start
  if (length(.jlme) == 0 && exists(".__DEVTOOLS__", asNamespace("jlme"))) {
    # If setup already ran, flag it as set up
    if (!is.null(get("pkgLocal", asNamespace("JuliaConnectoR"))$con)) {
      message("[dev] Using prior setup: `is_setup()` is TRUE in `load_all()`")
      return(TRUE)
    }
  }
  # nocov end
  isTRUE(.jlme$is_setup)
}
ensure_setup <- function() {
  if (!is_setup()) {
    jlme_setup(restart = FALSE)
  }
}

find_julia <- function() {
  asNamespace("JuliaConnectoR")$getJuliaExecutablePath()
}

julia_cli <- function(...) {
  x <- do.call(paste, list(...))
  julia_cmd <- find_julia()
  utils::tail(system2(julia_cmd, x, stdout = TRUE), 1L)
}

julia_version_compatible <- function() {
  as.package_version(julia_version()) >= "1.8"
}
julia_version <- function() {
  parse_julia_version(julia_cli("--version"))
}
parse_julia_version <- function(version) {
  gsub("^julia.version .*(\\d+\\.\\d+\\.\\d+).*$", "\\1", version)
}

loaded_libs <- function() {
  jl("sort(string.(keys(Pkg.project().dependencies)))", .R = TRUE)
}

#' @rdname jlme_setup
#' @export
check_julia_ok <- function() {
  JuliaConnectoR::juliaSetupOk() &&
    julia_version_compatible()
}

#' @rdname jlme_setup
#' @export
stop_julia <- function() {
  JuliaConnectoR::stopJulia()
  .jlme$is_setup <- FALSE
  invisible(TRUE)
}

#' @rdname jlme_setup
#' @export
jlme_status <- function() {
  active <- is_setup()
  if (active) {
    cat("jlme", as.character(utils::packageVersion("jlme")), "\n")
    cat(R.version.string, "\n")
    cat(JuliaConnectoR::juliaCall("versioninfo"))
    cat(JuliaConnectoR::juliaCall("Pkg.status"))
  } else {
    julia_cmd <- tryCatch(
      find_julia(),
      error = function(e) message("! ", e$message)
    )
    if (is.character(julia_cmd)) {
      message("Julia version ", julia_version(), " detected.")
      message("No active Julia connection. Please call `jlme_setup()`.")
    }
  }
  invisible(active)
}

#' Set up Julia connection for jlme
#'
#' @param ... Unused
#' @param add A character vector of additional Julia packages to add and load.
#' @param restart Whether to run `stop_julia()` first, before attempting setup
#' @param threads Number of threads to start Julia with. Defaults to `1`
#' @param verbose Whether to alert setup progress. Defaults to `interactive()`
#'
#' @return Invisibly returns `TRUE` on success
#' @export
#' @examplesIf check_julia_ok()
#' \donttest{
#' # Check whether Julia installation meets requirements
#' check_julia_ok()
#'
#' # Connect to a Julia runtime for use with `{jlme}`
#' jlme_setup()
#'
#' # Show information about the Julia runtime
#' jlme_status()
#'
#' # Stop Julia runtime
#' stop_julia()
#' }
jlme_setup <- function(...,
                       add = NULL,
                       restart = FALSE,
                       threads = NULL,
                       verbose = interactive()) {

  stopifnot(
    "Failed to discover Julia installation" = JuliaConnectoR::juliaSetupOk(),
    "Julia version >=1.8 required." = julia_version_compatible()
  )

  if (!is_setup() || (restart && is_setup())) {
    stop_julia()
  } else {
    message("Julia already running - skipping. Did you mean `restart = TRUE`?")
    return(invisible(TRUE))
  }

  .jlme$julia_cmd <- find_julia()

  params <- list(..., add = add, threads = threads, verbose = verbose)
  if (verbose) {
    do.call(.jlme_setup, params)
  } else {
    suppressMessages(do.call(.jlme_setup, params))
  }

  invisible(TRUE)

}

.jlme_setup <- function(..., add, threads, verbose = FALSE) {
  start <- Sys.time()
  start_julia(..., threads = threads)
  init_proj(add = add, verbose = verbose)
  load_libs(add = add)
  timing <- as.integer(difftime(Sys.time(), start, units = "secs"))
  message(sprintf("Successfully set up Julia connection. (%is)", timing))
  invisible(TRUE)
}

start_julia <- function(..., threads = NULL) {
  JULIA_NUM_THREADS <- Sys.getenv("JULIA_NUM_THREADS")
  if (!nzchar(JULIA_NUM_THREADS)) JULIA_NUM_THREADS <- NULL
  nthreads <- threads %||% JULIA_NUM_THREADS %||% 1L
  if (is.null(.jlme$is_setup)) stop_julia()
  if (is_setup()) {
    stop("There is already a connection to Julia established. Run `stop_julia()` first.")
  }
  if (nthreads > 1) {
    message(sprintf("Starting Julia with %i threads ...", nthreads))
    Sys.setenv("JULIA_NUM_THREADS" = nthreads)
  } else {
    message(sprintf("Starting Julia (v%s) ...", julia_version()))
  }
  .jlme$port <- suppressMessages(JuliaConnectoR::startJuliaServer())
  Sys.setenv("JULIA_NUM_THREADS" = JULIA_NUM_THREADS %||% "")
  .jlme$is_setup <- TRUE
  invisible(TRUE)
}

# guess_BLAS <- function() {
#   out <- tail(system(paste(
#     'julia --project=temp -q -E',
#     '"',
#     "using Pkg; Pkg.add(string(:LinearAlgebra));",
#     "using LinearAlgebra: BLAS; config = BLAS.get_config();",
#     "join(map(x -> basename(x.libname), BLAS.get_config().loaded_libs),',');",
#     '"'
#   ), intern = TRUE), 1)
#   out
# }

init_proj <- function(..., add = add, verbose = FALSE) {
  stopifnot(is.null(add) || is.character(add))

  jlme_deps <- c("StatsModels", "GLM", "MixedModels")
  deps <- unique(c(add, jlme_deps))
  jl('
    using Pkg;
    Pkg.activate(; temp=true, %1$s)
    Pkg.add(%2$s; %1$s)
  ', jl_io(verbose), vec_to_literal(deps))
  .jlme$projdir <- dirname(jl("Base.active_project()", .R = TRUE))
  invisible(TRUE)
}

load_libs <- function(..., add) {
  add_before <- intersect(add, c("MKL", "AppleAccelerate"))
  for (pkg in add_before) jl("using %s;", pkg)
  jl("
    using StatsModels;
    using GLM;
    using MixedModels;
  ")
  add_after <- setdiff(add, loaded_libs())
  for (pkg in add_after) jl("using %s;", pkg)
  invisible(TRUE)
}
