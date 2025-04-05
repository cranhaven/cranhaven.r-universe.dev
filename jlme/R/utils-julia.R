jl_io <- function(verbose) {
  if (verbose) "" else "io=devnull"
}

jl_pkg_installed <- function(x) {
  x %in% loaded_libs()
}

jl_pkg_add <- function(x, ..., verbose = interactive()) {
  if (!jl_pkg_installed(x)) {
    jl('Pkg.add("%2$s"; %1$s); using %2$s', jl_io(verbose), x)
  }
}

jl_try_pkg_install <- function(x, ..., verbose = interactive()) {
  if (!jl_pkg_installed(x)) {
    try(jl_pkg_add(x, verbose = verbose))
  }
  jl_pkg_installed(x)
}

sanitize_jl_error <- function(e, .call) {
  e$message <- gsub("Stacktrace:.*$", "", e$message)
  e$call <- .call
  e
}

jl_format <- function(x, ...) {
  jl_require("JuliaFormatter")
  JuliaConnectoR::juliaCall("JuliaFormatter.format_text", x, align_matrix = TRUE, ...)
}

jl_require <- function(x) {
  installed <- jl_try_pkg_install(x)
  if (!installed) stop("Unable to add package: ", x)
  invisible(installed)
}
