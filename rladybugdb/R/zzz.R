.onLoad <- function(libname, pkgname) {
  # Register as_tibble S3 method if tibble is available (it's in Suggests)
  if (requireNamespace("tibble", quietly = TRUE)) {
    registerS3method(
      genname = "as_tibble",
      class   = "lb_result",
      method  = get("as_tibble.lb_result", envir = asNamespace(pkgname)),
      envir   = asNamespace("tibble")
    )
  }
}
