package_path <- suppressWarnings(find.package("FAfA", quiet = TRUE))
installed_package <- length(package_path) == 1L && nzchar(package_path) &&
  file.exists(file.path(package_path, "Meta", "package.rds"))

if (installed_package) {
  FAfA::run_app()
} else {
  source_root <- normalizePath(
    file.path(getwd(), "..", ".."),
    winslash = "/",
    mustWork = TRUE
  )
  if (!requireNamespace("pkgload", quietly = TRUE)) {
    stop("The pkgload package is required when running UI tests from source.")
  }
  pkgload::load_all(source_root, quiet = TRUE)
  get("run_app", envir = asNamespace("FAfA"))()
}
