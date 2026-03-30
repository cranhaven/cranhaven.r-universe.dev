# Internal environment for package state
the <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  metadata_path <- system.file("extdata", "metadata.rds", package = pkgname)
  if (nzchar(metadata_path) && file.exists(metadata_path)) {
    the$metadata <- readRDS(metadata_path)
  }
}
