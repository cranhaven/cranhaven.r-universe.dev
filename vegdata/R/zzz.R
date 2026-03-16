.onAttach <- function(lib, pkg)  {
  packageStartupMessage(
    "This is vegdata ",
    utils::packageDescription("vegdata", field = "Version"),
    " - build: ",
    utils::packageDate("vegdata")
  )
  if (is.null(getOption("tv.iconv")))
    options(tv.iconv = "ISO-8859-15")
}

mssg <- function(v, ...) if (v) message(...)

assert <- function(x, y) {
  if (!is.null(x)) {
    if (!inherits(x, y)) {
      stop(deparse(substitute(x)), " must be of class ",
           paste0(y, collapse = ", "), call. = FALSE)
    }
  }
}

.my_cache <- NULL

.onLoad <- function(libname, pkgname) {

  .my_cache <<- hoardr::hoard()
  .my_cache$cache_path_set(pkgname)
  .my_cache$mkdir()

  inst_dir  <- system.file("extdata/tvdata", package = pkgname)
  cache_path <- .my_cache$cache_path_get()

  existing <- list.files(cache_path, recursive = TRUE)
  if (length(existing) == 0 && file.exists(inst_dir)) {
    subdirs <- list.dirs(inst_dir, recursive = FALSE, full.names = TRUE)
    for (s in subdirs) {
      target <- file.path(cache_path, basename(s))
      dir.create(target, showWarnings = FALSE)
      file.copy(
        from      = list.files(s, full.names = TRUE),
        to        = target,
        recursive = TRUE
      )
    }
  }
}



utils::globalVariables(c(
  ".", "multipatt", "write.dbf", "taxlevels",
  "gwindow", "gtree", "addHandlerDoubleclick", "svalue"
))
