needTerraAndRaster <- function(envir = parent.frame()) {
  rastDF <- data.frame(
    pkg = c("raster", "terra"),
    class = c("Raster", "SpatRaster"),
    read = c("raster::raster", "terra::rast"),
    stack = c("raster::stack", "terra::rast"),
    stackClass = c("RasterStack", "SpatRaster"),
    extent = c("raster::extent", "terra::ext")
  )

  if (!requireNamespace("raster", quietly = TRUE)) {
    rastDF <- rastDF[rastDF$pkg == "terra", ]
  }
  return(rastDF)
}

# puts tmpdir, tmpCache, opts, optsDebug in this environment,
# loads and libraries indicated plus testthat,
# puts tmpdir, tmpCache, tmpfile (can be vectorized with length >1 tmpFileExt),
#   optsAsk in this environment,
# loads and libraries indicated plus testthat,
# sets options("reproducible.ask" = FALSE) if ask = FALSE
testInit <- function(libraries = character(), ask = FALSE, verbose, tmpFileExt = "", opts = NULL) {
  data.table::setDTthreads(2)
  reproducible::set.randomseed()

  pf <- parent.frame()

  if (length(libraries)) {
    libraries <- unique(libraries)
    loadedAlready <- vapply(libraries, function(pkg)
      any(grepl(paste0("package:", pkg), search())), FUN.VALUE = logical(1))
    libraries <- libraries[!loadedAlready]

    if (length(libraries)) {
      pkgsLoaded <- unlist(lapply(libraries, requireNamespace, quietly = TRUE))
      if (!all(pkgsLoaded)) {
        lapply(libraries[!pkgsLoaded], skip_if_not_installed)
      }
      suppressWarnings(lapply(libraries, withr::local_package, .local_envir = pf))
    }
  }

  out <- list()

  ## set default options for tests
  withr::local_options(list(
    reproducible.ask = ask,
    reproducible.verbose = FALSE,
    # spades.debug = debug,
    # spades.moduleCodeChecks = smcc,
    spades.sessionInfo = FALSE,
    spades.recoveryMode = FALSE,
    spades.useRequire = FALSE
  ), .local_envir = pf)

  if (!missing(verbose)) {
    withr::local_options("reproducible.verbose" = verbose, .local_envir = pf)
  }
  if (!is.null(opts)) {
    withr::local_options(opts, .local_envir = pf)
  }
  tmpdir <- withr::local_tempdir(tmpdir = reproducible::tempdir2(), .local_envir = pf) |>
    reproducible::normPath()
  tmpCache <- withr::local_tempdir(tmpdir = tmpdir, .local_envir = pf) |>
    reproducible::normPath()
  if (isTRUE(any(nzchar(tmpFileExt)))) {
    dotStart <- startsWith(tmpFileExt, ".")
    if (any(!dotStart))
      tmpFileExt[!dotStart] <- paste0(".", tmpFileExt)
    out$tmpfile <- withr::local_tempfile(tmpdir = tmpdir, fileext = tmpFileExt) |>
      reproducible::normPath()
  }
  withr::local_dir(tmpdir, .local_envir = pf)

  out <- append(out, list(tmpdir = tmpdir, tmpCache = tmpCache))
  list2env(out, envir = pf)
  return(invisible(out))
}
