#' @import nanonext
#' @import nanoarrow
#' @import jsonlite
NULL


#' Create a unique IPC path for mangoro
#'
#' @param prefix Prefix for the temp file (default: "mangoro-echo")
#' @return IPC URL string suitable for nanonext and mangoro Go binaries
#' @export
create_ipc_path <- function(prefix = "mangoro-echo") {
  tmp_ipc <- tempfile(pattern = prefix, fileext = ".ipc")
  if (.Platform$OS.type == "windows") {
    ipc_url <- paste0("ipc://", gsub("/", "\\\\", tmp_ipc))
  } else {
    tmp_ipc <- gsub("/+", "/", x = tmp_ipc)
    ipc_url <- paste0("ipc://", tmp_ipc)
  }
  ipc_url
}

#' Find the path to the Go executable
#'
#' @return Path to the Go binary
#' @export
find_go <- function() {
  go <- Sys.which("go")
  if (!nzchar(go)) {
    stop("Go not found in PATH")
  }
  go
}

#' Find the path to the mangoro vendor directory
#'
#' @return Path to the vendor directory (inst/go/vendor)
#' @export
find_mangoro_vendor <- function() {
  vend <- system.file("go/vendor", package = "mangoro")
  if (!dir.exists(vend)) {
    stop("Vendor directory not found: ", vend)
  }
  vend
}

#' Compile a Go source file using the vendored dependencies
#'
#' @param src Path to the Go source file
#' @param out Path to the output binary
#' @param gomaxprocs Number of threads for Go build (sets GOMAXPROCS env variable)
#' @param gocache Path to Go build cache directory. If NULL (default), uses a temporary directory to comply with CRAN policy. Set to NA to use the default Go cache location.
#' @param ... Additional arguments to pass to Go build
#' @return Path to the compiled binary
#' @export
mangoro_go_build <- function(src, out, gomaxprocs = 1, gocache = NULL, ...) {
  go <- find_go()
  vend <- dirname(find_mangoro_vendor())

  # Set GOCACHE to temporary directory by default (CRAN compliance)
  old_gocache <- Sys.getenv("GOCACHE", unset = NA)
  if (is.null(gocache)) {
    # Default: use temp directory
    Sys.setenv(GOCACHE = tempdir())
  } else if (!is.na(gocache)) {
    # User-specified cache directory
    Sys.setenv(GOCACHE = gocache)
  }
  # If gocache = NA, leave GOCACHE unchanged

  # Restore original GOCACHE on exit
  on.exit(
    {
      if (!is.null(gocache) || is.na(old_gocache)) {
        if (is.na(old_gocache)) {
          Sys.unsetenv("GOCACHE")
        } else {
          Sys.setenv(GOCACHE = old_gocache)
        }
      }
    },
    add = TRUE
  )

  # Only one -mod flag can be used per go build invocation
  args <- c("build", "-mod=vendor", "-o", out, src)
  oldwd <- setwd(vend)
  on.exit(setwd(oldwd), add = TRUE)
  env <- character()
  if (!is.null(gomaxprocs)) {
    env <- c(sprintf("GOMAXPROCS=%s", as.integer(gomaxprocs)))
    go <- normalizePath(go)
    if (!.Platform$OS.type == "windows") go <- sprintf("%s %s", env, go)
  }
  cmd <- sprintf("%s %s", go, paste(shQuote(args), collapse = " "))
  message(cmd)
  status <- system(
    cmd,
    ignore.stdout = FALSE,
    ignore.stderr = FALSE,
    intern = TRUE,
    ...
  )
  if (!file.exists(out)) {
    message(paste(status, collapse = "\n"))
    stop("Go build failed")
  }
  out
}

#' Get the version of vendored mangos using Go tooling (no jsonlite)
#'
#' @return The version string of go.nanomsg.org/mangos/v3 in the vendor go.mod
#' @export
get_mangos_version <- function() {
  go <- find_go()
  vend <- dirname(find_mangoro_vendor())
  oldwd <- setwd(vend)
  on.exit(setwd(oldwd))
  res <- system2(
    go,
    c("list", "-m", "go.nanomsg.org/mangos/v3"),
    stdout = TRUE,
    stderr = TRUE
  )

  if (
    length(res) == 0 ||
      any(grepl(
        "not a module|no required module",
        res,
        ignore.case = TRUE
      ))
  ) {
    return(NA_character_)
  }
  # Output is like: "go.nanomsg.org/mangos/v3 v3.2.2"
  version <- sub(
    "^go\\.nanomsg\\.org/mangos/v3\\s+",
    "",
    grep("^go\\.nanomsg\\.org/mangos/v3", res, value = TRUE)
  )
  if (length(version) == 0) {
    return(NA_character_)
  }
  version
}

#' Get the version of vendored Arrow Go using Go tooling (no jsonlite)
#'
#' @return The version string of github.com/apache/arrow/go/v18 in the vendor go.mod
#' @export
get_arrow_go_version <- function() {
  go <- find_go()
  vend <- dirname(find_mangoro_vendor())
  oldwd <- setwd(vend)
  on.exit(setwd(oldwd))
  res <- system2(
    go,
    c("list", "-m", "github.com/apache/arrow/go/v18"),
    stdout = TRUE,
    stderr = TRUE
  )

  if (
    length(res) == 0 ||
      any(grepl(
        "not a module|no required module",
        res,
        ignore.case = TRUE
      ))
  ) {
    return(NA_character_)
  }
  # Output is like: "github.com/apache/arrow/go/v18 v18.0.0"
  version <- sub(
    "^github\\.com/apache/arrow/go/v18\\s+",
    "",
    grep("^github\\.com/apache/arrow/go/v18", res, value = TRUE)
  )
  if (length(version) == 0) {
    return(NA_character_)
  }
  version
}
