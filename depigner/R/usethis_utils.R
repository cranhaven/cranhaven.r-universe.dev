proj_crit <- function() {
  # copy-pasted from usethis:::proj_crit() to be used in proj_find()
  rprojroot::has_file(".here") |
    rprojroot::is_rstudio_project |
    rprojroot::is_r_package |
    rprojroot::is_git_root |
    rprojroot::is_remake_project |
    rprojroot::is_projectile_project
}




proj_find <- function(path = ".") {
  # copy-pasted from usethis:::proj_find() to be used in write_utf8()
  tryCatch(rprojroot::find_root(proj_crit(), path = path),
    error = function(e) NULL
  )
}




is_package <- function(base_path = usethis::proj_get()) {
  # copy-pasted from usethis:::is_package() to be used in project_name()
  tryCatch(
    !is.null(rprojroot::find_package_root_file(path = base_path)),
      error = function(e) NULL
  )
}




pkg_name <- function(base_path = usethis::proj_get()) {
  if (!is_package(base_path)) ui_stop("
    {base_path} must be a package path, and it does not.
    Please provide a package {ui_field('base_path')}.
  ")

  desc <- desc::description[["new"]](base_path)
  as.list(desc[["get"]](desc[["fields"]]()))[["Package"]]
}




platform_line_ending <- function() {
  # copy-pasted from usethis:::platform_line_ending to be used in
  # pkg_line_ending() and in write_utf8()
  if (.Platform[["OS.type"]] == "windows") "\r\n" else "\n"
}




detect_line_ending <- function(path) {
  # copy-pasted from usethis::detect_line_ending() to be used in
  samp <- suppressWarnings(readChar(path, nchars = 500L))
  if (isTRUE(grepl("\r\n", samp))) "\r\n" else "\n"
}




pkg_line_ending <- function() {
  # adapted from usethis:::proj_line_ending() to be used in write_utf8()

  proj_path <- usethis::proj_path(paste0(pkg_name(), ".Rproj"))

  if (fs::file_exists(proj_path)) {
    config <- base::readLines(proj_path,
      encoding = "UTF-8", warn = FALSE
    )

    if (any(grepl("^LineEndingConversion: Posix", config))) {
      return("\n")
    } else if (any(grepl("^LineEndingConversion: Windows", config))) {
      return("\r\n")
    }
  }

  desc_path <- usethis::proj_path("DESCRIPTION")

  if (fs::file_exists(desc_path)) return(detect_line_ending(desc_path))

  r_path <- usethis::proj_path("R")
  if (fs::dir_exists(r_path)) {
    r_files <- fs::dir_ls(r_path, pattern = "\\.[rR]$")
    if (length(r_files) > 0L) {
      return(detect_line_ending(r_files[[1L]]))
    }
  }

  platform_line_ending()
}


write_utf8 <- function(path, lines, append = FALSE) {
  # adapted from usethis:::write_utf8() to be used in block_append()

  stopifnot(is.character(path))
  stopifnot(is.character(lines))

  file_mode <- if (append) "ab" else "wb"
  con <- file(path, open = file_mode, encoding = "utf-8")
  on.exit(close(con), add = TRUE)

  if (!is.null(proj_find(path))) {
    line_ending <- pkg_line_ending()
  } else {
    line_ending <- platform_line_ending()
  }

  lines <- gsub("\r?\n", line_ending, lines)
  base::writeLines(
    enc2utf8(lines), con, sep = line_ending, useBytes = TRUE
  )
  invisible(TRUE)
}




block_find <- function(lines, block_start, block_end) {
  # adapted from usethis:::block_find() to be used in block_append()

  if (is.null(lines)) return(NULL)

  start <- which(lines == block_start)
  end   <- which(lines == block_end)

  if (length(start) == 0L && length(end) == 0L) return(NULL)

  if (!(length(start) == 1L && length(end) == 1L && start < end)) {
    ui_stop("
      Invalid block specification.
      Must start with {ui_code(block_start)}
      and end with {ui_code(block_end)}.
    ")
  }

  c(start = start + 1L, end = end - 1L)
}




block_append <- function(value, path) {
  # adapted from usethis:::block_append to be used in use_ui()

  block_start <- "## depigner namespace: start"
  block_lines <- NULL
  block_end   <- "## depigner namespace: end"

  if (fs::file_exists(path)) {
    lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
    if (value %in% lines) return(FALSE)

    block_lines <- block_find(lines, block_start, block_end)
  }

  if (is.null(block_lines)) {
    ui_todo("Copy and paste the following lines into {ui_value(path)}:")
    ui_code_block(c(block_start, value, block_end))
    return(FALSE)
  }

  ui_done("Adding {ui_value(value)} to {ui_path(path)}")

  end <- block_lines[["end"]]
  lines <- c(
    lines[rlang::seq2(1L, end)],
    value,
    lines[rlang::seq2(end + 1L, length(lines))]
  )

  write_utf8(path, lines)
}
