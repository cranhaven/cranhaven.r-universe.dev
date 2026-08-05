#' Include Markdown help
#'
#' @param ... Character vector specifying directory and or file to
#'     point to inside the current package.
#'
include_help <- function(...) {
  help_path <- system.file("app/help", ..., package = "biodosetools")
  tmp_path <- file.path(tempdir(), basename(help_path))
  file.copy(help_path, tmp_path, overwrite = TRUE)

  withMathJax(
    includeMarkdown(tmp_path)
  )
}


#' Load RMarkdown report
#'
#' @param ... Character vector specifying directory and or file to
#'     point to inside the current package.
#'
load_rmd_report <- function(...) {
  system.file("app/reports", ..., package = "biodosetools")
}
