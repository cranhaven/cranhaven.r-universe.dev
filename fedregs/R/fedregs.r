#' \code{fedregs} package
#'
#' Text Analysis of the US Code of Federal Regulations
#'
#' See the README on
#' \href{https://github.com/slarge/fedregs/blob/master/README.md}{GitHub}
#'
#' @docType package
#' @name fedregs

NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "parts", "chapters",
                                                        "min_parts", "max_parts", "SECTION_NUMBER",
                                                        "TEXT", "SUBPART_NAME", "word", "subpart", "file_path"))

if (packageVersion("tidyr") < "1.0.0") {
  stop("tidyr >= 1.0.0 needed for this function.", call. = FALSE)
}
