#' Collect Full Filenames from a Mix of Directories and Files
#'
#' The input can be one or more directories, one or more files, or the mixture of the two. 
#' It will return the full paths of all files in a recursive way, 
#' and sort them in increasing order. When files are put in 
#' different areas of your disk, you may need this function to collect them. 
#' It is essentially a wrapper of \code{\link{list.files}}.
#'
#' Failure may occur when obtaining absolute paths, please see \code{\link{normalizePath}} 
#' for possible reasons.
#'
#' @param ... names of directories and files; if the input is not vector, the function will try to 
#' coerce it. Relative paths and paths starting with "~/" are also accepted. 
#' In Windows, both "/"  and double inversed slashes inside filenames are accepted.
#' @param special a length 1 character or regular expression. Only filenames that have this pattern
#' will be collected. Default value is "" (character with size 0), and is to collect everything. 
#'
#' @return a character vector of full filenames with increasing order, and every name is 
#' unique. If no filename is collected, an error will be raised. 
#'
#' @export
#' @examples
#' x1 <- find.package("base")
#' x2 <- find.package("utils")
#' all_file <- dir_or_file(x1, x2, special = "rds$")
dir_or_file <-
function(..., special = "") {
  infolocale <- localestart2()
  on.exit(localeend2(infolocale))
  x <- as.character2(...)
  x <- whetherencode(x)
  x <- normalizePath(x, winslash = "/", mustWork = TRUE)
  dir_pos <- dir.exists(x)
  all_dir <- x[dir_pos]
  if (length(all_dir) > 0) {
    file_1 <- unlist(lapply(all_dir, list.files, all.files = TRUE, full.names = TRUE, recursive = TRUE))
  }
  else {
    file_1 <- NULL
  }
  file_pos <- file.exists(x) & !dir.exists(x)
  file_2 <- x[file_pos]
  file_12 <- c(file_1, file_2)
  if (is_character_vector(special, len = 1) == TRUE && special != "" && length(file_12) > 0) {
    file_12 <- file_12[grepl(special, file_12)]
  }
  if (length(file_12) == 0)
    stop("There is nothing to collect.")
  file_12 <- sort(unique(file_12))
  file_12 <- stringi::stri_encode(file_12, to = "UTF-8")
  return(file_12)
}
