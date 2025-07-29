#' insertChangedColsTab insert a list of changed columns found by
#' \code{qcStudbook} in the pedigree file
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return Text of the error list formatted as an HTML page
#'
#' @param errorLst list of errors and changes made by \code{qcStudbook}
#' @param pedigreeFileName name of file provided by user on Input tab
#' @importFrom stringi stri_c
#' @importFrom stringi stri_split_regex stri_trim_both
#' @noRd
insertChangedColsTab <- function(errorLst, pedigreeFileName) {
  text <- summary(errorLst)
  lines <- stri_split_regex(text$txt, pattern = "\n")[[1L]]
  newText <- stri_c(
    "<h3>Changes to Pedigree Column Names ",
    "File:</h3>\n<p>", pedigreeFileName,
    "\n<ul style=\"list-style-type:disc\">\n"
  )

  for (line in lines) {
    if (stri_trim_both(line) == "") {
      next
    }
    newText <- stri_c(
      newText, "	<li style=\"padding-bottom: 15px\">\n",
      line, "</li>\n"
    )
  }
  newText <- stri_c(newText, "</ul>\n</p>\n")
  newText
}
