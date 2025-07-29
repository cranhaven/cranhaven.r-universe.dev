#' insertErrorTab insert a list of errors found by \code{qcStudbook} in the
#' pedigree file
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return Text of the error list formatted as an HTML page
#'
#' @param errorLst list of errors and changes made by \code{qcStudbook}
#' @param pedigreeFileName name of file provided by user on Input tab
#' @importFrom htmlTable htmlTable
#' @importFrom stringi stri_c stri_trim_both
#' @importFrom stringi stri_split_regex
#' @noRd
insertErrorTab <- function(errorLst, pedigreeFileName) {
  text <- summary(errorLst)
  if (checkChangedColsLst(errorLst$changedCols)) {
    colsChangedTxt <- "and Changes to Pedigree Column Names "
  } else {
    colsChangedTxt <- ""
  }
  lines <- stri_split_regex(text$txt, pattern = "\n")[[1L]]
  newText <- stri_c(
    "<h3>Errors Detected ", colsChangedTxt,
    "</h3>\nFile: '", pedigreeFileName,
    "'\n<ul style=\"list-style-type:disc\">\n"
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
  if (nrow(text$sp) > 0L) {
    newText <- stri_c(
      newText, "	<li style=\"padding-bottom: 15px\">\n",
      "One or both parents are below the ",
      "minimum parental age. Check both parent and offspring ",
      "birth dates.", "</li>\n"
    )
    newText <- stri_c(newText, htmlTable(text$sp, rnames = FALSE))
  }
  newText <- stri_c(newText, "</ul>\n</p>\n</pre>")
  newText
}
