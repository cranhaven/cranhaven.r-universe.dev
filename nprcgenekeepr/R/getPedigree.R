#' Get pedigree from file
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return A pedigree file compatible with others in this package.
#'
#' @param fileName character vector of temporary file path.
#' @param sep column separator in CSV file
#' @import futile.logger
#' @importFrom readxl excel_format
#' @importFrom utils read.table
#' @export
#' @examples
#' library(nprcgenekeepr)
#' ped <- getPedigree(fileName = system.file("testdata", "qcPed.csv",
#'   package = "nprcgenekeepr"
#' ))
getPedigree <- function(fileName, sep = ",") {
  flog.debug(paste0("in getPedigree\n"),
    name = "nprcgenekeepr"
  )
  if (excel_format(fileName) %in% c("xls", "xlsx")) {
    pedigree <- readExcelPOSIXToCharacter(fileName)
    flog.debug(paste0(
      "in getPedigree after readxl, nrow(pedigree) = ",
      nrow(pedigree), "\n"
    ), name = "nprcgenekeepr")
  } else {
    pedigree <- read.table(fileName,
      header = TRUE,
      sep = sep,
      stringsAsFactors = FALSE,
      na.strings = c("", "NA"),
      check.names = FALSE
    )
    flog.debug(paste0(
      "in getPedigree after read.csv, nrow(pedigree) = ",
      nrow(pedigree), "\n"
    ), name = "nprcgenekeepr")
  }
  pedigree
}
