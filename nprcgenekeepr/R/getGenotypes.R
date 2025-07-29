#' Get genotypes from file
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' @return A genotype file compatible with others in this package.
#'
#' @param fileName character vector of temporary file path.
#' @param sep column separator in CSV file
#' @import futile.logger
#' @importFrom readxl excel_format
#' @importFrom utils read.table
#' @export
#' @examples
#' library(nprcgenekeepr)
#' pedCsv <- getGenotypes(fileName = system.file("testdata", "qcPed.csv",
#'   package = "nprcgenekeepr"
#' ))
getGenotypes <- function(fileName, sep = ",") {
  flog.debug(paste0("in getGenotypes\n"),
    name = "nprcgenekeepr"
  )
  if (excel_format(fileName) %in% c("xls", "xlsx")) {
    genotypes <- readExcelPOSIXToCharacter(fileName)
    flog.debug(paste0(
      "in getGenotypes after readxl, nrow(genotypes) = ",
      nrow(genotypes), "\n"
    ), name = "nprcgenekeepr")
  } else {
    genotypes <- read.table(fileName,
      header = TRUE,
      sep = sep,
      stringsAsFactors = FALSE,
      na.strings = c("", "NA"),
      check.names = FALSE
    )
    flog.debug(paste0(
      "in getGenotypes after read.csv, nrow(genotypes) = ",
      nrow(genotypes), "\n"
    ), name = "nprcgenekeepr")
  }
  genotypes
}
