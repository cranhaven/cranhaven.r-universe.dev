#' Write copy of dataframes to either CSV, TXT, or Excel file.
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#'
#' Takes a list of dataframes and creates a file based on the list name of
#' the dataframe and the extension for the file type.
#'
#' @return Full path name of files saved.
#'
#' @param dfList list of dataframes to be stored as files.
#' \code{"txt"}, \code{"csv"}, or \code{"xlsx"}. Default value is \code{"csv"}.
#' @param baseDir character vector of length on with the directory path.
#' @param fileType character vector of length one with possible values of
#' \code{"txt"}, \code{"csv"}, or \code{"xlsx"}. Default value is \code{"csv"}.
#'
#' @importFrom utils write.table write.csv
## ## rmsutilityr create_wkbk
#' @export
saveDataframesAsFiles <- function(dfList, baseDir, fileType = "csv") {
  if (!(inherits(dfList, "list") &&
    all(vapply(
      dfList, function(df) inherits(df, "data.frame"),
      logical(1L)
    )))) {
    stop("dfList must be a list containing only dataframes.")
  }
  stopifnot(any(fileType %in% c("txt", "csv", "excel")))
  filesWritten <- character(0L)
  for (i in seq_along(dfList)) {
    filename <- paste0(baseDir, "/", names(dfList)[i], ".", fileType)
    if (fileType == "csv") {
      write.csv(dfList[[i]],
        file = filename,
        row.names = FALSE
      )
    } else if (fileType == "excel") {
      status <-
        create_wkbk(
          file = filename,
          df_list = dfList[i],
          sheetnames = names(dfList)[i],
          replace = TRUE
        )
      if (!status) {
        stop("Failed to write example data out to ", filename, ".")
      }
    } else { # txt; tab delimited
      write.table(
        dfList[[i]],
        file = filename,
        row.names = FALSE,
        sep = "\t"
      )
    }
    filesWritten <- c(filesWritten, filename)
  }
  filesWritten
}
