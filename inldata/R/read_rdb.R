#' Read USGS RDB file
#'
#' @description Reads a RDB (Relational Database) file in table format and creates a data frame from it.
#'   The USGS (U.S. Geological Survey) RDB file is a variant of a tab-delimited ASCII file structure.
#'
#' @param file 'character' string.
#'   Path to file which the data are to be read from.
#' @param na_strings 'character' vector.
#'   Strings which are interpreted as `NA` values.
#'
#' @return A data frame containing a representation of the data in the file.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @keywords internal
#'
#' @examples
#' file <- system.file("extdata", "test.rdb", package = "inldata")
#' d <- read_rdb(file)
#' str(d)

read_rdb <- function(file, na_strings = "") {

  # check arguments
  file <- path.expand(file) |>
    normalizePath(winslash = "/", mustWork = FALSE)
  checkmate::assert_file_exists(file, access = "r", extension = "rdb")
  checkmate::assert_character(na_strings, any.missing = FALSE)

  # read text lines in file
  x <- readLines(file,
    n = 500L # maximal possible lines in any header section
  )

  # get the number of lines in the file's header section
  i <- 0L
  repeat {
    is <- x[i + 1] |>
      substr(start = 1, stop = 1) |>
      identical("#")
    if (is) {
      i <- i + 1L
    } else {
      break
    }
  }

  # get header section
  if (i > 0) {
    hdr_sect <- gsub("^#", "", x[1:i]) |>
      trimws()
  } else {
    hdr_sect <- NULL
  }

  # get column names
  col_names <- x[i + 1] |>
    strsplit(split = "\t") |>
    unlist()

  # get column definitions
  col_defs <- x[i + 2] |>
    strsplit(split = "\t") |>
    unlist()
  names(col_defs) <- col_names

  # read data section
  d <- utils::read.table(file,
    header = TRUE,
    sep = "\t",
    quote = "",
    row.names = NULL,
    col.names = col_names,
    na.strings = na_strings,
    colClasses = "character",
    skip = i + 1L,
    check.names = FALSE,
    fill = TRUE,
    strip.white = TRUE
  )

  # get column descriptions
  col_desc <- vapply(col_names,
    FUN = function(x) {
      pattern <- sprintf("^%s ", x)
      text <- grep(pattern, hdr_sect, value = TRUE)
      if (length(text) > 0) {
        sub(pattern, "", text) |>
          trimws()
      } else {
        NA_character_
      }
    },
    FUN.VALUE = character(1)
  )
  if (all(is.na(col_desc))) {
    col_desc <- NULL
  }

  # set attribute
  structure(d,
    "header_section" = paste(hdr_sect, collapse = "\n"),
    "column_definitions" = col_defs,
    "column_descriptions" = col_desc
  )
}
