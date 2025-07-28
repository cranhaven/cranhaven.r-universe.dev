#' Read Field Data in an Excel Workbook
#'
#' @description Read worksheet(s) of Multilevel Monitoring System (MLMS) field data in an Excel workbook.
#'
#' @param path 'character' string.
#'   Path to the Excel workbook (xlsx) to read.
#' @param sheet 'character' vector.
#'   Name of the Excel worksheet(s) to read.
#'   Sheet names may also be specified via the `pattern` argument.
#'   If neither argument specifies the sheet, defaults to the first sheet in the workbook.
#' @param pattern 'character' string.
#'   A pattern (regular expression) used to identify worksheet names in the workbook.
#'
#' @return A list with data frame components.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @export
#'
#' @examples
#' path <- system.file("extdata/ex-field.xlsx", package = "mlms")
#' l <- read_field_xlsx(path, sheet = "06-30-2022")
#' str(l, max.level = 1)

read_field_xlsx <- function(path,
                            sheet = NULL,
                            pattern = "^[0-9]{1,2}-[0-9]{1,2}-[0-9]{2,4}$") {

  # check arguments
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  checkmate::assert_file_exists(path, access = "r", extension = "xlsx")
  checkmate::assert_character(sheet, any.missing = FALSE, min.len = 1, null.ok = TRUE)
  checkmate::assert_string(pattern, null.ok = TRUE)

  # print status
  message("Reading XLSX file:\n  ", path)

  # check sheet names exist
  all_sheet_names <- readxl::excel_sheets(path)
  checkmate::assert_subset(sheet, choices = all_sheet_names)

  # search for sheet names
  if (!is.null(pattern)) {
    is <- grepl(pattern, all_sheet_names)
    if (all(!is)) {
      stop("No match for pattern: ", pattern, call. = FALSE)
    }
    sheet <- c(sheet, all_sheet_names[is])
  }

  # loop through sheets
  lapply(sheet, function(x) {
    message("  Sheet: ", x)
    read_excel_worksheet(path = path, sheet = x)
  })
}


# Function to read a worksheet in the Excel formatted field form
read_excel_worksheet <- function(path, sheet, max_rows = 100L) {

  # check arguments
  checkmate::assert_string(sheet)

  # define cell ranges to read
  ranges <- c(
    "R5C3:R11C3",
    "R5C8:R11C8",
    "R3C11",
    "R7C13",
    "R7C16",
    "R9C13",
    "R9C16",
    "R9C13",
    "R9C16"
  )

  # workaround for inconsistant cell range
  x <- read_cells(path = path, sheet = sheet, range = "R5C3")
  if (is.na(x)) {
    ranges[1] <- "R5C4:R11C4"
  }

  # read cells
  x <- lapply(ranges,
    FUN = function(range) {
      read_cells(path = path, sheet = sheet, range = range)
    }
  ) |>
    unlist()

  # define variable names
  var_names <- c(
    "site_nm",
    "alt_va",
    "stickup_va",
    "sensor_id",
    "press_max_va",
    "baro_id",
    "well_casing_tp",
    "site_no",
    "date",
    "times",
    "weather",
    "operators",
    "comment_1",
    "comment_2",
    "sheet_version_tx",
    "press_start_va",
    "press_end_va",
    "temp_start_va",
    "temp_end_va",
    "baro_start_va",
    "baro_end_va"
  )

  # make list and coerce variables to numeric
  is <- grepl("_va$", var_names)
  l <- lapply(seq_along(x),
    FUN = function(i) {
      if (is[i]) suppressWarnings(as.numeric(x[i])) else x[i]
    }
  )

  # set list names
  names(l) <- var_names

  # convert site name to uppper case
  l$site_nm <- toupper(l$site_nm)

  # set date time format
  l$date_format_cd <- "%Y-%m-%d %H:%M"

  # parse comment cell values
  l$comment_tx <- c(l$comment_1, l$comment_2) |>
    stats::na.omit() |>
    paste(collapse = " ")
  l$comment_1 <- NULL
  l$comment_2 <- NULL

  # coerce start and end times
  seconds <- as.numeric(l$date) * 86400
  date <- as.POSIXct(seconds, origin = "1899-12-30", tz = "GMT") |>
    format(format = "%Y-%m-%d")
  times <- strsplit(l$times, split = "+ ")[[1]]
  l$stime_dt <- paste(date, times[1])
  l$etime_dt <- paste(date, times[length(times)])
  l$date <- NULL
  l$times <- NULL

  # prepare output list
  out <- l

  # set column names
  col_names <- c(
    "port_nu",
    "port_depth_log_va",
    "tp_depth_va",
    "baro_va",
    "press_in_1_va",
    "press_va",
    "press_in_2_va",
    "temp_va",
    "time_hm",
    "press_head_va",
    "total_head_va",
    "comment_tx"
  )

  # read table
  dim <- c(max_rows, length(col_names))
  range <- readxl::anchored("A17", dim = dim)
  d <- suppressMessages(
    readxl::read_xlsx(
      path = path,
      sheet = sheet,
      range = range,
      col_names = FALSE,
      col_types = "text",
      na = "",
      progress = FALSE
    )
  )
  d <- as.data.frame(d)
  colnames(d) <- col_names
  rownames(d) <- NULL

  # remove extraneous table rows
  m <- apply(d, 1, function(x) all(is.na(x))) |>
    which() |> min() - 1L
  d <- d[seq_len(m), , drop = FALSE]

  # remove rows with missing values in required fields
  req_fields <- c("baro_va", "press_va", "temp_va", "time_hm")
  is <- apply(d[, req_fields], 1, function(x) all(x %in% c("", "NA", NA)))
  d <- d[!is, , drop = FALSE]

  # coerce column values to numeric
  for (nm in colnames(d)) {
    if (grepl("(_va$)|(_nu$)", nm)) {
      x <- d[[nm]]
      va <- suppressWarnings(as.numeric(x))
      if (nm %in% req_fields) {
        is <- is.na(va) & !(x %in% "NA")
        if (any(is)) {
          txt <- utils::capture.output(d[is, , drop = FALSE]) |> paste(collapse = "\n")
          message(txt)
          bad <- x[is] |> sQuote(q = FALSE) |> paste(collapse = ", ")
          warning("NAs introduced by coercion: ", bad, call. = FALSE, immediate. = TRUE)
        }
      }
      d[[nm]] <- va
    }
  }

  # set comment to empty string when missing
  d$comment_tx[is.na(d$comment_tx)] <- ""

  # parse measurement times into seconds
  seconds <- as.numeric(d$time_hm) * 86400

  # workaround for non-military time
  is <- seconds < 18000 # less than 4 hours (< 5:00 AM)
  seconds[is] <- seconds[is] + 43200 # add 12 hours

  # make date-time values
  times <- as.POSIXct(seconds, origin = "1899-12-30", tz = "GMT") |>
    format(format = "%H:%M")
  d$press_dt <- paste(date, times)

  # remove extraneous columns
  d$time_hm <- NULL
  d$port_depth_log_va <- NULL
  d$tp_depth_va <- NULL
  d$press_head_va <- NULL
  d$total_head_va <- NULL

  # add table to ouput list
  out$profile <- d

  # assign attributes
  out
}


# Function to read a 1-by-n block of cells in Excel
read_cells <- function(path, sheet, range, var_names = NULL) {

  # check arguments
  checkmate::assert_file_exists(path, access = "r", extension = "xlsx")
  checkmate::assert_string(sheet)
  checkmate::assert_string(range)
  checkmate::assert_character(var_names, any.missing = FALSE, null.ok = TRUE)

  # read cell values
  out <- suppressMessages({
    readxl::read_xlsx(
      path = path,
      sheet = sheet,
      range = range,
      col_names = FALSE,
      col_types = "text",
      na = "",
      progress = FALSE
    )
  })

  # check for read errors
  if (length(out) == 0) {
    return(NA_character_)
  }

  # assign variable names
  if (!is.null(var_names)) {
    names(out) <- var_names
  }

  out
}
