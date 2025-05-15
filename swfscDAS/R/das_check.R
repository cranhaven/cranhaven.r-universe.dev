#' Check DAS file
#'
#' Check that DAS file has accepted formatting and values
#'
#' @param file filename(s) of one or more DAS files
#' @param skip integer: see \code{\link[readr]{read_fwf}}. Default is 0
#' @param file.out filename to which to write the error log;
#'   default is \code{NULL}
#' @param sp.codes character; filename of .dat file from which to read
#'   accepted species codes. If \code{NULL}, species codes will not be checked.
#'   Default is \code{NULL}
#' @param print.cruise.nums logical; indicates if a table with all the
#'   cruise numbers in the \code{x} should be printed using
#'   \code{\link[base]{table}}. Default is \code{TRUE}
#'
#' @details
#' Precursor to a more comprehensive DASCHECK program.
#' This function checks that the following is true:
#' \itemize{
#'   \item Event codes are one of the following: #, *, ?, 1, 2, 3, 4, 5, 6, 7, 8,
#'     A, B, C, E, F, k, K, N, P, Q, r, R, s, S, t, V, W, g, G, p, X, Y, Z
#'   \item Latitude values are between -90 and 90 (inclusive; NA values are ignored)
#'   \item Longitude values are between -180 and 180 (inclusive; NA values are ignored)
#'   \item The effort dot matches effort determined using B, R, and E events
#'   \item There are an equal number of R and E events, and they alternate occurrences
#'   \item A BR event series or R event does not occur while already on effort
#'   \item An E event does not occur while already off effort
#'   \item All Data# columns for non-C events are right-justified
#'   \item Only C events have data past the 99th column in the DAS file
#'   \item The following events have NA (blank) Data# columns: *
#'   \item All of  *, B, R, E, V, W, N, P, and Q events have NA Data# columns
#'     where specified (see format pdf for more details)
#'   \item Event/column pairs meet the following requirements:
#' }
#'
#' \tabular{llll}{
#'   \emph{Item}              \tab \emph{Event}  \tab \emph{Column}  \tab \emph{Requirement}                                                        \cr
#'   Cruise number            \tab B             \tab Data1          \tab Can be converted to a numeric value                                       \cr
#'   Mode                     \tab B             \tab Data2          \tab Must be one of C, P, c, p, or NA (blank)                                  \cr
#'   Echo sounder             \tab B             \tab Data4          \tab Must be one of Y, N, y, n, or NA (blank)                                  \cr
#'   Effort type              \tab R             \tab Data1          \tab Must be one of F, N, S, or NA (blank)                                     \cr
#'   ESW sides                \tab R             \tab Data2          \tab Effective strip width; must be one of F, H, or NA (blank)                 \cr
#'   Course                   \tab N             \tab Data1          \tab Can be converted to a numeric value                                       \cr
#'   Speed                    \tab N             \tab Data2          \tab Can be converted to a numeric value                                       \cr
#'   Beaufort                 \tab V             \tab Data1          \tab Must be a whole number between 0 and 9                                    \cr
#'   Swell height             \tab V             \tab Data2          \tab Can be converted to a numeric value                                       \cr
#'   Wind speed               \tab V             \tab Data5          \tab Can be converted to a numeric value                                       \cr
#'   Rain or fog              \tab W             \tab Data1          \tab Must be between 0 and 5 and either a whole number or have decimal value .5\cr
#'   Horizontal sun           \tab W             \tab Data2          \tab Must be a whole number between 0 and 12                                   \cr
#'   Vertical sun             \tab W             \tab Data3          \tab Must be a whole number between 0 and 12                                   \cr
#'   Visibility               \tab W             \tab Data5          \tab Can be converted to a numeric value                                       \cr
#'   Sighting (mammal)        \tab S, K, M       \tab Data3-7        \tab Can be converted to a numeric value                                       \cr
#'   Sighting (mammal)        \tab G             \tab Data5-7        \tab Can be converted to a numeric value                                       \cr
#'   Sighting cue (mammal)    \tab S, K, M       \tab Data3          \tab Must be a whole number between 1 and 6                                    \cr
#'   Sighting method (mammal) \tab S, K, M, G    \tab Data4          \tab Must be a whole number between 1 and 7                                    \cr
#'   Bearing (mammal)         \tab S, K, M, G    \tab Data5          \tab Must be a whole number between 0 and 360                                  \cr
#'   Photos                   \tab A             \tab Data3          \tab Must be one of N, Y, n, y, or NA (blank)                                  \cr
#'   Birds                    \tab A             \tab Data4          \tab Must be one of N, Y, n, y, or NA (blank)                                  \cr
#'   Calibration school       \tab S, K, M       \tab Data10         \tab Must be one of N, Y, n, y, or NA (blank)                                  \cr
#'   Aerial photos taken      \tab S, K, M       \tab Data11         \tab Must be one of N, Y, n, y, or NA (blank)                                  \cr
#'   Biopsy taken             \tab S, K, M       \tab Data12         \tab Must be one of N, Y, n, y, or NA (blank)                                  \cr
#'   Species codes            \tab A             \tab Data5-8        \tab If a species codes file is provided, must be one of the provided codes    \cr
#'   Resight                  \tab s, k          \tab Data2-5        \tab Can be converted to a numeric value                                       \cr
#'   Turtle species           \tab t             \tab Data2          \tab If a species codes file is provided, must be one of the provided codes    \cr
#'   Turtle sighting          \tab t             \tab Data3-5, 7     \tab Can be converted to a numeric value                                       \cr
#'   Turtle JFR               \tab t             \tab Data6          \tab Must be one of F, J, N, R, or NA (blank)                                  \cr
#'   Fishing vessel           \tab F             \tab Data2-4        \tab Can be converted to a numeric value                                       \cr
#'   Sighting info            \tab 1-8           \tab Data2-8        \tab Can be converted to a numeric value                                       \cr
#'   Sighting info            \tab 1-8           \tab Data9          \tab The Data9 column must be NA (blank) for events 1-8
#' }
#'
#' In the table above, 'between' means inclusive.
#'
#' Long-term items, and checks that are not performed:
#' \itemize{
#'   \item Check that datetimes are sequential, meaning they
#'     1) are the same as or 2) come after the previous event
#'   \item Check that A events only come immediately after a G/S/K/M event,
#'     and all G/S/K/M events have an A after them.
#'     And that each has at least one group size estimate (1:8 event)
#' }
#'
#' @return
#' A data frame with columns: the file name, line number, cruise number,
#' 'ID' (columns 4-39 from the DAS file), and description of the issue
#'
#' If \code{file.out} is not \code{NULL}, then the error log data frame is also
#' written to \code{file.out} using \code{\link[utils:write.table]{write.csv}}
#'
#' A warning is printed if any events are r events; see \code{\link{das_process}} for details about r events
#'
#' @examples
#' y <- system.file("das_sample.das", package = "swfscDAS")
#' if (interactive()) das_check(y)
#'
#' @export
das_check <- function(file, skip = 0, file.out = NULL, sp.codes = NULL,
                      print.cruise.nums = TRUE) {

  if (length(unique(file)) != length(file))
    warning("Not all files are unique - this likely will cause an error in ",
            "airdas_check. Please ensure all files are unique.",
            immediate. = TRUE)

  error.out <- data.frame(
    File = NA, LineNum = NA, CruiseNum = NA, ID = NA, Description = NA,
    stringsAsFactors = FALSE
  )

  message("Reading DAS file")
  x <- suppressWarnings(das_read(file, skip = skip))
  x$idx <- seq_along(x$Event)
  x <- as_das_dfr(x)

  x.lines.list <- lapply(file, function(i) {
    if (skip > 0) readLines(i)[-c(1:skip)] else readLines(i)
  })
  x.lines <- substr(do.call(c, x.lines.list), 4, 39)

  message("Processing DAS file")
  x.proc <- suppressWarnings(das_process(x)) %>%
    left_join(select(x, .data$file_das, .data$line_num, .data$idx),
              by = c('file_das', "line_num"))
  x.proc <- as_das_df(x.proc)


  if ((nrow(x) != length(x.lines)) | (nrow(x) < nrow(x.proc)))
    stop("Error reading and processing DAS files. ",
         "Please try checking only a single file, or contact the developer")

  rm(x.lines.list)


  #----------------------------------------------------------------------------
  ### Process sp.codes file
  if (!is.null(sp.codes)) {
    message("Reading and processing SpCodes file")
    sp.acc.df <- read_fwf(
      sp.codes,
      col_positions = fwf_positions(start = c(1, 6, 18, 58), end = c(4, 15, 57, NA)),
      col_types = cols(.default = col_character()),
      trim_ws = TRUE, skip = 0, skip_empty_rows = FALSE
    )

    sp.acc <- sp.acc.df[[1]]
    sp.acc.all <- c(sp.acc, tolower(sp.acc))

    if (!all(nchar(sp.acc) %in% 2:3))
      warning("Some species codes from sp.codes are not two or three charcters. ",
              "Did you load the correct species code .dat file?",
              immediate. = TRUE)
  }


  #----------------------------------------------------------------------------
  message("Checking DAS file")

  ### Check event codes
  event.acc <- c("#", "*", "?", 1:8, "A", "B", "C", "E", "F", "k", "K", "M",
                 "N", "P", "Q","r", "R", "s", "S", "t", "V", "W",
                 "G", "g", "p", "X", "Y", "Z")
  ev.which <- which(!(x$Event %in% event.acc))
  error.out <- rbind(
    error.out,
    list(x$file_das[ev.which], x$line_num[ev.which], ev.which,
         x.lines[ev.which],
         rep("The event code is not recognized", length(ev.which)))
  )

  if (any(x$Event == "r"))
    warning("The provided file contains 'r' events. Is this on purpose?",
            immediate. = TRUE)


  ### Check lat/lon coordinates - NA events are ignored
  # x.proc.ll <- x.proc %>% filter(!(Event %in% c("?", 1:8)))
  lat.which <- which(!between(x.proc$Lat, -90, 90))
  lon.which <- which(!between(x.proc$Lat, -180, 1800))

  error.out <- rbind(
    error.out,
    .check_list(x.proc, x.lines, lat.which, "The latitude value is not between -90 and 90"),
    .check_list(x.proc, x.lines, lon.which, "The longitude value is not between -180 and 180")
  )


  #----------------------------------------------------------------------------
  ### Check that effort dot matches effort determined by B/R to E events
  x.proc.no1 <- x.proc[!(x.proc$Event %in% c("?", 1:8, "#", "C")), ]
  edot.which <- x.proc.no1$idx[(x.proc.no1$OnEffort != x.proc.no1$EffortDot)]

  error.out <- rbind(
    error.out,
    .check_list(x.proc, x.lines, edot.which,
                "Effort dot does not match B/R to E effort (for non-C events)")
  )
  rm(x.proc.no1)


  #----------------------------------------------------------------------------
  ### Check:
  ndx.B <- which(x$Event == "B")
  ndx.R <- which(x$Event == "R")
  ndx.E <- which(x$Event == "E")

  # 1) That there are equal number of R and E events, and they are alternating
  if (length(ndx.E) != length(ndx.R)) {
    error.out <- rbind(
      error.out,
      list(NA, NA, NA, NA,
           paste("Error: There are not an equal number of 'R' and 'E'",
                 "events in the provided DAS file - check this line"))
    )

  } else if (!all(ndx.E - ndx.R > 0) | !all(head(ndx.E, -1) < ndx.R[-1])) {
    er.which <- which(
      !all(ndx.E - ndx.R > 0) | !all(head(ndx.E, -1) < ndx.R[-1])
    )

    error.out <- rbind(
      error.out,
      .check_list(x.proc, x.lines, er.which,
                  paste("Error: There are an equal number of R and E events,",
                        "but not all 'R' events are followed by 'E' events"))
    )

  }

  # Create data frame with prev_columns
  x.proc.prev <- x.proc %>%
    mutate(Event_prev = lag(.data$Event),
           OnEffort_prev = lag(.data$OnEffort))

  # 2) All R events occur while off effort, or after a B event that occurs while off effort
  br.r.which <- x.proc.prev %>%
    filter((.data$Event == "R" & .data$OnEffort_prev & .data$Event_prev != "B") |
             (.data$Event == "B" & .data$OnEffort_prev)) %>%
    select(.data$idx) %>% unlist() %>% unname()

  # 3) All E events occur while on effort
  e.which <- x.proc.prev %>%
    filter(.data$Event == "E" & !.data$OnEffort_prev) %>%
    select(.data$idx) %>% unlist() %>% unname()


  error.out <- rbind(
    error.out,
    .check_list(x.proc, x.lines, br.r.which,
                "There is an R event (or BR event series) while already on effort"),
    .check_list(x.proc, x.lines, e.which,
                "There is an E event while already off effort")
  )
  rm(x.proc.prev)


  #----------------------------------------------------------------------------
  ### Check that Data# columns are right-justifed for selected events,
  ###   and that there is no extra data in these rows
  # * events should have no data - this is checked later
  x.tmp.filt <- data.frame(
    Event = substr(x.lines, 4, 4),
    Data1 = substr(x.lines, 40, 44),
    Data2 = substr(x.lines, 45, 49),
    Data3 = substr(x.lines, 50, 54),
    Data4 = substr(x.lines, 55, 59),
    Data5 = substr(x.lines, 60, 64),
    Data6 = substr(x.lines, 65, 69),
    Data7 = substr(x.lines, 70, 74),
    Data8 = substr(x.lines, 75, 79),
    Data9 = substr(x.lines, 80, 84),
    Data10 = substr(x.lines, 85, 89),
    Data11 = substr(x.lines, 90, 94),
    Data12 = substr(x.lines, 95, 99),
    Extra_data = substr(x.lines, 100, max(nchar(x.lines))),
    idx = seq_along(x.lines),
    stringsAsFactors = FALSE
  ) %>%
    filter(!(.data$Event %in% c("C", "*", "#"))) %>%
    mutate(Extra_data = trimws(.data$Extra_data, which = "both"))

  x.tmp.filt.data <- x.tmp.filt %>% select(starts_with("Data"))

  x.tmp.which <- lapply(1:ncol(x.tmp.filt.data), function(i) {
    x1 <- trimws(x.tmp.filt.data[[i]], which = "left")
    x2 <- trimws(x.tmp.filt.data[[i]], which = "both")
    which(x1 != x2 )
  })

  rj.which <- x.tmp.filt$idx[sort(unique(unlist(x.tmp.which)))]
  rj.extra.which <- x.tmp.filt$idx[x.tmp.filt$Extra_data != ""]


  error.out <- rbind(
    error.out,
    .check_list(x.proc, x.lines, rj.which, "Data column(s) are not right-justified"),
    .check_list(x.proc, x.lines, rj.extra.which, "Row contains information past the 84th column")
  )

  rm(x.tmp.filt, x.tmp.filt.data, x.tmp.which)


  #----------------------------------------------------------------------------
  ### Print cruise numbers
  if (print.cruise.nums) {
    cat("Cruise numbers:", collapse = "")
    print(table(x$Data1[x$Event == "B"], useNA = "always"))
  }


  #----------------------------------------------------------------------------
  ### Check that (non-sighting) Data# columns that should be blank are blank

  # "*" events
  idx.star.na <- .check_isna(x, c("*"), paste0("Data", 1:9))
  txt.star.na <- "* events should have no data in the Data# columns"

  # E events
  idx.e.na <- .check_isna(x, c("E"), paste0("Data", 2:9))
  txt.e.na <- "E events should only have data in the Data1 column"

  # R events
  idx.r.na <- .check_isna(x, c("R"), paste0("Data", 3:9))
  txt.r.na <- "R events should only have data in the Data1-2 columns"

  # N events
  idx.n.na <- .check_isna(x, c("N"), paste0("Data", 4:9))
  txt.n.na <- "N events should only have data in the Data1-3 columns"

  # B, P, and Q events
  idx.bpq.na <- .check_isna(x, c("B", "P", "Q"), paste0("Data", 5:9))
  txt.bpq.na <- "B, P, and Q events should only have data in the Data1-4 columns"

  # W events
  idx.w.na <- .check_isna(x, c("W"), paste0("Data", 6:9))
  txt.w.na <- "W events should only have data in the Data1-5 columns"

  # V events
  idx.v.na <- .check_isna(x, c("V"), paste0("Data", 7:9))
  txt.v.na <- "V events should only have data in the Data1-6 columns"


  error.out <- rbind(
    error.out,
    .check_list(x.proc, x.lines, idx.star.na, txt.star.na),
    .check_list(x.proc, x.lines, idx.e.na, txt.e.na),
    .check_list(x.proc, x.lines, idx.r.na, txt.r.na),
    .check_list(x.proc, x.lines, idx.n.na, txt.n.na),
    .check_list(x.proc, x.lines, idx.bpq.na, txt.bpq.na),
    .check_list(x.proc, x.lines, idx.w.na, txt.w.na),
    .check_list(x.proc, x.lines, idx.v.na, txt.v.na)
  )


  #----------------------------------------------------------------------------
  ### Check that value type of values in Data# columns are as expected for
  ###   columns that are added in das_process

  # Variables are code named as "z".event code'.'data# column' for
  #   line numbers with weird info,
  #   and "txt".event code'.'data# column' for the txt to go in error.out

  # Cruise number
  idx.b.1 <- .check_numeric(x, "B", "Data1")
  txt.b.1 <- "Cruise number (Data1 of B events) cannot be converted to a numeric"

  # Mode
  idx.b.2 <- .check_character(x, "B", "Data2", c("C", "P", "c", "p", NA))
  txt.b.2 <- "Effort type (Data2 of B events) is not one of C, P, c, p, or NA"

  # Echo sounder
  idx.b.4 <- .check_character(x, "B", "Data4", c("Y", "y", "N", "n", NA))
  txt.b.4 <- "Echo sounder (Data4 of B events) is not one of Y, y, N, n, or NA"

  # Effort type
  idx.r.1 <- .check_character(x, "R", "Data1", c("F", "N", "S", NA))
  txt.r.1 <- "Effort type (Data1 of R events) is not one of F, N, S, or NA"

  # Effective strip width sides
  idx.r.2 <- .check_character(x, "R", "Data2", c("F", "H", NA))
  txt.r.2 <- "Effective strip width sides (Data2 of R events) is not one of F, H, or NA"

  # Course
  idx.n.1 <- .check_numeric(x, "N", "Data1")
  txt.n.1 <- "Ship course (Data1 of N events) cannot be converted to a numeric"

  # Speed
  idx.n.2 <- .check_numeric(x, "N", "Data2")
  txt.n.2 <- "Ship speed (Data1 of N events) cannot be converted to a numeric"

  # Beaufort
  bft.acc <- c(0:9, sprintf("%02d", 0:9), NA)
  idx.v.1 <- .check_character(x, "V", "Data1", bft.acc)
  txt.v.1 <- "Beaufort (Data1 of V events) must be a whole number between 0 and 9"

  # Swell Height
  idx.v.2 <- .check_numeric(x, "V", "Data2")
  txt.v.2 <- "Swell height (Data2 of V events) cannot be converted to a numeric"

  # Wind speed
  idx.v.5 <- .check_numeric(x, "V", "Data5")
  txt.v.5 <- "Wind speed (Data5 of V events) cannot be converted to a numeric"

  # RainFog
  rf.acc <- c(seq(0, 5, by = 0.5), sprintf("%02d", 1:5), NA)
  idx.w.1 <- .check_character(x, "W", "Data1", rf.acc)
  txt.w.1 <- "Rain/fog (Data1 of W events) is not a whole number between 0 and 5, or a '#.5'"

  # Horizontal sun
  acc.sun <- c(0:12, sprintf("%02d", 0:9), NA)
  idx.w.2 <- .check_character(x, "W", "Data2", acc.sun)
  txt.w.2 <- "Horizontal sun (Data2 of W events) is not a whole number between 0 and 12"

  # Vertical sun
  idx.w.3 <- .check_character(x, "W", "Data3", acc.sun)
  txt.w.3 <- "Vertical sun (Data3 of W events) is not a whole number between 0 and 12"

  # Visibility
  idx.w.5 <- .check_numeric(x, "W", "Data5")
  txt.w.5 <- "Visibility (Data5 of W events) cannot be converted to a numeric"


  # Add text to error.out as needed and return
  error.out <- rbind(
    error.out,
    .check_list(x.proc, x.lines, idx.b.1, txt.b.1),
    .check_list(x.proc, x.lines, idx.b.2, txt.b.2),
    .check_list(x.proc, x.lines, idx.b.4, txt.b.4),
    .check_list(x.proc, x.lines, idx.r.1, txt.r.1),
    .check_list(x.proc, x.lines, idx.r.2, txt.r.2),
    .check_list(x.proc, x.lines, idx.n.1, txt.n.1),
    .check_list(x.proc, x.lines, idx.n.2, txt.n.2),
    .check_list(x.proc, x.lines, idx.v.1, txt.v.1),
    .check_list(x.proc, x.lines, idx.v.2, txt.v.2),
    .check_list(x.proc, x.lines, idx.v.5, txt.v.5),
    .check_list(x.proc, x.lines, idx.w.1, txt.w.1),
    .check_list(x.proc, x.lines, idx.w.2, txt.w.2),
    .check_list(x.proc, x.lines, idx.w.3, txt.w.3),
    .check_list(x.proc, x.lines, idx.w.5, txt.w.5)
  )


  #----------------------------------------------------------------------------
  ### Check Data# columns for sightings data format
  # Marine mammal sightings (SKMG)
  idx.skm.num <- .check_numeric(x, c("S", "K", "M"), paste0("Data", 3:7)) #3:9
  txt.skm.num <- paste(
    "At least one of the Data3-7 columns for S, K, and M events",
    "cannot be converted to a numeric"
  )

  idx.g.num <- .check_numeric(x, c("G"), paste0("Data", 5:7))
  txt.g.num <- paste(
    "At least one of the Data5-7 columns for G events",
    "cannot be converted to a numeric"
  )

  idx.skm.3 <- .check_character(x, c("S", "K", "M"), "Data3", c(1:6, NA))
  txt.skm.3 <- "Sighting cue (Data3 of S/K/M events) must be a whole number between 1 and 6"

  idx.skm.4 <- .check_character(x, c("S", "K", "M", "G"), "Data4", c(1:7, NA))
  txt.skm.4 <- "Sighting method (Data4 of S/K/M events) must be a whole number between 1 and 7"

  bearing.acc <- c(0:360, sprintf("%02d", 0:360), sprintf("%03d", 0:360))
  idx.skm.b <- .check_character(x, c("S", "K", "M", "G"), "Data5", c(bearing.acc, NA))
  txt.skm.b <- "Bearing (Data5 of S/K/M/G events) is not a whole number between 0 and 360, or NA"

  idx.skm.10 <- .check_character(x, c("S", "K", "M"), "Data10", c("N", "Y", "n", "y", NA))
  txt.skm.10 <- "Calibration school (Data10 of S/K/M events) is not one of N, Y, n, y, or NA"

  idx.skm.11 <- .check_character(x, c("S", "K", "M"), "Data11", c("N", "Y", "n", "y", NA))
  txt.skm.11 <- "Aerial photos (Data11 of S/K/M events) is not one of N, Y, n, y, or NA"

  idx.skm.12 <- .check_character(x, c("S", "K", "M"), "Data12", c("N", "Y", "n", "y", NA))
  txt.skm.12 <- "Biopsy (Data12 of S/K/M events) is not one of N, Y, n, y, or NA"


  # Auxillary info (A)
  idx.a.3 <- .check_character(x, "A", "Data3", c("N", "Y", "n", "y", NA))
  txt.a.3 <- "Photos (Data3 of A events) is not one of N, Y, n, y, or NA"

  idx.a.4 <- .check_character(x, "A", "Data4", c("N", "Y", "n", "y", NA))
  txt.a.4 <- "Birds (Data4 of A events) is not one of N, Y, n, y, or NA"

  txt.a.spcode <- "Species code(s) were not one of the accepted codes provided via sp.codes"
  idx.a.spcode <- if (is.null(sp.codes)) {
    integer(0)
  } else {
    .check_character(x, "A", paste0("Data", 5:8), c(sp.acc.all, NA))
  }

  # Resights (s and k)
  idx.res.num <- .check_numeric(x, c("s", "k"), paste0("Data", 2:5))
  txt.res.num <- paste(
    "At least one of the Data2-5 columns for s and k events",
    "cannot be converted to a numeric"
  )

  # Turtle
  txt.t.spcode <- "Turtle species code was not one of the accepted codes provided via sp.codes"
  idx.t.spcode <- if (is.null(sp.codes)) {
    integer(0)
  } else {
    .check_character(x, "t", paste0("Data", 2), c(sp.acc.all, NA))
  }

  idx.t.num <- .check_numeric(x, "t", paste0("Data", c(3:5, 7)))
  txt.t.num <- paste(
    "At least one of the Data3-5/Data7 columns for t events",
    "cannot be converted to a numeric"
  )

  idx.t.6 <- .check_character(x, "t", "Data6", c("F", "J", "N", "R", NA))
  txt.t.6 <- "Associated JFR (Data6 of t events) must be one of F, J, N, R, or NA"

  # Fishing boat
  idx.f.num <- .check_numeric(x, "F", paste0("Data", 2:4))
  txt.f.num <- paste(
    "At least one of the Data2-4 columns for F events",
    "cannot be converted to a numeric"
  )

  # Numeric events (1-8)
  idx.num.num <- .check_numeric(x, 1:8, paste0("Data", 2:8))
  txt.num.num <- paste(
    "At least one of the Data2-8 columns for 1-8 events",
    "cannot be converted to a numeric"
  )

  idx.num.9 <- .check_character(x, 1:8, "Data9", c(NA))
  txt.num.9 <- "The data9 column for 1-8 events is not NA (blank)"


  # Add to error.out
  error.out <- rbind(
    error.out,
    .check_list(x.proc, x.lines, idx.skm.num, txt.skm.num),
    .check_list(x.proc, x.lines, idx.g.num, txt.g.num),
    .check_list(x.proc, x.lines, idx.skm.3, txt.skm.3),
    .check_list(x.proc, x.lines, idx.skm.4, txt.skm.4),
    .check_list(x.proc, x.lines, idx.skm.b, txt.skm.b),
    .check_list(x.proc, x.lines, idx.skm.10, txt.skm.10),
    .check_list(x.proc, x.lines, idx.skm.11, txt.skm.11),
    .check_list(x.proc, x.lines, idx.skm.12, txt.skm.12),
    .check_list(x.proc, x.lines, idx.res.num, txt.res.num),
    .check_list(x.proc, x.lines, idx.f.num, txt.f.num),
    .check_list(x.proc, x.lines, idx.num.num, txt.num.num),
    .check_list(x.proc, x.lines, idx.a.3, txt.a.3),
    .check_list(x.proc, x.lines, idx.a.4, txt.a.4),
    .check_list(x.proc, x.lines, idx.a.spcode, txt.a.spcode),
    .check_list(x.proc, x.lines, idx.t.spcode, txt.t.spcode),
    .check_list(x.proc, x.lines, idx.t.num, txt.t.num),
    .check_list(x.proc, x.lines, idx.t.6, txt.t.6),
    .check_list(x.proc, x.lines, idx.num.9, txt.num.9)
  )


  #----------------------------------------------------------------------------
  # Remove first line and return
  if (nrow(error.out) == 1) {
    to.return <- data.frame(
      File = NA, LineNum = NA, CruiseNum = NA, ID = NA,
      Description = "No errors found",
      stringsAsFactors = FALSE
    )
  } else {
    to.return <- error.out[-1, ]
  }
  row.names(to.return) <- NULL

  if (!is.null(file.out)) {
    csv.try <- try(write.csv(to.return, file = file.out), silent = TRUE)
    if (inherits(csv.try, "try-error"))
      warning("The DAS check file was not written to the output file")
  }

  to.return
}
