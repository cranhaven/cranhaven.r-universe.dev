#' Converts date columns formatted as characters to be of type datetime
#'
## Copyright(c) 2017-2024 R. Mark Sharp
## This file is part of nprcgenekeepr
#' Part of Pedigree Curation
#'
## ##  rmsutilityr get_and_or_list
## ##  rmsutilityr is_valid_date_str
#'
#' @return A dataframe with an updated table with date columns converted from
#' \code{character} data type to \code{Date} data type. Values that do not
#' conform to the format %Y%m%d are set to NA. NA values are left as NA.
#'
#' @param ped a dataframe of pedigree information that may contain birth,
#' death, departure, or exit dates. The fields are optional, but will be used
#' if present.(optional fields: birth, death, departure, and exit).
#' @param timeOrigin date object used by \code{as.Date} to set \code{origin}.
#' @param reportErrors logical value if TRUE will scan the entire file and
#' make a list of all errors found. The errors will be returned in a
#' list of list where each sublist is a type of error found.
#' @importFrom stringi stri_trim_both stri_c
#' @export
#' @examples
#' library(lubridate)
#' set_seed(10)
#' someBirthDates <- paste0(
#'   sample(seq(0, 15, by = 3), 10,
#'     replace = TRUE
#'   ) + 2000, "-",
#'   sample(1:12, 10, replace = TRUE), "-",
#'   sample(1:28, 10, replace = TRUE)
#' )
#' someBadBirthDates <- paste0(
#'   sample(1:12, 10, replace = TRUE), "-",
#'   sample(1:28, 10, replace = TRUE), "-",
#'   sample(seq(0, 15, by = 3), 10,
#'     replace = TRUE
#'   ) + 2000
#' )
#' someDeathDates <- sample(someBirthDates, length(someBirthDates),
#'   replace = FALSE
#' )
#' someDepartureDates <- sample(someBirthDates, length(someBirthDates),
#'   replace = FALSE
#' )
#' ped1 <- data.frame(
#'   birth = someBadBirthDates, death = someDeathDates,
#'   departure = someDepartureDates
#' )
#' someDates <- ymd(someBirthDates)
#' ped2 <- data.frame(
#'   birth = someDates, death = someDeathDates,
#'   departure = someDepartureDates
#' )
#' ped3 <- data.frame(
#'   birth = someBirthDates, death = someDeathDates,
#'   departure = someDepartureDates
#' )
#' someNADeathDates <- someDeathDates
#' someNADeathDates[c(1, 3, 5)] <- ""
#' someNABirthDates <- someDates
#' someNABirthDates[c(2, 4, 6)] <- NA
#' ped4 <- data.frame(
#'   birth = someNABirthDates, death = someNADeathDates,
#'   departure = someDepartureDates
#' )
#'
#' ## convertDate identifies bad dates
#' result <- tryCatch(
#'   {
#'     convertDate(ped1)
#'   },
#'   warning = function(w) {
#'     print("Warning in date")
#'   },
#'   error = function(e) {
#'     print("Error in date")
#'   }
#' )
#'
#' ## convertDate with error flag returns error list and not an error
#' convertDate(ped1, reportErrors = TRUE)
#'
#' ## convertDate recognizes good dates
#' all(is.Date(convertDate(ped2)$birth))
#' all(is.Date(convertDate(ped3)$birth))
#'
#' ## convertDate handles NA and empty character string values correctly
#' convertDate(ped4)
convertDate <- function(ped, timeOrigin = as.Date("1970-01-01"),
                        reportErrors = FALSE) {
  ## Ignore records added because of unknown parents
  if (any("recordStatus" %in% names(ped))) {
    addedPed <- ped[ped$recordStatus == "added", ]
    ped <- ped[ped$recordStatus == "original", ]
    if (nrow(ped) == 0L) {
      return(rbind(ped, addedPed))
    }
  }

  headers <- tolower(names(ped))
  headers <- headers[headers %in% getDateColNames()]
  format <- "%Y-%m-%d"
  invalid_date_rows <- NULL
  for (header in headers) {
    dates <- ped[[header]]
    if (any(inherits(dates, c("factor", "logical", "integer")))) {
      dates <- as.character(dates)
    }
    if (inherits(dates, "Date")) {
      dates <- removeEarlyDates(dates, 1000L)
      originalNAs <- is.na(dates)
      dates <- dates[!originalNAs]
    } else if (inherits(dates, "character")) {
      dates[!nzchar(stri_trim_both(dates))] <- NA
      ped[[header]] <- dates
      originalNAs <- is.na(dates)
      dates <- dates[!originalNAs]
      if (length(dates) > 0L) {
        dates <- insertSeparators(dates)
        dates <- as.Date(dates,
          format = format, origin = timeOrigin,
          optional = TRUE
        )
        dates <- removeEarlyDates(dates, 1000L)
      }
    } else {
      stop(stri_c(
        "class(dates) is not 'character', 'factor', 'integer', or ",
        "'Date' it is == ", class(dates)
      ))
    }

    if (anyNA(dates)) {
      goodAndBadDates <- ifelse(is.na(dates), "bad", "good")
      originalDates <- as.character(ped[[header]])
      originalDates[originalNAs] <- "good"
      originalDates[!originalNAs] <- goodAndBadDates
      if (reportErrors) {
        invalid_date_rows <- c(
          invalid_date_rows,
          seq_along(originalDates)[originalDates == "bad"]
        )
        next
      }
      rowNums <- get_and_or_list(
        seq_along(originalDates)[originalDates == "bad"], "and"
      )
      stop(
        "Column '", header, "' has invalid dates on row(s) ",
        rowNums, "."
      )
    }
    ped[!originalNAs, header] <- dates
    ped[originalNAs, header] <- NA # For those NAs from dates <= 1000 CE
    ped[[header]] <- as.Date(as.integer(ped[[header]]), origin = timeOrigin)
  }
  if (reportErrors) {
    if (!is.null(invalid_date_rows)) {
      invalid_date_rows <- as.character(sort(invalid_date_rows))
    }
    return(invalid_date_rows)
  } else {
    ## Add back records of unknown parents
    if (any("recordStatus" %in% names(ped))) {
      ped <- rbind(ped, addedPed)
    }
    return(ped)
  }
}
