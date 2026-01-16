# Define globals ####
`%>%` <- magrittr::`%>%`
ignore_case <- TRUE
perl <- TRUE


# Functions ####

## trades_clean_entries ####

#' Mutate operation(s) in Scottish post office trades directory data.frame
#'   column(s)
#'
#' Attempts to clean entries of the provided Scottish post office trades
#'   directory data.frame.
#'
#' @param directory A Scottish post office trades directory in the form
#'   of a data.frame or other object that inherits from the data.frame class
#'   such as a \code{\link[tibble]{tibble}}. Columns must at least include
#'   `forename`, `surname`, `occupation`, `address.trade.number`,
#'   `address.trade.body`.
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#'
#' @return A data.frame of the same class as the one provided in `directory`;
#'   columns include the same as those in the data.frame provided in
#'   `directory`. Entries are cleaned of optical character recognition (OCR)
#'   errors and subject to a number of standardisation operations.
trades_clean_entries <- function(directory, verbose){

  clean <- function(...){
    # Trim extra white spaces ####
    utils_squish_all_columns(directory) %>%

    # Get rid of irrelevant info ####
      utils_clear_irrelevants(globals_regex_irrelevants, ignore_case) %>%

    # Clean occupations
      utils_clean_occupations() %>%

    # Clean names ####
      utils_clean_names() %>%

    # Clean addresses ####
      utils_clean_addresses()
  }

  utils_execute(verbose, clean, directory, ignore_case)
}


## trades_clean_directory_plain ####

#' Mutate operation(s) in Scottish post office trades directory data.frame
#'   column(s)
#'
#' Attempts to clean the provided Scottish post office trades directory
#'   data.frame.
#'
#' @param directory A Scottish post office trades directory in the form
#'   of a data.frame or other object that inherits from the data.frame class
#'   such as a \code{\link[tibble]{tibble}}. Columns must at least include
#'   `forename`, `surname`, `occupation``address.trade.number` and
#'   `address.trade.body`.
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#'
#' @return A data.frame of the same class as the one provided in `directory`;
#'   columns include at least `forename`, `surname`, `occupation`,
#'   `address.trade.number` and `address.trade.body`. Entries are cleaned of
#'   optical character recognition (OCR) errors and subject to a number of
#'   standardisation operations.
trades_clean_directory_plain <- function(directory, verbose){

  clean <- function(...){
    ## Clean entries ####
    trades_clean_entries(directory, verbose = verbose)
  }

  utils_execute(verbose, clean, directory = directory)
}


## trades_clean_directory_progress ####

#' Mutate operation(s) in Scottish post office trades directory data.frame
#'   column(s)
#'
#' Attempts to clean the provided Scottish post office trades directory
#'   data.frame. Shows a progress bar indicating function progression.
#'
#' @param directory A Scottish post office trades directory in the form
#'   of a data.frame or other object that inherits from the data.frame class
#'   such as a \code{\link[tibble]{tibble}}. Columns must at least include
#'   `forename`, `surname`, `occupation``address.trade.number` and
#'   `address.trade.body`.
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#'
#' @return A data.frame of the same class as the one provided in `directory`;
#'   columns include at least `forename`, `surname`, `occupation`,
#'   `address.trade.number` and `address.trade.body`. Entries are cleaned of
#'   optical character recognition (OCR) errors and subject to a number of
#'   standardisation operations.
trades_clean_directory_progress <- function(directory, verbose){

  directory_split <- split(directory, (1L:nrow(directory) %/% 500L))

  pb <- progress::progress_bar$new(
    format = "  cleaning records [:bar] :percent eta: :eta",
    total = length(directory_split), clear = FALSE, width = 100L
  )

  purrr::map_dfr(directory_split, function(sample) {
    pb$tick(); trades_clean_directory_plain(sample, verbose)
  })
}


## trades_clean_directory ####

#' Mutate operation(s) in Scottish post office trades directory data.frame
#'   column(s)
#'
#' Attempts to clean the provided Scottish post office trades directory
#'   data.frame.
#'
#' @param directory A Scottish post office trades directory in the form
#'   of a data.frame or other object that inherits from the data.frame class
#'   such as a \code{\link[tibble]{tibble}}. Columns must at least include
#'   `forename`, `surname`, `occupation``address.trade.number` and
#'   `address.trade.body`.
#' @param progress Whether progress should be shown (`TRUE`) or not (`FALSE`).
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#'
#' @return A data.frame of the same class as the one provided in `directory`;
#'   columns include at least `forename`, `surname`, `occupation`,
#'   `address.trade.number` and `address.trade.body`. Entries are cleaned of
#'   optical character recognition (OCR) errors and subject to a number of
#'   standardisation operations.
#'
#' @examples
#' pages <- rep("71", 2L)
#' ranks <- c("135", "326")
#' surnames <- c("ABOT", "ABRCROMBIE")
#' forenames <- c("Wm.", "Alex")
#' occupations <- c(
#'   "Wine and spirit mercht - See Advertisement in Appendix.", "Bkr"
#' )
#' types <- rep("OWN ACCOUNT", 2L)
#' numbers <- c("1S20", "I2")
#' bodies <- c("Londn rd.", "Dixen pl")
#' directory <- tibble::tibble(
#'   page = pages, rank = ranks, surname = surnames, forename = forenames,
#'   occupation = occupations, type = types,
#'   address.trade.number = numbers, address.trade.body = bodies
#' )
#' trades_clean_directory(directory, progress = TRUE, verbose = FALSE)
#'
#' @export
trades_clean_directory <- function(directory, progress = TRUE, verbose = FALSE){

  out <- if (progress) trades_clean_directory_progress(directory, verbose)
  else trades_clean_directory_plain(directory, verbose)

  dplyr::distinct(tibble::tibble(out))
}
