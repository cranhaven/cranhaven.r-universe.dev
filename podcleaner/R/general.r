# Define globals ####
`%>%` <- magrittr::`%>%`
ignore_case <- TRUE
perl <- TRUE


# Fix structure ####

## general_move_house_to_address ####

#' Mutate operation(s) in Scottish post office general directory data.frame
#'   column(s)
#'
#' For some raw Scottish post office general directory entries, the word "house"
#'   referring to address type lives in the occupation column as a result of
#'   parsing errors. `general_move_house_to_address` attempts to move this
#'   information to the appropriate destination: the `addresses` column.
#'
#' @param directory A Scottish post office general directory in the form
#'   of a data.frame or other object that inherits from the data.frame class
#'   such as a \code{\link[tibble]{tibble}}. Columns must at least include
#'   `occupation` and  `addresses`.
#' @param regex Regex to use for the task provided as a character string.
#'
#' @return A data.frame of the same class as the one provided in `directory`;
#'   columns include at least `occupation` and  `addresses`. Entries in the
#'   `occupation` column are cleaned of "house" suffix; entries showing "house"
#'   suffix in `occupation` column see "house, " pasted as prefix to
#'   corresponding `addresses` column content.
general_move_house_to_address <- function(directory, regex){
  addresses <- occupation <- NULL
  dplyr::mutate(
    directory,
    addresses = utils_paste_if_found(
      regex, occupation, addresses, ignore_case, "house", addresses, sep = ", "
    ),
    occupation = utils_gsub_if_found(
      regex, occupation, regex, "", occupation, occupation, ignore_case, ignore_case
    )
  ) %>% utils_clean_address(type = "ends")
}


## general_repatriate_occupation_from_address ####

#' Mutate operation(s) in Scottish post office general directory data.frame
#'   column(s)
#'
#' For some raw Scottish post office general directory entries occupation
#'   information lives in the `addresses` column as a result of parsing errors.
#'   `general_repatriate_occupation_from_address` attempts to move this
#'   information to the appropriate destination: the `occupation` column.
#'
#' @param directory A Scottish post office general directory in the form
#'   of a data.frame or other object that inherits from the data.frame class
#'   such as a \code{\link[tibble]{tibble}}. Columns must at least include
#'   `occupation` and `addresses`.
#' @param regex Regex to use for the task provided as a character string.
#'
#' @return A data.frame of the same class as the one provided in `directory`;
#'   columns include at least `occupation` and `addresses`.
general_repatriate_occupation_from_address <- function(directory, regex){
  addresses <- occupation <- NULL
  dplyr::mutate(
    directory,
    occupation = utils_paste_if_found(
      regex, addresses, occupation, ignore_case, occupation,
      regmatches(
        addresses, gregexpr(regex, addresses, ignore.case = TRUE, perl)
      ),
      sep = " "
    ),
    addresses = utils_gsub_if_found(
      regex, addresses, regex, "", addresses, addresses, ignore_case, ignore_case
    ),
    dplyr::across(.cols = c(occupation, addresses), .fns = clean_string_ends)
  )
}


## general_split_trade_house_addresses ####

#' Mutate operation(s) in Scottish post office general directory data.frame
#'   column(s)
#'
#' Attempts to separate house address from trade address(es) in the Scottish
#'   post office general directory data.frame provided for entries for which a
#'   house address is provided along trade address(es).
#'
#' @param directory A Scottish post office general directory in the form
#'   of a data.frame or other object that inherits from the data.frame class
#'   such as a \code{\link[tibble]{tibble}}. Columns must at least include
#'   `addresses`.
#' @param regex Regex to use for the task provided as a character string.
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#'
#' @return A data.frame of the same class as the one provided in `directory`;
#'   columns include at least `addresses.trade` and `address.house`. Trade
#'   addresses are separated from house address for entries for which a house
#'   address is provided along trade address(es).
general_split_trade_house_addresses <- function(directory, regex, verbose){

  load <- function(...){
    addresses <- address.house <- NULL
    dplyr::mutate(
      directory,
      addresses = utils_clean_address_number(addresses)
    ) %>%
      tidyr::separate(
        col = addresses,
        into = c("addresses.trade", "address.house"),
        sep = regex, remove = TRUE, extra = "merge"
      ) %>%
      dplyr::mutate(
        address.house = ifelse(is.na(address.house), "", address.house)
      ) %>% utils_clean_address(type = "ends")
  }

  utils_execute(verbose, load, directory, regex)
}


## general_split_trade_addresses ####

#' Mutate operation(s) in Scottish post office general directory data.frame
#'   column(s)
#'
#' Attempts to separate multiple trade addresses in the Scottish
#'   post office general directory data.frame provided for entries for which
#'   more than one are provided.
#'
#' @param directory A Scottish post office general directory in the form
#'   of a data.frame or other object that inherits from the data.frame class
#'   such as a \code{\link[tibble]{tibble}}. Columns must at least include
#'   `addresses.trade`.
#' @param regex_split Regex to use to split addresses.
#' @param ignore_case_split Boolean specifying whether case should be ignored
#'   (`TRUE`) or not (`FALSE`) for `regex_split` above.
#' @param regex_filter Regex to use to search for address entries with post-split
#'   undesired leftovers.
#' @param ignore_case_filter Boolean specifying whether case should be ignored
#'   (`TRUE`) or not (`FALSE`) for `regex_filter` above.
#' @param regex_match Regex to use to clear address entries from post-split
#'   undesired leftovers.
#' @param ignore_case_match Boolean specifying whether case should be ignored
#'   (`TRUE`) or not (`FALSE`) for `regex_match` above.
#'
#' @return A data.frame of the same class as the one provided in `directory`;
#'   columns include at least `address.trade`. Multiple trade addresses are
#'   separated for entries for which more than one are provided. Each trade
#'   address identified lives on an individual row with information in the other
#'   columns duplicated.
general_split_trade_addresses  <- function(
  directory,
  regex_split, ignore_case_split,
  regex_filter, ignore_case_filter,
  regex_match, ignore_case_match
){
  address.trade <- addresses.trade <- NULL
  dplyr::mutate(
    directory,
    address.trade = utils_regmatches_if_not_empty(
      addresses.trade, addresses.trade, regex_split, ignore_case_split
    )
  ) %>% tidyr::unnest(address.trade) %>% dplyr::select(-addresses.trade) %>%
    utils_clean_address(type = "ends")  %>%
    dplyr::mutate(
      address.trade = utils_regmatches_if_found(
        address.trade, regex_filter, address.trade, regex_match, address.trade,
        ignore_case_filter, ignore_case_match, not = FALSE
      ) %>% unlist()
    ) %>%
    utils_clean_address(type = "ends")
}


## general_split_address_numbers_bodies ####

#' Mutate operation(s) in Scottish post office general directory data.frame
#'   column(s)
#'
#' Attempts to separate number from body of address entries in the Scottish
#'   post office general directory data.frame provided
#'
#' @param directory A Scottish post office general directory in the form
#'   of a data.frame or other object that inherits from the data.frame class
#'   such as a \code{\link[tibble]{tibble}}. Columns must at least include
#'   `address.trade` and `address.house`.
#' @param regex_split_address_numbers Regex to use to match address number(s).
#' @param regex_split_address_body Regex to use to match address body(/ies).
#' @param regex_split_address_empty Regex to use to match empty address entries.
#' @param ignore_case_filter Boolean specifying whether case should be ignored
#'   (`TRUE`) or not (`FALSE`) for using one of the regexes above as filtering
#'   regex in \code{\link{utils_regmatches_if_found}}.
#' @param ignore_case_match Boolean specifying whether case should be ignored
#'   (`TRUE`) or not (`FALSE`) for using one of the regexes above as matching
#'   regex in \code{\link{utils_regmatches_if_found}}.
#'
#' @return A data.frame of the same class as the one provided in `directory`;
#'   columns include at least `address.trade.number`, `address.trade.body`,
#'   `address.house.number` and `address.house.body`.
general_split_address_numbers_bodies <- function(
  directory, regex_split_address_numbers, regex_split_address_body,
  regex_split_address_empty, ignore_case_filter, ignore_case_match
){
  address.house <- address.trade <- NULL
  dplyr::mutate(
    directory,
    dplyr::across(
      .cols = c(address.trade, address.house),
      ~ utils_regmatches_if_found(
        string_filter = .x, regex_filter = globals_regex_split_address_numbers,
        string_search = .x, regex_search = globals_regex_split_address_numbers,
        default = "", ignore_case_filter, ignore_case_match, not = FALSE
      ),
      .names = "{.col}.number"
    ),
    dplyr::across(
      .cols = c(address.trade, address.house),
      ~ utils_regmatches_if_found(
        string_filter = .x, regex_filter = globals_regex_split_address_empty,
        string_search = .x, regex_search = globals_regex_split_address_body,
        default = "", ignore_case_filter, ignore_case_match, not = TRUE
      ),
      .names = "{.col}.body"
    )
  ) %>%
    dplyr::select(-c(address.trade, address.house)) %>%
    utils_clean_address(type = "ends")
}


## general_fix_structure ####

#' Mutate operation(s) in Scottish post office general directory data.frame
#'   column(s)
#'
#' Attempts to fix the structure of the raw Scottish post office general
#'   directory data.frame provided. For each entry, `general_fix_structure`
#'   attempts to fix parsing errors by moving pieces of information provided to
#'   the right columns; further attempts to separate trade from house address,
#'   separate multiple trade addresses as well as separate number from
#'   address body.
#'
#' @param directory A Scottish post office general directory in the form
#'   of a data.frame or other object that inherits from the data.frame class
#'   such as a \code{\link[tibble]{tibble}}. Columns must at least include
#'   `occupation`, `addresses`.
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#'
#' @return A data.frame of the same class as the one provided in `directory`;
#'   columns include at least `occupation`, `address.trade.number`,
#'   `address.trade.body`, `address.house.number` and `address.house.body`.
#'   "house" suffix in `occupation` column is move to `addresses`, occupation
#'   information is repatriated from `addresses` to `occupation` column;
#'   `addresses` is split into trade and house address columns; additional
#'   records are created for each extra trade address identified.
general_fix_structure <- function(directory, verbose){

  fix <- function(...){
    # Split trade and house addresses when both are provided ####
    # If occupation terminates in "; house" or variant, delete and add "house, "
    # to the beginning of addresses columns.
    general_move_house_to_address(directory, globals_regex_house_to_address) %>%

    # Fix occupation in addresses column ####
    # when possible, move back to occupation column
      general_repatriate_occupation_from_address(
        globals_regex_occupation_from_address
      ) %>%

    # Get rid of address prefix ####
    # Address entries are cleared of "depot", "office", "store", "works" or
    # "workshops" prefixes.
      utils_remove_address_prefix(globals_regex_address_prefix, ignore_case) %>%

    # Create trade and house addresses ####
    # Splitting raw addresses on "house" or variant. If "residence" matches,
    # don't match "house", otherwise match "house".
      general_split_trade_house_addresses(
        globals_regex_house_split_trade, verbose = verbose
      ) %>%

    # Split multiple trade addresses ####
      general_split_trade_addresses(
        regex_split = globals_regex_split_trade_addresses,
        ignore_case_split = FALSE,
        regex_filter = globals_regex_and_filter,
        ignore_case_filter = TRUE,
        regex_match = globals_regex_and_match,
        ignore_case_match = FALSE
      ) %>%

    # Split numbers and address bodies ####
      general_split_address_numbers_bodies(
        globals_regex_split_address_numbers,
        globals_regex_split_address_body,
        globals_regex_split_address_empty,
        ignore_case_filter = TRUE,
        ignore_case_match = TRUE
      )
  }

  utils_execute(verbose, fix, directory = directory, ignore_case = ignore_case)
}


# Clean ####

## general_clean_entries ####

#' Mutate operation(s) in Scottish post office general directory data.frame
#'   column(s)
#'
#' Attempts to clean entries of the provided Scottish post office general
#'   directory data.frame provided.
#'
#' @param directory A Scottish post office general directory in the form
#'   of a data.frame or other object that inherits from the data.frame class
#'   such as a \code{\link[tibble]{tibble}}. Columns must at least include
#'   `forename`, `surname`, `occupation`, `address.trade.number`,
#'   `address.trade.body` and/or `address.house.number`, `address.house.body`.
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#'
#' @return A data.frame of the same class as the one provided in `directory`;
#'   columns include the same as those in the data.frame provided in
#'   `directory`. Entries are cleaned of optical character recognition (OCR)
#'   errors and subject to a number of standardisation operations.
general_clean_entries <- function(directory, verbose){

  clean <- function(...){
    ### Get rid of irrelevant info
    utils_clear_irrelevants(directory, globals_regex_irrelevants, ignore_case) %>%

      ### Clean occupation ####
      utils_clean_occupations() %>%

      ### Clean name ####
      utils_clean_names() %>%

      ### Clean addresses
      utils_clean_addresses()
  }

  utils_execute(verbose, clean, directory = directory, ignore_case = ignore_case)
}


## general_clean_directory_plain ####

#' Mutate operation(s) in Scottish post office general directory data.frame
#'   column(s)
#'
#' Attempts to clean the provided Scottish post office general directory
#'   data.frame.
#'
#' @param directory A Scottish post office general directory in the form
#'   of a data.frame or other object that inherits from the data.frame class
#'   such as a \code{\link[tibble]{tibble}}. Columns must at least include
#'   `forename`, `surname`, `occupation` and `addresses`.
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#'
#' @return A data.frame of the same class as the one provided in `directory`;
#'   columns include at least `forename`, `surname`, `occupation`,
#'   `address.trade.number`, `address.trade.body`, `address.house.number` and
#'   `address.house.body`. "house" suffix in `occupation` column is move to
#'   `addresses`, occupation information is repatriated from `addresses` to
#'   `occupation` column; `addresses` is split into trade and house address
#'   column; additional records are created for each extra trade address
#'   identified. Entries are further cleaned of optical character recognition
#'   (OCR) errors and subject to a number of standardisation operations.
general_clean_directory_plain <- function(directory, verbose){

  clean <- function(...){
    # Fix structure ####
    general_fix_structure(directory, verbose = verbose) %>%

    # Clean entries ####
      general_clean_entries(verbose = verbose)
  }

  utils_execute(verbose, clean, directory = directory)
}


## general_clean_directory_progress ####

#' Mutate operation(s) in Scottish post office general directory data.frame
#'   column(s)
#'
#' Attempts to clean the provided Scottish post office general directory
#'   data.frame. Shows a progress bar indication the progression of the function.
#'
#' @param directory A Scottish post office general directory in the form
#'   of a data.frame or other object that inherits from the data.frame class
#'   such as a \code{\link[tibble]{tibble}}. Columns must at least include
#'   `forename`, `surname`, `occupation` and `addresses`.
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#'
#' @return A data.frame of the same class as the one provided in `directory`;
#'   columns include at least `forename`, `surname`, `occupation`,
#'   `address.trade.number`, `address.trade.body`, `address.house.number` and
#'   `address.house.body`. "house" suffix in `occupation` column is move to
#'   `addresses`, occupation information is repatriated from `addresses` to
#'   `occupation` column; `addresses` is split into trade and house address
#'   column; additional records are created for each extra trade address
#'   identified. Entries are further cleaned of optical character recognition
#'   (OCR) errors and subject to a number of standardisation operations.
general_clean_directory_progress <- function(directory, verbose){

  directory_split <- split(directory, (1L:nrow(directory) %/% 500L))

  pb <- progress::progress_bar$new(
    format = "  cleaning records [:bar] :percent eta: :eta",
    total = length(directory_split), clear = FALSE, width = 100L
  )

  purrr::map_dfr(directory_split, function(df) {
    pb$tick(); general_clean_directory_plain(df, verbose)
  })
}


## general_clean_directory ####

#' Mutate operation(s) in Scottish post office general directory data.frame
#'   column(s)
#'
#' Attempts to clean the provided Scottish post office general directory
#'   data.frame.
#'
#' @param directory A Scottish post office general directory in the form
#'   of a data.frame or other object that inherits from the data.frame class
#'   such as a \code{\link[tibble]{tibble}}. Columns must at least include
#'   `forename`, `surname`, `occupation` and `addresses`.
#' @param progress Whether progress should be shown (`TRUE`) or not (`FALSE`).
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#'
#' @return A \code{\link[tibble]{tibble}}; columns include at least
#'   `forename`, `surname`, `occupation`, `address.trade.number`,
#'   `address.trade.body`, `address.house.number` and `address.house.body`.
#'   "house" suffix in `occupation` column is move to `addresses`, occupation
#'   information is repatriated from `addresses` to `occupation` column;
#'   `addresses` is split into trade and house address columns; additional
#'   records are created for each extra trade address identified. Entries are
#'   further cleaned of optical character recognition (OCR) errors and subject
#'   to a number of standardisation operations.
#'
#' @examples
#' pages <- rep("71", 2L)
#' surnames <- c("ABOT", "ABRCROMBIE")
#' forenames <- c("Wm.", "Alex")
#' occupations <- c("Wine and spirit mercht - See Advertisement in Appendix.", "")
#' addresses = c(
#'   "1S20 Londn rd; ho. 13<J Queun sq",
#'   "Bkr; I2 Dixon Street, & 29 Auderstn Qu.; res 2G5 Argul st."
#' )
#' directory <- tibble::tibble(
#'   page = pages, surname = surnames, forename = forenames,
#'   occupation = occupations, addresses = addresses
#' )
#' general_clean_directory(directory, progress = TRUE, verbose = FALSE)
#'
#' @export
general_clean_directory <- function(directory, progress = TRUE, verbose = FALSE){

  out <- if (progress) general_clean_directory_progress(directory, verbose)
  else general_clean_directory_plain(directory, verbose)

  dplyr::distinct(tibble::tibble(out))
}

