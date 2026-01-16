# Define globals ####
`%>%` <- magrittr::`%>%`
ignore_case <- TRUE
perl <- TRUE


# combine_random_string_if_pattern ####

#' Conditionally return a random string
#'
#' Search for specified pattern in provided string; if found returns a 22
#'   character long random string otherwise return original string.
#'
#' @param string A character string.
#' @param regex Character string regex specifying the pattern to look for in
#'   `string`.
#'
#' @return A length 1 character string vector: 22 character long random string
#'   if `regex` found in `string`, `string` otherwise.
combine_random_string_if_pattern <- function(string, regex){
  ifelse(
    grepl(regex, string, ignore_case, perl),
    stringi::stri_rand_strings(1L, 22L), string
  )
}


# combine_random_string_if_no_address ####

#' Conditionally return a random string
#'
#' Returns a 22 character long random string if address provided is labelled as
#'   missing ("No trade/house address found").
#'
#' @param address A character string.
#'
#' @return A length 1 character string vector: 22 character long random string
#'   if `address` labelled as missing ("No trade/house address found"),
#'   `address` otherwise.
combine_random_string_if_no_address <- function(address){
  regex <- "^No.+address\\sfound$"
  combine_random_string_if_pattern(address, regex)
}


# combine_no_trade_address_to_random_string ####

#' Mutate operation(s) in directory data.frame address.trade column.
#'
#' Replaces missing trade address(es) in the provided Scottish post office
#'   directory data.frame with random string(s). Random string(s) only show(s)
#'   in body of trade address entry(/ies).
#'
#' @param directory A Scottish post office directory in the form of a data.frame
#'   or other object that inherits from the data.frame class such as a
#'   \code{\link[tibble]{tibble}}. Columns must at least include `address.trade`.
#'
#' @return A data.frame of the same class as the one provided in `directory`;
#'   columns include at least `address.trade`.
#'
#' @section Details:
#' Prevents unwarranted matches when matching general to trades directory.
#'   Unrelated records with similar name and trade address entry labelled as
#'   missing would be otherwise matched.
combine_no_trade_address_to_random_string <- function(directory){
  address.trade <- NULL

  dplyr::mutate(
    directory,
    address.trade = purrr::map_chr(
      address.trade, ~ combine_random_string_if_no_address(.x)
    )
  )
}


# combine_make_match_string ####

#' Mutate operation(s) in directory data.frame trade address column
#'
#' Creates a 'match.string' column in the provided Scottish post office
#'   directory data.frame composed of entry(/ies) full name and trade address
#'   pasted together. Missing trade address entry(/ies) are replaced with a
#'   random generated string.
#'
#' @param directory A Scottish post office directory in the form of a data.frame
#'   or other object that inherits from the data.frame class such as a
#'   \code{\link[tibble]{tibble}}. Columns must at least include `forename`,
#'   `surname`, `address.trade.number`, `address.trade.body`.
#'
#' @return A data.frame of the same class as the one provided in `directory`;
#'   columns include at least `forename`, `surname`, `address.trade.number`,
#'   `address.trade.body`, `match.string`.
#'
#' @section Details:
#' The purpose of the 'match.string' column is to facilitates the matching of
#'   the general to trades directory down the line. It allows to calculate a
#'   string distance metric between each pair of entries and match those falling
#'   below a specified threshold.
#'
#' @seealso \code{\link{combine_match_general_to_trades}} for the matching of
#'   the general to trades directory.
combine_make_match_string <- function(directory){
  address.trade <- name <- NULL

  tidyr::unite(
    directory,
    "address.trade", dplyr::matches("address.trade"),
    sep = ", ", remove = FALSE, na.rm = TRUE
  ) %>%
    tidyr::unite(
      "name", dplyr::matches("name$"), sep = " ", remove = FALSE, na.rm = TRUE
    ) %>%
    utils_clean_ends(name, address.trade) %>%
    combine_no_trade_address_to_random_string() %>%
    tidyr::unite(
      "match.string", c(dplyr::matches("^name"), "address.trade"), sep = " - ",
      remove = FALSE, na.rm = TRUE
    ) %>%
    dplyr::select(-c(address.trade, name))
}


# combine_has_match_failed ####

#' Check for failed matches
#'
#' Provided with two equal length vectors, returns TRUE for indexes where both
#'   entries are "NA" and FALSE otherwise.
#'
#' @param number A vector of address number(s). Integer or character string.
#' @param body A character string vector of address body(/ies).
#'
#' @return A boolean vector: `TRUE` for indexes where both
#'   `number` and `body` are "NA", `FALSE` otherwise.
combine_has_match_failed <- function(number, body){
  (is.na(number) & is.na(body))
}


# combine_label_if_match_failed ####

#' Label failed matches
#'
#' Labels failed matches as such.
#'
#' @param type A Character string, one of: "number" or "body". Type of column to
#'   label.
#' @param ... Further arguments to be passed down to
#'   \code{\link{combine_has_match_failed}}
#'
#' @return A character string vector: address(es) "number" or "body" as
#'   specified in `type` if match succeeded, "" (type = "number") or
#'   "Failed to match with general directory" (type = "body") otherwise.
combine_label_if_match_failed <- function(type = c("number", "body"), ...){
  txt <- switch(
    type, "number" = "", "body" = "Failed to match with general directory"
  )
  args <- list(...)
  dplyr::if_else(combine_has_match_failed(...), txt, args[[type]])
}


# combine_get_address_house_type ####

#' Get house address column type
#'
#' Identifies the type of the house address column provided: number or body.
#'
#' @param column A Character string: ends in "house.number" or "house.body".
#'
#' @return A Character string: "number" or "body".
combine_get_address_house_type <- function(column){
  regmatches(
    column, regexpr(globals_regex_get_address_house_type, column, perl = perl)
  )
}


# combine_label_failed_matches ####

#' Label failed matches
#'
#' Labels failed matches as such in the provided Scottish post office directory
#'   data.frame.
#'
#' @param directory A Scottish post office directory in the form of a data.frame
#'   or other object that inherits from the data.frame class such as a
#'   \code{\link[tibble]{tibble}}. Columns must at least include
#'   `address.house.number`, `address.house.body`.
#'
#' @return A data.frame of the same class as the one provided in `directory`.
#'   Columns include `address.house.number`, `address.house.body`. For entries
#'   for which both `address.house.number` and `address.house.body` are `NA`,
#'   `address.house.number` and `address.house.body` are labelled as "" and
#'   "Failed to match with general directory" respectively.
combine_label_failed_matches <- function(directory){
  dplyr::mutate(
    directory,
    dplyr::across(
      .cols = dplyr::matches(globals_regex_address_house_body_number),
      ~ combine_label_if_match_failed(
        type = combine_get_address_house_type(dplyr::cur_column()),
        number = address.house.number, body = address.house.body
      )
    )
  )
}


# combine_match_general_to_trades_plain ####

#' Match general to trades directory records
#'
#' Attempts to complement Scottish post office trades directory data.frame with
#'   house address information from the Scottish post office general directory
#'   data.frame provided by matching records from the two datasets using the
#'   distance metric specified.
#'
#' @param trades_directory A Scottish post office trades directory in the form
#'   of a data.frame or other object that inherits from the data.frame class
#'   such as a \code{\link[tibble]{tibble}}. Columns must at least include
#'   `surname`, `forename`, `address.trade.number`, `address.trade.body`.
#' @param general_directory A Scottish post office general directory in the form
#'   of a data.frame or other object that inherits from the data.frame class
#'   such as a \code{\link[tibble]{tibble}}. Columns must at least include
#'   `surname`, `forename`, `address.trade.number`, `address.trade.body`,
#'   `address.house.number`, `address.house.body`.
#' @param matches Whether (`TRUE`) or not (`FALSE`) a column 'match' showing
#'   general directory matches' name and address(es) should be added to the
#'   output dataset.
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#' @param ... Further arguments to be passed down to
#'   \code{\link[fuzzyjoin]{stringdist_left_join}}.
#'
#' @return A data.frame of the same class as that of the one provided in
#'   `trades_directory` and/or `general_directory`. Should `trades_directory` and
#'   `general_directory`be provided as objects of different classes, the class of
#'   the return data.frame will be that of the parent class. i.e. if
#'   `trades_directory` and `general_directory` are provided as a pure data.frame
#'   and a \code{\link[tibble]{tibble}} respectively, a pure data.frame is
#'   returned. Columns include at least `surname`, `forename`,
#'   `address.trade.number`, `address.trade.body`, `address.house.number`,
#'   `address.house.body`.
#'
#' @seealso \code{\link{combine_match_general_to_trades}}.
combine_match_general_to_trades_plain <- function(
  trades_directory, general_directory, verbose, matches, ...
) {

  fun <- function(trades_directory, general_directory, matches, ...) {
    match.string <- NULL

    trades_directory <- combine_make_match_string(trades_directory)
    general_directory <- combine_make_match_string(general_directory) %>%
      dplyr::select(dplyr::matches("^address.house"), match.string)

    combined <- fuzzyjoin::stringdist_left_join(
      x = trades_directory, y = general_directory, by = "match.string", ...
    )

    combined <- if (matches){
      dplyr::select(
        combined, -dplyr::matches("match.string.x"), match = dplyr::matches("match.string.y")
      ) %>%
        dplyr::relocate(match, .after = dplyr::last_col())
    } else { dplyr::select(combined, -dplyr::matches("match.string")) }

    combine_label_failed_matches(combined)

  }

  utils_execute(verbose, fun, trades_directory, general_directory, matches, ...)
}


# combine_match_general_to_trades_progress ####

#' Match general to trades directory records
#'
#' Attempts to complement Scottish post office trades directory data.frame with
#'   house address information from the Scottish post office general directory
#'   data.frame provided by matching records from the two datasets using the
#'   distance metric specified. Shows a progress bar indicating function
#'   progression.
#'
#' @param trades_directory A Scottish post office trades directory in the form
#'   of a data.frame or other object that inherits from the data.frame class
#'   such as a \code{\link[tibble]{tibble}}. Columns must at least include
#'   `surname`, `forename`, `address.trade.number`, `address.trade.body`.
#' @param general_directory A Scottish post office general directory in the form
#'   of a data.frame or other object that inherits from the data.frame class
#'   such as a \code{\link[tibble]{tibble}}. Columns must at least include
#'   `surname`, `forename`, `address.trade.number`, `address.trade.body`,
#'   `address.house.number`, `address.house.body`.
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#' @param matches Whether (`TRUE`) or not (`FALSE`) a column 'match' showing
#'   general directory matches' name and address(es) should be added to the
#'   output dataset.
#' @param ... Further arguments to be passed down to
#'   \code{\link[fuzzyjoin]{stringdist_left_join}}.
#'
#' @return A data.frame of the same class as that of the one provided in
#'   `trades_directory` and/or `general_directory`. Should `trades_directory` and
#'   `general_directory`be provided as objects of different classes, the class of
#'   the return data.frame will be that of the parent class. i.e. if
#'   `trades_directory` and `general_directory` are provided as a pure data.frame
#'   and a \code{\link[tibble]{tibble}} respectively, a pure data.frame is
#'   returned. Columns include at least `surname`, `forename`,
#'   `address.trade.number`, `address.trade.body`, `address.house.number`,
#'   `address.house.body`.
#'
#' @seealso \code{\link{combine_match_general_to_trades}}.
combine_match_general_to_trades_progress <- function(
  trades_directory, general_directory, verbose, matches, ...
) {

  trades_directory_split <- split(
    trades_directory, (1L:nrow(trades_directory) %/% 100L)
  )

  pb <- progress::progress_bar$new(
    format = "  matching records [:bar] :percent eta: :eta",
    total = length(trades_directory_split), clear = FALSE, width = 100L
  )

  purrr::map_dfr(trades_directory_split, function(df) {
    pb$tick()
    combine_match_general_to_trades_plain(df, general_directory, verbose, matches, ...)
  })
}


# combine_match_general_to_trades ####

#' Match general to trades directory records
#'
#' Attempts to complement Scottish post office trades directory data.frame with
#'   house address information from the Scottish post office general directory
#'   data.frame provided by matching records from the two datasets using the
#'   distance metric specified.
#'
#' @param trades_directory A Scottish post office trades directory in the form
#'   of a data.frame or other object that inherits from the data.frame class
#'   such as a \code{\link[tibble]{tibble}}. Columns must at least include
#'   `surname`, `forename`, `address.trade.number`, `address.trade.body`.
#' @param general_directory A Scottish post office general directory in the form
#'   of a data.frame or other object that inherits from the data.frame class
#'   such as a \code{\link[tibble]{tibble}}. Columns must at least include
#'   `surname`, `forename`, `address.trade.number`, `address.trade.body`,
#'   `address.house.number`, `address.house.body`.
#' @param progress Whether progress should be shown (`TRUE`) or not (`FALSE`).
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#' @param distance Whether (`TRUE`) or not (`FALSE`) a column 'distance' showing
#'   the string distance between records used for their matching and calculated
#'   using the method specified below should be added to the output dataset.
#' @param matches Whether (`TRUE`) or not (`FALSE`) a column 'match' showing
#'   general directory matches' name and address(es) should be added to the
#'   output dataset.
#' @param ... Further arguments to be passed down to
#'   \code{\link[fuzzyjoin]{stringdist_left_join}}.
#'
#' @return A \code{\link[tibble]{tibble}}; columns include at least `surname`,
#'   `forename`, `address.trade.number`, `address.trade.body`,
#'   `address.house.number`, `address.house.body`.
#'
#' @examples
#' trades_directory <- tibble::tibble(
#'   page = rep("71", 3L),
#'   rank = c("135", "326", "586"),
#'   surname = c("Abbott", "Abercromby", "Blair"),
#'   forename = c("William", "Alexander", "John Hugh"),
#'   occupation = c("Wine and spirit merchant", "Baker", "Victualler"),
#'   type = rep("OWN ACCOUNT", 3L),
#'   address.trade.number = c("18, 20", "12", "280"),
#'   address.trade.body = c("London Road", "Dixon Place", "High Street")
#' )
#' general_directory <- tibble::tibble(
#'   page = rep("71", 2L),
#'   surname = c("Abbott", "Abercromby"), forename = c("William", "Alexander"),
#'   occupation = c("Wine and spirit merchant", "Baker"),
#'   address.trade.number = c("18, 20", ""),
#'   address.house.number = c("136", "29"),
#'   address.trade.body = c("London Road", "Dixon Place"),
#'   address.house.body = c("Queen Square", "Anderston Quay")
#' )
#' combine_match_general_to_trades(
#'  trades_directory, general_directory, progress = TRUE, verbose = FALSE,
#'  distance = TRUE, method = "osa", max_dist = 5
#' )
#'
#' @export
combine_match_general_to_trades <- function(
  trades_directory, general_directory, progress = TRUE, verbose = FALSE,
  distance = TRUE, matches = TRUE, ...
){

  combined <- if (progress)
    combine_match_general_to_trades_progress(
      trades_directory, general_directory, verbose, matches,
      distance_col = "distance", ...
    )
  else
    combine_match_general_to_trades_plain(
      trades_directory, general_directory, verbose, matches,
      distance_col = "distance", ...
    )

  combined <- dplyr::distinct(tibble::tibble(combined))

  if (distance) { combined } else { dplyr::select(combined, -distance) }
}






