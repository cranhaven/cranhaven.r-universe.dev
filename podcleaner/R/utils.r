# Define globals ####
`%>%` <- magrittr::`%>%`
ignore_case <- TRUE
perl <- TRUE



# Load ####

## utils_format_directory_raw ####

#' Format raw directory for further processing
#'
#' Takes a raw directory dataframe (just loaded), adds a column with the
#'   corresponding directory name, replaces all `NA` entries with an empty
#'   string, clear all entries of unwanted blank characters, format page number
#'   as integer, returns the output with the directory name column in first
#'   position.
#'
#' @param df A raw directory dataframe as output by
#'   \code{\link{utils_load_directories_csv}}.
#' @param name Directory name provided as a character string.
#'
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#'   directory <- data.frame(
#'     page = c("71", "71"),
#'     surname = c("ABOT     ", " ABRCROMBIE"), forename = c("Wm.", "Alex"),
#'     occupation = c("wine and    spirit mercht", "    bkr"),
#'     addresses = c(
#'       "depot -; 1820 London    st. ; house, Mary hill.*",
#'       "workshop,,12 &;Dixon st.; residence,    Craigrownie, Cove.$   "
#'     ),
#'     stringsAsFactors = FALSE
#'   )
#'   utils_format_directory_raw(directory, "1861-1862")
#' }
utils_format_directory_raw <- function(df, name){
  page <- directory <- NULL

  dplyr::mutate(
    df,
    directory = name,
    dplyr::across(.cols = dplyr::everything(), ~ replace(., is.na(.), ""))
  ) %>%
    utils_squish_all_columns() %>%
    dplyr::mutate(page = as.integer(page)) %>%
    dplyr::select(directory, dplyr::everything())
}


## utils_squish_all_columns ####

#' Clear extra white spaces in dataframe
#'
#' Removes blanks (white spaces and tabs) at the beginning and end of all entries
#'   of the provided dataframe. Converts all series of white space and/or tab(s)
#'   in the body of all dataframe entries into a single white space.
#'
#' @param df A dataframe.
#'
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#'   df <- data.frame(
#'     location = "  glasgow ", occupation = "wine    merchant",
#'     stringsAsFactors = FALSE
#'   )
#'   df <- utils_squish_all_columns(df)
#' }
utils_squish_all_columns <- function(df) {
  dplyr::mutate(df, dplyr::across(.cols = dplyr::everything(), stringr::str_squish))
}


## utils_load_directories_csv ####

#' Load directory "csv" file(s) into memory
#'
#' Loads specified directory "csv" file(s) into memory. Stacks individual
#'   directories into a single dataframe and further passes the output down to
#'   \code{\link{utils_format_directory_raw}} for initial formatting.
#'
#' @param type A character string: "general" or "trades". Refers to the type of
#'   directory to shall be loaded.
#' @param directories A character string vector providing the name(s) of the
#'   directory(/ies) to load.
#' @param path A character string specifying the path to the folder where the
#'   directory(/ies) live as ".csv" file(s).
#' @param verbose Whether the function should be executed silently (`FALSE`) or
#'   not (`TRUE`).
#'
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#'   utils_load_directories_csv(
#'     "general", "1861-1862",
#'     "home/projects/glasgow-entrepreneurs/data/general-directories", FALSE
#'   )
#' }
#'
#' @export
utils_load_directories_csv <- function(
  type = c("general", "trades"), directories, path, verbose
) {

  load <- function(...){

    call <- paste0("colnames <- globals_", type, "_colnames")
    eval(parse(text = call))
    types <- rep("c", NROW(colnames)) %>% paste(collapse = "")
    purrr::map_df(directories, function(directory){
      file <- paste(path, utils_make_file(directory, extension = "csv"), sep = "/")
      readr::read_csv(file, col_names = colnames, col_types = types, skip = 1L) %>%
        utils_format_directory_raw(directory)
    })
  }

  utils_execute(
    verbose, load, type = type, directories = directories, path = path
  )
}

# Fix structure ####

## utils_remove_address_prefix ####

#' Clear undesired address prefixes
#'
#' Clear address entries in the provided directory dataframe of undesired
#'   prefixes such as "depot", "office", "store", "works" or "workshops".
#'
#' @param directory A directory dataframe with an `addresses` column.
#' @param regex Regex character string to be use for matching.
#' @param ignore_case Boolean specifying whether case should be ignored
#'   (`TRUE`) or not (`FALSE`) in search for `regex` in `addresses` column
#'   entries of `directory`.
#'
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#'   directory <- data.frame(
#'     page = c("71", "71"),
#'     surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
#'     occupation = c("Wine and spirit merchant", "Baker"),
#'     addresses = c(
#'       "depot -; 1820 London    st. ; house, Mary hill.*",
#'       "workshop,,12 &;Dixon st.; residence,    Craigrownie, Cove.$   "
#'     ),
#'     stringsAsFactors = FALSE
#'   )
#'   regex <- globals_regex_address_prefix
#'   utils_remove_address_prefix(directory, regex, TRUE)
#' }
utils_remove_address_prefix <- function(directory, regex, ignore_case){
  addresses <- NULL

  dplyr::mutate(
    directory, addresses = gsub(regex, "", addresses, ignore.case = ignore_case, perl = perl)
  ) %>% utils_clean_address(type = "ends")
}



# Clean ####

## utils_clear_irrelevants ####

#' Mutate operation(s) in directory dataframe column(s)
#'
#' Attempts to get rid of irrelevant information in all columns of the provided
#'   directory dataframe provided
#'
#' @param directory A directory dataframe.
#' @param ... Further arguments to be passed down to \code{\link{utils_clear_content}}.
#'
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#'   directory <- data.frame(
#'     page = c("71", "71"),
#'     surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
#'     occupation = c("Wine and spirit merchant — See Advertisement in Appendix.", "Baker"),
#'     address.trade.number = c("18, 20", "12"),
#'     address.house.number = c("136", "265"),
#'     address.trade.body = c("London Street.", "Dixon Street."),
#'     address.house.body = c("Queen Street.", "Argyle Street"),
#'     stringsAsFactors = FALSE
#'   )
#'   utils_clear_irrelevants(directory, globals_regex_irrelevants, ignore_case = TRUE)
#' }
utils_clear_irrelevants <- function(directory, ...){
  dplyr::mutate(directory, dplyr::across(.cols = dplyr::everything(), utils_clear_content, ...))
}


## utils_clean_occupations ####

#' Clean entries occupation record
#'
#' Clean "occupation" column of provided directory dataframe.
#'
#' @param directory A directory dataframe.
#'
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#'   directory <- data.frame(
#'     page = c("71", "71"),
#'     surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
#'     occupation = c("wine and spirit mercht", "bkr"),
#'     address.number = c(" -; 1820", ",,12"),
#'     address.body = c(
#'       "London st. ; house, Mary hill.*",
#'       "&;Dixon st.; residence, Craigrownie, Cove.$"
#'     ),
#'     stringsAsFactors = FALSE
#'   )
#'   utils_clean_occupations(directory)
#' }
utils_clean_occupations <- function(directory){
  occupation <- NULL

  dplyr::mutate(
    directory,
    occupation = clean_occupation(occupation) %>% stringr::str_squish() %>%
      stringr::str_to_sentence()
  )
}


## utils_clean_names ####

#' Clean entries name records
#'
#' Clean name columns (forename & surname) of provided directory dataframe.
#'
#' @param directory A directory dataframe.
#'
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#'   directory <- data.frame(
#'     page = c("71", "71"),
#'     surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
#'     occupation = c("Wine and spirit merchant", "Baker"),
#'     address.number = c(" -; 1820", ",,12"),
#'     address.body = c(
#'       "London st. ; house, Mary hill.*",
#'       "&;Dixon st.; residence, Craigrownie, Cove.$"
#'     ),
#'     stringsAsFactors = FALSE
#'   )
#'   utils_clean_names(directory)
#' }
utils_clean_names <- function(directory){
  forename <- surname <- NULL

  dplyr::mutate(
    directory,
    dplyr::across(.cols = dplyr::matches("name"), clean_title),
    forename = clean_forename(forename) %>% stringr::str_to_title(),
    surname = clean_surname(surname) %>% stringr::str_to_title()
  )
}


## utils_clean_address_ends ####

#' Clean address entry ends
#'
#' Clean beginning and end of the provided address entries.
#'
#' @param addresses A character string vector of address(es).
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   utils_clean_address_ends(
#'     c(
#'       " -; 18, 20 London st.; house, Mary hill.*",
#'       ",,12 &;Dixon st.; residence, Craigrownie, Cove.$"
#'     )
#'   )
#' }
utils_clean_address_ends <- function(addresses) { clean_address_ends(addresses) }


## utils_clean_address_number ####

#' Clean address(es) number
#'
#' Clean number record of provided address(es).
#'
#' @param addresses A character string vector of address(es).
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   utils_clean_address_number(c(" -; 1820", ",,12"))
#' }
utils_clean_address_number <- function(addresses) { clean_address_number(addresses) }


## utils_clean_address_body ####

#' Clean address(es) body
#'
#' Clean body record of provided address(es).
#'
#' @param addresses A character string vector of address(es).
#'
#' @return A vector of character strings.
#'
#' @examples
#' \dontrun{
#'   utils_clean_address_body(
#'     c("London st.", "Mary hill.*", "&;Dixon st.", "Craigrownie, Cove.$")
#'   )
#' }
utils_clean_address_body <- function(addresses) { clean_address_body(addresses) }


## utils_clean_address ####

#' Clean directory address entries
#'
#' Clean address entries in the provided directory dataframe.
#'
#' @param directory A directory dataframe.
#' @param type A character string: "body", "number" or "ends". Specifies the type
#'   of address cleaning to be performed. For "body", "number" and "ends"
#'   \code{\link{clean_address_body}}, \code{\link{clean_address_number}} and
#'   \code{\link{clean_address_ends}} are called respectively
#'
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#'   directory <- data.frame(
#'     page = c("71", "71"),
#'     surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
#'     occupation = c("Wine and spirit merchant", "Baker"),
#'     address.number = c(" -; 1820", ",,12"),
#'     address.body = c(
#'       "London st. ; house, Mary hill.*",
#'       "&;Dixon st.; residence, Craigrownie, Cove.$"
#'     ),
#'     stringsAsFactors = FALSE
#'   )
#'   utils_clean_address(directory, "body")
#'   utils_clean_address(directory, "number")
#' }
utils_clean_address <- function(directory, type = c("body", "number", "ends")){
  regex_column <- ifelse(
    type == "ends",
    "(?:address\\.(?:house|trade)$|body$)", paste0("^address.+", type, "$")
  )
  # regex_column <- paste0("^address.+", ifelse(type == "ends", "", type), "$")
  fun <- get(paste("clean_address", type, sep = "_"))
  dplyr::mutate(directory, dplyr::across(.cols = dplyr::matches(regex_column), fun))
}


## utils_is_address_missing ####

#' Check is address entry not missing
#'
#' Checks whether or not for each address in the evaluation environment, body
#'   and number are filled/not empty.
#'
#' @param type A character string: "house" or "trade", specifying the type of
#'   address to check.
#'
#' @return A Boolean vector: TRUE if both number and body are empty.
#'
#' @section Details:
#' The function is for primarily use in the
#'   \code{\link{utils_label_address_if_missing}} function called by
#'   \code{\link{utils_label_missing_addresses}} where it provides a filtering
#'   vector used for labelling missing addresses. `utils_is_address_missing` creates
#'   an expression and further evaluates it two levels up in the environment tree,
#'   in other words in the directory dataframe eventually passed down to
#'   \code{\link{utils_label_missing_addresses}}.
utils_is_address_missing <- function(type){
  expr <- paste(
    paste("address", type, "number", sep = "."), "== '' &",
    paste("address", type, "body", sep = "."), "== ''",
    sep = " "
  )
  eval(parse(text = expr), envir = rlang::caller_env(n = 2L))
}


## utils_label_address_if_missing ####

#' Label addresses if missing
#'
#' If address is empty label body accordingly: "no house/trade address
#'   found".
#'
#'
#' @return A character string vector of address bodies, unchanged if provided,
#'   labelled as missing otherwise.
#'
#' @section Details:
#' The function is for primarily use in the
#'   \code{\link{utils_label_missing_addresses}} function where it provides a
#'   vector of address bodies `utils_label_address_if_missing` creates an
#'   expression and further evaluates it one level up in the environment tree,
#'   in other words in the directory dataframe eventually passed down to
#'   \code{\link{utils_label_missing_addresses}}.
utils_label_address_if_missing <- function(){
  column <- dplyr::cur_column()
  type <- regmatches(column, regexpr("(?<=\\.).+(?=\\.)", column, perl = TRUE))
  filter <- utils_is_address_missing(type)
  true <- paste("No", type, "address found", sep = " ")
  false <- paste("address", type, "body", sep = ".")
  expr <- paste(
    paste0("ifelse(c(", paste(filter, collapse = ", "), "),"),
    paste0("'", true, "',"), paste0(false, ")"),
    sep = " "
  )
  eval(parse(text = expr), envir = rlang::caller_env(n = 1L))
}


## utils_label_missing_addresses ####

#' Label empty addresses as missing
#'
#' Labels empty address bodies as "not house/trade address found" in the
#'   provided directory dataframe.
#'
#' @param directory A directory dataframe. Columns must include
#'   `address.house.number`, `address.house.number` and/or
#'   `address.trade.number`, `address.trade.number`.
#'
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#'   directory <- data.frame(
#'     page = c("71", "71"),
#'     surname = c("ABOT", "ABRCROMBIE"), forename = c("Wm.", "Alex"),
#'     occupation = c("Wine and spirit merchant", "Baker"),
#'     address.number = c(" -; 1820", ""),
#'     address.body = c(
#'       "London st. ; house, Mary hill.*",
#'       ""
#'     ),
#'     stringsAsFactors = FALSE
#'   )
#'   utils_label_missing_addresses(directory)
#' }
utils_label_missing_addresses <- function(directory){
  dplyr::mutate(
    directory,
    dplyr::across(.cols = dplyr::matches("body"), ~ utils_label_address_if_missing())
  )
}


## utils_clean_addresses ####

#' Clean directory addresses
#'
#' Clean all address records in provided directory dataframe.
#'
#' @param directory A directory dataframe. Columns must include
#'   `address.house.number`, `address.house.number` and/or
#'   `address.trade.number`, `address.trade.number`.
#'
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#'   directory <- data.frame(
#'     page = c("71", "71", "71"),
#'     surname = c("ABOT", "ABRCROMBIE", "BLAI"), forename = c("Wm.", "Alex", "Jn Huh"),
#'     occupation = c("Wine and spirit merchant", "Baker", "Victualer"),
#'     address.trade.number = c(" -; 1820", "", "280"),
#'     address.trade.body = c("London st. ; house, Mary hill.*", "", "High stret"),
#'     stringsAsFactors = FALSE
#'   )
#'   utils_clean_addresses(directory)
#' }
utils_clean_addresses <- function(directory){
  surname <- forename <- occupation <- NULL
  utils_clean_address(directory, type = "body") %>%
    utils_clean_address(type = "number") %>%
    utils_label_missing_addresses() %>%
    dplyr::select(dplyr::any_of(globals_union_colnames)) %>%
    dplyr::arrange(surname, forename, occupation)
}


# Combine ####

## utils_clean_ends ####

#' Clean entry ends
#'
#' Clean entry ends for the specified columns in the directory dataframe
#'   provided
#'
#' @param directory A directory dataframe.
#' @param ... Columns to clean provided as expressions.
#'
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#'   directory <- data.frame(
#'     page = c("71", "71", "71"),
#'     surname = c("ABOT", "ABRCROMBIE", "BLAI"), forename = c("Wm.", "Alex", "Jn Huh"),
#'     occupation = c("Wine and spirit merchant", "Baker", "Victualer"),
#'     address.trade.number = c(" -; 1820", "", "280"),
#'     address.trade.body = c("London st. ; house, Mary hill.*", "", "High stret"),
#'     stringsAsFactors = FALSE
#'   )
#'   utils_clean_ends(directory, address.trade.number, address.trade.body)
#' }
utils_clean_ends <- function(directory, ...){
  dplyr::mutate(directory, dplyr::across(c(...), ~ clean_string_ends(.x)))
}


# Helpers ####

## utils_make_path ####

#' Make destination path
#'
#' Pastes the arguments provided together using '/' as separator.
#'
#' @param ... Path components as character string(s).
#'
#' @return Path to last element provided as a character string.
#'
#' @examples
#' utils_make_path("home", "projects", "glasgow-entrepreneurs.csv")
#'
#' @export
utils_make_path <- function(...) paste(..., sep = "/")


## utils_make_file ####

#' Make file name
#'
#' Pastes the arguments provided together using '-'. Appends result string with
#'   the extension provided.
#'
#' @param ... File name component(s) as character string(s).
#' @param extension File extension as character string
#'
#' @return File name as a character string.
#'
#' @examples
#' utils_make_file("glasgow", "entrepreneurs", extension = "csv")
#'
#' @export
utils_make_file <- function(..., extension)
  paste(paste(..., sep = "-"), extension, sep = ".")


## utils_IO_path ####

#' Make path for input/output operations
#'
#' Paste provided path to directory and file name provided using '/' as
#'   separator.
#'
#' @param directory_path Path to directory where \code{file_name} lives as
#'   character string.
#' @param ... File name components provided as character strings to be passed
#'   down to \code{\link{utils_make_file}}.
#' @param extension File extension as character string
#'
#' @return Path to destination file as a character string.
#'
#' @examples
#' \dontrun{
#'   utils_IO_path("home/projects", "glasgow-entrepreneurs", "csv")
#' }
utils_IO_path <- function(directory_path, ..., extension){

  file_name <- utils_make_file(..., extension = extension)
  utils_make_path(directory_path, file_name)
}


## utils_IO_write ####

#' Write object to long term memory
#'
#' Save the object provided to specified path as `.rds` file.
#'
#' @param data R object to save.
#' @param ... Destination parameters to be passed to \code{\link{utils_IO_path}}.
#'
#' @return No return value, called for side effects.
#'
#' @examples
#' \dontrun{
#'   utils_IO_write(mtcars, "home/projects", "mtcars")
#' }
#'
#' @export
utils_IO_write <- function(data, ...){

  file_path <- utils_IO_path(..., file_name_extension = "rds")
  readr::write_rds(data, file_path)
}


## utils_IO_load ####

#' Load object into memory
#'
#' Load saved object as `.rds` file back into memory.
#'
#' @param ... Destination parameters to be passed to \code{\link{utils_IO_path}}.
#'
#' @return R object from destination `.rds` file.
#'
#' @examples
#' \dontrun{
#'   utils_IO_load("home/projects", "glasgow-entrepreneurs")
#' }
utils_IO_load <- function(...){

  file_path <- utils_IO_path(..., file_name_extension = "rds")
  readr::read_rds(file_path)
}


## utils_split_and_name ####

#' Split string into tibble
#'
#' Split provided string according to specified pattern. Organise output as a
#'   \code{\link[tibble]{tibble}}.
#'
#' @param string Character string to be split.
#' @param pattern Pattern to split on as character string (can be a regex).
#' @param num_col Number of parts to split the string into as integer.
#' @param colnames Column names for the output tibble.
#'
#' @return A \code{\link[tibble]{tibble}}
#'
#' @examples
#' \dontrun{
#'   utils_split_and_name("glasgow-entrepreneurs", "-", 2, c("location", "occupation"))
#' }
utils_split_and_name <- function(string, pattern, num_col, colnames) {
  stringr::str_split_fixed(string, pattern, num_col) %>%
    magrittr::set_colnames(colnames) %>% dplyr::as_tibble()
}


## utils_squish_all_columns ####

#' Clear extra white spaces in dataframe
#'
#' Removes blanks (white spaces and tabs) at the beginning and end of all entries
#'   of the provided dataframe. Converts all series of white space and/or tab(s)
#'   in the body of all dataframe entries into a single white space.
#'
#' @param df A dataframe.
#'
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#'   df <- data.frame(
#'     location = "  glasgow ", occupation = "wine    merchant",
#'     stringsAsFactors = FALSE
#'   )
#'   df <- utils_squish_all_columns(df)
#' }
utils_squish_all_columns <- function(df) {
  dplyr::mutate(df, dplyr::across(.cols = dplyr::everything(), stringr::str_squish))
}


## utils_clear_content ####

#' Clear string of matched content
#'
#' Clears the provided string of the content specified as a regex.
#'
#' @param string_search Character string to search for match(es).
#' @param regex_content PCRE type regex provided as a character string
#'   of match(es) to search for.
#' @param ignore_case Boolean specifying whether case should be ignored (`TRUE`)
#'   or not (`FALSE`).
#'
#' @return A character string.
#'
#' @examples
#' \dontrun{
#'   utils_clear_content("glasgow-entrepreneurs", "^.+-", TRUE)
#' }
utils_clear_content <- function(string_search, regex_content, ignore_case){
  gsub(regex_content, "", string_search, ignore.case = ignore_case, perl = TRUE)
}


## utils_mute ####

#' Mute a function call execution
#'
#' Executes the function provided while silencing the potential messages related
#'   to its execution
#'
#' @param fun Function to execute as an expression.
#' @param ... Argument(s) to be passed to the function above for execution.
#'
#' @return Whatever the provided function in `fun` returns.
#'
#' @examples
#' \dontrun{
#'   utils_mute(message, "I'm not showing in console")
#' }
utils_mute <- function(fun, ...){

  out <- suppressWarnings(fun(...))

  if (!is.null(out)) return(out)
}


## utils_execute ####

#' Execute function
#'
#' Executes the function provided. Execution can be silenced via the `verbose`
#'   parameter.
#'
#' @param verbose Boolean specifying whether to silence the function execution
#'   (`FALSE`) or not (`TRUE`).
#' @param fun Function to execute provided as an expression.
#' @param ... Argument(s) to be passed to the function above for execution.
#'
#' @return Whatever the provided function returns.
#'
#' @examples
#' \dontrun{
#'   utils_execute(TRUE, message, "I'm showing in console")
#' }
utils_execute <- function(verbose, fun, ...)
  if (!verbose) utils_mute(fun, ...) else fun(...)


## utils_mutate_across ####

#' Mutate operation(s) in dataframe column(s)
#'
#' Applies provided function across specified column(s) in provided dataframe.
#'
#' @param df A dataframe.
#' @param columns Vector of expression(s) or character string(s) specifying the
#'   columns to apply the function below to in the provided dataframe.
#' @param fun Function to execute provided as an expression.
#' @param ... Argument(s) to be passed to the function above for execution.
#'
#' @return A dataframe.
#'
#' @examples
#' \dontrun{
#'   df <- data.frame(
#'     location = "glasgow", occupation = "wine merchant",
#'     stringsAsFactors = FALSE
#'   )
#'   utils_mutate_across(df, c("location", "occupation"), paste0, "!")
#' }
utils_mutate_across <- function(df, columns, fun, ...){
  dplyr::mutate(df, dplyr::across(.cols = columns, fun, ...))
}


## utils_paste_if_found ####

#' Conditionally amend character string vector.
#'
#' Searches for specified pattern in provided character string. Return pasted
#'   provided character string(s) if found or provided default character string
#'   if not.
#'
#' @param regex_filter Pattern to look for provided as a character string regex.
#' @param string_filter Character string vector to search into for the pattern
#'   provided in `regex_filter` above.
#' @param default Character string returned if pattern provided in `regex_filter`
#'   not found.
#' @param ignore_case Boolean specifying whether case should be ignored (`TRUE`)
#'   or not (`FALSE`).
#' @param ... Character string(s) to be paste together using a space as separator
#'   and returned if pattern provided in `regex_filter` found.
#'
#' @return A character string vector.
#'
#' @examples
#' \dontrun{
#'   utils_paste_if_found(
#'     "^glasgow", c("glasgow-entrepreneurs", "aberdeen-entrepreneurs"),
#'     "pattern not found", TRUE, "pattern", "found"
#'   )
#' }
utils_paste_if_found <- function(regex_filter, string_filter, default, ignore_case, ...){
  dplyr::if_else(
    grepl(regex_filter, string_filter, ignore.case = ignore_case, perl = perl),
    paste(...), default
  ) %>% unlist()
}


## utils_gsub_if_found ####

#' Conditionally amend character string vector.
#'
#' Searches for specified pattern in provided character string vector. If found,
#'   substitutes all occurrences of an alternative pattern in an alternative
#'   character string and returns the output. If not return the default character
#'   string provided.
#'
#' @param regex_filter Pattern to look for provided as a character string regex.
#' @param string_filter Character string vector to search into for the pattern
#'   provided in `regex_filter` above.
#' @param regex_search Alternative pattern provided as a character string regex
#'   to look in the alternative character string provided in `string_search`
#'   below.
#' @param string_replace Substitution character string for matches of
#'   `regex_search` above in `string_search` below.
#' @param string_search Alternative character string to search into for the
#'   pattern provided in `regex_search` above.
#' @param default Character string returned if pattern provided in `regex_filter`
#'   not found.
#' @param ignore_case_filter Boolean specifying whether case should be ignored (`TRUE`)
#'   or not (`FALSE`) in search for `regex_filter` in `string_filter`.
#' @param ignore_case_search Boolean specifying whether case should be ignored (`TRUE`)
#'   or not (`FALSE`) in search for `regex_search` in `string_search`.
#'
#' @return A character string vector.
#'
#' @examples
#' \dontrun{
#'   utils_gsub_if_found(
#'     "^glasgow", c("glasgow-entrepreneurs", "aberdeen-entrepreneurs"),
#'     "(?<=-).+$", "merchant", "edinburgh-entrepreneurs", "pattern not found",
#'     TRUE, TRUE
#'   )
#' }
utils_gsub_if_found <- function(
  regex_filter, string_filter, regex_search, string_replace, string_search,
  default, ignore_case_filter, ignore_case_search
){
  dplyr::if_else(
    grepl(regex_filter, string_filter, ignore.case = ignore_case_filter, perl = perl),
    gsub(
      regex_search, string_replace, string_search,
      ignore.case = ignore_case_search, perl = perl
    ),
    default
  ) %>% unlist()
}


## utils_regmatches_if_found ####

#' Conditionally amend character string vector.
#'
#' Searches for specified pattern in provided character string vector. If found,
#'   searches for alternative pattern in an alternative character string and returns
#'   any match or an empty string if none. If original pattern not found, returns
#'   the default character string provided.
#'
#' @param regex_filter Pattern to look for provided as a character string regex.
#' @param string_filter Character string vector to search into for the pattern
#'   provided in `regex_filter` above.
#' @param regex_search Alternative pattern provided as a character string regex
#'   to look for in the alternative character string provided in `string_search`
#'   below.
#' @param string_search Alternative character string to search into for the
#'   pattern provided in `regex_search` above.
#' @param default Character string returned if pattern provided in `regex_filter`
#'   not found.
#' @param ignore_case_filter Boolean specifying whether case should be ignored
#'   (`TRUE`) or not (`FALSE`) in search for `regex_filter` in `string_filter`.
#' @param ignore_case_match Boolean specifying whether case should be ignored
#'   (`TRUE`) or not (`FALSE`) in search for `regex_search` in `string_search`.
#' @param not Boolean specifying whether to negate the `regex_filter` search
#'   pattern (`TRUE`) or not (`FALSE`).
#'
#' @return A character string vector.
#'
#' @examples
#' \dontrun{
#'   utils_regmatches_if_found(
#'     c("glasgow-entrepreneurs", "aberdeen-entrepreneurs"), "^glasgow",
#'     "edinburgh-entrepreneurs", "^.+(?=-)", "merchant", TRUE, TRUE, FALSE
#'   )
#' }
utils_regmatches_if_found <- function(
  string_filter, regex_filter, string_search, regex_search, default,
  ignore_case_filter, ignore_case_match, not
){
  filter <- paste0(
    ifelse(not, "!", ""),
    "grepl(regex_filter, string_filter, ignore.case = ignore_case_filter, perl = perl)"
  )

  ifelse(
    eval(parse(text = filter)),
    regmatches(
      string_search,
      gregexpr(regex_search, string_search, ignore.case = ignore_case_match, perl = perl)
    ),
    default
  ) %>% unlist()
}


## utils_regmatches_if_not_empty ####

#' Conditionally amend character string vector.
#'
#' Searches for non-empty string in provided character string vector. If found
#'    searches for alternative pattern in an alternative character string and
#'    returns any match or an empty string if none.
#'
#' @param string_filter A Character string vector.
#' @param string_search Alternative character string to search into for the
#'   pattern provided in `regex_search` below
#' @param regex_search Alternative pattern provided as a character string regex
#'   to look for in the alternative character string provided in `string_search`
#'   above.
#' @param ignore_case_search Boolean specifying whether case should be ignored
#'   (`TRUE`) or not (`FALSE`) in search for `regex_search` in `string_search`.
#'
#' @return A list of character string vectors.
#'
#' @examples
#' \dontrun{
#'   utils_regmatches_if_not_empty(
#'     c("glasgow-entrepreneurs", "", "aberdeen-entrepreneurs"),
#'     "edinburgh-entrepreneurs" , "^edinburgh", TRUE
#'   )
#' }
utils_regmatches_if_not_empty <- function(
  string_filter, string_search, regex_search, ignore_case_search
){
  ifelse(
    string_filter == "", string_filter,
    regmatches(
      string_search,
      gregexpr(regex_search, string_search, ignore.case = ignore_case_search, perl = perl)
    )
  )
}

