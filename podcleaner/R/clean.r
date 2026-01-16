# Define globals ####
`%>%` <- magrittr::`%>%`
ignore_case <- TRUE
perl <- TRUE


# Main ####

## Address ####

### clean_address_body ####

#' Clean address entry(/ies) body
#'
#' Attempts to clean body of address entry(/ies) provided.
#'
#' @param addresses A character string vector of address(es).
#'
#' @return A character string vector of address(es) with cleaned bodies.
clean_address_body <- function(addresses){

  # Pre-clean
  clean <- clean_address_pre_clean(addresses)
  # Places
  clean <- clean_address_places(clean)
  # Worksites
  clean <- clean_address_worksites(clean)
  # Names
  clean <- clean_address_names(clean)
  # Post clean
  clean <- clean_address_post_clean(clean)

  clean
}

### clean_address_number ####

#' Clean address entry numbers
#'
#' Attempts to clean number of address entry(/ies) provided.
#'
#' @param addresses A character string vector of address(es).
#'
#' @return A character string vector of address(es) with cleaned numbers.
clean_address_number <- function(addresses){

  clean <- clean_string_ends(addresses)

  # OCR errors

  ## clean misread numbers
  purrr::pwalk(globals_numbers, function(pattern, replacement, ignore_case){
    clean <<- gsub(pattern, replacement, clean, ignore.case = ignore_case, perl = perl)
  })

  ## get rid of random chars around digits
  clean <- gsub('(?<=\\d)["*-](?=\\d)?', "", clean, ignore.case = FALSE, perl = perl)
  clean <- gsub("(?<=\\d)['](?=\\d)?", "", clean, ignore.case = FALSE, perl = perl)


  # separate 2 3-digit address numbers stuck together
  clean <- gsub(
    "(\\d{2,3})\\s?(\\d{2,3})(?!\\/)", "\\1, \\2", clean,
    ignore.case = ignore_case, perl = perl
  )


  # separate address numbers with commas in place of "&" or "and"
  clean <- gsub(
    "(\\d),?\\s?(?:f|and|&|,)\\s?(\\d)", "\\1, \\2", clean,
    ignore.case = ignore_case, perl = perl
  )


  # harmonise address ranges to first number-second number
  clean <- gsub(
    "(?<=\\d),?\\s?to,?\\s?(?=\\d)", "-", clean,
    ignore.case = ignore_case, perl = perl
  )
  clean <- gsub(
    "(\\d+)\\s(\\d)+", "\\1-\\2", clean, ignore.case = ignore_case, perl = perl
  )


  # separate numbers from words
  clean <- gsub(
    "((?<=\\d))((?=[a-z]))", "\\1 \\2", clean,
    ignore.case = ignore_case, perl = perl
  )


  # delete No. before numbers
  clean <- gsub(
    "No[.\\s]+?(?=\\d)", "", clean, ignore.case = ignore_case, perl = perl
  )


  # delete space or period between digits
  clean <- gsub(
    "(?<=\\d)[\\s.](?=\\d(?!\\/))", "", clean,
    ignore.case = ignore_case, perl = perl
  )

  # fix 1/2 issue
  clean <- gsub("0\\.5", " 1/2", clean, ignore.case = ignore_case, perl = perl)
  clean <- gsub(
    "(?<=\\d)(1\\/2)", " \\1", clean, ignore.case = ignore_case, perl = perl
  )


  clean
}

## Name ####

### clean_title ####

#' Clean entry(/ies) name title
#'
#' Attempts to clean titles attached to names provided: Captain, Major, etc.
#'
#' @param names A character string vector of name(s).
#'
#' @return A character string vector of name(s) with cleaned title(s).
clean_title <- function(names){

  clean <- names

  purrr::pwalk(globals_titles, function(pattern, replacement, ignore_case){
    regex_title <- paste0(
      "[[:punct:][:blank:]]*(?:", pattern, ")[[:punct:][:blank:]]*(?=\\s[\\w\\$])"
    )

    clean <<- gsub(regex_title, replacement, clean, ignore_case, perl = perl)
  })

  clean

}

### clean_forename ####

#' Clean entry(/ies) forename
#'
#' Attempts to clean provided forename.
#'
#' @param names A character string vector of forename(s).
#'
#' @return A character string vector of cleaned forename(s).
#'
#' @section Details:
#' Single letter forenames are standardised to the forename starting with that
#'   letter occurring the most frequently in the dataset. i.e A. -> Alexander,
#'   B. -> Bernard, C. -> Colin, D. -> David, etc.
clean_forename <- function(names){

  clean <- clean_mac(names)
  clean <- clean_forename_separate_words(clean)
  clean <- clean_forename_spelling(clean)
  clean <- clean_forename_punctuation(clean)
  clean <- clean_name_ends(clean)

  clean
}

### clean_surname ####

#' Clean entry(/ies) surname
#'
#' Attempts to clean provided surname.
#'
#' @param names A character string vector of surname(s).
#'
#' @return A character string vector of cleaned surname(s).
#'
#' @section Details:
#' Multiple spelling names are standardised to that of the capital letter header
#'   in the general directory. i.e. Abercrombie, Abercromby -> Abercromby;
#'   Bayne, Baynes -> Bayne; Beattie, Beatty -> Beatty; etc.
clean_surname <- function(names){

  clean <- clean_mac(names)
  clean <- clean_parentheses(clean)
  clean <- clean_surname_spelling(clean)
  clean <- clean_surname_punctuation(clean)
  clean <- clean_name_ends(clean)

  clean
}

## clean_occupation ####

#' Clean entry(/ies) occupation
#'
#' Attempts to clean provided occupation.
#'
#' @param occupations A character string vector of occupation(s).
#'
#' @return A character string vector of cleaned occupation(s).
clean_occupation <- function(occupations){

  clean <- occupations

  purrr::pwalk(globals_occupations, function(pattern, replacement, ignore_case){
    clean <<- gsub(pattern, replacement, clean, ignore_case, perl = perl)
  })

  clean
}



# Utils ####

## clean_specials ####

#' Clean entry(/ies) special characters
#'
#' Attempts to clean entry(/ies) of unwanted special character(s).
#'
#' @param x A character string vector.
#'
#' @return A character string vector with special character(s) removed.
clean_specials <- function(x){

  clean <- gsub('[\u00bb\u00a6\u2022"]', "", x, ignore.case = ignore_case, perl = perl)
  clean <- gsub("\u2019", "'", clean, ignore.case = ignore_case, perl = perl)

  clean
}

## clean_parentheses ####

#' Clean entry(/ies) of in brackets information
#'
#' Attempts to clean entry(/ies) of unwanted information displayed in brackets.
#'
#' @param x A character string vector.
#'
#' @return A character string vector with within brackets content removed.
clean_parentheses <- function(x){

  clean <-gsub("(.*?)\\s?\\(.*", "\\1", x, ignore.case = ignore_case, perl = perl)
  clean
}

## clean_mac ####

#' Standardise "Mac" prefix in people's name
#'
#' Attempts to standardise "Mac" prefix in provided name entry(/ies).
#'
#' @param names A character string vector of name(s).
#'
#' @return A character string vector of name(s) with clean "Mac" prefix(es).
clean_mac <- function(names){

  # Standardise Mac prefix
  clean <- gsub(
    "\\bMa?c(\\w{2,}\\b)", "Mac \\1", names,
    ignore.case = ignore_case, perl = perl
  )
  clean
}







# Helpers ####

## Address ####

### clean_address_pre_clean ####

#' Pre-cleaning operation for address entry(/ies)
#'
#' Performs pre-cleaning operations on provided address entry(/ies).
#'
#' @param addresses A character string vector of address(es).
#'
#' @return A character string vector with address(es) cleaner than the one
#'   provided in `addresses`.
clean_address_pre_clean <- function(addresses){

  # Special characters
  clean <- clean_specials(addresses)
  # Clean ends
  clean <- clean_address_ends(clean)
  # Separate words
  clean <- clean_address_attached_words(clean)
  # clean Mac name pre-fixes
  # clean <- address_clean_mac(clean)
  # Saints
  clean <- clean_address_saints(clean)
  # Possessives
  clean <- clean_address_possessives(clean)
  # Suffixes
  clean <- clean_address_suffixes(clean)

  clean
}


### clean_address_post_clean ####

#' Post-cleaning operation for address entry(/ies)
#'
#' Performs post-cleaning operations on provided address entry(/ies).
#'
#' @param addresses A character string vector of address(es).
#'
#' @return A character string vector with address(es) cleaner than the one
#'   provided in `addresses`.
clean_address_post_clean <- function(addresses){

  # Others
  clean <- clean_address_others(addresses)
  # Clean ends
  clean <- clean_address_ends(clean)

  clean
}


### clean_address_mac ####

#' Standardise "Mac" prefix in address entry(/ies)
#'
#' Attempts to standardise "Mac" prefix in provided address entry(/ies).
#'
#' @param addresses A character string vector of address(es).
#'
#' @return A character string vector of addresses with clean "Mac" prefix(es).
clean_address_mac <- function(addresses){

  clean <- addresses

  purrr::pwalk(globals_macs, function(pattern, replacement, ignore_case){
    clean <<- gsub(
      pattern, paste0(replacement, " "), clean,
      ignore.case = ignore_case, perl = perl
    )
  })

  clean
}


### clean_address_saints ####

#' Clean "Saint" prefix in address entry(/ies)
#'
#' Attempts to clean "Saint" prefix in provided address entry(/ies).
#'
#' @param addresses A character string vector of address(es).
#'
#' @return A character string vector of address(es) with clean "Saint" prefix(es).
clean_address_saints <- function(addresses){

  clean <- gsub(
    paste0(
      "\\bst[\\.,;]*?\\s*?(?=",
      paste0(globals_saints$pattern, collapse = "|"),
      ")"
    ),
    "Saint ",
    addresses, ignore.case = ignore_case, perl = perl
  )

  purrr::pwalk(globals_saints, function(pattern, replacement, ignore_case) {
    clean <<- gsub(
      paste0("Saint\\s", pattern), paste0("Saint ", replacement),
      clean, ignore.case = ignore_case, perl = perl
    )
  })

  clean
}


### clean_address_possessives ####

#' Standardise possessives in address entry(/ies)
#'
#' Attempts to standardise possessives in provided address entry(/ies).
#'
#' @param addresses A character string vector of address(es).
#'
#' @return A character string vector of address(es) with clean possessive(s).
clean_address_possessives <- function(addresses){

  clean <- gsub(
    "\\b(\\w+)\\b\\.?'?s'?", "\\1s", addresses,
    ignore.case = ignore_case, perl = perl
  )
  clean <- gsub(
    "(\\b\\w+s\\b)'\\.?", "\\1", clean, ignore.case = ignore_case, perl = perl
  )

  clean
}

### clean_address_ends ####

#' Clean ends in address entry(/ies)
#'
#' Attempts to clean ends in provided address entry(/ies).
#'
#' @param addresses A character string vector of address(es).
#'
#' @return A character string vector of address(es) with clean ends.
clean_address_ends <- function(addresses){

  # Trim white space(s) at start and end as well as multiple white spaces in a row
  clean <- stringr::str_squish(addresses)
  # clean <- gsub("^[[:space:][:punct:]]+", "", address, ignore.case = ignore_case, perl = perl)
  clean <- gsub("^[\\W]+", "", clean, ignore.case = ignore_case, perl = perl)
  clean <- gsub("\\W+$", "", clean, ignore.case = ignore_case, perl = perl)
  clean <- gsub("\\s{2,}", " ", clean, ignore.case = ignore_case, perl = perl)

  # Remove single letter at start or end of address.
  clean <- gsub(
    "(?:^[A-Za-z]\\s|,?\\s[A-Za-z]$)", "", clean, ignore.case = FALSE, perl = perl
  )

  # Place period at the end of address if none
  clean <- gsub("([^.])$", "\\1\\.", clean, ignore.case = ignore_case, perl = perl)

  clean <- stringr::str_squish(clean)

  clean
}

### clean_address_attached_words ####

#' Clean attached words in address entry(/ies)
#'
#' Attempts to separate attached words in provided address entry(/ies).
#'
#' @param addresses A character string vector of address(es).
#'
#' @return A character string vector of address(es) cleaned of attached words.
clean_address_attached_words <- function(addresses){

  # If two words are only separated by a period or comma, replace period with
  # white space
  clean <- gsub(
    "([a-z])[.,]([a-z])", "\\1 \\2", addresses,
    ignore.case = ignore_case, perl = perl
  )

  # if lower case followed by upper case, add space between the two.
  clean <- gsub(
    "([a-z])([A-Z])", "\\1 \\2", clean, ignore.case = FALSE, perl = perl
  )

  clean <- stringr::str_squish(clean)

  clean
}

### clean_address_places ####

#' Clean places in address entry(/ies)
#'
#' Attempts to clean places in provided address entry(/ies): street, road, place,
#'   quay, etc.
#'
#' @param addresses A character string vector of address(es).
#'
#' @return A character string vector of address(es) with clean place name(s).
clean_address_places <- function(addresses){

  clean <- addresses

  purrr::pwalk(globals_places_regex, function(pattern, replacement, ignore_case) {
    clean <<- gsub(pattern, replacement,clean, ignore.case = ignore_case, perl = perl
    )
  })

  clean <- gsub(
    "((?<!Mil|Mill|B))(road|street)\\.?", "\\1 \\2", clean,
    ignore.case = ignore_case, perl = perl
  )

  clean <- stringr::str_squish(clean)

  clean
}

### clean_address_worksites ####

#' Clean worksites in address entry(/ies)
#'
#' Attempts to clean worksites in provided address entry(/ies).
#'
#' @param addresses A character string vector of address(es).
#'
#' @return A character string vector of address(es) with clean worksite name(s).
clean_address_worksites <- function(addresses){

  clean <- addresses

  purrr::pwalk(globals_worksites, function(pattern, replacement, ignore_case){
    clean <<- gsub(pattern, replacement, clean, ignore.case = ignore_case, perl = perl)
  })

  clean
}

### clean_address_suffixes ####

#' Clean unwanted suffixes in address entry(/ies)
#'
#' Attempts to clean unwanted suffixes in provided address entry(/ies).
#'
#' @param addresses A character string vector of address(es).
#'
#' @return A character string vector of address(es) with unwanted suffix(es) removed.
clean_address_suffixes <- function(addresses){

  clean <- addresses

  purrr::pwalk(globals_suffixes, function(pattern, replacement, ignore_case) {
    clean <<- gsub(pattern, replacement,clean, ignore.case = ignore_case, perl = perl
    )
  })

  clean <- stringr::str_squish(clean)

  clean
}

### clean_address_names ####

#' Clean place name(s) in address entry(/ies)
#'
#' Attempts to clean place names in provided address entry(/ies).
#'
#' @param addresses A character string vector of address(es).
#'
#' @return A character string vector of address(es) with clean name(s).
clean_address_names <- function(addresses){

  clean <- addresses

  purrr::pwalk(globals_address_names, function(pattern, replacement, ignore_case) {
    clean <<- gsub(pattern, replacement,clean, ignore.case = ignore_case, perl = perl
    )
  })

  clean <- stringr::str_squish(clean)

  clean
}

### clean_address_others ####

#' Miscellaneous cleaning operations in address entry(/ies)
#'
#' Carries out miscellaneous cleaning operations in provided address entry(/ies).
#'
#' @param addresses A character string vector of address(es).
#'
#' @return A character string vector of clean address(es).
clean_address_others <- function(addresses){

  # Get rid of parasite postfixes
  ## (Agents)
  clean <- gsub(
    "(.*?)\\s\\(?\\bag(?:en)?ts?\\b\\)?.?", "\\1", addresses,
    ignore.case = ignore_case, perl = perl
  )

  # Get rid of parasite punctation
  clean <- gsub("'", "", clean, ignore.case = ignore_case, perl = perl)
  clean <- gsub('\\"\\b', "", clean, ignore.case = ignore_case, perl = perl)
  clean <- gsub("\\b[\\-.]\\s", " ", clean, ignore.case = ignore_case, perl = perl)
  clean <- gsub(
    "\\b([[:punct:]])[[:punct:]]\\s", "\\1 ", clean,
    ignore.case = ignore_case, perl = perl
  )
  clean <- gsub(
    "\\b[[:punct:]]\\s(?=\\w)", " ", clean, ignore.case = ignore_case, perl = perl
  )
  clean <- gsub(
    "(?<=\\s)[[:punct:]]\\s", "\\1", clean, ignore.case = ignore_case, perl = perl
  )
  clean <- gsub(
    "\\s[[:punct:]]\\s", "\\s", clean, ignore.case = ignore_case, perl = perl
  )

  # Separate numbers with comma
  clean <- gsub(
    "(?<=\\d)\\s+(?=\\d)", ", ", clean, ignore.case = ignore_case, perl = perl
  )

  # Remove single letters
  clean <- gsub(
    "(?:\\s[a-z]\\b|\\s\\.[a-z]\\.(?:\\s|$))", "", clean,
    ignore.case = FALSE, perl = perl
  )
  clean <- gsub("\\b\\s[a-z]\\s\\b", "", clean, ignore.case = FALSE, perl = perl)

  # Unwanted processing outcomes
  ## "street" processing sometimes outputs orpheans ", reet,". Replace with comma.
  clean <- gsub(", reet,", ",", clean, ignore.case = ignore_case, perl = perl)

  # If placed between two words, replace period with a comma.
  clean <- gsub("\\b\\.\\s\\b", ", ", clean, ignore.case = ignore_case, perl = perl
  )

  # If place not located at the end of the address and not followed by a word that
  # is itself a place, append a comma.
  clean <- gsub(
    paste0(
      "\\b(",
      paste(globals_places_raw, collapse = "|"),
      ")\\b(?!$|[,\\-]|\\s+?(?:\\(|",
      paste(globals_places_raw, collapse = "|"),
      "))"),
    "\\1, ",
    clean,
    ignore.case = ignore_case, perl = perl
  )

  # If place not located at the beginning of the address and separated from previous
  # word-that is not itself a place-with comma and space, delete comma.
  clean <- gsub(
    paste0(
      "(?<!",
      paste(globals_places_raw, collapse = "|"),
      "),\\s+?(",
      paste(globals_places_raw, collapse = "|"),
      ")"
    ),
    " \\1",
    clean,
    ignore.case = ignore_case, perl = perl
  )

  # Clean address ends
  clean <- clean_string_ends(clean) %>% stringr::str_squish()

  clean
}


## Names ####

### clean_name_ends ####

#' Clean ends in entry(/ies) names
#'
#' Attempts to clean ends in provided name entry(/ies).
#'
#' @param names A character string vector of names
#'
#' @return A character string vector of names with clean ends.
clean_name_ends <- function(names){
  clean <- gsub(
    "\\b\\W*$", "", names, ignore.case = ignore_case, perl = perl
  )
  clean <- gsub(
    "^\\W*\\b", "", clean, ignore.case = ignore_case, perl = perl
  )

  clean
}

### Forename ####

#### clean_forename_separate_words ####

#' Separate double-barrelled forename(s)
#'
#' Attempts to separate double-barrelled forename(s) in provided forename entry(/ies).
#'
#' @param forenames A character string vector of forename(s).
#'
#' @return A character string vector of forename(s) with clean double-barrelled
#'   forename(s).
clean_forename_separate_words <- function(forenames){

  clean <- gsub(
    "(?<=[a-z.])([A-Z])", " \\1", forenames, ignore.case = FALSE, perl = perl
  )
  clean <- gsub(
    "([A-Z])([A-Z])", "\\1. \\2 ", clean, ignore.case = FALSE, perl = perl
  )
  clean <- gsub(
    "([A-Za-z])&([A-Z])", "\\1 & \\2", clean, ignore.case = FALSE, perl = perl
  )
  clean <- gsub(
    "([A-Z])\\.([A-Z]+\\b)", "\\1\\. \\2", clean,
    ignore.case = ignore_case, perl = perl
  )
  clean
}

#### clean_forename_punctuation ####

#' Standardise punctuation in forename(s)
#'
#' Attempts to standardise punctuation in provided forename entry(/ies).
#'
#' @param forenames A character string vector of forename(s).
#'
#' @return A character string vector of forename(s) with clean punctuation.
clean_forename_punctuation <- function(forenames){

  # Standardise end of string punctuation to a period.
  clean <- gsub("\\W*$", ".", forenames, ignore.case = ignore_case, perl = perl)
  # # Empty entry(/ies) remain empty.
  # clean <- gsub("^\\.$", "", clean, ignore.case = ignore_case, perl = perl)

  # Standardise inbetween words to a space
  clean <- gsub(
    "\\b[,'.\\s]+\\b", " ", clean, ignore.case = ignore_case, perl = perl
  )

  # Add period to single capital letters and Mrs/Miss/Misses
  clean <- gsub(
    "(\\b(?:[A-Z]|Mrs|Miss|Misses)\\b)(?!\\.)", "\\1.", clean,
    ignore.case = FALSE, perl = perl
  )

  # Empty entry(/ies) remain empty.
  clean <- gsub(
    "^[.[:blank:]]+$", "", clean, ignore.case = ignore_case, perl = perl
  )

  clean
}

#### clean_forename_spelling ####

#' Clean forename(s) spelling
#'
#' Attempts to clean spelling in provided forename entry(/ies).
#'
#' @param forenames A character string vector of forename(s).
#'
#' @return A character string vector of forename(s) with clean forename(s) spelling.
clean_forename_spelling <- function(forenames){

  clean <- forenames

  # Clean OCR errors, standardise spelling
  purrr::pwalk(globals_forenames, function(pattern, replacement, ignore_case) {
    clean <<- gsub(
      pattern, replacement, clean, ignore.case = ignore_case, perl = perl
    )
  })

  clean
}

### Surname ####

#### clean_surname_punctuation ####

#' Standardise punctuation in surname(s)
#'
#' Attempts to standardise punctuation in provided surname entry(/ies).
#'
#' @param surnames A character string vector of surname(s).
#'
#' @return A character string vector of surname(s) with clean punctuation.
clean_surname_punctuation <- function(surnames){

  # Get rid of orphan star characters
  clean <- gsub("\\*", "", surnames, ignore.case = ignore_case, perl = perl)

  clean
}

#### clean_surname_spelling ####

#' Clean surname(s) spelling
#'
#' Attempts to clean spelling in provided surname entry(/ies).
#'
#' @param surnames A character string vector of surnames.
#'
#' @return A character string vector of surnames with clean spelling.
clean_surname_spelling <- function(surnames){

  clean <- surnames

  # Clean OCR errors, standardise spelling
  purrr::pwalk(globals_surnames, function(pattern, replacement, ignore_case) {
    clean <<- gsub(
      pattern, replacement, clean, ignore.case = ignore_case, perl = perl
    )
  })

  # Get rid of Junior/Senior postfix
  clean <- gsub(
    "\\s+\\b(?:Junior|Senior)\\b[\\s$]?", "", clean,
    ignore.case = ignore_case, perl = perl
  )

  clean
}

## Others ####

### clean_string_ends ####

#' Clean string ends
#'
#' Attempts to clean ends of strings provided.
#'
#' @param strings A character string vector.
#'
#' @return A character string vector with clean entry(/ies) ends.
clean_string_ends <- function(strings){
  clean <- gsub(
    "^[[:punct:][:space:]]+\\b", "", strings,
    ignore.case = ignore_case, perl = perl
  )
  clean <- gsub(
    "\\b[[:punct:][:space:]]+$", "", clean, ignore.case = ignore_case, perl = perl
  )

  clean
}
