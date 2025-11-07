#' Regex miRNAs.
#' @noRd
pattern_mir <- '[Mm][Ii][Cc]?[Rr][Oo]?[Rr]?[Nn]?[Aa]?[\\-\\s]?\\d+[a-zA-Z]?[\\-]?[12345]?[\\-]?[35]?[Pp]?'

#' Regex let-7
#' @noRd
pattern_let <- '[Ll][Ee][Tt][\\-\\s]?\\d+[a-zA-Z]?[\\-]?[35]?[Pp]?'

#' Extract miRNAs from abstracts - Helper.
#'
#' Helper function. Extract miRNAs from strings, that can then be used
#' to extract miRNAs from abstracts.
#'
#' @param string String. Specifies which string to search for miRNAs.
#' @param threshold Integer. Specifies how often a miRNA must be mentioned to be extracted.
#' @param extract_letters Boolean. If extract_letters = TRUE, extract trailing
#' letters of miRNAs. If extract_letters = FALSE, extract only miRNA with number.
#' @importFrom magrittr %>%
#'
#' @noRd
retrieve_mir <- function(string,
                         threshold = 1,
                         extract_letters = FALSE) {

  . = NULL

  # This algorithm is trickier than expected; on the one hand, you would
  # like to make look ahead work, on the other hand, it might lead to trouble;

  # Insert "-" between "miRNA[number]". This is necessary as the next step would
  # otherwise remove the trailing digit behind the "A".
  string <- stringr::str_replace_all(string,
                                     "(?<=[Rr][Nn][Aa])([0-9])",
                                     "-\\1")

  # Clean string from miR2Disease, trailing digits right behind a letter (e.g. MiR7a2)

  string <- stringr::str_remove_all(string, "miR2Disease") %>% # removes miR2Disease
    stringr::str_remove_all("(?<=[a-fA-F])([0-9])") %>%   # removes any trailing digit right behind a letter
    stringr::str_remove_all("(?<=\\d[a-fA-F]?)(-2[s\\.])") %>%
    stringr::str_remove_all("3'") %>%
    stringr::str_remove_all("5'")

  #Extract first miRNAs

  miR <-  stringr::str_extract_all(string, pattern_mir) %>%
    unlist()

   mir_catcher <- c()

   # Catch chained letters (e.g. miR-33a/b)
   mir_chained_letter <- stringr::str_extract_all(string,
                                                  "([Mm][Ii][Cc]?[Rr][Oo]?[Rr]?[Nn]?[Aa]?[\\-\\s]?\\d+)([a-z][/]?)+",
                                                  simplify = TRUE) %>%
     stringr::str_match("([Mm][Ii][Cc]?[Rr][Oo]?[Rr]?[Nn]?[Aa]?[\\-\\s]?\\d+)(.*)")


   # Vectorize over chained letters (e.g. miR-33a/b to miR-33a and miR-33b)
   mir_chained_letter <- mir_chained_letter %>%
     as.data.frame(stringsAsFactors = FALSE) %>%
     dplyr::as_tibble() %>%
     dplyr::select(2:3) %>%
     # Get rid of / if / is last character (e.g. miR-146a/TRAF6-axis might cause trouble)
     dplyr::mutate(V3 = stringr::str_replace_all(V3, "(.*)(/$)", "\\1")) %>%
     tidyr::separate_rows(V3, sep = "/") %>%
     tidyr::unite(col = "Coerced", sep = "") %>%
     dplyr::pull() %>%
     unique()

   mir_catcher <- c(mir_catcher, mir_chained_letter)

   #Check if miRNAs are chained (e.g. miR-34/-27/-29 etc.)

  for (miRNA in miR) {
    pattern_look_ahead <- stringr::str_c("(?<=", miRNA, ")([/,;and\\s?]+[-]?\\d+[a-z]?[-]?[1-5]?[-]?[53]?[pP]?\\s?)+",
                                         collapse = "")
    miR_2 <- stringr::str_extract_all(string, pattern_look_ahead) %>%
      unlist()
    mir_catcher <- c(mir_catcher, miR_2)
  }


  #Clean miRNA names

  miRNAs <- c(miR, mir_catcher) %>%
    stringr::str_replace_all("[Mm][Ii][Cc]?[Rr][Oo]?[Rr]?[Nn]?[Aa]?[\\-\\s]?", "") %>% #clean microRNA- etc.
    stringr::str_replace_all("[\\-\\s]?3[Pp]|[\\-\\s]?5[Pp]", "") %>% #clean -3p/-5p
    #unique() %>% # Getting rid of "unique" here to count the miRNAs subsequently
    stringr::str_c(collapse = " ") %>% #unify string
    stringr::str_replace_all("(?<=\\d[a-zA-Z]?)(-[12345])", "") %>% #clean trailing -1, -2 (e.g. miR-27-1)
    stringr::str_replace_all("(?<=[a-zA-Z])([12345])", "") %>% # clean trailing 1, 2 after a letter (e.g. 196a2)
    stringr::str_replace_all("and", "")

  miRNA_numbers <- stringr::str_extract_all(miRNAs, pattern = "\\d+[a-zA-Z]?") %>% #extract miRNA numbers
    unlist()

  if(!purrr::is_empty(miRNA_numbers)) {
    miRNAs_final <- stringr::str_c("miR-", miRNA_numbers) #compose miR- + number
  } else {
    miRNAs_final <- ""
  }

  #Extract let-7

  let <- stringr::str_extract_all(string, pattern_let) %>%
    unlist() %>%
    stringr::str_replace_all("[Ll][Ee][Tt][\\-\\s]?", "let-") %>%
    stringr::str_replace_all("[\\-\\s]?3[Pp]|[\\-\\s]?5[Pp]", "") #%>% #clean -3p/-5p
    #unique() Also keeping the unique here before deprecating it

  #Join miRNAs and let

  vec <- c(miRNAs_final, let) #%>%
    #unique()

  vec <- vec[!is.na(vec)]

  # Turns trailing letter behind number to lowercase (e.g. miR-125B to miR-125b)
  if(extract_letters == TRUE) {
    vec <- sub("(?<=\\d)([A-Z])", "\\L\\1", vec, perl = TRUE)
  }

  # Replace old miRNA name versions with new miRNA name as of
  # miRBase version 22
  vec <- vec %>%
    gsub("miR-97$", "miR-30a", .) %>%
    gsub("miR-102$", "miR-29b", .) %>%
    gsub("miR-180a$", "miR-172a", .) %>%
    gsub("miR-180b$", "miR-172b", .) %>%
    gsub("miR-180$", "miR-172", .)


  if(extract_letters == FALSE) {
    # drop trailing letters
    vec <- stringr::str_replace_all(vec, "(?<=\\d)([a-zA-Z])", "") #%>%
      #unique() Here as well the deprecation of "unique" for the new addition (below)
  }

  # New Addition --------------------------
  # count miRNAs in abstract and return if they are mentioned above threshold
  # Count miRNAs in vec
  mir_count <- as.data.frame(table(vec), stringsAsFactors = FALSE)
  # Subset count for miRNAs mentioned above a threshold
  mir_subset <- subset(mir_count, Freq >= threshold)
  # Get miRNA names
  vec <- mir_subset[["vec"]] %>%
    unique()


  if (!purrr::is_empty(vec)) {
    return (vec)
  } else {
    return("")
  }
}

#' Extract miRNA names from abstracts in data frame
#'
#' Extract miRNA names from abstracts in a data frame.
#'
#' Extract miRNA names from abstracts in a data frame. miRNA names can
#' either be extracted with their stem only, e.g. *miR-23*, or with their trailing
#' letter, e.g. *miR-23a*. miRNA names are adapted to the most recent miRBase
#' version (e.g. miR-97, miR-102, miR-180(a/b) become miR-30a, miR-29a,
#' and miR-172(a/b), respectively). Additionally, how often a miRNA must be
#' mentioned in an
#' abstract to be extracted can be regulated via the `threshold` argument.
#' Ultimately, abstracts not containing any miRNA names
#' are silently dropped.
#' As many abstracts do not adhere to the miRNA nomenclature,
#' it is recommended to extract only the miRNA stem with
#' `extract_letters = FALSE`.
#'
#' @param df Data frame containing abstracts.
#' @param threshold Integer. Specifies how often a miRNA must be mentioned in an
#' abstract to be extracted.
#' @param col.abstract Symbol. Column containing abstracts.
#' @param extract_letters Boolean. If `extract_letters = FALSE`, only the miRNA stem
#' is extracted (e.g. *miR-23*). If `extract_letters = TRUE`, the miRNA stem with
#' trailing letter (e.g. *miR-23a*) is extracted.
#'
#' @return Data frame with miRNA names extracted from abstracts.
#'
#' @seealso [extract_mir_string()]
#'
#' @family extract functions
#'
#' @importFrom textclean replace_non_ascii
#'
#' @export
extract_mir_df <- function(df,
                           threshold = 1,
                           col.abstract = Abstract,
                           extract_letters = FALSE) {
  df_miRNA <- df %>%
    dplyr::mutate(miR_extract =
                     textclean::replace_non_ascii({{col.abstract}},
                                                  remove.nonconverted = FALSE)) %>%
    dplyr::mutate(miRNA = purrr::map(miR_extract, ~ retrieve_mir(string = .x,
                                                                 threshold = threshold,
                                                                 extract_letters = extract_letters))) %>%
    tidyr::unnest(miRNA) %>%
    dplyr::filter(miRNA != "") %>%
    dplyr::select(-miR_extract)

  return(df_miRNA)
}

#' Extract miRNA names from string
#'
#' Extract miRNA names from a string.
#'
#' Extract miRNA names from a string. miRNA names can
#' either be extracted with their stem only, e.g. *miR-23*, or with their trailing
#' letter, e.g. *miR-23a*. Furthermore, miRNA names are adapted to the most recent
#' miRBase
#' version (e.g. miR-97, miR-102, miR-180(a/b) become miR-30a, miR-29a,
#' and miR-172(a/b), respectively).
#'
#' @param string String. String to search for miRNA names.
#' @param threshold Integer. Specifies how often a miRNA must be mentioned in `string`
#' to be extracted.
#' @param extract_letters Boolean. If `extract_letters = FALSE`, only the miRNA stem
#' is extracted (e.g. *miR-23*). If `extract_letters = TRUE`, the miRNA stem with
#' trailing letter (e.g. *miR-23a*) is extracted.
#'
#' @return Character vector containing
#' miRNA names, if miRNA names are present in the string.
#' If no miRNA names are present in the string, a message
#' is returned saying *"No miRNA found."*.
#'
#' @seealso [extract_mir_df()]
#'
#' @family extract functions
#'
#' @export
#'
#' @importFrom magrittr %>%
extract_mir_string <- function(string,
                               threshold = 1,
                               extract_letters = FALSE) {

  . = NULL

  #Extract first miRNAs

  miR <- stringr::str_extract_all(string, pattern_mir) %>%
    unlist()

  #Check if miRNAs are chained (e.g. miR-34/-27/-29 etc.)

  mir_catcher <- c()
  for (miRNA in miR) {
    pattern_look_ahead <- stringr::str_c("(?<=", miRNA, ")([/,and\\s]+[-]?\\d+[a-z]?[-]?[1-5]?[-]?[53]?[pP]?\\s?)+", collapse = "")
    miR_2 <- stringr::str_extract_all(string, pattern_look_ahead) %>%
      unlist()
    mir_catcher <- c(mir_catcher, miR_2)
  }


  #Clean miRNA names

  miRNAs <- c(miR, mir_catcher) %>%
    stringr::str_replace_all("[Mm][Ii][Cc]?[Rr][Oo]?[Rr]?[Nn]?[Aa]?[\\-\\s]?", "") %>% #clean microRNA- etc.
    stringr::str_replace_all("[\\-\\s]?3[Pp]|[\\-\\s]?5[Pp]", "") %>% #clean -3p/-5p
    #unique() %>%
    stringr::str_c(collapse = " ") %>% #unify string
    stringr::str_replace_all("(?<=\\d[a-zA-Z]?)(-[12345])", "") %>% #clean trailing -1, -2 (e.g. miR-27-1)
    stringr::str_replace_all("and", "")

  miRNA_numbers <- stringr::str_extract_all(miRNAs, pattern = "\\d+[a-zA-Z]?") %>% #extract miRNA numbers
    unlist()

  if(!purrr::is_empty(miRNA_numbers)) {
    miRNAs_final <- stringr::str_c("miR-", miRNA_numbers) #compose miR- + number
  } else {
    miRNAs_final <- ""
  }

  #Extract let-7

  let <- stringr::str_extract_all(string, pattern_let) %>%
    unlist() %>%
    stringr::str_replace_all("[Ll][Ee][Tt][\\-\\s]?", "let-") %>%
    stringr::str_replace_all("[\\-\\s]?3[Pp]|[\\-\\s]?5[Pp]", "") %>% #clean -3p/-5p
    unique()

  #Join miRNAs and let

  vec <- c(miRNAs_final, let) #%>%
    #unique()

  vec <- vec[!is.na(vec)]


  # Turns trailing letter behind number to lowercase (e.g. miR-125B to miR-125b)
  if(extract_letters == TRUE) {
    vec <- sub("(?<=\\d)([A-Z])", "\\L\\1", vec, perl = TRUE)
  }

  # Replace old miRNA name versions with new miRNA name as of
  # miRBase version 22
  vec <- vec %>%
    gsub("miR-97$", "miR-30a", .) %>%
    gsub("miR-102$", "miR-29b", .) %>%
    gsub("miR-180a$", "miR-172a", .) %>%
    gsub("miR-180b$", "miR-172b", .) %>%
    gsub("miR-180$", "miR-172", .)

  if(extract_letters == FALSE) {
    vec <- stringr::str_replace_all(vec, "(?<=\\d)([a-zA-Z])", "") #%>%
      #unique()
  }

  # New Addition --------------------------
  # count miRNAs in abstract and return if they are mentioned above threshold
  # Count miRNAs in vec
  mir_count <- as.data.frame(table(vec))

  # Subset count for miRNAs mentioned above a threshold
  mir_subset <- subset(mir_count, Freq >= threshold)
  # Get miRNA names
  vec <- as.character(mir_subset[["vec"]]) %>%
    unique()

  return(vec)

  if (vec != "") {
    return (vec)
  } else {
    message("No miRNA found.")
  }
}
