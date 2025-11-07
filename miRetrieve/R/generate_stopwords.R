#' Generate data frame containing stop words
#'
#' Generate a data frame containing stop words.
#'
#' Generate data frame containing stop words from a character vector. This data
#' frame consists of two columns, namely `word`, containing the stop words, and
#' `lexicon`, containing the string "self-defined".
#' Additionally, the created data frame can be combined with other stop words
#' containing data frames, e.g. `tidytext::stop_words` or
#' `stopwords_miretrieve`.
#'
#' @param stopwords Character vector. Vector containing stop words.
#' @param combine_with Data frame containing stop words. Optional.
#' Data frame provided here must have only two columns, namely
#' `word` and `lexicon`. This data frame is combined with
#' the data frame created from `stopwords`.
#'  Exemplary data frames are
#' * `tidytext::stop_words` from the \pkg{tidytext} package, or
#' * `stopwords_miretrieve` from this package.
#'
#' @return Data frame containing stop words.
#'
#' @seealso [combine_stopwords()], [stopwords_miretrieve], [tidytext::stop_words]
#'
#' @family stopword functions
#'
#' @references Silge, Julia, and David Robinson. 2016.
#' “tidytext: Text Mining and Analysis Using Tidy Data Principles in R.”
#' JOSS 1 (3). The Open Journal. https://doi.org/10.21105/joss.00037.
#'
#' @export
generate_stopwords <- function(stopwords,
                               combine_with = NULL) {
  stopwords_new <- data.frame("word" = stopwords,
                              "lexicon" = "self-defined",
                              stringsAsFactors = FALSE) %>%
    dplyr::as_tibble()

  if(is.null(combine_with)) {
    return(stopwords_new)
  } else {
    if(!identical(colnames(combine_with), colnames(stopwords_new))) {
      stop("The data frame provided in 'combine_with' does not have two
           columns named 'word' and 'lexicon'. Please provide a
           data frame with two columns named 'word' and 'lexicon'.")
    }

    stopwords_full <- rbind(stopwords_new,
                            combine_with)

    return(stopwords_full)
  }
}

#' Combine data frames containing stop words
#'
#' Combine data frames containing stop words into one data frame.
#'
#' Combine data frames containing stop words into one data frame. Provided
#' data frames must have two columns named "word" and "lexicon".
#'
#' @param ... Data frames with stop words. Data frames must have two columns
#' named "word" and "lexicon".
#'
#' @return Combined data frame with stop words.
#'
#' @seealso [generate_stopwords()], [stopwords_miretrieve], [tidytext::stop_words]
#'
#' @family stopword functions
#'
#' @export
combine_stopwords <- function(...) {
  # Pack data frames into a list
  list_df <- list(...)

  # Check if column names are "word" and "lexicon"
  colnames_vec <- c()

  for (dataframe in list_df) {
    colnames_df <- colnames(dataframe)
    colnames_vec <- c(colnames_vec, colnames_df)
  }

  colnames_vec <- unique(colnames_vec)

  first_version <- c("word", "lexicon")
  second_version <- c("lexicon", "word")

  if(!identical(colnames_vec, first_version) & !identical(colnames_vec, second_version)) {
    stop("Data frames provided do not have two columns named 'word' and 'lexicon'.
         Please provide only data frames with two columns names 'word' and
         'lexicon'.")
  }

  # Combine data frames
  new_df <- dplyr::bind_rows(list_df)

  return(new_df)
}
