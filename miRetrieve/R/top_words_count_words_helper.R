#' Get top words of a data frame - helper.
#'
#' Helper function. Get top words of a data frame.
#'
#' Helper function. Get top words of a data frame. Returns a vector containing
#' top words.
#'
#' @param df Data frame with word and count columns.
#' @param top Integer. Number of top terms.
#' @param col.word Symbol. Column with words/terms.
#' @param col.wt Symbol. Column with count.
#'
#' @return Vector with top words.
#'
#' @noRd
get_top_words <- function(df,
                          top,
                          col.word = word,
                          col.wt = mirna_count) {

  top_words <- df %>%
    dplyr::top_n(n = top, wt = {{col.wt}}) %>%
    dplyr::select({{col.word}}) %>%
    dplyr::pull() %>%
    unique()

  return(top_words)
}

#' Count words per miRNA - helper.
#'
#' Helper function. Count words per miRNA name.
#'
#' Helper function. Count how often a term is associated with a miRNA.
#' Takes a data frame containing miRNA names and abstracts as an argument.
#' Abstracts are tokenized, stop words are removed and it is counted how often a
#' miRNA is associated with a term.
#'
#' @param df Data frame containing miRNAs and abstracts.
#' @param stopwords Data frame containing stop words.
#' @param token String. Specifies how abstracts are tokenized. For more information,
#' see the documentation for `unnest_tokens()` from the tidytext package.
#' @param stopwords_ngram Boolean. Specifies if stop words shall be removed
#' from abstracts when using ngrams. Only applied when `token = 'ngrams'`.
#' @param col.mir Symbol. Column with miRNA names.
#' @param col.abstract Symbol. Column with abstracts.
#' @param ... Additional arguments for tokenization, if necessary.
#'
#' @return Data frame with terms counted per miRNA.
#'
#' @importFrom rlang :=
#'
#' @noRd
count_words_mir <- function(df,
                            stopwords = stopwords_miretrieve,
                            token = "words",
                            stopwords_ngram = TRUE,
                            col.mir = miRNA,
                            col.abstract = Abstract,
                            ...) {

  # checks if all columns are atomic
  # unnest_tokens() needs all columns to be atomic
  all_atomic <- purrr::map_lgl(df, is.atomic)

  # discard any column that is not atomic if needed
  if(any(!all_atomic)) {
    df <- df[, all_atomic]
  }

  if(stopwords_ngram == TRUE & token == "ngrams") {
    df <- df %>%
      dplyr::mutate({{col.abstract}} := stringr::str_to_lower({{col.abstract}})) %>%
      dplyr::mutate({{col.abstract}} := stringr::str_replace_all({{col.abstract}},
                                                             ngram_stopwords, " "))
  }

  df_count <- df %>%
    tidytext::unnest_tokens(input = {{col.abstract}},
                            output = word,
                            format = "text",
                            token = token,
                            ... = ...) %>%
    dplyr::mutate(sort_out = stringr::str_detect(word, "^\\d+[a-z]?|\\d+mg|^\\d*\\.\\d*|
                                                 mir|microrna|mirna")) %>%
    dplyr::filter(sort_out != TRUE) %>%
    dplyr::select(-sort_out) %>%
    dplyr::anti_join(stopwords) %>%
    dplyr::add_count({{col.mir}}, word, name = "mirna_count")

  # If "ngrams", get rid of n-grams with a frequency <=3
  if(token == "ngrams") {
    df_count <- df_count %>%
      dplyr::filter(mirna_count > 3)
  }

  return(df_count)
}
