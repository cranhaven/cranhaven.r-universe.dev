#' Tf-Idf for terms / miRNA - helper
#'
#' Helper function. Weighs terms in Tf-Idf fashion for miRNAs.
#'
#' Helper function. Weighs terms in Tf-Idf fashion for miRNAs. This function
#' treats each miRNA name as a separate document and basically compares how
#' often a term is associated with one miRNA, but not with other miRNAs.
#' If a term is highly associated with one miRNA, but not with other miRNAs,
#' it gets more weight than a term that is highly associated with one miRNA,
#' but also with other miRNAs.
#'
#' @param df Data frame containing miRNA names and abstracts.
#' @param stopwords Data frame containing stop words.
#' @param token String. Specifies how abstracts are tokenized. For more information,
#' see the documentation for `unnest_tokens()` from the tidytext package.
#' @param stopwords_ngram Boolean. Specifies if stop words shall be removed
#' from abstracts when using ngrams. Only applied when `token = 'ngrams'`.
#' @param ... Additional arguments for tokenization, if necessary.
#' @param col.mir Symbol. Column with miRNAs.
#' @param col.abstract Symbol. Column with abstracts.
#'
#' @return Data frame with terms counted in Tf-Idf fashion.
#'
#' @importFrom rlang :=
#'
#' @noRd
tfidf_words_mir <- function(df,
                            stopwords = stopwords_miretrieve,
                            token = "words",
                            stopwords_ngram = TRUE,
                            ...,
                            col.mir = miRNA,
                            col.abstract = Abstract) {

  # checks if all columns are atomic
  # unnest_tokens() needs all columns to be atomic
  all_atomic <- purrr::map_lgl(df, is.atomic)

  # discard any column that is not atomic if needed
  if(any(!all_atomic)) {
    df <- df[, all_atomic]
  }

  if(stopwords_ngram & token == "ngrams") {
    df <- df %>%
      dplyr::mutate({{col.abstract}} := stringr::str_replace_all({{col.abstract}},
                                                                ngram_stopwords, " "))
  }

  mir_word_tfidf <- df %>%
    tidytext::unnest_tokens(input = {{col.abstract}},
                            output = word,
                            token = token,
                            ...,
                            format = "text") %>%
    dplyr::mutate(sort_out = stringr::str_detect(word, "^\\d+[a-z]?|\\d+mg|^\\d*\\.\\d*|mir|microrna|mirna")) %>%
    dplyr::filter(sort_out != TRUE) %>%
    dplyr::select(-sort_out) %>%
    dplyr::anti_join(stopwords) %>%
    dplyr::add_count({{col.mir}}, word, name = "mirna_count") %>%
    tidytext::bind_tf_idf(term = word,
                          document = {{col.mir}},
                          n = mirna_count) %>%
    dplyr::mutate(mirna_count = tf_idf)

  return(mir_word_tfidf)
}
