#' Prepares a document-term-matrix from a data frame - helper.
#'
#' Helper function. Prepares a document-term-matrix from a data frame.
#'
#' Prepares a document-term-matrix from a data frame.
#'
#' @param df Data frame containing columns with PubMed-IDs and
#' abstracts. Data frame to prepare the document term matrix from.
#' @param token String. Specifies how abstracts shall be split up. For more
#' information, see unnest_tokens() from the tidytext package.
#' @param col.pmid Symbol. Column with PubMed-IDs.
#' @param col.abstract Symbol. Column with PubMed-IDs.
#' @param stopwords Data.frame. Data frame with stop words.
#' @param stopwords_ngram Boolean. Specifies if stop words shall be removed
#' from abstracts when using ngrams. Only applied when `token = 'ngrams'`.
#' @param tf.idf Boolean. If tf.idf = FALSE, no weighing of the terms.
#' If tf.idf = TRUE, weighs terms in Tf-Idf manner.
#' If fitting an LDA-model, `tf.idf = TRUE` is not advisable.
#'
#' @return Data frame with PubMed-IDs, terms, and terms counted.
#'
#' @importFrom magrittr %>%
#' @importFrom rlang :=
#'
#' @noRd
prepare_dtm <- function(df,
                        token = "words",
                        col.pmid = PMID,
                        col.abstract = Abstract,
                        stopwords = stopwords_miretrieve,
                        stopwords_ngram = TRUE,
                        tf.idf = FALSE) {

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

  df_count <- df %>%
    tidytext::unnest_tokens(input = {{col.abstract}},
                            output = word,
                            format = "text",
                            token = token) %>%
    dplyr::mutate(sort_out = stringr::str_detect(word,
                                        "^\\d+[a-z]?|\\d+mg|^\\d*\\.\\d*|mir|microrna|mirna")) %>%
    dplyr::filter(sort_out != TRUE) %>%
    dplyr::select(-sort_out) %>%
    dplyr::anti_join(stopwords) %>%
    dplyr::count({{col.pmid}}, word)

  if(tf.idf == TRUE) {
    df_count <- df_count %>%
      tidytext::bind_tf_idf(term = word,
                            document = {{col.pmid}},
                            n = n) %>%
      dplyr::mutate(n = tf_idf) %>%
      dplyr::select(1, 2, 3) %>%
      dplyr::mutate(n = trunc(n*10000))
  }

  df_count <- df_count %>%
    dplyr::rename(PMID = 1, word = 2, n = 3)

  return(df_count)
}

#' Make Document-Term-Matrix from a prepared data frame - helper.
#'
#' Helper function. Make Document-Term-Matrix from a prepared data frame.
#'
#' Helper function. Make Document-Term-Matrix from a prepared data frame.
#'
#' @param df_count Data frame. Data frame containing columns for documents,
#' terms, and counts.
#' @param stopwords Data.frame. Data frame with stopwords.
#' @param tf.idf Boolean. If tf.idf = FALSE, no weighing of the terms.
#' If tf.idf = TRUE, weighs terms in Tf-Idf manner.
#' If fitting an LDA-model, `tf.idf = TRUE` is not advisable.
#' @param col.pmid Column with PubMed-IDs.
#' @param col.abstract Column with abstracts.
#'
#' @return Document-Term-Matrix.
#'
#' @importFrom magrittr %>%
#' @noRd
make_dtm <- function(df_count,
                     stopwords = stopwords_miretrieve,
                     tf.idf = FALSE,
                     col.pmid = PMID,
                     col.abstract = Abstract) {
  dtm <- df_count %>%
    prepare_dtm(token = "words",
                stopwords = stopwords,
                tf.idf = tf.idf,
                col.pmid = {{col.pmid}},
                col.abstract = {{col.abstract}}) %>%
    tidytext::cast_dtm(document = PMID,
             term = word,
             value = n)

  return(dtm)
}
