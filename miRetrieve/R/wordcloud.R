#' Create wordcloud of terms associated with a miRNA name
#'
#' Create wordcloud of terms associated with a miRNA name.
#'
#' Create wordcloud of terms associated with a miRNA name.
#' miRNA names must be in a data frame `df`, while terms are taken
#' from abstracts contained in `df`.
#' Number of terms to plot is regulated by `max.terms`, while `min.freq` regulates
#' the least number of times a term must be mentioned to be plotted.
#' Terms can either be evaluated as their raw count, e.g. how often they are
#' mentioned in conjunction with the miRNA of interest, or weighed in a tf-idf
#' fashion. If `tf.idf = TRUE`, miRNA names are considered as separate documents,
#' and terms often associated with one miRNA, but not with other miRNAs get
#' more weight.
#' `plot_wordcloud()` is based on the tools available in the \pkg{wordcloud}
#' package.
#'
#' @param df Data frame containing miRNA names, abstracts, and PubMed-IDs.
#' @param mir String. miRNA name of interest.
#' @param min.freq Integer. Specifies least number of times a term must be associated with
#' `mir` to be plotted.
#' @param max.terms Integer. Maximum number of terms to plot.
#' @param tf.idf Boolean. If `tf.idf = TRUE`, terms are weighed in a tf-idf
#' fashion. miRNA names are considered as separate documents, and terms often
#' associated with one miRNA, but not with other miRNAs get more weight.
#' Cannot be used if `normalize = TRUE`. If `tf.idf = TRUE` and `normalize = TRUE`,
#' `tf.idf = TRUE` is ignored.
#' @param token String. Specifies how abstracts shall be split up. Taken from
#' `unnest_tokens()` in the \pkg{tidytext} package:
#' "Unit for tokenizing, or a custom tokenizing function. Built-in options are
#' "words" (default), "characters", "character_shingles", "ngrams", "skip_ngrams",
#' "sentences", "lines", "paragraphs", "regex",
#' (...),
#' and "ptb" (Penn Treebank). If a function, should take a character vector and
#' return a list of character vectors of the same length."
#' @param ... Additional arguments for tokenization, if necessary.
#' @param stopwords Data frame containing stop words.
#' @param stopwords_ngram Boolean. Specifies if stop words shall be removed
#' from abstracts when using ngrams. Only applied when `token = 'ngrams'`.
#' @param colours Vector of strings. Colours for wordcloud.
#' @param random.colour Boolean. Taken from `wordcloud()` in the
#' \pkg{wordcloud} package:
#' "Choose colours randomly from `colours`. If false, the colour is chosen
#' based on the frequency."
#' @param ordered.colour Boolean. Taken from `wordcloud()` in the
#' \pkg{wordcloud} package:
#' "If true, then colours are assigned to words in order."
#' @param col.mir Symbol. Column containing miRNA names.
#' @param col.abstract Symbol. Column containing abstracts.
#' @param col.pmid Symbol. Column containing PubMed-IDs.
#'
#' @return Wordcloud of terms associated with a miRNA name.
#'
#' @seealso [plot_mir_terms()], [wordcloud::wordcloud()], [tidytext::unnest_tokens()]
#'
#' @family miR term functions
#'
#' @export
plot_wordcloud <- function(df,
                           mir,
                           min.freq = 1,
                           max.terms = 20,
                           tf.idf = FALSE,
                           token = "words",
                           ...,
                           stopwords = stopwords_miretrieve,
                           stopwords_ngram = TRUE,
                           colours = "black",
                           random.colour = TRUE,
                           ordered.colour = FALSE,
                           col.mir = miRNA,
                           col.abstract = Abstract,
                           col.pmid = PMID) {

  # checks if all columns are atomic
  # unnest_tokens() needs all columns to be atomic
  all_atomic <- purrr::map_lgl(df, is.atomic)

  # discard any column that is not atomic if needed
  if(any(!all_atomic)) {
    df <- df[, all_atomic]
  }

  df_mir <- df %>%
    dplyr::filter({{col.mir}} == mir) %>%
    prepare_dtm(col.pmid = {{col.pmid}},
                col.abstract = {{col.abstract}},
                stopwords = stopwords,
                tf.idf = tf.idf,
                token = token,
                ... = ...,
                stopwords_ngram = stopwords_ngram)

  # Make tf-idf values to integers > 1 to be processed with wordcloud
  if(tf.idf == TRUE) {
    df_mir <- df_mir %>%
      dplyr::mutate(n = trunc(n * 10000))
  }

  df_mir <- df_mir %>%
    dplyr::top_n(n = 1000, wt = n) %>%
    dplyr::mutate(word = as.character(word)) %>%
    dplyr::mutate(n = as.integer(n))

  wordcloud_plot <- wordcloud::wordcloud(df_mir[["word"]],
                                         df_mir[["n"]],
                                         min.freq = min.freq,
                                         max.words = max.terms,
                                         random.color = random.colour,
                                         ordered.colors = ordered.colour,
                                         colors = colours)

  return(df_mir)
}
