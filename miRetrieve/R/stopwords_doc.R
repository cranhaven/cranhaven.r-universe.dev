#' Stop words for text mining with miRetrieve
#'
#' Data frame containing English stop words, PubMed stop words,
#' and common 2-gram stopwords. English stop words are based on
#' tidytext::stop_words, while PubMed stop words are manually curated from
#' PubMed abstracts
#'
#' @usage stopwords_miretrieve
#'
#' @format Tibble.
#' * `word`: Column containing stop words. Pulled from various PubMed
#' abstracts.
#' * `lexicon`: Column specifying lexicon.
#'
#' @source tidytext::stop_words; manually created from various PubMed abstracts.
"stopwords_miretrieve"

#' Stop words for text mining from PubMed abstracts
#'
#' Data frame containing PubMed stop words, manually curated from
#' PubMed abstracts
#'
#' @usage stopwords_pubmed
#'
#' @format Tibble.
#' * `word`: Column containing stop words. Pulled from various PubMed
#' abstracts.
#' * `lexicon`: Column specifying lexicon.
#'
#' @source Manually created from various PubMed abstracts.
"stopwords_pubmed"


#' Stop words for text mining with common PubMed 2-grams
#'
#' Data frame containing PubMed 2-gram stop words, manually curated from
#' PubMed abstracts
#'
#' @usage stopwords_2gram
#'
#' @format Tibble.
#' * `word`: Column containing stop words. Pulled from various PubMed
#' abstracts.
#' * `lexicon`: Column specifying lexicon.
#'
#' @source Manually created from various PubMed abstracts.
"stopwords_2gram"
