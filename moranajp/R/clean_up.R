#' Clean up result of morphological analyzed data frame
#' 
#' @param df               A dataframe including result of morphological analysis.
#' @param use_common_data  A logical. TRUE: use data(stop_words).
#' @param add_stop_words   A string vector adding into stop words. 
#'                         When use_common_data is TRUE and add_stop_words are given, 
#'                         both of them will be used as stop_words.
#' @param synonym_df       A data.frame including synonym word pairs. 
#'                         The first column: replace from, the second: replace to.
#' @param synonym_from,synonym_to
#'                         A string vector. Length of synonym_from and synonym_to 
#'                         should be the same.
#'                         When synonym_df and synonym pairs (synonym_from and synonym_to)
#'                         are given, both of them will be used as synonym.
#' @param add_depend       A logical. Available for ginza
#' @param ...              Extra arguments to internal functions.
#' @return A data.frame.
#' @name clean_up
#' @examples
#' data(neko_mecab)
#' data(neko_ginza)
#' data(review_sudachi_c)
#' data(synonym)
#' synonym <- 
#'   synonym |> unescape_utf()
#' 
#' neko_mecab <- 
#'   neko_mecab |>
#'   unescape_utf() |>
#'   print()
#' 
#' neko_mecab |>
#'   clean_up(use_common_data = TRUE, synonym_df = synonym)
#' 
#' review_ginza |>
#'   unescape_utf() |>
#'   add_sentence_no() |>
#'   clean_up(add_depend = TRUE, use_common_data = TRUE, synonym_df = synonym)
#' 
#' review_sudachi_c |>
#'   unescape_utf() |>
#'   add_sentence_no() |>
#'   clean_up(use_common_data = TRUE, synonym_df = synonym)
#' 
#' @export
clean_up <- function(df, add_depend = FALSE, ...){
  df <- 
    df |>
    pos_filter() |>
    delete_stop_words(...) |>
    replace_words(...)
  if(add_depend){ df <- add_depend_ginza(df) }
  return(df)
}

#' @rdname clean_up
#' @export
pos_filter <- function(df){
  pos_0 <- term_pos_0(df)
  pos_1 <- term_pos_1(df)
  filter_pos_0 <- 
    c("\\u540d\\u8a5e",      "\\u52d5\\u8a5e",
      "\\u5f62\\u5bb9\\u8a5e", "\\u5f62\\u72b6\\u8a5e") |>
    unescape_utf()
  filter_pos_1 <- 
    c("\\u666e\\u901a\\u540d\\u8a5e",
      "\\u56fa\\u6709\\u540d\\u8a5e",
      "\\u56fa\\u6709",
      "\\u4e00\\u822c",
      "\\u81ea\\u7acb",
      "\\u30b5\\u5909\\u63a5\\u7d9a",
      "\\u5f62\\u5bb9\\u52d5\\u8a5e\\u8a9e\\u5e79",
      "\\u30ca\\u30a4\\u5f62\\u5bb9\\u8a5e\\u8a9e\\u5e79",
      "\\u526f\\u8a5e\\u53ef\\u80fd") |>
      unescape_utf()
  df |>
    dplyr::filter(.data[[pos_0]] %in% filter_pos_0) |>
    dplyr::filter(.data[[pos_1]] %in% filter_pos_1)
}

#' @rdname clean_up
#' @export
add_depend_ginza <- function(df){
  s_id <- "sentence"
  term <- term_lemma(df)
  head <- ifelse("head" %in% colnames(df), 
                 "head",  
                 unescape_utf("\\u4fc2\\u53d7\\u5143"))
  h_id <- paste0(head, "_id")
  t_dep <- paste0(term, "_dep")
  if(!s_id %in% colnames(df)) df <- add_sentence_no(df, {{s_id}})

  df <- 
    df |>
    dplyr::mutate(
        "word_no" := .data[["id"]], 
        "id" := stringr::str_c(.data[[s_id]], "_", .data[["word_no"]]))
  depend <- 
    df |>
    dplyr::select({{h_id}} := .data[["id"]], {{t_dep}} := .data[[term]])
  df <- 
    df |>
    dplyr::mutate({{h_id}} := 
        stringr::str_c(.data[[s_id]], "_", .data[[head]])) |>
    dplyr::left_join(depend, by = unescape_utf("\\u4fc2\\u53d7\\u5143_id"))
  return(df)
}

#' @rdname clean_up
#' @export
delete_stop_words <- function(df,
                              use_common_data = TRUE,
                              add_stop_words = NULL,
                              ...){ # `...' will be omitted
  term <- term_lemma(df)
  stop_words <- 
    if(use_common_data){
        utils::data(stop_words, envir = environment())
        stop_words |>
          purrr::map_dfr(unescape_utf) |>
          `colnames<-`(term)
    } else {
        tibble::tibble()
    }
  stop_words <- 
      tibble::tibble(add_stop_words) |>
          `colnames<-`(term) |>
          dplyr::bind_rows(stop_words)
  df <- dplyr::anti_join(df, stop_words, by = term)
  return(df)
}

#' @rdname clean_up
#' @export
replace_words <- function(df, 
                          synonym_df = tibble::tibble(),
                          synonym_from = "",
                          synonym_to = "",
                          ...){ # `...' will be omitted
  if(nrow(synonym_df) == 0 & synonym_from == "" & synonym_to   == "" ){ return(df) }
  term <- term_lemma(df)
  rep_words        <- synonym_to
  names(rep_words) <- synonym_from
  if(!is.null(synonym_df)){
    rep_words        <- c(synonym_to,   synonym_df[[2]]) # 2: TO
    names(rep_words) <- c(synonym_from, synonym_df[[1]]) # 1: FROM
  }
  rep_words <- rep_words[names(rep_words) != ""] # skip from == ""
  if(length(rep_words) == 0){ return(df) }
  df <- 
    df |>
    dplyr::mutate(`:=`({{term}}, stringr::str_replace_all(.data[[term]], rep_words)))
  return(df)
}

#' @rdname clean_up
#' @export
term_lemma <- function(df){
  term <- ifelse("lemma" %in% colnames(df), 
          "lemma",  
          unescape_utf("\\u539f\\u5f62"))
  return(term)
}

#' @rdname clean_up
#' @export
term_pos_0 <- function(df){
  pos_0 <- ifelse("pos"   %in% colnames(df), 
                  "pos",
                  unescape_utf("\\u54c1\\u8a5e"))
  return(pos_0)
}

#' @rdname clean_up
#' @export
term_pos_1 <- function(df){
  pos_1 <- ifelse("pos_1" %in% colnames(df), 
                  "pos_1", 
                  unescape_utf("\\u54c1\\u8a5e\\u7d30\\u5206\\u985e1"))
  return(pos_1)
}

#' Combine words after morphological analysis
#' 
#' @name combine_words
#' @param df     A dataframe including result of morphological analysis.
#' @param combi  A string (combi_words()) or string vector (combine_words()) to combine words.
#' @param sep    A string of separator of words
#' @param x      A pair of string joining with "-"
#' @return A data.frame with combined words.
#' 
#' @examples
#' x <- letters[1:10]
#' combi <- c("b-c")
#' combi_words(x, combi)
#' expected <- c("a", "bc", NA, "d", "e", "f", "g", "h", "i", "j")
#' testthat::expect_equal(combi_words(x, combi), expected)
#' 
#' df <- unescape_utf(review_chamame) |> head(20)
#' combi <- unescape_utf(
#'            c("\\u751f\\u7269-\\u591a\\u69d8", "\\u8fb2\\u5730-\\u306f"       ,
#'              "\\u8fb2\\u7523-\\u7269"       , "\\u751f\\u7523-\\u3059\\u308b"))
#' combine_words(df, combi)
#' 
#' @export
combine_words <- function(df, combi, sep = "-"){
  term <- term_lemma(df)
  df <- # avoid all NA cols
    df |>
    dplyr::mutate_if(is.logical, as.character) |>
    dplyr::mutate_if(is.character, function(x){ tidyr::replace_na(x, "") })
  for(com in combi){
    combined <- combi_words(df[[term]], combi = com, sep = sep)
    df[[term]] <- combined
    df <- na.omit(df)
  }
  return(df)
}

#' @rdname combine_words
#' @export
combi_words <- function(x, combi, sep = "-"){
  big <- stringr::str_c(x, sep, dplyr::lead(x, default = ""))
  index <- seq(big)[big == combi]
  x[index] <- c(stringr::str_remove(combi, sep))
  x[index + 1] <- NA
  return(x)
}
  # library(tidyverse)
  # df <- unescape_utf(review_chamame) |> head(20)
  # combine_words(df, NULL)
  # combine_words(df, "")
  # delete_stop_words(df, FALSE, "")
  # delete_stop_words(df, FALSE, NULL)
  # replace_words(df, NULL)
  # replace_words(df, "")
  # shiny::runApp("d:/matu/work/todo/textmining/R")
