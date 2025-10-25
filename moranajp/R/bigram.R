#' Draw bigram network using morphological analysis data.
#' 
#' @param df           A dataframe including result of morphological analysis.
#' @param group        A string to specify sentence.
#' @param bigram       A result of bigram().
#' @param big_net      A result of bigram_network().
#' @param draw         A logical.
#' @param rand_seed    A numeric.
#' @param threshold    A numeric used as threshold for frequency of bigram.
#' @param term_depend  A string of dependent terms column to use bigram.
#' @param depend       A logical.
#' @param freq         A numeric of word frequency in bigram_network.
#'                     Can be got using word_freq().
#' @param arrow_size,circle_size,text_size,
#'                     A numeric.
#' @param font_family  A string. 
#' @param arrow_col,circle_col
#'                     A string to specify arrow and circle color 
#'                     in bigram network.
#' @param x_limits,y_limits
#'                     A Pair of numeric to specify range.
#' @param no_scale     A logical. FALSE: Not draw x and y axis.
#' @param ...          Extra arguments to internal functions.
#' @return  A list including df (input), bigram, freq (frequency) and 
#'          gg (ggplot2 object of bigram network plot).
#' @examples
#' 
#' sentences <- 50
#' len <- 30
#' n <- sentences * len
#' x <- letters
#' prob <- (length(x):1) ^ 3
#' df <- 
#'   tibble::tibble(
#'     lemma = sample(x = x, size = n, replace = TRUE, prob = prob),
#'     sentence = rep(seq(sentences), each = len))
#' draw_bigram_network(df)
#' 
#' @export
draw_bigram_network <- function(df, draw = TRUE, ...){
  big <- bigram(df, ...)
  big_net <- bigram_network(big, ...)
  freq <- word_freq(df, big_net, ...)
  gg <- bigram_network_plot(big_net, freq = freq, ...)
  if(draw) print(gg)
  res <- list(df = df, bigram = big, freq = freq, gg = gg)
  return(res)
}

#' @rdname draw_bigram_network
#' @export
bigram <- function(df, group = "sentence", 
                   depend = FALSE, term_depend = NULL, 
                   ...){ # `...' will be omitted
  term <- term_lemma(df)
  word_1 <- "word_1"
  word_2 <- "word_2"
  freq <- "freq"
  big_dep <- if(depend) bigram_depend(df, group) else NULL
  big <- 
    df |>
    dplyr::group_by(.data[[group]]) |>
  # according to arrow direction in ggplot: "word_2-word_1"
    dplyr::transmute(.data[[group]], 
                     {{word_2}} := .data[[term]], 
                     {{word_1}} := dplyr::lag(.data[[term]])) |>
    dplyr::ungroup() |>
    stats::na.omit()
  big <- 
    big |>
    dplyr::bind_rows(big_dep) |>
    dplyr::filter(.data[[word_1]] != "EOS") |>
    dplyr::filter(.data[[word_2]] != "EOS") |>
    dplyr::filter(.data[[word_1]] != "*") |>
    dplyr::filter(.data[[word_2]] != "*") |>
    dplyr::distinct()
  n_group <- big[[group]] |> unique() |> length()
  if(n_group > 1){  
    big |>
      dplyr::group_by(.data[[word_1]], .data[[word_2]]) |>
      dplyr::tally(name = {{freq}}) |>
      dplyr::ungroup() |>
      dplyr::arrange(dplyr::desc(.data[[freq]]))
  }else{
    big |>
      dplyr::tally(name = {{freq}}) |>
      dplyr::arrange(dplyr::desc(.data[[freq]]))
    warn <- paste0("Not used group. " , group, " has only one category.")
    warning(warn)
  }
}
#' @rdname draw_bigram_network
#' @export
trigram <- function(df, group = "sentence"){
  term <- term_lemma(df)
  word_1 <- "word_1"
  word_2 <- "word_2"
  word_3 <- "word_3"
  freq <- "freq"
  big <- 
    df |>
    dplyr::group_by(.data[[group]]) |>
    dplyr::transmute(.data[[group]], 
                     {{word_3}} := .data[[term]], 
                     {{word_2}} := dplyr::lag(.data[[term]], n = 1),
                     {{word_1}} := dplyr::lag(.data[[term]], n = 2)) |>
    dplyr::ungroup() |>
    stats::na.omit()
  big <- 
    big |>
    dplyr::filter(! .data[[word_1]] %in% c("EOS", "*")) |>
    dplyr::filter(! .data[[word_2]] %in% c("EOS", "*")) |>
    dplyr::filter(! .data[[word_3]] %in% c("EOS", "*")) |>
  #     dplyr::filter(.data[[word_1]] != "EOS|*") |>
  #     dplyr::filter(.data[[word_1]] != "*") |>
    dplyr::distinct()
  n_group <- big[[group]] |> unique() |> length()
  if(n_group > 1){  
    big |>
      dplyr::group_by(.data[[word_1]], .data[[word_2]], .data[[word_3]]) |>
      dplyr::tally(name = {{freq}}) |>
      dplyr::ungroup() |>
      dplyr::arrange(dplyr::desc(.data[[freq]]))
  }else{
    big |>
      dplyr::tally(name = {{freq}}) |>
      dplyr::arrange(dplyr::desc(.data[[freq]]))
    warn <- paste0("Not used group. " , group, " has only one category.")
    warning(warn)
  }
}

#' @rdname draw_bigram_network
#' @export
bigram_depend <- function(df, group = "sentence"){
  term <- term_lemma(df)
  term_depend <- ifelse("head" %in% colnames(df), 
                 "head_dep",  
                 paste0(term, "_dep"))
  big_dep <- 
    df |>
    dplyr::transmute(.data[[group]], 
      "word_1" := .data[[term]], "word_2" := .data[[term_depend]]) |>
    dplyr::distinct()
  return(big_dep)
}

#' @rdname draw_bigram_network
#' @export
bigram_network <- function(bigram, 
                           rand_seed = 12, 
                           threshold = 100, ...){ # `...' will be omitted
  set.seed(rand_seed)
  freq_thresh <- dplyr::slice(bigram, threshold)[["freq"]]
  if(length(freq_thresh) == 0){ freq_thresh <- 1 }
  bigram |>
    dplyr::filter(.data[["freq"]] > freq_thresh) |>
    igraph::graph_from_data_frame()
}

#' @rdname draw_bigram_network
#' @export
word_freq <- function(df, big_net, ...){
  term <- term_lemma(df)
  freq <- "freq"
  name <- 
    big_net |>
    igraph::V() |>
    attr("name")
  df <- 
    df |>
    dplyr::group_by(.data[[term]]) |>
    dplyr::tally(name = freq)
  dplyr::left_join(
    tibble::tibble({{term}} := name), df, 
    by = term) |>
    `[[`(_, freq) |>
    log() |>
    round(0) * 2
}

#' @rdname draw_bigram_network
#' @export
bigram_network_plot <- function(big_net, freq,
                                ...,  # `...' will be omitted
                                arrow_size  = 5,
                                circle_size = 5,
                                text_size   = 5,
                                font_family = "",
                                arrow_col   = "darkgreen",
                                circle_col  = "skyblue",
                                x_limits    = NULL,
                                y_limits    = NULL,
                                no_scale    = FALSE){
  # settings
  cap_size    <- ggraph::circle(arrow_size, 'mm')
  arrow_size  <- grid::unit(arrow_size, 'mm')
  breaks      <- if(no_scale) NULL else ggplot2::waiver()

  big_net_plot <- 
    big_net |>
    # the most understandable layout
    ggraph::ggraph(layout = "fr") + 
    ggraph::geom_edge_link(color  = arrow_col, 
                           arrow  = grid::arrow(length = arrow_size), 
                           start_cap = cap_size, 
                           end_cap   = cap_size) +
    ggraph::geom_node_point(color = circle_col, 
                                  # freq *      5      * 0.2
                            size  = freq * circle_size * 0.2) +  
    ggraph::geom_node_text(ggplot2::aes(label = .data[["name"]]), 
  #     ggraph::geom_node_text(ggplot2::aes(label = .data[["name"]] |>
  #                                         iconv(from = "UTF-8", to = "ASCII//TRANSLIT")), |>
                           vjust  = 1, 
                           hjust  = 1, 
                           size   = text_size, 
                           family = font_family) +
    ggplot2::theme_bw(base_family = font_family) + 
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank()) + 
    ggplot2::scale_x_continuous(limits = x_limits, breaks = breaks) + 
    ggplot2::scale_y_continuous(limits = y_limits, breaks = breaks)

  return(big_net_plot)
}
