#' Plot count of top terms associated with a miRNA name
#'
#' Plot count of top terms associated with a miRNA name.
#'
#' Plot count of top terms associated with a miRNA name.
#' Top terms associated with `mir` have to be in `df` as abstracts.
#' Number of top terms to plot is regulated via the `top` argument.
#' Terms can either be evaluated as their count or in a tf-idf fashion.
#' If terms are evaluated as their count, they can either be
#' evaluated as their raw count, e.g. in how many abstracts they are mentioned
#' in conjunction with the miRNA name, or as their relative count, e.g.
#' in how many abstracts containing the miRNA they are mentioned compared to all
#' abstracts containing the miRNA.
#' If terms are evaluated in a tf-idf fashion, miRNA names are considered as
#' separate documents and
#' terms often associated with one miRNA, but not with other miRNAs get
#' more weight.
#' `plot_mir_terms()` is based on the tools available in the \pkg{tidytext} package.
#'
#' @param df Data frame containing miRNA names, abstracts, and PubMed-IDs.
#' @param mir String. miRNA name of interest.
#' @param top Integer. Number of top terms to plot.
#' @param tf.idf Boolean. If `tf.idf = TRUE`, terms are weighed in a tf-idf
#' fashion. miRNA names are considered as separate documents and terms often
#' associated with one miRNA, but not with other miRNAs get more weight.
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
#' @param normalize Boolean. If `normalize = TRUE`, normalizes the number of
#' abstracts to the total number of abstracts with a miRNA name in a topic. Cannot
#' be applied with `tf.idf = TRUE`.
#' @param colour String. Colour of bar plot.
#' @param col.mir Symbol. Column containing miRNA names
#' @param col.abstract Symbol. Column containing abstracts.
#' @param col.pmid Symbol. Column containing PubMed-IDs.
#' @param title String. Title plot.
#'
#' @return Bar plot displaying the count of the top terms associated with a
#' miRNA name.
#'
#' @seealso [plot_wordcloud()], [tidytext::unnest_tokens()]
#'
#' @family miR term functions
#'
#' @export
plot_mir_terms <- function(df,
                           mir,
                           top = 20,
                           tf.idf = FALSE,
                           token = "words",
                           ...,
                           stopwords = stopwords_miretrieve,
                           stopwords_ngram = TRUE,
                           normalize = TRUE,
                           colour = "steelblue3",
                           col.mir = miRNA,
                           col.abstract = Abstract,
                           col.pmid = PMID,
                           title = NULL) {

  # checks if all columns are atomic
  # unnest_tokens() needs all columns to be atomic
  all_atomic <- purrr::map_lgl(df, is.atomic)

  # discard any column that is not atomic if needed
  if(any(!all_atomic)) {
    df <- df[, all_atomic]
  }

  if(tf.idf == TRUE & normalize == TRUE) {
    warning("Cannot apply 'normalize' with 'tf.idf'. Ignoring 'normalize'.")
  }

  # Save ... as a list
  args <- list(...)

  # Check if args contains n if token = "ngrams"
  if(token == "ngrams" & is.null(args)) {
    stop("'token' is set to 'ngrams', but 'n' is not specified. Please
         specify 'n'.")
  }

  if(is.null(title)) {
    if(token == "words") {
      title <- stringr::str_c("Top terms for ", mir)
    } else if(token == "ngrams") {
      title <- stringr::str_c("Top ", args$n, "-grams for ", mir)
    }
  }

  if(normalize == TRUE) {
    ylab_string <- paste0("Mentioned in % of abstracts with ", mir)
  } else {
    ylab_string <- paste0("Mentioned in # of abstracts with ", mir)
  }

  if(tf.idf == TRUE) {

    title <- paste0("Top tf-idf terms for ", mir)

    df_count <- tfidf_words_mir(df = df,
                                stopwords = stopwords,
                                token = token,
                                stopwords_ngram = stopwords_ngram,
                                ... = ...,
                                col.abstract = {{col.abstract}},
                                col.mir = {{col.mir}}) # Helper function miRetrieve
    df_count <- df_count %>%
      dplyr::filter({{col.mir}} == mir) %>%
      dplyr::distinct({{col.mir}}, word, mirna_count) %>%
      dplyr::top_n(top, mirna_count) %>%
      dplyr::mutate(word = forcats::fct_reorder(word, mirna_count))

    plot <- ggplot(df_count, aes(x = word, y = mirna_count)) +
      geom_col(show.legend = FALSE,
               fill = colour) +
      coord_flip() +
      theme_classic() +
      scale_y_discrete(expand=c(0.3, 0.3)) +
      xlab("Terms") +
      ylab(paste0("Tf-idf weight for ", mir)) +
      ggtitle(title)

    plot <- pretty_breaks_miretrieve(plot, df_count$mirna_count)

    return(plot)

  } else {
    df <- df %>%
      dplyr::filter({{col.mir}} == mir)

    df_count <- count_words_mir(df = df,
                                stopwords = stopwords,
                                token = token,
                                stopwords_ngram = stopwords_ngram,
                                ... = ...,
                                col.abstract = {{col.abstract}},
                                col.mir = {{col.mir}}) # Helper function miRetrieve
  }


  df_mir <- df_count %>%
    dplyr::mutate(total = length(unique({{col.pmid}}))) %>%
    dplyr::distinct({{col.pmid}}, total, word) %>%
    dplyr::add_count(word, name = "no_of_abstract") %>%
    dplyr::mutate(perc = no_of_abstract/total)

  if(normalize == TRUE) {
    df_mir <- df_mir %>%
      dplyr::mutate(no_of_abstract = perc)
  }



  df_mir <- df_mir %>%
    dplyr::distinct(word, no_of_abstract) %>%
    dplyr::top_n(top, no_of_abstract) %>%
    dplyr::mutate(word = forcats::fct_reorder(word, no_of_abstract))



  plot <- ggplot(df_mir, aes(x = word, y = no_of_abstract)) +
    geom_col(show.legend = FALSE,
             fill = colour) +
    coord_flip() +
    theme_classic() +
    # theme(axis.text.y = element_text(size = 12),
    #       plot.title = element_text(size = 10),
    #       axis.title = element_text(size = 10)) +
    scale_y_discrete(expand=c(0.3, 0.3)) +
    xlab("Terms") +
    ylab(ylab_string) +
    ggtitle(title)

  plot <- pretty_breaks_miretrieve(plot, df_mir$no_of_abstract)

  if(normalize == TRUE) {
    plot <- plot +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                         expand = c(0,0))
  }


  return(plot)
}


#' Compare count of terms associated with a miRNA name over various topics
#'
#' Compare count of top terms associated with a miRNA name over various topics.
#'
#' Compare count of top terms associated with a miRNA name
#' over various topics.
#' miRNA names and topics must be in a data frame `df`, while terms are taken
#' from abstracts contained in `df`.
#' Number of top terms to plot is regulated by `top`. Terms can either be
#' evaluated as their raw count, e.g. in how many abstracts they are mentioned
#' in conjunction with the miRNA name, or as their relative count, e.g.
#' in how many abstracts containing the miRNA they are mentioned compared to all
#' abstracts containing the miRNA.
#' `compare_mir_terms()` is based on the tools available in the
#' \pkg{tidytext} package.
#'
#' @param df Data frame containing miRNA names, abstracts, topics, and PubMed-IDs.
#' @param mir String. miRNA name of interest.
#' @param top Integer. Number of top terms to plot.
#' @param token String. Specifies how abstracts shall be split up. Taken from
#' `unnest_tokens()` in the \pkg{tidytext} package:
#' "Unit for tokenizing, or a custom tokenizing function. Built-in options are
#' "words" (default), "characters", "character_shingles", "ngrams", "skip_ngrams",
#' "sentences", "lines", "paragraphs", "regex",
#' (...),
#' and "ptb" (Penn Treebank). If a function, should take a character vector and
#' return a list of character vectors of the same length."
#' @param ... Additional arguments for tokenization, if necessary.
#' @param topic Character vector. Optional. Specifies topics to plot.
#' If `topic = NULL`, all topics in `df` are plotted.
#' @param shared Boolean. If `shared = TRUE`, only terms that are shared
#' between all topics are plotted.
#' @param normalize Boolean. If `normalize = TRUE`, normalizes the number of
#' abstracts to the total number of abstracts with a miRNA name in a topic.
#' @param stopwords Data frame containing stop words.
#' @param stopwords_ngram Boolean. Specifies if stop words shall be removed
#' from abstracts when using ngrams. Only applied when `token = 'ngrams'`.
#' @param position Character vector. Vector containing either "dodge" or "facet".
#' Determines if bar plots are on top of or next to each other.
#' @param col.mir Symbol. Column containing miRNA names.
#' @param col.abstract Symbol. Column containing abstracts.
#' @param col.topic Symbol. Column containing topic names.
#' @param col.pmid Symbol. Column containing PubMed-IDs.
#' @param title String. Plot title.
#'
#' @return Bar plot comparing the count of terms associated with a
#' miRNA name over two topics.
#'
#' @seealso [compare_mir_terms_log2()], [compare_mir_terms_scatter()]
#'
#' @family compare functions
#'
#' @export
compare_mir_terms <- function(df,
                              mir,
                              top = 20,
                              token = "words",
                              ...,
                              topic = NULL,
                              shared = TRUE,
                              normalize = TRUE,
                              stopwords = stopwords_miretrieve,
                              stopwords_ngram = TRUE,
                              position = "dodge",
                              col.mir = miRNA,
                              col.abstract = Abstract,
                              col.topic = Topic,
                              col.pmid = PMID,
                              title = NULL) {


  # Filters for topic of interest
  if(!is.null(topic)) {
    df_group <- df %>%
      dplyr::filter({{col.topic}} %in% topic)
  } else {
    df_group <- df
  }

  if(is.null(title)) {
    title <- paste0("Comparison of miRNA-term association for ", mir)
  }

    # Splits df per topic
    list_df <- df_group %>%
      dplyr::filter({{col.mir}} == mir) %>%
      dplyr::group_split({{col.topic}})

    df_list_count <- data.frame()

    for (df in list_df) {
      df_ <- count_words_mir(df,
                             stopwords = stopwords,
                             token = token,
                             stopwords_ngram = stopwords_ngram,
                             col.mir = {{col.mir}},
                             col.abstract = {{col.abstract}},
                             ...)
      df_list_count <- rbind(df_list_count, df_)
    }

    # Check that at least two topics are in df
    length_new_topic <- df_list_count %>%
      dplyr::select({{col.topic}}) %>%
      dplyr::pull() %>%
      unique() %>%
      length()

    if(length_new_topic < 2) {
      return(warning(paste0("No shared terms for ", mir, " with these settings.")))
    }


  # Count proportion of each word / abstract / topic
  df_mir <- df_list_count %>%
    dplyr::group_by({{col.topic}}) %>%
    dplyr::mutate(total = length(unique({{col.pmid}}))) %>%
    dplyr::ungroup() %>%
    dplyr::distinct({{col.pmid}}, total, word, {{col.topic}}) %>%
    dplyr::add_count({{col.topic}}, word, name = "no_of_abstract") %>%
    dplyr::mutate(perc = no_of_abstract/total)

  if(normalize == TRUE) {
    df_mir <- df_mir %>%
      dplyr::mutate(no_of_abstract = perc)
  }

  df_mir <- df_mir %>%
    dplyr::distinct(word, no_of_abstract, {{col.topic}}) %>%
    dplyr::mutate(word = forcats::fct_reorder(word, no_of_abstract)) %>%
    dplyr::add_count(word, name = "number_topic")

  if(shared == TRUE) {
    max_topic <- df_mir %>%
      dplyr::select({{col.topic}}) %>%
      dplyr::pull() %>%
      unique() %>%
      length()

    df_mir <- df_mir %>%
      dplyr::filter(number_topic == max_topic)
  }

  if(nrow(df_mir) == 0) {
    return(warning(paste0("No shared terms for ", mir, " with these settings.")))
  }


  # Splits dataframe per Topic
  df_mir_list <- df_mir %>%
    dplyr::group_split({{col.topic}})

  # Gets top words per topic
  top_words <- purrr::map(df_mir_list, ~ get_top_words(df = .x,
                                                top = top,
                                                col.word = word,
                                                col.wt = no_of_abstract)) %>%
    unlist() %>%
    unique()

  # Filters for top word in df
  df_mir <- df_mir %>%
    dplyr::filter(word %in% top_words) %>%
    dplyr::mutate(topic = factor({{col.topic}}))


  plot <- ggplot(df_mir, aes(x = word, y = no_of_abstract, fill = topic)) +
    coord_flip() +
    theme_classic() +
    xlab("Terms") +
    ggtitle(title) +
    scale_fill_discrete(name = "Topic",
                        guide = guide_legend(reverse=TRUE)) +
    # theme(axis.text.y = element_text(size = 12),
    #       plot.title = element_text(size = 10),
    #       axis.title = element_text(size = 10)) +
    scale_y_discrete(expand=c(0.3, 0.3))

  plot <- pretty_breaks_miretrieve(plot, df_mir$no_of_abstract)

  if (normalize == TRUE) {
    plot <- plot +
      ylab("Mentioned in % of abstracts") +
      scale_y_continuous(labels = scales::percent,
                         expand = c(0,0))
  } else {
    plot <- plot +
      ylab("Mentioned in # abstracts")
  }

  if(position == "dodge") {
    plot <- plot +
      geom_col(position = "dodge")
  } else if (position == "facet") {
    plot <- plot +
      geom_col() +
      facet_wrap(~topic)
  }

  return(plot)
}

#' Compare log2-frequency count of terms associated with a miRNA name
#'
#' Compare log2-frequency count of terms associated with a miRNA name
#' over two topics.
#'
#' Compare log2-frequency count of terms associated with a miRNA name over two topics by
#' plotting the log2-ratio of the term count associated with a miRNA name
#' over two topics.
#' miRNA names and topics must be in a data frame `df`, while terms are taken
#' from abstracts contained in `df`.
#' Number of top terms to plot is regulated by `top`. Terms can either be
#' evaluated as their raw count, e.g. in how many abstracts they are mentioned
#' in conjunction with the miRNA name, or as their relative count, e.g.
#' in how many abstracts containing the miRNA they are mentioned compared to all
#' abstracts containing the miRNA.
#' `compare_mir_terms_log2()` is based on the tools available in the
#' \pkg{tidytext} package.
#' The log2-plot is greatly inspired by the book
#'  “tidytext: Text Mining and Analysis Using Tidy Data Principles in R.” by
#'  Silge and Robinson.
#'
#' @param df Data frame containing miRNA names, abstracts, topics, and PubMed-IDs.
#' @param mir String. miRNA name of interest.
#' @param top Integer. Number of top terms to plot.
#' @param token String. Specifies how abstracts shall be split up. Taken from
#' `unnest_tokens()` in the \pkg{tidytext} package:
#' "Unit for tokenizing, or a custom tokenizing function. Built-in options are
#' "words" (default), "characters", "character_shingles", "ngrams", "skip_ngrams",
#' "sentences", "lines", "paragraphs", "regex",
#' (...),
#' and "ptb" (Penn Treebank). If a function, should take a character vector and
#' return a list of character vectors of the same length."
#' @param ... Additional arguments for tokenization, if necessary.
#' @param topic Character vector. Optional. Specifies which topics to plot.
#' Must have length two.
#' If `topic = NULL`, all topics in `df` are plotted.
#' @param shared Boolean. If `shared = TRUE`, only terms that are shared
#' between the two topics are plotted.
#' @param normalize Boolean. If `normalize = TRUE`, normalizes the number of
#' abstracts to the total number of abstracts in a topic.
#' @param stopwords Data frame containing stop words.
#' @param stopwords_ngram Boolean. Specifies if stop words shall be removed
#' from abstracts when using ngrams. Only applied when `token = 'ngrams'`.
#' @param col.mir Symbol. Column containing miRNA names.
#' @param col.abstract Symbol. Column containing abstracts.
#' @param col.topic Symbol. Column containing topic names.
#' @param col.pmid Symbol. Column containing PubMed-IDs.
#' @param title String. Plot title.
#'
#' @return List containing bar plot comparing the log2-frequency of terms associated with a
#' miRNA over two topics and its corresponding data frame.
#'
#' @seealso [compare_mir_terms()], [compare_mir_terms_scatter()]
#'
#' @family compare functions
#'
#' @references Silge, Julia, and David Robinson. 2016.
#' “tidytext: Text Mining and Analysis Using Tidy Data Principles in R.”
#' JOSS 1 (3). The Open Journal. https://doi.org/10.21105/joss.00037.
#'
#' @export
compare_mir_terms_log2 <- function(df,
                                   mir,
                                   top = 20,
                                   token = "words",
                                   ...,
                                   topic = NULL,
                                   shared = TRUE,
                                   normalize = TRUE,
                                   stopwords = stopwords_miretrieve,
                                   stopwords_ngram = TRUE,
                                   col.mir = miRNA,
                                   col.abstract = Abstract,
                                   col.topic = Topic,
                                   col.pmid = PMID,
                                   title = NULL) {

  if(is.null(topic)) {
    topic <- df %>%
      dplyr::select({{col.topic}}) %>%
      dplyr::pull() %>%
      unique()
  }

  if(length(topic) != 2) {
    stop("'topic' must be a character vector of length 2.")
  }

  if(is.null(title)) {
    title <- paste0("Comparison of miRNA-term association for ", mir)
  }

  # Filters for topic of interest,
  # filter for miR
  df_group <- df %>%
    dplyr::filter({{col.topic}} %in% topic) %>%
    dplyr::filter({{col.mir}} == mir)


  # Get shared miRNAs between two topics
  shared_mir <- get_all_shared_mir(df = df,
                                   col.mir = {{col.mir}},
                                   col.topic = {{col.topic}})


  # Check if miRNAs given in 'mir' are shared between both topics
  if(any(!mir %in% shared_mir)){
    mir_not_shared <- mir[!mir %in% shared_mir]
    stop(paste0(mir_not_shared, " is not shared between both topics.
    Please set 'mir' only to miRNAs shared between both topics."))
  }

  if(normalize == TRUE) {
    ylab_string <- paste0("Log2 ratio of ", topic[2], " / ", topic[1],
                          " (normalized)")
  } else {
    ylab_string <- paste0("Log2 ratio of ", topic[2], " / ", topic[1])
  }



  # Splits df per topic
  list_df <- df_group %>%
    dplyr::group_split({{col.topic}})

    df_list_count <- data.frame()

    for (df in list_df) {
      df_ <- count_words_mir(df,
                             stopwords = stopwords,
                             token = token,
                             stopwords_ngram = stopwords_ngram,
                             col.mir = {{col.mir}},
                             col.abstract = {{col.abstract}},
                             ...)
      df_list_count <- rbind(df_list_count, df_)
    }

    # Check that still two topics are in df
    length_new_topic <- df_list_count %>%
      dplyr::select({{col.topic}}) %>%
      dplyr::pull() %>%
      unique() %>%
      length()

    if(length_new_topic != length(topic)) {
      return(warning(paste0("No shared terms for ", mir, " with these settings.")))
    }

    # Count proportion of each word / abstract / topic
    df_mir <- df_list_count %>%
      dplyr::group_by({{col.topic}}) %>%
      dplyr::mutate(total = length(unique({{col.pmid}}))) %>%
      dplyr::ungroup() %>%
      dplyr::distinct({{col.pmid}}, total, word, {{col.topic}}) %>%
      dplyr::add_count({{col.topic}}, word, name = "no_of_abstract") %>%
      dplyr::mutate(perc = no_of_abstract/total)

    if(normalize == TRUE) {
      df_mir <- df_mir %>%
        dplyr::mutate(no_of_abstract = perc)
    }

    df_mir <- df_mir %>%
      dplyr::distinct(word, no_of_abstract, {{col.topic}}) %>%
      dplyr::mutate(word = forcats::fct_reorder(word, no_of_abstract)) %>%
      dplyr::add_count(word, name = "number_topic")

    if(shared == TRUE) {
      max_topic <- df_mir %>%
        dplyr::select({{col.topic}}) %>%
        dplyr::pull() %>%
        unique() %>%
        length()

      df_mir <- df_mir %>%
        dplyr::filter(number_topic == max_topic)
    }

    if(nrow(df_mir) == 0) {
      return(warning(paste0("No shared terms for ", mir, " with these settings.")))
    }



    # Splits dataframe per Topic
    df_mir_list <- df_mir %>%
      dplyr::group_split({{col.topic}})

    # Gets top words per topic
    top_words <- purrr::map(df_mir_list, ~ get_top_words(df = .x,
                                                         top = top,
                                                         col.word = word,
                                                         col.wt = no_of_abstract)) %>%
      unlist() %>%
      unique()

    # Filters for top word in df
    df_mir <- df_mir %>%
      dplyr::filter(word %in% top_words) %>%
      tidyr::spread({{col.topic}}, no_of_abstract) %>%
      dplyr::mutate(log2_ratio = log2(!!sym(topic[2])/!!sym(topic[1]))) %>%
      dplyr::mutate(word = forcats::fct_reorder(word, log2_ratio))


    # Make breaks for color
    if(min(df_mir$log2_ratio) >= 0) {
      breaks_min <- NULL
    } else {
      breaks_min <- min(df_mir$log2_ratio)
    }

    if(is.null(breaks_min)) {
      label_min <- NULL
    } else {
      label_min <- topic[1]
    }

    if(max(df_mir$log2_ratio) <= 0) {
      breaks_max <- NULL
    } else {
      breaks_max <- max(df_mir$log2_ratio)
    }

    if(is.null(breaks_max)) {
      label_max <- NULL
    } else {
      label_max <- topic[2]
    }

    # Plot log2-ratio
  plot <- ggplot(df_mir, aes(x = factor(word),
                             y = log2_ratio,
                             fill = log2_ratio)) +
    geom_col(color = "black", size = 0.2) +
    theme_classic() +
    xlab("Word") +
    ylab(ylab_string) +
    coord_flip()+
    ggtitle(title) +
    scale_fill_gradient2(high = "blue", low = "red",
                         midpoint = 0, guide = "legend",
                         breaks = c(breaks_min, breaks_max),
                         labels = c(label_min, label_max)) +
    guides(fill = guide_legend(reverse = TRUE, title = "Higher for"))

  list_df_plot <- list(data.frame = df_mir,
                       plot = plot)

  return(list_df_plot)
}

#' Compare shared terms associated with a miRNA name
#'
#' Compare shared terms associated with a miRNA name over two topics.
#'
#' Compare shared terms associated with a miRNA name over two topics. These terms are displayed
#' as a scatter plot, which is either interactive as an HTML-widget, or static. This
#' is regulated via the `html` argument.
#' miRNA names and topics must be in a data frame `df`, while terms are taken
#' from abstracts contained in `df`.
#' Number of top terms to choose is regulated by `top`. Terms are
#' evaluated as their raw count and plotted on a log10-scale.
#' `compare_mir_terms_scatter()` is based on the tools available in the
#' \pkg{tidytext} package.
#' The term-plot is greatly inspired by
#' “tidytext: Text Mining and Analysis Using Tidy Data Principles in R.” by
#' Silge and Robinson.
#'
#' @param df Data frame containing miRNA names, abstracts, topics, and PubMed-IDs.
#' @param mir String. miRNA name of interest.
#' @param top Integer. Number of top terms to plot.
#' @param token String. Specifies how abstracts shall be split up. Taken from
#' `unnest_tokens()` in the \pkg{tidytext} package:
#' "Unit for tokenizing, or a custom tokenizing function. Built-in options are
#' "words" (default), "characters", "character_shingles", "ngrams", "skip_ngrams",
#' "sentences", "lines", "paragraphs", "regex",
#' (...),
#' and "ptb" (Penn Treebank). If a function, should take a character vector and
#' return a list of character vectors of the same length."
#' @param ... Additional arguments for tokenization, if necessary.
#' @param topic Character vector. Optional. Specifies which topics to plot.
#' Must have length two.
#' If `topic = NULL`, all topics in `df` are plotted.
#' @param stopwords Data frame containing stop words.
#' @param stopwords_ngram Boolean. Specifies if stop words shall be removed
#' from abstracts when using ngrams. Only applied when `token = 'ngrams'`.
#' @param html Boolean. Specifies if plot is returned as an HTML-widget
#' or static.
#' @param colour.point String. Colour of points for scatter plot.
#' @param colour.term String. Colour of terms for scatter plot.
#' @param col.mir Symbol. Column containing miRNAs.
#' @param col.abstract Symbol. Column containing abstracts.
#' @param col.topic Symbol. Column containing topics names.
#' @param col.pmid Symbol. Column containing PubMed-IDs.
#' @param title String. Plot title.
#'
#' @return Scatter plot comparing shared terms of a miRNA between two topics.
#'
#' @seealso [compare_mir_terms()], [compare_mir_terms_log2()]
#'
#' @family compare functions
#'
#' @references Silge, Julia, and David Robinson. 2016.
#' “tidytext: Text Mining and Analysis Using Tidy Data Principles in R.”
#' JOSS 1 (3). The Open Journal. https://doi.org/10.21105/joss.00037.
#'
#' @importFrom rlang :=
#'
#' @export
compare_mir_terms_scatter <- function(df,
                              mir,
                              top = 1000,
                              token = "words",
                              ...,
                              topic = NULL,
                              stopwords = stopwords_miretrieve,
                              stopwords_ngram = TRUE,
                              html = TRUE,
                              colour.point = "red",
                              colour.term = "black",
                              col.mir = miRNA,
                              col.abstract = Abstract,
                              col.topic = Topic,
                              col.pmid = PMID,
                              title = NULL) {
  if(is.null(topic)) {
    topic <- df %>%
      dplyr::select({{col.topic}}) %>%
      dplyr::pull() %>%
      unique()
  }

  if(length(topic)!= 2) {
    stop("'topic' must be a character vector of length 2.")
  }

  if(length(mir) != 1) {
    stop("'miR' must be a character vector of length 1.")
  }

  # Get shared miRNAs between two topics
  shared_mir <- get_all_shared_mir(df = df,
                                   col.mir = {{col.mir}},
                                   col.topic = {{col.topic}})


  # Check if miRNAs given in 'mir' are shared between both topics
  if(any(!mir %in% shared_mir)){
    mir_not_shared <- mir[!mir %in% shared_mir]
    stop(paste0(mir_not_shared, " is not shared between both topics.
    Please set 'mir' only to miRNAs shared between both topics."))
  }

  if(is.null(title)) {
    title <- paste0("Comparison of miRNA-term association for ", mir)
  }

  # Filters for topic of interest
  # Filter for miRNA of interest
  df_group <- df %>%
    dplyr::filter({{col.topic}} %in% topic) %>%
    dplyr::filter({{col.mir}} == mir)

  # Splits df per topic
  list_df <- df_group %>%
    dplyr::group_split({{col.topic}})


  #Applies counting
  df_list_count <- data.frame()

  for (df in list_df) {
    df_ <- count_words_mir(df,
                           stopwords = stopwords,
                           token = token,
                           stopwords_ngram = stopwords_ngram,
                           col.mir = {{col.mir}},
                           col.abstract = {{col.abstract}},
                           ...)
    df_list_count <- rbind(df_list_count, df_)
  }

  # Check that still two topics are in df
  length_new_topic <- df_list_count %>%
    dplyr::select({{col.topic}}) %>%
    dplyr::pull() %>%
    unique() %>%
    length()

  if(length_new_topic != 2) {
    return(warning(paste0("No shared terms for ", mir, " with these settings.")))
  }



  # Filter for miR
  df_mir <- df_list_count %>%
    dplyr::filter({{col.mir}} == mir)

  # Count proportion of each word / abstract / topic
  df_mir <- df_mir %>%
    dplyr::group_by({{col.topic}}) %>%
    dplyr::mutate(n_abstracts_topic = length(unique({{col.pmid}}))) %>%
    dplyr::ungroup() %>%
    dplyr::group_by({{col.topic}}, word) %>%
    dplyr::mutate(n_abstracts_word = length(unique({{col.pmid}}))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(perc_abstract = n_abstracts_word / n_abstracts_topic) %>%
    dplyr::mutate(mirna_count = perc_abstract)

  # Selects Topic, word, and count
  df_mir <- df_mir %>%
    dplyr::select({{col.topic}}, word, mirna_count) %>%
    dplyr::distinct()

  # Splits dataframe per Topic
  df_mir_list <- df_mir %>%
    dplyr::group_split({{col.topic}})

  # Gets top words per topic
  top_words <- purrr::map(df_mir_list, ~ get_top_words(df = .x,
                                                       top = top,
                                                       col.word = word,
                                                       col.wt = mirna_count)) %>%
    unlist() %>%
    unique()

  # Filters for top word in df
  df_mir <- df_mir %>%
    dplyr::filter(word %in% top_words) %>%
    tidyr::spread({{col.topic}}, mirna_count) %>%
    dplyr::mutate(!!sym(topic[1]) := round(!!sym(topic[1]), 3),
                  !!sym(topic[2]) := round(!!sym(topic[2]), 3))




  # Scatter plot
  plot <- ggplot(df_mir, aes(x = !!sym(topic[1]), y = !!sym(topic[2]),
                               text = word)) +
    geom_abline(color = "gray40", lty = 2, intercept = 0, slope = 1) +
    geom_jitter(alpha = 0.1,
                size = 1.5,
                width = 0.05,
                height = 0.05,
                colour = colour.point) +
    scale_x_log10(labels = scales::percent_format()) +
    scale_y_log10(labels = scales::percent_format()) +
    scale_color_gradient(limits = c(0, 0.001), low = "darkslategray4", high = "gray75") +
    theme_minimal()+
    ggtitle(title)

  if(html == TRUE) {
    plot <- plotly::ggplotly(plot)
  }else{
    plot <- plot +
      geom_text(aes(label = word),
                check_overlap = TRUE,
                vjust = 1.5,
                size = 3,
                colour = colour.term)
  }

  return(plot)
}



#' Compare terms uniquely associated with a miRNA name
#'
#' Compare terms uniquely associated with a miRNA name over topics.
#'
#' Compare terms uniquely associated with a miRNA name over topics.
#' miRNA names and topics must be in a data frame `df`, while terms are taken
#' from abstracts contained in `df`.
#' Number of top terms to choose is regulated by `top`. Terms are
#' evaluated either as the number of times they are mentioned in all abstracts
#' with the miRNA name of interest, or the number of times they are relatively mentioned
#' compared to all abstracts with the miRNA name of interest.
#' `compare_mir_terms_unique()` is based on the tools available in the
#' \pkg{tidytext} package.
#'
#' @param df Data frame containing miRNA names, abstracts, topics, and PubMed-IDs.
#' @param mir String. miRNA name of interest.
#' @param top Integer. Number of top terms to plot.
#' @param token String. Specifies how abstracts shall be split up. Taken from
#' `unnest_tokens()` in the \pkg{tidytext} package:
#' "Unit for tokenizing, or a custom tokenizing function. Built-in options are
#' "words" (default), "characters", "character_shingles", "ngrams", "skip_ngrams",
#' "sentences", "lines", "paragraphs", "regex",
#' (...),
#' and "ptb" (Penn Treebank). If a function, should take a character vector and
#' return a list of character vectors of the same length."
#' @param ... Additional arguments for tokenization, if necessary.
#' @param topic Character vector. Optional. Specifies which topics to plot.
#' If `topic = NULL`, all topics in `df` are plotted.
#' @param stopwords Data frame containing stop words.
#' @param stopwords_ngram Boolean. Specifies if stop words shall be removed
#' from abstracts when using ngrams. Only applied when `token = 'ngrams'`.
#' @param normalize Boolean. If `normalize = TRUE`, relative term frequency is
#' plotted, denoting the relative number of papers with `mir` mentioning the term
#' compared to all papers with `mir` mentioning the term. If `normalize = FALSE`,
#' absolute term frequency is plotted, denoting the number of papers with `mir`
#' the term is mentioned in.
#' @param colour String. Colour of bar plot.
#' @param col.mir Symbol. Column containing miRNAs.
#' @param col.abstract Symbol. Column containing abstracts.
#' @param col.topic Symbol. Column containing topics names.
#' @param col.pmid Symbol. Column containing PubMed-IDs.
#' @param title String. Plot title.
#'
#' @return Bar plot containing unique miRNA-terms associations per topic.
#'
#' @seealso [compare_mir_terms()], [compare_mir_terms_log2()], [compare_mir_terms_scatter()]
#'
#' @family compare functions
#'
#' @export

compare_mir_terms_unique <- function(df,
                                     mir,
                                     top = 20,
                                     token = "words",
                                     ...,
                                     topic = NULL,
                                     stopwords = stopwords_miretrieve,
                                     stopwords_ngram = TRUE,
                                     normalize = TRUE,
                                     colour = "steelblue3",
                                     col.mir = miRNA,
                                     col.abstract = Abstract,
                                     col.topic = Topic,
                                     col.pmid = PMID,
                                     title = NULL) {

  if(is.null(topic)) {
    topic <- df %>%
      dplyr::select({{col.topic}}) %>%
      dplyr::pull() %>%
      unique()
  }


  if(length(mir) != 1) {
    stop("'miR' must be a character vector of length 1.")
  }

  # Get shared miRNAs between two topics
  shared_mir <- get_all_shared_mir(df = df,
                                   col.mir = {{col.mir}},
                                   col.topic = {{col.topic}})


  # Check if miRNAs given in 'mir' are shared between topics
  if(any(!mir %in% shared_mir)){
    mir_not_shared <- mir[!mir %in% shared_mir]
    stop(paste0(mir_not_shared, " is not shared between both topics.
    Please set 'mir' only to miRNAs shared between both topics."))
  }

  if(is.null(title)) {
    title <- paste0("Unique miRNA-term association for ", mir)
  }

  if(normalize == TRUE) {
    ylab_string <- paste0("Mentioned in % of abstracts with ", mir)
  } else {
    ylab_string <- paste0("Mentioned in # of abstracts with ", mir)
  }

  # Filter for relevant topic
  # Filter for relevant miRNA
  # Get total number of abstracts / topic
  df_total <- df %>%
    dplyr::filter({{col.topic}} %in% topic) %>%
    dplyr::filter(miRNA == mir) %>%
    dplyr::group_by({{col.topic}}) %>%
    dplyr::mutate(total = length(unique({{col.pmid}}))) %>%
    dplyr::ungroup()

  # Get terms
  df_word_unique <- count_words_mir(df = df_total,
                                    stopwords = stopwords,
                                    token = token,
                                    stopwords_ngram = stopwords_ngram,
                                    col.mir = {{col.mir}},
                                    col.abstract = {{col.abstract}},
                                    ...) %>%
    dplyr::distinct({{col.pmid}}, {{col.topic}}, word, total) %>%
    # Identifies in how many abstracts a word was mentioned per abstract
    dplyr::add_count({{col.topic}}, word, name = "no_per_topic") %>%
    # Identifies in how many abstracts a word was mentioned in percent
    dplyr::mutate(perc = no_per_topic/total) %>%
    dplyr::select(-{{col.pmid}})

  # Normalize number of abstracts
  if(normalize == TRUE) {
    df_word_unique <- df_word_unique %>%
      dplyr::mutate(no_per_topic = perc)
  }

  # Get unique terms/topics
  df_word_unique <- df_word_unique %>%
    dplyr::distinct({{col.topic}}, word, no_per_topic) %>%
    dplyr::add_count(word, name = "no_topic") %>%
    dplyr::filter(no_topic == 1) %>%
    dplyr::group_by({{col.topic}}) %>%
    dplyr::top_n(top, no_per_topic) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(word = forcats::fct_reorder(word, no_per_topic))

  # Plot unique terms
  plot_terms_unique <- ggplot(df_word_unique, aes(x = word, y = no_per_topic)) +
    geom_col(fill = colour) +
    coord_flip() +
    facet_wrap(vars({{col.topic}}), scales = "free_y") +
    ylab(ylab_string) +
    xlab("Term") +
    theme_classic() +
    theme(strip.background = element_blank()) +
    ggtitle(title)

  plot_terms_unique <- pretty_breaks_miretrieve(plot_terms_unique,
                                                df_word_unique$no_per_topic)

  if(normalize == TRUE) {
    plot_terms_unique <- plot_terms_unique +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                         expand = c(0,0))
  }

  return(plot_terms_unique)
}
