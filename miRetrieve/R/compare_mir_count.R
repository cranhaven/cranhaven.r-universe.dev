#' Compare count of miRNA names between different topics
#'
#' Compare count of miRNA names between different topics.
#'
#' Compare count of miRNA names between different topics by plotting the number
#' of abstracts mentioning the miRNA in a topic. This count can either be normalized,
#' thus plotting the proportion of abstracts mentioning a miRNA name compared to all
#' abstracts of a topic, or it can be not normalized, thus plotting the absolute
#' number of abstracts mentioning a miRNA per topic.
#'
#' @param df Data frame containing columns for miRNA names, topics,
#' and PubMed-IDs.
#' @param mir Character vector. Vector specifying which miRNA names to
#' compare.
#' @param topic Character vector. Optional. Vector
#' specifying which topics to compare.
#' @param normalize Boolean. If `normalize = TRUE`, plot the proportion of
#' abstracts mentioning a miRNA name compared to all abstracts in a topic.
#' If `normalize = FALSE`, plot the absolute number of abstracts mentioning a miRNA
#' in a topic.
#' @param col.topic Symbol. Column containing topic names.
#' @param col.mir Symbol. Column containing miRNA names.
#' @param col.pmid Symbol. Column containing PubMed-IDs.
#' @param title String. Plot title.
#'
#' @return Bar plot comparing the count of miRNA names between different topics.
#'
#' @seealso [compare_mir_count_log2()], [compare_mir_count_unique()]
#'
#' @family compare functions
#'
#' @export
compare_mir_count <- function(df,
                              mir,
                              topic = NULL,
                              normalize = TRUE,
                              col.topic = Topic,
                              col.mir = miRNA,
                              col.pmid = PMID,
                              title = NULL) {

  if(!is.null(topic)) {
    df <- df %>%
      dplyr::filter({{col.topic}} %in% topic)
  }

  # Set title
  if(is.null(title)) {
    title <- "Comparison of miRNA count"
  }

  # Add column with total number of distinct abstracts
  # Calculate percentage per topic
  df_count <- df %>%
    dplyr::group_by({{col.topic}}) %>%
    dplyr::mutate(n_row = length(unique({{col.pmid}}))) %>%
    dplyr::ungroup() %>%
    dplyr::add_count({{col.topic}}, {{col.mir}}, name = "count_mir") %>%
    dplyr::mutate(perc_per_topic = count_mir / n_row)

  # Filter for miRNAs of interest
  df_count <- df_count %>%
    dplyr::filter({{col.mir}} %in% mir)

  if(normalize == TRUE) {
    df_count <- df_count %>%
      dplyr::mutate(count_mir = perc_per_topic)
  }

  plot <- ggplot(df_count, aes(x = factor({{col.mir}}),
                               y = count_mir,
                               fill = {{col.topic}})) +
    geom_col(position = "dodge") +
    theme_classic() +
    xlab("miRNA") +
    coord_flip() +
    ggtitle(title)

  plot <- pretty_breaks_miretrieve(plot, df_count$count_mir)

  if(normalize == TRUE) {
    plot <- plot +
      ylab("Mentioned in % of abstracts per topic") +
      scale_y_continuous(labels = scales::percent,
                         expand = c(0,0))

    return(plot)
  } else {
    plot <- plot +
      ylab("Mentioned in # of abstracts")

    return(plot)
  }
}

#' Compare log2-frequency count of miRNA names between two topics
#'
#' Compare log2-frequency count of miRNA names between two topics
#'
#' Compare log2-frequency count of miRNA names between two topics by plotting the
#' log2-ratio of the miRNA count in two topics. The miRNA count per topic can
#' either be normalized, thus taking the proportion of abstracts mentioning
#' a miRNA name compared to all abstracts in a topic, or not normalized, thus
#' taking the absolute number of abstracts mentioning a miRNA in a topic.
#' The log2-plot is greatly inspired by the book
#' “tidytext: Text Mining and Analysis Using Tidy Data Principles in R.” by
#' Silge and Robinson.
#'
#' @param df Data frame containing miRNA names, topics, and PubMed-IDs.
#' @param mir Character vector. Vector specifying which miRNA names to
#' compare.
#' @param topic Character vector. Optional. Vector specifying which
#' topics to compare. If `topic = NULL`, all topics in `df` are used.
#' @param normalize Boolean. If `normalize = TRUE`, proportion of abstracts
#' mentioning a miRNA name compared to all abstracts of a topic are used. If
#' `normalize = FALSE`, the absolute number of abstracts mentioning a miRNA
#' name is used.
#' @param col.topic Symbol. Column containing topics.
#' @param col.mir Symbol. Column containing miRNA names.
#' @param col.pmid Symbol. Column containing PubMed-IDs.
#' @param title String. Plot title.
#'
#' @return List containing bar plot comparing the log2-frequency count of miRNA names between
#' two topics and its corresponding data frame.
#'
#' @seealso [compare_mir_count()], [compare_mir_count_unique()]
#'
#' @family compare functions
#'
#'
#' @references Silge, Julia, and David Robinson. 2016.
#' “tidytext: Text Mining and Analysis Using Tidy Data Principles in R.”
#' JOSS 1 (3). The Open Journal. https://doi.org/10.21105/joss.00037.
#'
#' @importFrom rlang :=
#'
#' @export
compare_mir_count_log2 <- function(df,
                             mir,
                             topic = NULL,
                             normalize = TRUE,
                             col.topic = Topic,
                             col.mir = miRNA,
                             col.pmid = PMID,
                             title = NULL) {

  if(is.null(topic)) {
    topic <- df %>%
      dplyr::select({{col.topic}}) %>%
      dplyr::pull() %>%
      unique()
  }

  if(!length(topic) == 2) {
    stop("Number of topics must be 2. Please specify two topics to be used in 'topic'.")
  }

  df <- df %>%
    dplyr::filter({{col.topic}} %in% topic)

  # Get shared miRNAs between two topics
  shared_mir <- get_all_shared_mir(df = df,
                                   col.mir = {{col.mir}},
                                   col.topic = {{col.topic}})


  # Check if miRNAs given in 'mir' are shared between both topics
  if(any(!mir %in% shared_mir)){
    mir_not_shared <- mir[!mir %in% shared_mir]
    stop(paste0("There are miRNAs specified in 'mir' that are not shared between
                both topics, namely ", mir_not_shared, ". Please set 'mir' only to
                miRNAs shared between both topics."))
  }

  # Set title
  if(is.null(title)) {
    title <- "Comparison of miRNA count"
  }

  if(normalize == TRUE) {
    ylab_string <- paste0("Log2 ratio of ", topic[2], " / ", topic[1],
                          " (normalized)")
  } else {
    ylab_string <- paste0("Log2 ratio of ", topic[2], " / ", topic[1])
  }

  # Add column with total number of distinct abstracts
  # Calculate percentage per topic
  df_count <- df %>%
    dplyr::group_by({{col.topic}}) %>%
    dplyr::mutate(n_row = length(unique({{col.pmid}}))) %>%
    dplyr::ungroup() %>%
    dplyr::add_count({{col.topic}}, {{col.mir}}, name = "count_mir") %>%
    dplyr::mutate(perc_per_topic = count_mir / n_row) %>%
    dplyr::select(-n_row)

  if(normalize == TRUE) {
    df_count <- df_count %>%
      dplyr::mutate(count_mir = perc_per_topic)
  }

  # Filter for miRNAs of interest
  df_count <- df_count %>%
    dplyr::filter({{col.mir}} %in% mir)

  df_count <- df_count %>%
    dplyr::select({{col.mir}}, {{col.topic}}, count_mir) %>%
    dplyr::distinct() %>%
    tidyr::spread({{col.topic}}, count_mir) %>%
    dplyr::mutate(log2_ratio = log2(!!sym(topic[2])/!!sym(topic[1]))) %>%
    dplyr::mutate({{col.mir}} := forcats::fct_reorder({{col.mir}}, log2_ratio))

  # Make breaks for color
  if(min(df_count$log2_ratio) >= 0) {
    breaks_min <- NULL
  } else {
    breaks_min <- min(df_count$log2_ratio)
  }

  if(is.null(breaks_min)) {
    label_min <- NULL
  } else {
    label_min <- topic[1]
  }

  if(max(df_count$log2_ratio) <= 0) {
    breaks_max <- NULL
  } else {
    breaks_max <- max(df_count$log2_ratio)
  }

  if(is.null(breaks_max)) {
    label_max <- NULL
  } else {
    label_max <- topic[2]
  }


  plot <- ggplot(df_count, aes(x = factor({{col.mir}}),
                               y = log2_ratio,
                               fill = log2_ratio)) +
    geom_col(color = "black", size = 0.2) +
    theme_classic() +
    xlab("miRNA") +
    ylab(ylab_string) +
    coord_flip() +
    ggtitle(title) +
    scale_fill_gradient2(high = "blue", low = "red",
                         midpoint = 0, guide = "legend",
                         breaks = c(breaks_min, breaks_max),
                         labels = c(label_min, label_max)) +
    guides(fill = guide_legend(reverse = TRUE, title = "Higher for"))

  list_df_plot <- list(data.frame = df_count,
                       plot = plot)

  return(list_df_plot)
}

#' Compare top count of unique miRNA names per topic
#'
#' Compare top count of unique miRNA names per topic
#'
#' Compare top count of unique miRNA names per topic by plotting the
#' the miRNA count of unique miRNAs per topic. Per topic, the unique miRNAs
#' are identified and their count is plotted.
#' The miRNA count can either be normalized, thus taking the proportion of abstracts mentioning
#' a miRNA name compared to all abstracts in a topic, or not normalized, thus
#' taking the absolute number of abstracts mentioning a miRNA in a topic.
#'
#' @param df Data frame containing miRNA names, topics, and PubMed-IDs.
#' @param top Integer. Specifies number of top unique miRNAs to plot.
#' @param topic Character vector. Optional. Vector specifying which
#' topics to compare. If `topic = NULL`, all topics in `df` are used.
#' @param normalize Boolean. If `normalize = TRUE`, proportion of abstracts
#' mentioning a miRNA name compared to all abstracts of a topic are used. If
#' `normalize = FALSE`, the absolute number of abstracts mentioning a miRNA
#' name is used.
#' @param colour String. Colour of bar plot.
#' @param col.topic Symbol. Column containing topics.
#' @param col.mir Symbol. Column containing miRNA names.
#' @param col.pmid Symbol. Column containing PubMed-IDs.
#' @param title String. Plot title.
#'
#' @return Bar plot comparing frequency of unique miRNA count per topic.
#'
#' @seealso [compare_mir_count()], [compare_mir_count_log2()]
#'
#' @family compare functions
#'
#' @export
compare_mir_count_unique <- function(df,
                                     top = 5,
                                     topic = NULL,
                                     normalize = TRUE,
                                     colour = "steelblue3",
                                     col.topic = Topic,
                                     col.mir = miRNA,
                                     col.pmid = PMID,
                                     title = NULL) {
  # Get topic names
  if(is.null(topic)) {
    topic <- df %>%
      dplyr::select({{col.topic}}) %>%
      dplyr::pull() %>%
      unique()
  }

  # Filter for topic names
  df <- df %>%
    dplyr::filter({{col.topic}} %in% topic)

  # Set plot title
  if(is.null(title)) {
    title <- "Comparison of unique miRNAs"
  }

  # Set y-axis title
  if(normalize == TRUE) {
    ylab_string <- "Mentioned in % of abstracts"
  } else if (normalize == FALSE) {
    ylab_string <- "Mentioned in # of abstracts"
  }

  # Filter for unique miRNAs / topic
  df_unique <- df %>%
    dplyr::group_by({{col.topic}}) %>%
    # Get total number of abstracts
    dplyr::mutate(total = length(unique({{col.pmid}}))) %>%
    dplyr::ungroup() %>%
    dplyr::add_count({{col.topic}}, {{col.mir}}, name = "count_mir") %>%
    # Count relative number abstracts mentioning miRNA
    dplyr::mutate(perc = count_mir/total) %>%
    dplyr::select(-total)

  if(normalize == TRUE) {
    df_unique <- df_unique %>%
      dplyr::mutate(count_mir = perc)
  }

  df_unique <- df_unique %>%
    dplyr::select({{col.mir}}, count_mir, {{col.topic}}) %>%
    dplyr::distinct() %>%
    # Filter for miRNAs in only one topic
    dplyr::add_count({{col.mir}}, name = "no_topic") %>%
    dplyr::filter(no_topic == 1) %>%
    dplyr::group_by({{col.topic}}) %>%
    dplyr::top_n(top, count_mir) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(miRNA = forcats::fct_reorder({{col.mir}}, count_mir))

  # Plot
  plot_unique <- ggplot(df_unique, aes(x = {{col.mir}}, y = count_mir)) +
    geom_col(fill = colour) +
    coord_flip() +
    facet_wrap(vars({{col.topic}}), scales = "free_y") +
    ylab(ylab_string) +
    theme_classic() +
    theme(strip.background = element_blank()) +
    ggtitle(title)

  plot_unique <- pretty_breaks_miretrieve(plot_unique, df_unique$count_mir)

  if(normalize == TRUE) {
    plot_unique <- plot_unique +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                         expand = c(0,0))
  }

  return(plot_unique)
}
