#' Count miRNA names in a data frame
#'
#' Count occurrence of miRNA names in a data frame.
#'
#' Count occurrence of miRNA names in  a data frame. The count of miRNA names
#' is returned as a separate data frame, only listing the miRNA
#' names and their respective frequency.
#'
#' @param df Data frame containing miRNA names.
#' @param col.mir Symbol. Column containing miRNA names.
#'
#' @return Data frame. Data frame containing miRNA names and their
#' respective frequency.
#'
#' @seealso [plot_mir_count()], [count_mir_threshold()], [plot_mir_count_threshold()]
#'
#' @family count functions
#'
#' @importFrom dplyr desc
#'
#' @export
count_mir <- function(df,
                      col.mir = miRNA) {
  df_count <- df %>%
    dplyr::add_count({{col.mir}}) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::rename("Mentioned_n" = n) %>%
    dplyr::select({{col.mir}}, Mentioned_n) %>%
    dplyr::distinct()
  return(df_count)
}

#' Plot count of most frequently mentioned miRNA names
#'
#' Plot count of most frequently mentioned miRNA names in a data frame.
#'
#' Plot count of most frequently mentioned miRNA names in a data frame. How many
#' most frequently mentioned miRNAs are plotted is determined via the `top`
#' argument. Ties among the most frequently mentioned miRNAs are treated as
#' the same rank, e.g. if *miR-126*, *miR-34*, and *miR-29* were all mentioned
#' the most often, they would all be plotted by specifying `top = 1`, `top = 2`,
#' or `top = 3`.
#'
#' @param df Data frame containing miRNA names.
#' @param top Integer. Specifies number of most frequent miRNA names to plot.
#' @param colour String. Colour of bar plot.
#' @param col.mir Symbol. Column containing miRNA names.
#' @param title String. Plot title.
#'
#' @return Bar plot with the most frequently mentioned miRNAs names in `df`.
#'
#' @seealso [count_mir()], [count_mir_threshold()], [plot_mir_count_threshold()]
#'
#' @family count functions
#'
#' @importFrom magrittr %>%
#' @import ggplot2
#'
#' @export
plot_mir_count <- function(df,
                           top = 10,
                           colour = "steelblue3",
                           col.mir = miRNA,
                           title = NULL) {

  # Set title
  if(is.null(title)) {
    title <- "Most frequently mentioned miRNAs"
  }

  df_count <- df %>%
    dplyr::add_count({{col.mir}}) %>%
    dplyr::select({{col.mir}}, n) %>%
    dplyr::distinct() %>%
    dplyr::top_n(top, n) %>%
    dplyr::mutate(miR = as.factor({{col.mir}})) %>%
    dplyr::mutate(miR = forcats::fct_reorder(miR, n))

  plot <- ggplot(df_count, aes(x = miR, y = n)) +
    geom_col(fill = colour) +
    coord_flip() +
    xlab("miRNA") +
    ylab("Mentioned in # of abstracts") +
    ggtitle(title) +
    theme_classic()

  plot <- pretty_breaks_miretrieve(plot, df_count$n)


  return(plot)
}
