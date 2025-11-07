#' Plot number of newly mentioned miRNA names/year
#'
#' Plot number of newly mentioned miRNA names/year.
#'
#' Plot how many miRNAs are mentioned for the first time in different year.
#' If a miRNA is considered to be "mentioned" in a year can be regulated
#' via the `threshold` argument. If, for example, `threshold` is set to 3, but a
#' miRNA is mentioned only twice in a year, it is not considered
#' to be "mentioned" for this year.
#'
#' @param df Data frame containing miRNA names and publication years.
#' @param threshold Integer. Specifies how often a miRNA must be
#' mentioned in a year to be considered "mentioned".
#' @param start Integer. Optional. Beginning of publication period.
#' If `start = NULL`, `start` is set to the least recent year in `df`.
#' @param end Integer. Optional. End of publication period.
#' If `end = NULL`, `end` is set to the most recent year in `df`.
#' @param colour String. Colour of bar plot.
#' @param col.mir Symbol. Column containing miRNA names.
#' @param col.year Symbol. Column containing publication year.
#' @param title String. Plot title.
#'
#' @return Bar plot displaying the number of newly mentioned miRNA names/year.
#'
#' @family miR development functions
#'
#' @export
plot_mir_new <- function(df,
                         threshold = 1,
                         start = NULL,
                         end = NULL,
                         colour = "steelblue3",
                         col.mir = miRNA,
                         col.year = Year,
                         title = NULL) {
  if(!threshold >= 1) {
    stop("'threshold' must be >= 1.")
  }

  # Set title
  if(is.null(title)) {
    title <- "Number of new miRNAs per year"
  }

  if(is.null(start)) {
    start <- df %>%
      dplyr::select({{col.year}}) %>%
      dplyr::pull() %>%
      min()
  }

  if(is.null(end)) {
    end <- df %>%
      dplyr::select({{col.year}}) %>%
      dplyr::pull() %>%
      max()
  }

  df_ <- df %>%
    dplyr::add_count({{col.year}}, {{col.mir}}) %>%
    dplyr::filter(n >= threshold) %>%
    dplyr::group_by({{col.mir}}) %>%
    dplyr::summarise(first_mentioned = min({{col.year}})) %>%
    dplyr::add_count(first_mentioned, name = "new_miRNAs") %>%
    dplyr::select(first_mentioned, new_miRNAs) %>%
    dplyr::distinct() %>%
    dplyr::filter(dplyr::between(first_mentioned, start, end))

  # Make empty dataframe per miRNA and year = 0
  # Dummy column is not really necessary, but hey.
  df_empty <- data.frame(first_mentioned = seq(start, end, by = 1),
                          stringsAsFactors = FALSE) %>%
    dplyr::as_tibble()

  # Join df_ to df_empty

  df_plot <- df_empty %>%
    dplyr::left_join(df_, by = "first_mentioned") %>%
    dplyr::mutate(new_miRNAs = ifelse(is.na(new_miRNAs), 0, new_miRNAs))

  plot <- ggplot(df_plot, aes(x = factor(first_mentioned), y = new_miRNAs)) +
    geom_col(fill = colour) +
    theme_classic() +
    xlab("Year") +
    ylab("# of new miRNAs") +
    labs(caption = paste("Threshold: ", threshold)) +
    ggtitle(title)

  plot <- pretty_breaks_miretrieve(plot, df_plot$new_miRNAs)


  return(plot)
}


#' Plot development of miRNA name mentioning over time
#'
#' Plot development of miRNA name mentioning over time.
#'
#' Plot how often a miRNA name was mentioned per year.
#'
#' @param df Data frame containing miRNA names and publication years.
#' @param mir Character vector. Vector containing miRNA names to plot.
#' @param start Numeric. Optional. Specifies start year. If `start = NULL`,
#' `start` is set to the oldest year in `df`.
#' @param end Numeric. Optional. Specifies end year. If `end = NULL`,
#' `end` is set to the youngest year in `df`.
#' @param linetype String. Specifies linetype. `linetype` can take on values
#' as mentioned in the geom_line documentation of \pkg{ggplot2}. Additionally,
#' `linetype` can be set to `"miRNA"`. If `linetype = "miRNA"`, each miRNA name
#' in `mir` has its own linetype.
#' @param alpha Float. Opacity of lines.
#' @param width Float. Width of dodging lines.
#' @param col.mir Symbol. Column containing miRNA names.
#' @param col.year Symbol. Column containing year.
#' @param title String. Plot title.
#'
#' @return Line plot displaying how often a miRNA name was mentioned per year..
#'
#' @family miR development functions
#'
#' @importFrom rlang :=
#'
#' @export
plot_mir_development <- function(df,
                                 mir,
                                 start = NULL,
                                 end = NULL,
                                 linetype = "miRNA",
                                 alpha = 0.8,
                                 width = 0.3,
                                 col.mir = miRNA,
                                 col.year = Year,
                                 title = NULL) {
  # Define start if is.null(start)
  if(is.null(start)) {
    start <- df %>%
      dplyr::select({{col.year}}) %>%
      dplyr::pull() %>%
      min()
  }

  # Define end if is.null(end)
  if(is.null(end)) {
    end <- df %>%
      dplyr::select({{col.year}}) %>%
      dplyr::pull() %>%
      max()
  }

  if(!is.numeric(start) | !is.numeric(end)) {
    stop("'start' and 'end' must be numeric.")
  }

  # Set title
  if(is.null(title)) {
    title <- paste0("Development of ", paste(mir, collapse = ", "), " in PubMed abstracts")
  }


  #Make empty dataframe per miRNA and year = 0
  df_empty <- expand.grid(mir,
                          seq(start, end, by = 1),
                          0,
                          stringsAsFactors = FALSE) %>%
    dplyr::as_tibble() %>%
    dplyr::rename({{col.mir}} := Var1, {{col.year}} := Var2)

  #Select miRNAs of interest
  df_ <- df %>%
    dplyr::select({{col.mir}}, {{col.year}}) %>%
    dplyr::filter({{col.mir}} %in% mir) %>%
    dplyr::add_count({{col.mir}}, {{col.year}}, name = "n")

  #Join dataframes
  df_plot <- dplyr::left_join(df_empty, df_) %>%
    dplyr::mutate(n = ifelse(is.na(n), 0, n))

  #Plot development
  plot <- ggplot(df_plot, aes(x = factor({{col.year}}),
                      y = n,
                      col = {{col.mir}},
                      group = {{col.mir}})) +
    theme_classic() +
    scale_y_continuous(limits = c(0, as.integer(max(df_$n) + 2)),
                       expand = c(0.01,0.01),
                       breaks = function(x)
                         unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))) + #only integers
    xlab("Year") +
    ylab("Mentioned in # of abstracts") +
    labs(color = "microRNA") +
    ggtitle(title)

  # Map linetype

  if(linetype == "miRNA") {
    plot <- plot + geom_line(alpha = alpha,
                             position=position_dodge(width=width),
                             aes(linetype = {{col.mir}})) +
      scale_linetype(guide = FALSE)
  } else {
    plot <- plot + geom_line(alpha = alpha,
                             position=position_dodge(width=width),
                             linetype = linetype)
  }

  return(plot)
}
