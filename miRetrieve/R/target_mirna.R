#' Count targets in data frame
#'
#' Count occurrence of targets in  a data frame.
#'
#' Count occurrence of targets in  a data frame. The count of targets
#' can either be returned as a separate data frame, only listing the targets
#' and their respective frequency, or it can be added to the
#' data frame provided as an extra column.
#'
#' @param df Data frame containing a column with targets.
#' @param col.target Symbol. Column containing targets.
#' @param add.df Boolean. If `add.df = TRUE`, adds column `Target_count` to `df`
#' containing the count of targets. If `add.df = FALSE`, returns a new data frame
#' with the count of targets.
#'
#' @return Data frame, either with the targets and their frequency as a new
#' data frame,
#' or with the frequency of targets added as a
#' new column to the input data frame `df`.
#'
#' @seealso [join_targets()], [plot_target_count()]
#'
#' @family target functions
#'
#' @importFrom dplyr desc
#'
#' @export
count_target <- function(df,
                         col.target = Target,
                         add.df = TRUE) {

  df_count <- df %>%
    dplyr::add_count({{col.target}}) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::rename("Target_count" = n) %>%
    dplyr::select({{col.target}}, Target_count) %>%
    dplyr::distinct()
  if (add.df == FALSE) {
    return(df_count)
  } else {
    df_merged <- dplyr::left_join(df, df_count)
    return(df_merged)
  }
}

#' Provide an overview of targets - helper
#'
#' Helper function. Provide an overview of targets.
#'
#' Returns a data frame providing an overview of the target count. Unlike
#' `count_target()`, it has no option of `add.df`. Furthermore, `threshold`
#' and `top` can be defined, which comes in handy with the following
#' functions.
#'
#' @param df Data frame with a column containing miRNA targets.
#' @param threshold Numeric. Either threshold or top must be given.
#' Must be >= 0. Specifies how often a target must be provided in col.target.
#' @param top Numeric. Optional. Either threshold or top must be given.
#' Must be >= 0. Specifies number of top targets to be displayed.
#' @param col.target Symbol. Column with miRNA targets.
#'
#' @return Data frame with miRNA targets counted.
#'
#' @importFrom dplyr desc
#'
#' @noRd
count_target_helper <- function(df,
                         threshold = NULL,
                         top = NULL,
                         col.target = Target) {

  if(is.null(top) & is.null(threshold)) {
    top <- 5
  }

  if(!is.null(top) & !is.null(threshold)) {
    stop("'top' and 'threshold' cannot be specified at the same time.
         Please specify either 'top' or 'threshold'.")
  }


  df_count <- df %>%
    dplyr::count({{col.target}}, name = "Target_count") %>%
    dplyr::arrange(desc(Target_count))

  if(!is.null(threshold)) {
    df_count <- df_count %>%
      dplyr::filter(Target_count >= threshold)
  } else if(!is.null(top)) {
    df_count <- df_count %>%
      dplyr::top_n(n = top, wt = Target_count)
  }

  return(df_count)
}

#' Plot count of miRNA targets
#'
#' Plot count of miRNA targets.
#'
#' Plot count of miRNA targets as a bar plot. How many
#' targets are plotted is determined either by the `top` or by
#' the `threshold` argument.
#' If `top` is given, targets with the highest count are plotted.
#' Ties among targets with the highest count are treated as
#' the same rank, e.g. if *PTEN*, *AKT*, and *VEGFA* all had the highest count,
#' they would all be plotted by specifying `top = 1`, `top = 2`,
#' and `top = 3`.
#' If `threshold` is given, only targets with a count of at least `threshold`
#' are plotted.
#' If neither `top` nor `threshold` is given, `top` is automatically set
#' to `5`.
#'
#' @param df Data frame with miRNA targets.
#' @param top Numeric. Specifies number of top targets to be plotted.
#' @param threshold Numeric. Specifies how often a target must be in `col.target`
#' to be plotted.
#' @param colour String. Colour of bar plot.
#' @param col.target Symbol. Column containing miRNA targets.
#' @param title String. Plot title.
#'
#' @return Bar plot with target counts.
#'
#' @seealso [count_target()], [join_targets()]
#'
#' @family target functions
#'
#' @export
plot_target_count <- function(df,
                              top = NULL,
                              threshold = NULL,
                              colour = "steelblue3",
                              col.target = Target,
                              title = NULL) {
  if(is.null(top) & is.null(threshold)) {
    top <- 5
  }

  if(!is.null(top) & !is.null(threshold)) {
    stop("'top' and 'threshold' cannot be specified at the same time.
         Please specify either 'top' or 'threshold'.")
  }

  # Set title
  if(is.null(title)) {
    title <- "Top validated targets"
  }

  df_count <- count_target_helper(df,
                               top = top,
                               threshold = threshold,
                               col.target = {{col.target}}) %>%
    dplyr::mutate(Target = forcats::fct_reorder(Target, Target_count))

  plot <- ggplot(df_count, aes(x = Target,
                               y = Target_count)) +
    geom_col(show.legend = FALSE,
             fill = colour) +
    coord_flip() +
    xlab("Target") +
    ylab("Validated in at least # studies") +
    theme_classic() +
    ggtitle(title)

  plot <- pretty_breaks_miretrieve(plot, df_count$Target_count)

  return(plot)

}

#' Plot targets and corresponding miRNAs as a scatter plot
#'
#' Plot targets and corresponding miRNAs as a scatter plot.
#'
#' Plot targets and corresponding miRNAs as a scatter plot.
#' With `filter_for`, it can be determined if the focus shall be
#' on the top targets to plot their corresponding miRNAs,
#' or if the focus
#' shall be on the top miRNA names to plot their corresponding targets.
#' What "top targets" or "top miRNA names" mean can be determined via the
#' `top` and `threshold` arguments.
#' * If `top` is given, `df` is filtered for the most frequent targets/miRNA
#' names.
#' * If `threshold` is given, data frame is filtered for all targets/miRNA names
#' mentioned at least `threshold` times.
#' * If neither `top` nor `threshold` is given, `top` is automatically set
#' to `5`.
#'
#' By plotting miRNAs
#' against their targets, it is visualized if one miRNA regulates many targets,
#' or if one target is regulated by many miRNAs. Furthermore, the miRNA-target
#' interactions are labelled according to their topic in `col.topic`, thereby
#' facilitating comparison of miRNA-target interactions across different topics.
#'
#' @param df Data frame containing targets and miRNA names.
#' @param mir String or character vector. Specifies which miRNAs to plot.
#' @param target String or character vector. Specifies which targets to plot.
#' @param top Numeric. Specifies number of top targets/miRNA names to be plotted.
#' @param threshold Numeric. Specifies how often a target/miRNA name must be in
#' `df` to be plotted.
#' @param filter_for String. Must either be `"target"` or `"miRNA"`. Specifies if
#' `threshold`/`top` shall be applied to targets or miRNA names.
#' @param col.target Symbol. Column containing miRNA targets.
#' @param col.mir Symbol. Column containing miRNA names.
#' @param col.topic Symbol. Column containing topic names.
#' @param col.pmid Symbol. Column containing PubMed-IDs.
#' @param title String. Plot title.
#' @param height Double. Specifies height of jitter.
#' @param width Double. Specifies width of jitter.
#' @param alpha Double. Specifies opacity of points.
#'
#' @return Scatter plot with targets and corresponding miRNAs.
#'
#' @seealso [join_targets()]
#'
#' @family target functions
#'
#' @importFrom rlang :=
#'
#' @export
plot_target_mir_scatter <- function(df,
                                    mir = NULL,
                                    target = NULL,
                                    top = NULL,
                                    threshold = NULL,
                                    filter_for = "target",
                                    col.target = Target,
                                    col.mir = miRNA,
                                    col.topic = Topic,
                                    col.pmid = PMID,
                                    title = NULL,
                                    height = 0.05,
                                    width = 0.05,
                                    alpha = 0.6) {

  filter_for <- stringr::str_to_lower(filter_for)

  if(is.null(title)) {
    title <- "miRNA-target interaction"
  }


  if(is.null(top) & is.null(threshold)) {
    top <- 5
  }

  if(!is.null(top) & !is.null(threshold)) {
    stop("'top' and 'threshold' cannot be specified at the same time.
         Please specify either 'top' or 'threshold'.")
  }

  if(filter_for != "target" & filter_for != "mirna") {
    stop("'filter_for' must be either 'Target'/'target' or 'miRNA'.")
  }

  if(!"Topic" %in% colnames(df)) {
    df <- df %>%
      dplyr::mutate(Topic = "NA")
  }

  # Make all targets uppercase
  df <- df %>%
    dplyr::mutate({{col.target}}:= stringr::str_to_upper({{col.target}}))

  # Filter for mir
  if(!is.null(mir)) {
    df <- df %>%
      dplyr::filter({{col.mir}} %in% mir)
  }



  # Filter for target
  if(!is.null(target)) {
    df <- df %>%
      dplyr::filter({{col.target}} %in% target)
  }

  if(filter_for == "mirna") {
    df <- df %>%
      tidyr::drop_na({{col.mir}})
  } else {
    df <- df %>%
      tidyr::drop_na({{col.target}})
  }


  df_count <- df %>%
    dplyr::select({{col.target}}, {{col.mir}}, {{col.topic}}, {{col.pmid}}) %>%
    dplyr::add_count({{col.target}}, name = "Target_count") %>%
    dplyr::add_count({{col.mir}}, name = "miRNA_count") %>%
    dplyr::mutate(miRNA = forcats::fct_reorder({{col.mir}}, miRNA_count)) %>%
    dplyr::mutate(Target = forcats::fct_reorder({{col.target}}, Target_count))



  # Specifies if count is based on targets or miRNAs.
  if(filter_for == "target") {
    df_count <- df_count %>%
      dplyr::select(Target, miRNA, Target_count, {{col.topic}}, {{col.pmid}}) %>%
      dplyr::rename(n = 3)
  } else if(filter_for == "mirna") {
    df_count <- df_count %>%
      dplyr::select(Target, miRNA, miRNA_count, {{col.topic}}, {{col.pmid}}) %>%
      dplyr::rename(n = 3)
  }

  # Specifies if count is filtered for threshold or targets.
  if(!is.null(threshold)) {
    df_count <- df_count %>%
      dplyr::filter(n >= threshold)
  } else if(!is.null(top)) {
    if(filter_for == "mirna") {
      vector <- df_count %>%
        dplyr::select(-Target, -{{col.pmid}}, -{{col.topic}}) %>%
        dplyr::distinct() %>%
        dplyr::top_n(n = top, wt = n) %>%
        dplyr::select(miRNA) %>%
        dplyr::pull() %>%
        unique()

      df_count <- df_count %>%
        dplyr::filter(miRNA %in% vector)

    } else {
      vector <- df_count %>%
        dplyr::select(-miRNA, -{{col.pmid}}, -{{col.topic}}) %>%
        dplyr::distinct() %>%
        dplyr::top_n(n = top, wt = n) %>%
        dplyr::select(Target) %>%
        dplyr::pull() %>%
        unique()

      df_count <- df_count %>%
        dplyr::filter(Target %in% vector)
    }
  }

  df_count <- df_count %>%
    dplyr::distinct({{col.pmid}}, miRNA, Target, {{col.topic}})


  # Scatterplot.
  plot <- ggplot(df_count, aes(x = Target,
                               y = miRNA,
                               color = {{col.topic}})) +
    geom_jitter(height = height,
                width = width,
                alpha = alpha,
                size = 2,
                show.legend = TRUE) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60,
                                     hjust = 1)) +
    xlab("Target") +
    ylab("miRNA") +
    ggtitle(title)

  return(plot)
}
