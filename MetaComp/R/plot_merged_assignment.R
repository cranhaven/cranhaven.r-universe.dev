#' @importFrom reshape2 melt
#' @importFrom plyr daply
#' @importFrom dplyr desc
#' @importFrom dplyr select
#' @importFrom grDevices dev.off
#' @importFrom Cairo CairoPDF
#' @importFrom Cairo CairoSVG
NULL

#' Generates a single column ggplot for a taxonomic assignment table.
#'
#' This implementation...
#'
#' @param assignment The gottcha-like merged assignment table.
#' @param taxonomy_level The level which need to be plotted.
#' @param sorting_order the order in which rows shall be sorted, "abundance" is defult,
#'                       "alphabetical" is an alternative.
#' @param row_limit the max amount of rows to plot (default is 60).
#' @param min_row_abundance the minimal sum of abundances in a row required to plot.
#'  Rows whose sum is less than this value are dropped even if row_limit is specified.
#'  Ignored for "alphabetical" order. (default 0.0).
#' @param plot_title The plot title.
#' @param filename The output file mask, PDF and SVG files will be produced with Cairo device.
#'
#' @examples
#' \dontrun{
#' hmp_even_fp <- system.file("extdata", "HMP_even", package="MetaComp")
#' hmp_stagger_fp <- system.file("extdata", "HMP_stagger", package="MetaComp")
#' data_files <- data.frame(V1 = c("HMP_even", "HMP_stagger"),
#'                          V2 = c(file.path(hmp_even_fp, "allReads-gottcha2-speDB-b.list.txt"),
#'                                 file.path(hmp_stagger_fp, "allReads-gottcha2-speDB-b.list.txt")))
#' write.table(data_files, file.path(tempdir(), "assignments.txt"),
#'                                                  row.names = FALSE, col.names = FALSE)
#' gottcha2_assignments = merge_edge_assignments(
#'                          load_edge_assignments(
#'                            file.path(tempdir(), "assignments.txt"), type = "gottcha2"))
#' plot_merged_assignment(gottcha2_assignments, "family", 'alphabetical', 100, 0,
#'                                        "HMP side-to-side", file.path(tempdir(), "assignment.pdf"))
#' }
#'
#' @export
plot_merged_assignment <- function(assignment, taxonomy_level, sorting_order = "abundance",
               row_limit = 60, min_row_abundance = 0, plot_title, filename) {

  TAXA <- LEVEL <- SUM <- value <- variable <- NULL # fix the CRAN note

  # filter only the requested level
  df <- dplyr::filter(assignment, LEVEL == taxonomy_level)

  # get rid of the level column
  df <- within(df, rm(LEVEL))

  values_range <- range(df[, 2:length(names(df))])

  # rescale if needed
  #
  if (values_range[2] <= 1) {
    # scale the values
    for (i in c(2:length(names(df)))) {
      df[, i] <- df[, i] * 100
    }
  }

  # compute row sum for each of rows
  sums <- plyr::ddply(df, plyr::.(TAXA), function(x){
                            sum(x[-1]) / (length(x) - 1)
                                                    }
                     )
  names(sums) <- c("TAXA", "SUM")

  df <- base::merge.data.frame(df, sums, by = c("TAXA"))

  # cut the table by the threshold if too long
  if (dim(df)[1] > row_limit) {
    if (sorting_order == "alphabetical") {
      # order rows by the name
      df <- dplyr::arrange(df, TAXA)
      df <- df[1:row_limit, ]
      # inverse the order for plotting
      df <- dplyr::arrange(df, dplyr::desc(TAXA))
    } else {
      # order rows by the sum value
      df <- dplyr::arrange(df, dplyr::desc(SUM))
      df <- df[1:row_limit, ]
      # inverse the order for plotting
      df <- dplyr::arrange(df, SUM)
    }
  } else {
    # sort any way ...
    if (sorting_order == "alphabetical") {
      # order rows by the name
      df <- dplyr::arrange(df, dplyr::desc(TAXA))
    } else {
      # order rows by the sum value
      df <- dplyr::arrange(df, SUM)
      to_keep <- which(df$SUM > min_row_abundance)
      if (length(to_keep) == 0) {
        # keep just one so it doesnt fail
        df <- df[1, ]
      }
      df <- df[to_keep, ]
    }
  }

  df$TAXA <- factor(x = df$TAXA, levels = unique(df$TAXA), ordered = T)

  ## sort project names
  #
  # extract project names
  project_names <- as.character(unlist(colnames(df)[2:(dim(df)[2] - 1)]))
  # order alphabetically
  project_names_order <- order(project_names)
  # create the desired column ordering
  column_index <- c(1, project_names_order + 1, as.numeric(dim(df)[2]) )
  # re-order columns
  df <- dplyr::select(df, column_index )

  x_colnames <- factor(x = colnames(df)[-1], levels = colnames(df)[-1], ordered = T)

  # melt for plotting
  melted_df <- reshape2::melt(within(df, rm(SUM)), id.vars = c("TAXA"))
  melted_df$variable <- factor(x = as.character(melted_df$variable),
                               levels = levels(x_colnames), ordered = T)

  p <- ggplot2::ggplot( data = melted_df, ggplot2::aes(y = TAXA, x = variable, fill = value) ) +
        ggplot2::theme_bw() +
        ggplot2::geom_tile(color = "grey80", size = 0.3) +
        ggplot2::ggtitle(plot_title) +
        ggplot2::scale_x_discrete(expand = c(0, 0), position = "top") +
        ggplot2::scale_y_discrete(expand = c(0, 0)) +
        ggplot2::coord_fixed(ratio = 1) +
        ggplot2::scale_fill_gradientn(name = "Normalized abundance: ",
            limits = c(0.1, 100), trans = "log", colours =
            c("darkblue", "blue", "lightblue", "cyan2", "green",
                 "yellow", "orange", "darkorange1", "red"),
                  breaks = c(0.1, 1, 10, 100),
                  # nolint start
                  labels = expression(10^-1, 10^0, 10^1, 10^2),
                  # nolint end
                  guide = ggplot2::guide_colorbar(title.theme =
                      ggplot2::element_text(size = 12, angle = 0),
                      title.vjust = 0.9, barheight = 0.6, barwidth = 6,
                      label.theme = ggplot2::element_text(size = 9, angle = 0),
                      label.hjust = 0.2)) +
       ggplot2::theme(plot.title = ggplot2::element_text(size = 14, hjust = 1),
                   axis.title.x = ggplot2::element_text(size = 0),
                   axis.title.y = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(size = 10, angle = 55,
                                                       hjust = 0, vjust = 1),
                   axis.ticks.y = ggplot2::element_blank(),
                   axis.text.y = ggplot2::element_text(size = 10),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank(),
                   #legend.position = c(0, 0),
                   #legend.justification = c(0, 0.3),
                   #plot.margin=grid::unit(c(0.1,0.1,3,0.1), 'lines'),
                   legend.direction = "horizontal", legend.position = "bottom")

  # align the TITLE to the left workaround
  #
  #title.grob <- grid::textGrob(
  #    label = plot_title, x = grid::unit(0, "lines"), y = grid::unit(0, "lines"),
  #    hjust = -1, vjust = 0, gp = grid::gpar(fontsize = 16))
  #p1 <- suppressWarnings(gridExtra::arrangeGrob(p, top = title.grob))

  Cairo::CairoPDF(file = filename, width = 0.3 * length(df[1, ]) + 6,
                  height = 0.15 * length(df$TAXA) + 5,
                  onefile = TRUE, family = "Helvetica",
                  title = "R Graphics Output", version = "1.1",
                  paper = "special", bg = "white", pointsize = 10)
  #suppressWarnings(grid::grid.draw(p1))
  suppressWarnings(print(p))
  dev.off()

  Cairo::CairoSVG(file = filename, width = 0.3 * length(df[1, ]) + 6,
                  height = 0.15 * length(df$TAXA) + 5,
                  onefile = TRUE, family = "Helvetica",
                  title = "R Graphics Output", version = "1.1",
                  paper = "special", bg = "white", pointsize = 10)
  #suppressWarnings(grid::grid.draw(p1))
  suppressWarnings(print(p))
  dev.off()

}
