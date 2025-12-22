#' @importFrom dplyr filter select
#' @importFrom Cairo CairoPDF
#' @importFrom grDevices dev.off
NULL

#' Generates a single column ggplot for a taxonomic assignment table and also outputs a PDF.
#'
#' This implementation is built upon ggplot geom_tile.
#'
#' @param assignment The EDGE-like assignment table.
#' @param level The taxonomic level to plot (i.e., family, strain, etc...).
#' @param plot_title The plot title, e.g., "Project XX, Run YY".
#' @param column_title The column title.
#' @param filename The PDF file name mask.
#'
#' @return the ggplot2 plot.
#'
#' @examples
#' pa_fpath <- system.file("extdata", "HMP_even//allReads-pangia.list.txt", package="MetaComp")
#' pangia_assignment = load_edge_assignment(pa_fpath, type = "pangia")
#'
#' plot_edge_assignment(pangia_assignment, "phylum", "Pangia", "HMP Even",
#'                                                      file.path(tempdir(), "assignment.pdf"))
#'
#' @export
plot_edge_assignment <- function(assignment, level, plot_title, column_title, filename) {

  LEVEL <- TAXA <- ABUNDANCE <- tool <- NULL # fix the CRAN note

  # subset rows
  sub_table <- dplyr::filter(assignment, LEVEL == level)

  # subset columns
  vals <- dplyr::select(sub_table, TAXA, ABUNDANCE)

  # sort rows by the abundance
  vals <- dplyr::arrange(vals, ABUNDANCE)

  # rescale values
  vals$ABUNDANCE <- vals$ABUNDANCE

  # assign factor to TAXA
  vals$TAXA <- factor(x = vals$TAXA, levels = vals$TAXA, ordered = T)

  # assign the column title
  vals$tool <- column_title

     p <- ggplot2::ggplot( data = vals, ggplot2::aes(y = TAXA, x = tool, fill = ABUNDANCE) ) +
       ggplot2::theme_bw() +
       ggplot2::geom_tile(color = "grey", size = 0.3) +
       ggplot2::ggtitle(plot_title) +
       ggplot2::scale_x_discrete(expand = c(0, 0)) +
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
       ggplot2::theme(legend.position = "bottom", plot.title = ggplot2::element_text(size = 14),
            axis.title.x = ggplot2::element_blank(), axis.title.y = ggplot2::element_blank(),
            axis.text.x = ggplot2::element_text(size = 10),
            axis.ticks.y = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_text(size = 10),
            panel.grid.major.y = ggplot2::element_blank(),
            panel.grid.minor.y = ggplot2::element_blank())


     Cairo::CairoPDF(file = filename, width = 7, height = 0.15 * length(vals$TAXA) + 5,
                     onefile = TRUE, family = "Helvetica",
              title = "R Graphics Output", version = "1.1",
              paper = "special", bg = "white", pointsize = 10)
     print(p)
     dev.off()

     p
}
