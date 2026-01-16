#' Custom ggplot2 theme for publication-style figures
#' 
#' @param base_size Base font size.
#' @param base_family Base font family.
#' @param aspect.ratio Aspect ratio of the plot.
#'
#' @return A ggplot2 theme object.
#' @export

theme_Publication <- function(base_size=24, base_family="Arial",aspect.ratio = 0.75) {
  (ggthemes::theme_foundation(base_size=base_size, base_family=base_family)
   + theme(plot.title = element_text(face = "bold",
                                     size = rel(1.2), hjust = 0.5),
           text = element_text(),
           panel.background = element_rect(colour = NA, fill="#FCFCFC"),
           plot.background = element_rect(colour = NA),
           panel.border = element_rect(fill = NA, colour = "black", size=0.5),
           axis.title = element_text(size = rel(1)),
           axis.title.y = element_text(angle=90,vjust =2),
           axis.title.x = element_text(vjust = -0.2),
           axis.text = element_text(),
           axis.line = element_line(colour="black"),
           axis.ticks = element_line(),
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           plot.margin=grid::unit(c(10,5,5,5),"mm"),
           aspect.ratio=aspect.ratio
   ))
}

#' Custom ggplot2 theme with no y-axis title
#' @inheritParams theme_Publication
#'
#' @return A ggplot2 theme object.
#' @export

theme_Publication_noYaxis <- function(base_size=24, base_family="Arial",aspect.ratio = 0.75) {

  (ggthemes::theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA, fill="#FCFCFC"),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(fill = NA, colour = "black", size=1.5),
            axis.title = element_text(size = rel(1)),
            axis.title.x = element_text(vjust = -0.2),
            axis.title.y = element_blank(),
            axis.text = element_text(),
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin=grid::unit(c(10,5,5,5),"mm"),
            aspect.ratio=aspect.ratio
    ))
}

#' Theme for matrix-style plots (e.g. heatmaps or abundance matrices)
#' @param base_size Base font size.
#' @param base_family Base font family.
#'
#' @return A ggplot2 theme object.
#' @export

theme_Matrix <- function(base_size=24, base_family="Arial") {
  (ggthemes::theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(size = rel(1.2), hjust = 0.5),
            text = element_text(),
            panel.background = element_rect(colour = NA, fill="#FCFCFC"),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(fill = NA, colour = "black", size=1.5),
            axis.title = element_text(size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text.x = element_text(angle = 45,hjust=1),
            axis.text = element_text(),
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            legend.title = element_text(angle = -90),
            legend.title.align = 0.5,
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            plot.margin=grid::unit(c(10,5,5,5),"mm")
    ))
}

#' Global variables


utils::globalVariables(c(
  "ID", "pipeline_choice","mean_freq",
  "cumulative_freq", "value", "cluster", "time", "frequency", "Generations",
  "q_value", "q_type", "hex", ".", "iso1", "iso2",
  "dist", "dist_small", "cutoff", "Frequency", "average", "Time"
))

cluster.colors=c("#3cb44b","#4363d8","#e6194B","#e8ca00","#911eb4","#f58231","#22766dff","#42d4f4","#f032e6","#9A6324",
                 "#2F4C39", "#1D3F6E","#94170f","#665679","#F17829","#97A69C","#606EA9","#A9606E","#A99060","#F8F1AE",
                 "#bcf60c", "#fabebe", "#008080", "#e6beff", "#9a6324", "#fffac8")
