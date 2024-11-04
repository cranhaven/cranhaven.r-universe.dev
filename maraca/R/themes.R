# Different themes that can be used to style the plots of the package

.theme_common <- function(p) {

  p <- p +
    ggplot2::xlab("Outcomes") +
    ggplot2::ylab("Cumulative percentage") +
    ggplot2::theme(
      axis.text.x.bottom = ggplot2::element_text(
        angle = 90,
        vjust = 0.5,
        hjust = 1
      )
    ) +
    ggplot2::guides(fill = "none")

  return(p)

}

.theme_common_cp <- function(p) {

  n <- length(levels(p$data$GROUP))
  p <- p +
    ggplot2::geom_vline(xintercept = seq(0.5, n + 1.5, 1),
                        linetype = 2, linewidth = 0.3, color = "darkgray") +
    # Axis showing percentages
    ggplot2::scale_y_continuous(labels = function(x) paste0(round(x, 2), "%"),
                                expand = ggplot2::expansion(mult = c(0, .3))) +
    ggplot2::ylab("Percent of all comparisons") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "bottom",
                   legend.title = ggplot2::element_blank(),
                   axis.title.y = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   panel.grid.minor.y = ggplot2::element_blank())

  return(p)

}

.theme_maraca_cp <- function(p) {

  p <- .theme_common_cp(p)

  return(p)

}


.theme_maraca <- function(p) {

  p <- p +
    ggplot2::theme_bw()

  p <- .theme_common(p)

  return(p)

}

.theme_maraca_old <- function(p) {

  p <- .theme_common(p)

  p <- p +
    ggplot2::scale_color_discrete("Arm", labels = levels(p$data$arm)) +
    ggplot2::theme(
      axis.title.x.bottom =  ggplot2::element_blank()
    )

  return(p)

}

.theme_color1_cp <- function(p) {

  p <- .theme_common_cp(p)
  p <- p +
    ggplot2::scale_fill_manual(values = c("#830051", "#F0AB00",
                                          "#d3d3d3"), name = NULL)

  return(p)

}

.theme_color1 <- function(p) {

  p <- p +
    ggplot2::theme_bw()

  p <- .theme_common(p)

  colScheme <- c("#830051", "#F0AB00")
  names(colScheme) <- levels(p$data$arm)

  p <- p +
    ggplot2::scale_color_manual(name = "Arm", values = colScheme) +
    ggplot2::scale_fill_manual(values = colScheme)

  return(p)
}

.theme_color2_cp <- function(p) {

  p <- .theme_common_cp(p)
  p <- p +
    ggplot2::scale_fill_manual(values = c("#35B779FF", "#31688EFF",
                                          "#d3d3d3"), name = NULL)

  return(p)

}

.theme_color2 <- function(p) {

  p <- p +
    ggplot2::theme_bw()

  p <- .theme_common(p)

  colScheme <- c("#35B779FF", "#31688EFF")
  names(colScheme) <- levels(p$data$arm)

  p <- p +
    ggplot2::scale_color_manual(name = "Arm", values = colScheme) +
    ggplot2::scale_fill_manual(values = colScheme)

  return(p)
}
