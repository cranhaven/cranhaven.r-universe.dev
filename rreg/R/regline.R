##' Line plot to show trend
##'
##' Create a line plot that can be used to elucidate if trends exit over time.
##'
##' @inheritParams regbar
##' @param grp Group variable
##' @param ylab Label for y-axis
##' @param colp Color palettes to use from ColorBrewer. To check other palettes run
##'   library(RColorBrewer); display.brewer.all()
##' @param digit Number of digit to show
##'
##' @import ggplot2 directlabels
##'
##' @examples
##' regline(data = yrdata, x=year, y=pros, grp=var)
##' regline(yrdata, year, pros, var, colp="Set1", digit=1)
##'
##' @export


regline <- function(data, x, y,
                    grp, title, ylab,
                    colp, digit,
                    ...) {


  ## missing data
  if (missing(data)) {
    stop("'data' must be provided",
         call. = FALSE)
  }

  ## missing x or y
  if (missing(x) | missing(y)) {
    stop("Both 'x' and 'y' should be specified",
         call. = FALSE)
  }


  ## x-axis
  data$xvar <- data[, as.character(substitute(x))]
  data$xvar <- as.factor(data$xvar)
  ## yvar
  data$yvar <- data[, as.character(substitute(y))]

  ## group
  ## missing data
  if (missing(grp)) {
    stop("'grp' must be specified to draw lines for categories",
         call. = FALSE)
  }

  data$grp <- data[, as.character(substitute(grp))]
  data$grp <- as.factor(data$grp)

  ## Title
  if (missing(title)){
    title <- ""
  } else {
    title = title
  }

  ## y-label
  if (missing(ylab)){
    ylab <- substitute(y)
  } else {
    ylab = ylab
  }

  ## Colour
  if(missing(colp)){
    colp <- "Blues"
  } else {
    colp = colp
  }

  ## Number of digits to show
  if(missing(digit)) {
    data$yvar <- round(data$yvar, digits = 0)
  } else {
    data$yvar <- round(data$yvar, digits = digit)
  }

  ## Theme
  ptheme <- theme_bw() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 10, color = "black"),
      plot.margin = unit(c(0,2,0.5,0.5), "cm"),
      axis.text.y = element_text(size = 9, color = "black"),
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(size = 0.5),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.title.y = element_text(size = 9),
      axis.title.x = element_text(size = 9)
    )


  p <- ggplot(data, aes(xvar, yvar, group = grp)) +
    geom_line(aes(color = grp), size = 0.5) +
    geom_point(aes(shape = grp, color = grp), stat = "identity") +
    geom_text(aes(label = yvar), vjust = -0.6, color = "grey50", size = 2.8) +
    directlabels::geom_dl(aes(label = grp),
                          method = list(dl.trans(x = x + 0.4),
                                        "last.points", cex = 0.7)) +
    scale_x_discrete(expand = c(0, 1.5)) +
    scale_color_brewer(palette = colp) +
    labs(title = title, y = ylab, x = "") +
    ptheme


  p <- ggplot_gtable(ggplot_build(p))
  p$layout$clip[p$layout$name == "panel"] <- "off"
  grid.draw(p)

  }
