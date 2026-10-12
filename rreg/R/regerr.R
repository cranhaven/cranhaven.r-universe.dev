##' Plot for variability
##'
##' Create a plot to show uncertainty either by showing the Standard Error of the
##' Mean (SEM) or Confidence Interval (CI). Lower and upper limit should be
##' specified. Figure should also be commented if the variability is a SEM or CI.
##'
##' @inheritParams regbar
##' @param ll Lower limit
##' @param ul Upper limit
##'
##' @import ggplot2
##'
##' @examples
##' # basic usage
##' regerr(hfdata, inst, case2, ll, ul)
##' regerr(hfdata, inst, case2, ll, ul, comp="Sabah")
##'
##' @export

regerr <- function(data, x, y,
                   ll, ul,
                   title, ylab,
                   comp,
                   col1, col2,
                   ascending = TRUE,
                   flip = TRUE,
                   ...){
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
  ## yvar
  data$yvar <- data[, as.character(substitute(y))]


  ## missing ll and ul
  if (missing(ll) | missing(ul)) {
    stop("Both 'll' and 'ul' should be specified",
         call. = FALSE)
  }


  ## x-axis
  data$llvar <- data[, as.character(substitute(ll))]
  ## yvar
  data$ulvar <- data[, as.character(substitute(ul))]


  ## Title
  if (missing(title)){
    title <- ""
  } else {
    title = title
  }

  ## Label y-axis
  if (missing(ylab)){
    ylab <- as.character(substitute(y))
    ## ylab <- paste0("Pls specify eg. ylab = ", "\"", "Confidence Interval", "\"")
    ylab <- sprintf("Pls specify if [%s] is CI or SEM", ylab)
  } else {
    ylab = ylab
  }

  ## Theme
  ptheme <- theme_bw() +
    theme(
      axis.text = element_text(size = 10), #text for y and x axis
      axis.ticks.y = element_blank(),
      axis.line.x = element_line(size = 0.5),
      axis.title.y = element_blank(), #no title in y axis
      axis.title.x = element_text(size = 12),
      plot.margin = unit(c(0, 2, 1,1), 'cm'),
      plot.title = element_text(size = 14),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank())

  ## Colour
  if (missing(col1)) {
    col1 <- "lightblue"
  } else {
    col1 = col1
  }

  if(missing(col2)){
    col2 <- "#6baed6"
    col3 <- c(col1, col2)
  } else {
    col3 <- c(col1, col2)
  }


  ## Ascending order of xvar according to yvar
  if (ascending) {
    data$xvar <- with(data, factor(xvar, levels = xvar[order(yvar)]))
  }


  ## Base plot
  p <- ggplot(data, aes(xvar, yvar)) +
    geom_errorbar(aes(ymax = ulvar, ymin = llvar), width = 0.25, size = 0.4)

  ## Compared or not
  if (missing(comp)) {
    p <- p + geom_label(aes(label = yvar), size = 3,
                        label.padding = unit(0.1, "lines"),
                        label.size = 0, fontface = "bold",
                        fill = col1)
  } else {
    p <- p + geom_label(aes(label = yvar, fill = xvar == comp), size = 3,
                        label.padding = unit(0.1, "lines"),
                        label.size = 0,  fontface = "bold")

  }

  # Plot
  p <- p +
    labs(title = title, y = ylab, x = "") +
    scale_fill_manual(values = col3, guide = 'none') +
    scale_color_manual(values = "black")


  ## Theme for line and grid
  ltheme <- theme(
    panel.grid.major.x = element_line(color = "grey", size = 0.1, linetype = 2),
    ##panel.grid.minor.x = element_line(color = "grey", size = 0.1, linetype = 2),
    panel.grid.major.y = element_line(color = "grey", size = 0.1, linetype = 1),
    axis.line.x = element_line(size = 0.3)
    )

  ## Theme
  p <- p + ptheme + ltheme

  ## Flip plot
  if (flip) {
    p <- p + coord_flip()
  }

  return(p)

}
