##' Dartboard diagram for precision
##'
##' Create a dartboard style diagram to visualise precision. The middle point represent complete precision for example
##' the objectives or plans.  Imagine it's like a dartboard and the center means 100\% precision or it could be completeness/achievement.
##' The standard division of the proportion to show precision allocated in the diagram is 50\%, 80\% and 100\%.
##'
##' @note The \code{ggplot2} package is required to run this function
##' @param data Data set
##' @param x Names of variable
##' @param y Value of the variable
##' @param long Split whitespaces of the variable names
##' @param title Title for the plot
##' @param size Size of the point
##' @param pct1 Percentage first pie proportion
##' @param pct2 Percentage second pie proportion
##' @param col1 Colour of the first pie proportion
##' @param col2 Colour of the second pie proportion
##' @param col3 Colour of the third pie proportion
##' @param ... Additional arguments
##' @details These parameters should be specified:
##' \itemize{
##'  \item \code{x-axis} {1st column: The names of the different institutions}
##'  \item \code{y-axis} {2nd column: The value to show completeness}
##' }
##' @source  \code{hfdata} is a sample data which does not derive from a real data
##'
##' @import ggplot2 grid
##'
##' @examples
##' # basic usage
##' library("rreg")
##' regrad(data = hfdata)
##' regrad(data = hfdata, title = "Plot title", long = TRUE)
##' regrad(hfdata, y= case1, title="Plot title", size=10, col1="blue", col2="green", col3="yellow")
##'
##' @export

regrad <- function(data,
                   x,
                   y,
                   long = FALSE,
                   title,
                   size,
                   pct1,
                   pct2,
                   col1,
                   col2,
                   col3, ...) {

  if (missing(data)) {
    stop("'data' must be provided",
         call. = FALSE)
  }

  ## data frame
  if (!is.data.frame(data)) data <- as.data.frame(data)

  ## x-axis
  if (missing(x)) {
    names(data)[1] <- "x"
  } else {
    x <- as.character(substitute(x))
    data$x <- data[, x]
  }

  ## convert x-axis as character if not character
  if (!is.character(data$x)) {
    data$x <- as.character(data$x)
  }

  if (missing(y)) {
    names(data)[2] <- "y"
  }else{
    y <- as.character(substitute(y))
    data$y <- data[, y]
  }

  ## split whitespace if too long
  if (long) {
    data$x <- as.factor(data$x)
    levels(data$x) <- gsub(" ", "\n", levels(data$x))
  }

  if (missing(title)) {
    title <- " "
  }

  if (missing(size)) {
    size <- 10
  }

  if (missing(pct1)) {
    pct1 <- 50
  }

  if (missing(pct2)) {
    pct2 <- 80
  }

  if (missing(col2)) {
    col2 <- "#2171B5"
  }

  if (missing(col1)) {
    col1 <- "#99CCFF"
  }

  if (missing(col3)) {
    col3 <- "#000033"
  }


  pct2a <- 2 + pct2
  colnr <- dim(data)[1]

  p <- ggplot(data) +
    scale_x_discrete() +
    scale_y_reverse(limit = c(100, 0)) +
    geom_rect(xmin=Inf, xmax = -Inf, ymin = 0 - pct2a, ymax = 0 - pct1, fill=col2) +
    geom_rect(xmin=Inf, xmax = -Inf, ymin = 0 - pct1, ymax = 0, fill=col1) +
    geom_rect(xmin=Inf, xmax = -Inf, ymin = -100, ymax = 0 - pct2, fill=col3) +
    geom_vline(xintercept=1:colnr, size=1.5, color="white") +
    geom_hline(yintercept=c(pct1, pct2, 100), size=0.1, color="white") +
    geom_point(aes(x=x, y=y),
               shape=21, fill="#FF9933", size=size, position="identity") +
    ggtitle(title) +
    coord_polar() +
    theme(
      plot.title=element_text(face = "bold", color = "black", size = 17, ),
      panel.background=element_rect(fill = c("white")),
      panel.grid=element_blank(),
      panel.grid.major=element_line(size=2),
      panel.grid.minor.y=element_blank(),
      axis.text.x=element_text(vjust=5),
      axis.text=element_text(size = 12, color = "black", face = "bold"),
      axis.text.y=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      axis.ticks=element_blank())

  p <- ggplot_gtable(ggplot_build(p))
  p$layout$clip[p$layout$name == "panel"] <- "off"
  grid::grid.draw(p)
}
