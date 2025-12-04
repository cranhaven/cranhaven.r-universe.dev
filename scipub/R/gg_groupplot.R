#' Create ggplot to display group differences (box+point+hist)
#'
#' The `gg_groupplot` function can be used to create group
#' difference plots for scientific publication.
#' This is intended to summarize a continuous outcome (`y`)
#' based on a factor ('x') from an input dataset (`data`).
#' The plot will include standard ggplot2::geom_boxplot
#' indicating 25th, median, and 75th percentile for the box
#' and 1.5 * IQR for the whiskers. Outliers are not
#' highlighted.
#' Raw data is displayed with standard ggplot2::geom_point
#' and lateral but not vertical jittering.
#' Histograms are shown with gghalves::geom_half_violin
#' to the right of each boxplot.
#' If meanline = = TRUE (default), gray dots will indicate
#' the mean for each variable (vs. median in boxplot)
#' connected by a gray line.
#' This function will drop any NA values.
#' Requires `ggplot2` and `gghalves` libraries.
#' @param data The input dataset.
#' @param x The grouping factor, e.g. Sex
#' @param y The numeric outcome variable, e.g. Age
#' @param meanline Optional indicator of means
#' @return Output group plot
#' @import 	ggplot2
#' @import gghalves
#' @export
#' @examples
#' gg_groupplot(data = psydat, x = Sex, y = depressT, meanline = TRUE)
#'

gg_groupplot <- function(data, x , y ,
            meanline = c(TRUE, FALSE)) {


 if (missing(x)) {
  stop("please declare x variable", call. = FALSE)
 }
 if (missing(y)) {
  stop("please declare y variable", call. = FALSE)
 }
  x <- deparse(substitute(x))
  y <- deparse(substitute(y))

  data <- as.data.frame(data)

 data[, x] <- as.factor(data[, x])
 data[, y] <- as.numeric(as.character((data[, y])))

 g <- ggplot(data = data[!is.na(data[, x]) & !is.na(data[, y]), ],
       aes(x = get(x), y = get(y),
           color = get(x), fill = get(x), shape = get(x))) +
   gghalves::geom_half_violin(position = position_nudge(x = .3, y = 0),
           alpha = .8, width = .5, side = "r", color = NA) +
  geom_point(position = position_jitterdodge(jitter.width = .5),
             alpha = .8, size = 1.5) +
  geom_boxplot(outlier.alpha = 0, width = .5, fill = NA, color = "black") +
  xlab("") + ylab("") +
  theme_bw(base_size = 12) +
  theme(legend.position = "none",
     panel.grid.minor = element_line(linetype = "dashed", size = .5),
     axis.title.x = element_text(face = "bold"),
     axis.title.y = element_text(face = "bold")) +
  scale_shape(solid = FALSE)

 if (meanline[1]) {
 g <- g + stat_summary(fun = mean, geom = "line",
                       color = "darkgray", size = 1, aes(group = 1)) +
  stat_summary(fun = mean, geom = "point",
               color = "darkgray", size = 2, shape = 16, aes(group = 1))
 }

return(g)
}
