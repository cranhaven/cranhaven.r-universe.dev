#' Plot similarity measures between two consecutive biclusterings.
#'
#' Creates a ggplot of the three similarity measures used in \code{biclustermd::bicluster()}
#'     for both row and column dimensions.
#'
#' @param object Object of class "biclustermd_sim"
#' @param similarity A character vector indicating which similarity measure to plot.
#'     Can be any of `"Rand"`, `"HA"`, `"Jaccard"`, or `"used"`. If `"used"`,
#'     plot only the measure used as the stopping condition in the algorithm).
#'     By default (`NULL`) all three are plotted. When plotted, the used measure
#'     will have an asterisk.
#' @param facet If \code{TRUE} (default), each similarity measure will be in its own plot.
#'     if \code{FALSE}, all three similarity measures for rows and columns are
#'     given in one plot.
#' @param ncol If faceting, the number of columns to arrange the plots in.
#' @param ... Arguments to pass to \code{ggplot2::geom_point()}
#'
#' @export
#' @importFrom ggplot2 aes autoplot facet_wrap geom_line geom_point ggplot ylim
#' @importFrom tidyr gather separate
#' @return A ggplot object.
#' @examples
#' data("synthetic")
#'
#' bc <- biclustermd(synthetic, col_clusters = 3, row_clusters = 2,
#'                 miss_val = mean(synthetic, na.rm = TRUE),
#'                 miss_val_sd = sd(synthetic, na.rm = TRUE),
#'                 col_min_num = 2, row_min_num = 2,
#'                 col_num_to_move = 1, row_num_to_move = 1,
#'                 max.iter = 10)
#' bc
#' autoplot(bc$Similarities, ncol = 1)

autoplot.biclustermd_sim <- function(object, similarity = NULL, facet = TRUE, ncol = NULL, ...) {

  names(object) <- c("Column_Rand", "Column_HA", "Column_Jaccard", "Row_Rand", "Row_HA", "Row_Jaccard", "Iteration")
  dat <- gather(object, metric, value, -Iteration)

  if(is.null(similarity)) {
    similarity <- c("Rand", "HA", "Jaccard")
  } else if("used" %in% similarity) {
    similarity[similarity == "used"] <- attr(object, "used")
    similarity <- unique(similarity)
  }

  dat <- separate(dat, metric, into = c("Dimension", "Metric"))
  dat <- dat[dat$Metric %in% similarity,]
  dat$Metric[dat$Metric == attr(object, "used")] <- paste0(dat$Metric[dat$Metric == attr(object, "used")], '*')
  dat$Metric[dat$Metric == 'HA*'] <- "Adjusted Rand (HA)*"
  dat$Metric[dat$Metric == 'HA'] <- "Adjusted Rand (HA)"

  if(facet) {
    if(is.null(ncol)) {
      ncol <- 2
    }

    p <- ggplot(dat, aes(Iteration, value, color = Dimension)) +
      geom_line() +
      geom_point(...) +
      ylim(min(c(0, min(object[, -7]))), 1) +
      facet_wrap(~ Metric, ncol = ncol)

  } else if(!facet) {

    p <- ggplot(dat, aes(Iteration, value, color = Dimension)) +
      geom_line(aes(linetype = Metric)) +
      geom_point(...) +
      ylim(min(c(0, min(object[, -7]))), 1)

  }
  p
}
