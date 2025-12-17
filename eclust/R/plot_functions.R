#' Function to generate heatmap
#'
#' @description Plots a heatmap of a similarity matrix such as a correlation
#'   matrix or a TOM matrix. This function is a plotting method for an object of
#'   class similarity. These objects are returned by the
#'   \code{\link{s_generate_data}} and \code{\link{s_generate_data_mars}}
#'   functions
#' @param x an object of class similarity. This is a p x p symmetric matix such
#'   as a correlation matrix or a TOM matrix, where p is the number of genes
#' @param color colors for the heatmap. By default it uses the \code{viridis}
#'   color scheme. The \code{viridis} package needs to be installed.
#' @param truemodule a numeric vector of length p where p is the number of
#'   genes, giving the module membership. By default, 0 = Grey, 1 = Turquoise, 2
#'   = Blue, 3 = Red, 4 = Green, and 5 = Yellow. This information is used for
#'   annotating the heatmap
#' @param active a binary vector of length p (where p is the number of genes)
#'   where 0 means that gene is not related to the response, and 1 means that
#'   the gene is associated to the response.
#' @param ... other arguments passed to the pheatmap function
#'
#' @note this function is only meant to be used with output from the
#'   \code{\link{s_generate_data}} and \code{\link{s_generate_data_mars}}
#'   functions, since it assumes a fixed number of modules.
#' @return a heatmap of a similarity matrix
#' @examples
#' \dontrun{
#' corrX <- cor(simdata[,c(-1,-2)])
#' class(corrX) <- append(class(corrX), "similarity")
#' plot(corrX, truemodule = c(rep(1:5, each=150), rep(0, 250)))
#' }
#' @export

plot.similarity <- function(x,
                            color = viridis::viridis(100),
                            truemodule,
                            active, ...){

  pacman::p_load(char = "pheatmap")
  pacman::p_load(char = "viridis")

  if (missing(active)) {
    annotation_col <- data.frame(
      module = factor(truemodule,
                      labels = c("Grey","Turquoise","Blue","Red",
                                 "Green","Yellow")))

    rownames(annotation_col) <- dimnames(x)[[2]]
    ann_colors <- list(
      module = c(Turquoise = "turquoise",
                 Blue = "blue",
                 Red = "red",
                 Green = "green",
                 Yellow = "yellow",
                 Grey = "grey90")
    )
  } else {
    annotation_col <- data.frame(
      module = factor(truemodule,
                      labels = c("Grey","Turquoise","Blue","Red",
                                 "Green","Yellow")),
      active = factor(active,
                      levels = c(0,1),
                      labels = c("no","yes")))

    rownames(annotation_col) <- dimnames(x)[[2]]
    ann_colors <- list(
      module = c(Turquoise = "turquoise",
                 Blue = "blue",
                 Red = "red",
                 Green = "green",
                 Yellow = "yellow",
                 Grey = "grey90"),
      active = c(no = "black",
                 yes = "orange")
    )
  }

  pheatmap::pheatmap(x,
           show_rownames = F, show_colnames = F,
           color = color,
           annotation_col = annotation_col,
           annotation_row = annotation_col,
           annotation_colors = ann_colors,
           annotation_names_row = FALSE,
           annotation_names_col = TRUE,
           drop_levels = FALSE,
           annotation_legend = TRUE, ...)

}




#' Plot Heatmap of Cluster Summaries by Exposure Status
#'
#'
#' @description Plots cluster summaries such as the 1st principal component or
#'   average by exposure status. This is a plot method for object of class
#'   eclust returned by the \code{\link{r_cluster_data}} function. Two heatmaps,
#'   side-by-side are returned, where the first heatmap corresponds to the
#'   unexposed subjects and the second heatmap corresponds to the exposed
#'   subjects.
#'
#' @param x object of class \code{eclust}, which is returned by the
#'   \code{\link{r_cluster_data}} function
#' @param type show results from the "ECLUST" (which considers the environment)
#'   or "CLUST" (which ignores the environment) methods. Default is "ECLUST".
#'   See \code{\link{r_cluster_data}} for details. This function uses the
#'   \code{clustersAddon} object for "ECLUST" and the \code{clustersAll} for
#'   "CLUST"
#' @param summary show the 1st principal component or the average of each
#'   cluster. Default is "pc".
#' @param sample which sample to show, the "training" or the "test" set. Default
#'   is "training". This is determined by the \code{train_index} and
#'   \code{test_index} arguments in the \code{\link{r_cluster_data}} function.
#'   If you want to show all subjects, then provide the numeric vector 1:n to
#'   either argument, where n is the entire sample size.
#' @param unexposed_title The title for the unexposed subjects heatmap. Default
#'   is "E=0".
#' @param exposed_title The title for the exposed subjects heatmap. Default is
#'   "E=1".
#' @param ... other arguments passed to the
#'   \code{\link[ComplexHeatmap]{Heatmap}} function
#' @details Rows are the cluster summaries and columns are the subjects. This
#'   function determines the minimum and maximum value for the whole dataset and
#'   then creates a color scale using those values with the
#'   \code{\link[circlize]{colorRamp2}}. This is so that both heatmaps are on
#'   the same color scale, i.e., each color represents the same value in both
#'   heatmaps. This is done for being able to visually compare the results.
#' @return a plot of two Heatmaps, side-by-side, of the cluster summaries by exposure status
#' @export
#'
#' @examples
#' \dontrun{
#' data("tcgaov")
#' tcgaov[1:5,1:6, with = FALSE]
#' Y <- log(tcgaov[["OS"]])
#' E <- tcgaov[["E"]]
#' genes <- as.matrix(tcgaov[,-c("OS","rn","subtype","E","status"),with = FALSE])
#' trainIndex <- drop(caret::createDataPartition(Y, p = 1, list = FALSE, times = 1))
#' testIndex <- setdiff(seq_len(length(Y)),trainIndex)
#'
#' cluster_res <- r_cluster_data(data = genes,
#'                               response = Y,
#'                               exposure = E,
#'                               train_index = trainIndex,
#'                               test_index = testIndex,
#'                               cluster_distance = "tom",
#'                               eclust_distance = "difftom",
#'                               measure_distance = "euclidean",
#'                               clustMethod = "hclust",
#'                               cutMethod = "dynamic",
#'                               method = "average",
#'                               nPC = 1,
#'                               minimum_cluster_size = 60)
#'
#' class(cluster_res)
#'
#' plot(cluster_res, show_column_names = FALSE)
#' }
plot.eclust <- function(x,
                        type = c( "ECLUST","CLUST"),
                        summary = c("pc","avg"),
                        sample = c("training", "test"),
                        unexposed_title = "E=0",
                        exposed_title = "E=1",
                        ...) {

  # x <- cluster_res
  # summary <- "avg"
  # type = "ECLUST"
  # sample = "training"
  # unexposed_title = "E=0"
  # exposed_title = "E=1"

  #============================

  pacman::p_load(char = "ComplexHeatmap")
  pacman::p_load(char = "circlize")

  type <- match.arg(type)
  summary <- match.arg(summary)
  sample <- match.arg(sample)

  plot_data <- switch(type,
                      ECLUST = {
                        x$clustersAddon
                      },
                      CLUST = {
                        x$clustersAll
                      })

  plot_data_2 <- switch(summary,
                        pc = {
                          switch(sample,
                                 training = {
                                   plot_data$PC
                                 },
                                 test = {
                                   plot_data$PCTest
                                 })
                        },
                        avg = {
                          switch(sample,
                                 training = {
                                   plot_data$averageExpr
                                 },
                                 test = {
                                   plot_data$averageExprTest
                                 })
                        })

  max_heat <- max(c(max(plot_data_2[which(x$etrain==0),]),max(plot_data_2[which(x$etrain==1),])))
  min_heat <- min(c(min(plot_data_2[which(x$etrain==0),]),min(plot_data_2[which(x$etrain==1),])))

  cm <- circlize::colorRamp2(seq(min_heat, max_heat, length.out = 100), viridis::viridis(100))
  ht1 = ComplexHeatmap::Heatmap(t(plot_data_2[which(x$etrain==0),]),
                                name = "E=0",
                                col = cm,
                                column_title = unexposed_title, ...)
  ht2 = ComplexHeatmap::Heatmap(t(plot_data_2[which(x$etrain==1),]),
                                name = "E=1",
                                col = cm,
                                column_title = exposed_title, ...)

  ComplexHeatmap::add_heatmap(ht1,ht2)

}
