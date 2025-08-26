#' Plot sums of squared errors (SSEs) consecutive biclustering iterations.
#'
#' Creates a ggplot of the decrease in SSE recorded in \code{biclustermd::bicluster()}.
#'
#' @param object Object of class "biclustermd_sse" with columns "Iteration" and "SSE"
#' @param ... Arguments to pass to \code{ggplot2::geom_point()}
#' 
#' @export
#' @importFrom ggplot2 aes autoplot geom_line geom_point ggplot
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
#' autoplot(bc$SSE)

autoplot.biclustermd_sse <- function(object, ...) {
  
  ggplot(data.frame(object), aes(Iteration, SSE)) +
    geom_line() +
    geom_point(...)
  
}