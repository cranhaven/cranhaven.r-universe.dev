#' @title ggBiplot
#' @description Plot a biplot of a Principal Components Analysis using ggplot2.
#' 
#' @param pca result from a call to \code{\link{princomp}}.
#' @param x,y the number or column names of the components to plot.
#' @param mult.fac multiplier factor for lengths of arrows from \code{0:1}.
#' @param arrow.size thickness of arrow lines.
#' @param label.size size of labels.
#' 
#' @return the ggplot2 object is invisibly returned.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' pc.cr <- princomp(USArrests, cor = TRUE)
#' ggBiplot(pc.cr)
#' 
#' @export
#' 
ggBiplot <- function(pca, x = 1, y = 2, mult.fac = 0.8, arrow.size = 1.5, label.size = 6) {
  if(is.numeric(x)) x <- colnames(pca$scores)[x]
  if(is.numeric(y)) y <- colnames(pca$scores)[y]
  
  scores <- as.data.frame(pca$scores) |>  
    dplyr::select(x, y) |> 
    stats::setNames(c("x", "y"))
  
  mult <- min(
    (max(scores[, 1]) - min(scores[, 1]) / diff(range(pca$loadings[, x]))),
    (max(scores[, 2]) - min(scores[, 2]) / diff(range(pca$loadings[, y])))
  ) * mult.fac
  
  ldngs <- data.frame(cbind(pca$loadings))[, c(x, y)] |> 
    stats::setNames(c("x", "y")) |> 
    tibble::rownames_to_column("id") |> 
    dplyr::select(.data$id, .data$x, .data$y) |> 
    dplyr::mutate(
      x = .data$x * mult,
      y = .data$y * mult,
      origin = 0
    ) 
  
  g <- ggplot2::ggplot(scores, ggplot2::aes_string("x", "y")) + 
    ggplot2::geom_hline(yintercept = 0) + 
    ggplot2::geom_vline(xintercept = 0) +
    ggplot2::geom_point(color = "grey", alpha = 0.7) + 
    ggplot2::geom_segment(
      ggplot2::aes_string(x = "origin", xend = "x", y = "origin", yend = "y"), 
      data = ldngs, 
      color = "red", 
      size = arrow.size, 
      arrow = ggplot2::arrow()
    ) +
    ggrepel::geom_label_repel(
      ggplot2::aes_string(x = "x", y = "y", label = "id"), 
      data = ldngs, 
      color = "red", 
      size = label.size
    ) +
    ggplot2::labs(x = x, y = y)
  print(g)
  
  invisible(g)
}