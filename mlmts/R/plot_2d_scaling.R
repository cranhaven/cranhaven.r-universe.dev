

#' Constructs a 2-dimensional scaling plot based on a given dissimilarity matrix.
#'
#' \code{plot_2d_scaling} represents a 2-dimensional scaling plane starting from
#' a dissimilarity matrix.
#' @param distance_matrix A distance matrix.
#' @param cluster_labels The labels associated with the elements involving the
#' entries in \code{distance_matrix}. The points in the plot are coloured
#' according to these labels. If no labels are provided (default), all points
#' are represented in the same colour.
#' @param title The title of the graph (default is no title).
#' @return The 2-dimensional scaling plane.
#' @examples
#' distance_matrix_qcd <- dis_qcd(SyntheticData1$data[1 : 30]) # Computing the pairwise
#' # distance matrix for the first 30 elements in dataset SyntheticData1 based on dis_qcd
#' plot_2d_scaling(distance_matrix_qcd, cluster_labels = SyntheticData1$classes[1 : 30])
#' # Constructing the corresponding 2d-scaling plot. Each class is represented
#' # in a different colour
#' @details
#' Given a distance matrix, the function constructs the corresponding 2-dimensional
#' scaling, which is a 2d plane in which the distances between the points represent
#' the original distances as correctly as possible. If the vector \code{cluster_labels}
#' is provided to the function, points in the 2d plane are coloured according to the
#' given class labels.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @export


plot_2d_scaling <- function(distance_matrix, cluster_labels = NULL, title = '') {


  # Performing the 2d scaling

  mds <- stats::cmdscale(distance_matrix, 2, list. = TRUE)
  gof <- mds$GOF[1]

  if (is.null(cluster_labels)) {

    df <- data.frame(cbind(mds$points))
    X1 <- df$X1
    X2 <- df$X2
    plot <- ggplot2::ggplot(df, ggplot2::aes(x = X1, y = X2)) + ggplot2::geom_point(size = 2.5, col = 'blue') +
      ggplot2::xlab('Coordinate 1') + ggplot2::ylab('Coordinate 2') +
      ggplot2::theme(axis.text = ggplot2::element_text(size = 15),
            axis.title = ggplot2::element_text(size = 17),
            plot.title = ggplot2::element_text(hjust = 0.5,  size = 18),
            legend.position = 'bottom', legend.title = ggplot2::element_blank(),
            legend.text = ggplot2::element_text(size = 12)) + ggplot2::ggtitle(title)
    return_list <- list(plot = plot, coordinates_2d = mds$points, gof = gof)
    return(return_list)


  } else {

    n_labels <- length(unique(cluster_labels))
    vector_labels <- numeric(n_labels)

    for (i in 1 : n_labels) {

      vector_labels[i] <- paste0('Cluster ', i)

    }

    df <- data.frame(cbind(mds$points), factor(cluster_labels))
    colnames(df)[3] <- 'series'
    series <- df$series
    plot <- ggplot2::ggplot(df, ggplot2::aes(x = X1, y = X2, col = series )) + ggplot2::geom_point(size = 2.5) +
      ggplot2::scale_color_discrete(labels = vector_labels) +
      ggplot2::xlab('Coordinate 1') + ggplot2::ylab('Coordinate 2') +
      ggplot2::theme(axis.text = ggplot2::element_text(size = 15),
            axis.title = ggplot2::element_text(size = 17),
            plot.title = ggplot2::element_text(hjust = 0.5,  size = 18),
            legend.position = 'bottom', legend.title = ggplot2::element_blank(),
            legend.text = ggplot2::element_text(size = 12)) + ggplot2::ggtitle(title)
    return_list <- list(plot = plot, coordinates_2d = mds$points, gof = gof)
    return(return_list)



  }


}
