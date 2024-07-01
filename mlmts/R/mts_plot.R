
#' Constructs a plot of a MTS
#'
#' \code{mts_plot} constructs a plot of a MTS. Each univariate series comprising
#' the MTS object is displayed in a different colour.
#'
#' @param series A MTS (numerical matrix).
#' @param title Title for the plot (string). Default corresponds to no title.
#' @return The corresponding plot.
#' @examples
#' mts_plot(BasicMotions$data[[1]]) # Represents the first MTS in dataset
#' # BasicMotions
#' @details
#' Given a MTS, the function constructs the corresponding plot, in which a
#' different colour is used for each univariate series comprising the
#' MTS object. Therefore, the MTS is represented as a collection of univariate
#' series in a single graph.
#' @encoding UTF-8
#' @author
#' Ángel López-Oriona, José A. Vilar
#' @export


mts_plot <- function(series, title = ''){

  df_series <- as.matrix(series)

  df <- NULL
  temp_df <- NULL
  n <- nrow(df_series)
  c <- ncol(df_series)

  for (i in 1 : c) {

    temp_df <- data.frame(x = 1 : n, y = df_series[,i], col = rep(i, n))
    df <- rbind(df,temp_df)

  }

  x <- df$x
  y <- df$y


  if (c > 5) {

    figure <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, group = col, colour = factor(col))) + ggplot2::geom_line(size = 0.8) + ggplot2::xlab('') +
    ggplot2::ylab('') + ggplot2::theme(legend.position = "none") + ggplot2::ggtitle(title) + ggplot2::theme(legend.title = ggplot2::element_blank(),
                                                                        legend.text = ggplot2::element_text(size = 14),
                                                                        axis.text = ggplot2::element_text(size = 14),
                                                                        axis.title = ggplot2::element_text(size = 14),
                                                                        plot.title = ggplot2::element_text(hjust = 0.5, size = 18))


    return(figure) } else {

      vector_labels <- numeric()

      for (i in 1 : c) {

        vector_labels[i] <- paste('UTS', i)

      }


      figure <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, group = col, colour = factor(col))) + ggplot2::geom_line(size = 0.8) + ggplot2::xlab('') +
        ggplot2::ylab('') + ggplot2::theme(legend.position = "bottom") + ggplot2::ggtitle(title) + ggplot2::theme(legend.title = ggplot2::element_blank(),
                                                                                                                legend.text = ggplot2::element_text(size = 14),
                                                                                                                axis.text = ggplot2::element_text(size = 14),
                                                                                                                axis.title = ggplot2::element_text(size = 14),
                                                                                                                plot.title = ggplot2::element_text(hjust = 0.5, size = 18)) +
        ggplot2::scale_color_discrete(labels = vector_labels)

      return(figure)}





}

