#' Clean 'ggplot2' Histogram to be Converted to 'Plotly'
#' @description
#' Create 'ggplot2' histogram that translate nicely to 'plotly'.
#'
#' @inheritParams ggplot2::geom_histogram
#' @return
#' ggplot bar layer
#' @export
#' @details
#' `ggplotly_histogram()` is a function that is used to create a 'ggplot2' histogram, yet on conversion to 'plotly' using 'ggplotly()', the resulted plot will hold the correct labeling information, which are "Range", "Count" and "Density".
#'
#' @importFrom ggplot2 ggplot stat_bin geom_bar layer_data aes
#' @importFrom plotly ggplotly
#' @importFrom rlang .data
#' @examples
#'
#' library(ggplot2)
#' library(plotly)
#' # create the histogram using `ggplotly_histogram()`
#' p <- ggplot() + ggplotly_histogram(data = ToothGrowth, mapping = aes(len))+
#' xlab("len")
#'
#' # convert `ggplot` object to `plotly` object
#' ggplotly(p, tooltip = c("Range", "count", "density"))

ggplotly_histogram <- function(data = NULL, mapping = NULL, position = "stack",
                               ..., binwidth = NULL, na.rm = FALSE,
                               show.legend = NA) {
        p <-  ggplot() +
                stat_bin(
                        mapping = mapping,
                        data = data,
                        position = position,
                        binwidth = binwidth,
                        na.rm = na.rm,
                        show.legend = show.legend,
                        ...
                )


        layerdata <- layer_data(p, 1)

        layerdata$Range <-
                paste(round(layerdata$xmin, 2), "-", round(layerdata$xmax, 2))
        names(layerdata)[1] <- "Count"

        #Create Bar chart of the histogram layer
        geom_bar(
                data = layerdata,
                mapping = aes(
                        x = .data$x,
                        y = .data$count,
                        label1 = .data$Range,
                        label2 = .data$Count,
                        label3 = .data$density
                ),
                ...,
                stat = "identity",
                position = "stack",
                show.legend = show.legend,
                width = 1
        )
}
