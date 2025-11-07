#' Pretty breaks miRetrieve - Helper
#'
#' Pretty breaks miRetrieve Helper.
#'
#' Make integers in bar plots pretty.
#' Takes a plot as an argument and expands the y-axis to c(0,0),
#' hence leaving no space between y-axis and the plot.
#' Furthermore, breaks up to a number of 4 are added manually,
#' while breaks with a number of 5 and above rely on scales::pretty_breaks().
#'
#' @param plot Bar plot that pretty breaks shall be added to.
#' @param y_axis Parameter that is used for the y-axis of the bar plot.
#'
#' @return Plot with pretty breaks
#'
#' @noRd
pretty_breaks_miretrieve <- function(plot, y_axis) {
    max_y <- max(y_axis)

    if(max_y %in% c(1,2,3,4)) {
        plot_pretty <- plot + ggplot2::scale_y_continuous(breaks = seq(1, max_y),
                                                          expand = c(0,0),
                                                          limits = c(0, max_y))
    } else {
        plot_pretty <- plot + ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(),
                                                          expand = c(0,0),
                                                          limits = c(0, max_y))
    }

    return(plot_pretty)
}
