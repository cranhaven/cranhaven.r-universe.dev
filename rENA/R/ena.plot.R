##
#' @title Generate a plot of an ENAset
#'
#' @description Generates an a plot from a given ENA set object
#'
#' @details This function defines the axes and other features of a plot for displaying an ENAset; generates an ENAplot object that can used to plot points, network graphs, and other information from an ENAset
#'
#' @export
#'
#' @param enaset The \code{\link{ENAset}} that will be used to generate a plot
#' @param title A character used for the title of the plot, default: ENA Plot
#' @param dimension.labels A character vector containing labels for the axes, default: c(X, Y)
#' @param font.size An integer determining the font size for graph labels, default: 10
#' @param font.color A character determining the color of label font, default: black
#' @param font.family A character determining the font type, choices: Arial, Courier New, Times New Roman, default: Arial
#' @param scale.to "network" (default), "points", or a list with x and y ranges. Network and points both scale to the c(-max, max) of the corresponding data.frame
#' @param ... additional parameters addressed in inner function
#'
#'
#' @seealso \code{\link{ena.make.set}}, \code{\link{ena.plot.points}}
#'
#' @return \code{\link{ENAplot}} used for plotting an ENAset

##
ena.plot <- function(
  enaset,

  title = "ENA Plot",

  dimension.labels = c("",""),

  font.size = 10,
  font.color = "#000000",
  font.family = c("Arial", "Courier New", "Times New Roman"),
  scale.to = "network", #, "points"),
  ...
) {
  if (is(enaset, "ENAset")) {
    warning(paste0("Usage of ENAset objects will be deprecated ",
      "and potentially removed altogether in future versions."))

    enaset <- ena.set(enaset);
  }

  font.family = match.arg(font.family);

  plot = ENAplot$new(enaset,
                     title,
                     dimension.labels,
                     font.size,
                     font.color,
                     font.family,
                     scale.to = scale.to,
                     ...
                   );

  return(plot);
}
