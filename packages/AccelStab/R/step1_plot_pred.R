#' @title  Plot Model Predictions
#'
#' @description Plot the stability data and visualise the predictions.
#'
#' @details Use the fit object from the step1.down function to plot the accelerated
#'  stability data and visualise the predictions.
#'
#' @param step1_down_object The fit object from the step1.down function (required).
#' @param xname Label for the x-axis (optional).
#' @param yname Label for the y-axis (optional).
#' @param xlim x-axis limits (optional).
#' @param ylim y-axis limits (optional).
#'
#' @return Plot of accelerated stability data with prediction curves.
#'
#' @examples
#' #load antigenicity data
#' data(antigenicity)
#'
#' fit1 <- step1_down(data = antigenicity, y = "conc", .time = "time",
#'  C = "Celsius", max_time_pred = 3)
#'
#' step1_plot_pred(step1_down_object = fit1, xlim = NULL, ylim = NULL,
#'  xname = "Time (Years)", yname = "Concentration")
#'
#' @import ggplot2
#'
#' @export step1_plot_pred

step1_plot_pred <- function (step1_down_object, xname = NULL, yname = NULL,
                             xlim = NULL, ylim = NULL)
{
  if (is.null(step1_down_object))
    stop("First, run the model")
  if (is.null(xname))
    xname = "Time"
  if (is.null(yname))
    yname = "Response Variable"
  dat = step1_down_object$data
  pred = step1_down_object$prediction

  mytheme <- ggplot2::theme(legend.position = "bottom", strip.background = element_rect(fill = "white"),
                            legend.key = element_rect(fill = "white"), legend.key.width = unit(2,"cm"),
                            axis.text = element_text(size = 13), axis.title = element_text(size = 13),
                            strip.text = element_text(size = 13),
                            legend.text = element_text(size = 13),
                            legend.title = element_text(size = 13))

  validation = step1_down_object$user_parameters$validation
  if(!is.null(validation)){
    shape_types <- c(16,1)
    names(shape_types) <- c("Fit", "Validation")
  }

  plot = ggplot() + geom_point(data=dat, mapping=aes(x= time, y = y, colour = Celsius, shape = validation)) +
   labs( x = xname, y = yname) +
   {if(!is.null(xlim))scale_x_continuous(limits = xlim)} +
   {if(!is.null(ylim))scale_y_continuous(limits = ylim)} +
   mytheme +
   geom_line(data=pred, mapping=aes(x= time, y = Response, colour = Celsius)) +
   scale_linetype_manual(name = NULL, values=c("solid", "dotted", "longdash")) +
   {if(!is.null(validation))scale_shape_manual(values = shape_types, name = NULL)} +
   theme(legend.box = "vertical", legend.spacing = unit(-0.4,"line"))

  return(plot)
}
