#' @title Plot Confidence Intervals
#'
#' @description Plot the stability data and visualise the predictions with
#'  confidence intervals.
#'
#' @details Use the fit object obtained from the step1.down function to plot the
#'  data and visualise the predictions with confidence intervals applied.
#'  There is an option to view the confidence intervals as a ribbon. The
#'  confidence interval value is chosen in the step1.down function.
#'
#' @param step1_down_object The fit object from the step1.down function (required).
#' @param xname Label for the x-axis (optional).
#' @param yname Label for the y-axis (optional).
#' @param xlim x-axis limits (optional).
#' @param ylim y-axis limits (optional).
#' @param ribbon Add shade to confidence intervals (optional).
#'
#' @return Plot of stability data with prediction curves and confidence intervals.
#'
#' @examples
#' #load antigenciity data
#' data(antigenicity)
#'
#' #run step1.down fit
#' fit1 <- step1_down(data = antigenicity, y = "conc", .time = "time",
#'  C = "Celsius", max_time_pred = 3, confidence_interval = 0.9)
#'
#' #plot raw data with prediction curves and confidence intervals.
#' step1_plot_CI(step1_down_object = fit1, xlim = NULL, ylim = NULL,
#'  xname = "Time (Years)", yname = "Concentration", ribbon = TRUE)
#'
#' @import ggplot2
#'
#' @export step1_plot_CI

step1_plot_CI <- function (step1_down_object, xname = NULL, yname = NULL,
                           xlim = NULL, ylim = NULL, ribbon = FALSE)
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

  confidence_i <- paste0(step1_down_object$user_parameters$confidence_interval * 100," % CI")
  line_types <- if(ribbon){c("solid", "dotted")}else{c("dotted", "solid")}
  names(line_types) <- c("Prediction",confidence_i)

  plot = ggplot() + geom_point(data=dat, mapping=aes(x= time, y = y, colour = Celsius, shape = validation))  +
   labs( x = xname, y = yname) +
   {if(!is.null(xlim))scale_x_continuous(limits = xlim)} +
   {if(!is.null(ylim))scale_y_continuous(limits = ylim)} +
   mytheme  +
   geom_line(data=pred, mapping=aes(x= time, y = Response, colour = Celsius, linetype = "Prediction")) +
   geom_line(data=pred, mapping=aes(x= time, y = CI1, colour = Celsius, linetype = confidence_i)) +
   geom_line(data=pred, mapping=aes(x= time, y = CI2, colour = Celsius, linetype = confidence_i)) +
   scale_linetype_manual(name = NULL, values = line_types) +
   {if(ribbon)geom_ribbon(data=pred, aes(x = time, ymin=CI1, ymax=CI2, fill = Celsius), alpha=0.13, show.legend = FALSE)} +
   {if(!is.null(validation))scale_shape_manual(values = shape_types, name = NULL)} +
   theme(legend.box = "vertical", legend.spacing = unit(-0.4,"line"))

  return(plot)
}

globalVariables(c('time','y','Celsius','Response','CI1','CI2'))

