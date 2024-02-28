#' @title  Focus on Temperature
#'
#' @description Plot the stability data and visualise the predictions with focus on
#'  one temperature.
#'
#' @details Plot the stability data and visualise the predictions focusing on one
#'  chosen temperature with confidence and prediction intervals.
#'
#' @param step1_down_object The fit object from the step1.down function (required).
#' @param focus_T Selected temperature to highlight on the plot.
#' @param xname Label for the x-axis (optional).
#' @param yname Label for the y-axis (optional).
#' @param xlim the x-axis limits (optional).
#' @param ylim the y-axis limits (optional).
#' @param ribbon adds shade to confidence and prediction intervals (optional).
#'
#' @return ggplot2 object with focus on chosen temperature.
#'
#' @examples
#' #load potency data
#' data(potency)
#'
#' #run step1_down fit
#' fit1 <- step1_down(data = potency, y = "Potency", .time = "Time",
#'  C = "Celsius", zero_order = TRUE)
#'
#' #plot raw data with prediction curves with focus on temperature in dataset.
#' step1_plot_T(fit1, focus_T = 5,ribbon = TRUE, xlim = NULL, ylim = c(0,12),
#'  xname = "Time (Month)", yname = "Potency")
#'
#' #plot raw data with prediction curves with focus on temperature not in dataset.
#' step1_plot_T(fit1, focus_T = -10,ribbon = TRUE, xlim = NULL, ylim = c(0,12),
#'  xname = "Time (Months)", yname = "Potency")
#'
#' @import ggplot2
#' @import scales
#'
#' @export step1_plot_T

step1_plot_T <- function (step1_down_object, focus_T = NULL, xname = NULL, yname = NULL,
                       xlim = NULL, ylim = NULL, ribbon = FALSE)
{
  if (is.null(step1_down_object))
    stop("First, run the model")
  if (is.null(focus_T))
    stop("You must select a temperature to focus on")
  if (is.null(xname))
    xname = "Time"
  if (is.null(yname))
    yname = "Response Variable"

  if(!(focus_T %in% step1_down_object$prediction$Celsius)){
    step1_down_object_temp <- step1_down(
      data = step1_down_object$user_parameters$data,
      y = step1_down_object$user_parameters$y,
      .time = step1_down_object$user_parameters$.time,
      K = step1_down_object$user_parameters$K,
      C = step1_down_object$user_parameters$C,
      validation = step1_down_object$user_parameters$validation,
      draw = step1_down_object$user_parameters$draw,
      parms = step1_down_object$user_parameters$parms,
      temp_pred_C = c(step1_down_object$user_parameters$temp_pred_C,focus_T),
      max_time_pred = step1_down_object$user_parameters$max_time_pred,
      confidence_interval = step1_down_object$user_parameters$confidence_interval,
      by = step1_down_object$user_parameters$by,
      reparameterisation = step1_down_object$user_parameters$reparameterisation,
      zero_order = step1_down_object$user_parameters$zero_order
      )
    dat = step1_down_object_temp$data
    pred = step1_down_object_temp$prediction
    confidence_interval = step1_down_object_temp$user_parameters$confidence_interval
  }else{
    dat = step1_down_object$data
    pred = step1_down_object$prediction
    confidence_interval = step1_down_object$user_parameters$confidence_interval
  }

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

  xx = pred$Celsius == focus_T

  confidence_i <- paste0(confidence_interval * 100," % CI")
  prediction_i <- paste0(confidence_interval * 100," % PI")

  lines_t <- c("solid","dotted","longdash")
  names(lines_t) <- c("Prediction",confidence_i,prediction_i)

  colour_t <- scales::hue_pal()(length(unique(pred$Celsius)))
  names(colour_t) <- as.character(unique(pred$Celsius))

  plot = ggplot() +
   labs( x = xname, y = yname) +
    {if(!is.null(xlim))scale_x_continuous(limits = xlim)} +
    {if(!is.null(ylim))scale_y_continuous(limits = ylim)} +
   mytheme  +
   geom_line(data=pred, mapping=aes(x= time, y = Response, colour = Celsius, linetype = "Prediction")) +
   geom_line(data=pred[xx,], mapping=aes(x= time, y = CI1, colour = Celsius, linetype = confidence_i)) +
   geom_line(data=pred[xx,], mapping=aes(x= time, y = CI2, colour = Celsius, linetype = confidence_i)) +
  {if(ribbon)geom_ribbon(data=pred[xx,], aes(x = time, ymin=PI1, ymax=PI2, fill = Celsius), alpha=0.08, show.legend = FALSE)} +
  {if(ribbon)geom_ribbon(data=pred[xx,], aes(x = time, ymin=CI1, ymax=CI2, fill= Celsius), alpha=0.13, show.legend = FALSE)} +
   geom_line(data=pred[xx,], mapping=aes(x= time, y = PI1, colour = Celsius, linetype = prediction_i)) +
   geom_line(data=pred[xx,], mapping=aes(x= time, y = PI2, colour = Celsius, linetype = prediction_i)) +
   geom_point(data=dat, mapping=aes(x= time, y = y, colour = Celsius, shape = validation)) +
   scale_linetype_manual(name = NULL, values = lines_t) +
   scale_colour_manual(name = "Celsius", values = colour_t) +
   scale_fill_manual(name = NULL, values = colour_t) +
   {if(!is.null(validation))scale_shape_manual(values = shape_types, name = NULL)} +
   theme(legend.box = "vertical", legend.spacing = unit(-0.4,"line"))

  return(plot)
}

globalVariables(c('PI1','PI2'))
