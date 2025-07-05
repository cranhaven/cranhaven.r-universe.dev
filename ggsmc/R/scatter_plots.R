AncestorValue = Target = ExternalTarget = ParameterName = Parameter = Dimension = Value = Target = LogWeight = Iteration = ExternalIndex = Particle = Chain = NULL

.data = NULL

#' A histogram of a single variable from a single target
#'
#' @param output Output from the SMC or EnK algorithm.
#' @param x_parameter The parameter indexed by the x-axis.
#' @param x_dimension (optional) The dimension of the x-parameter we wish to histogram. (default is 1)
#' @param y_parameter The parameter indexed by the y-axis.
#' @param y_dimension (optional) The dimension of the y-parameter we wish to histogram. (default is 1)
#' @param target (optional) The index of the target we wish to plot. (default is to use all targets)
#' @param external_target (optional) The index of the external target to plot. (default is to use all external targets, or to ignore if the column is not present)
#' @param use_initial_points (optional) If target is not specified and this argument is TRUE, will add the initial unweighted proposed points to the output to be plotted. (default is TRUE)
#' @param use_weights (optional) If FALSE, will ignore particle weights in the scatter plot. If TRUE, will use the particle weights. (defaults to TRUE)
#' @param max_size (optional) The maximum size of the points in the plot. (default=1)
#' @param alpha (optional) The transparency of the points in the plot. (default=0.1)
#' @param xlimits (optional) Input of the form c(start,end), which specifies the ends of the x-axis.
#' @param ylimits (optional) Input of the form c(start,end), which specifies the ends of the y-axis.
#' @param default_title (optional) If TRUE, will provide a default title for the figure. If FALSE, no title is used. (defaults to FALSE)
#' @return A scatter plot in a ggplot figure.
#' @export
plot_scatter = function(output,
                        x_parameter,
                        x_dimension=1,
                        y_parameter,
                        y_dimension=1,
                        target=NULL,
                        external_target=NULL,
                        use_initial_points=TRUE,
                        use_weights=TRUE,
                        max_size=1,
                        alpha=0.1,
                        xlimits=NULL,
                        ylimits=NULL,
                        default_title=FALSE)
{

  if (!is.null(target) && !(target %in% output$Target))
  {
    stop('target not found in output.')
  }

  if (!is.null(external_target) && !("ExternalTarget" %in% names(output)))
  {
    stop("ExternalTarget column not found in output.")
  }

  if ("Value" %in% names(output))
  {
    new_variable_names = mapply(FUN = function(a,b) { paste(a,"_",b,sep="") },output$ParameterName,output$Dimension)
    output = subset(output,select = -c(ParameterName,Dimension))
    output$Parameter = new_variable_names

    output = poorman::distinct(output)

    #output = poorman::pivot_wider(output, names_from = "Parameter", values_from = "Value")
    output_to_use = poorman::pivot_wider(output,names_from=Parameter,values_from=Value)
  }
  else
  {
    output_to_use = output
  }

  x_parameter = paste(x_parameter,"_",x_dimension,sep="")
  y_parameter = paste(y_parameter,"_",y_dimension,sep="")

  target_data = extract_target_data(output_to_use,target,external_target,use_initial_points)
  output_to_use = target_data[[1]]
  target_parameters = target_data[[2]]

  if ( ("LogWeight" %in% names(output_to_use)) && (use_weights==TRUE) )
  {
    plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=.data[[x_parameter]],
                                                       y=.data[[y_parameter]],
                                                       size=exp(LogWeight),
                                                       stroke=0))
  }
  else
  {
    plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=.data[[x_parameter]],
                                                       y=.data[[y_parameter]],
                                                       size=1/nrow(output_to_use),
                                                       stroke=0))
  }

  x_split_result = strsplit(x_parameter,"_")[[1]] #stringr::str_split(x_parameter, "(?<=\\D)(?=\\d)|(?<=\\d)(?=\\D)", n = Inf, simplify = TRUE)
  y_split_result = strsplit(y_parameter,"_")[[1]] #stringr::str_split(y_parameter, "(?<=\\D)(?=\\d)|(?<=\\d)(?=\\D)", n = Inf, simplify = TRUE)

  if ( (length(x_split_result)>2) || (length(x_split_result)==0) || (length(y_split_result)>2) || (length(y_split_result)==0) )
  {
    stop("For automated captions, parameters need to be of the form name_number (with no other underscores in the name).")
  }
  if ( (length(x_split_result)==1) && (length(y_split_result)==1) )
  {
    x_parameter_for_plot = x_parameter
    y_parameter_for_plot = y_parameter
    if (is.null(target) || (target_parameters=="") )
    {
      default_title_for_plot = bquote("Scatter plot of"~.(x_parameter)~"and"~.(y_parameter))
    }
    else
    {
      default_title_for_plot = bquote("Scatter plot of"~.(x_parameter)~"and"~.(y_parameter)~"("*.(target_parameters)*")")
    }
  }
  else if ( (length(x_split_result)==2) && (length(y_split_result)==1) )
  {
    if (is.null(target) || (target_parameters=="") )
    {
      default_title_for_plot = bquote("Scatter plot of"~.(x_split_result[1])[.(x_split_result[2])]~"and"~.(y_parameter))
    }
    else
    {
      default_title_for_plot = bquote("Scatter plot of"~.(x_split_result[1])[.(x_split_result[2])]~"and"~.(y_parameter)~"("*.(target_parameters)*")")
    }
    x_parameter_for_plot = bquote(.(x_split_result[1])[.(x_split_result[2])])
    y_parameter_for_plot = y_parameter
  }
  else if ( (length(x_split_result)==1) && (length(y_split_result)==2) )
  {
    if (is.null(target) || (target_parameters=="") )
    {
      default_title_for_plot = bquote("Scatter plot of"~.(x_parameter)~"and"~.(y_split_result[1])[.(y_split_result[2])])
    }
    else
    {
      default_title_for_plot = bquote("Scatter plot of"~.(x_parameter)~"and"~.(y_split_result[1])[.(y_split_result[2])]~"("*.(target_parameters)*")")
    }
    x_parameter_for_plot = x_parameter
    y_parameter_for_plot = bquote(.(y_split_result[1])[.(y_split_result[2])])
  }
  else if ( (length(x_split_result)==2) && (length(y_split_result)==2) )
  {
    if (is.null(target) || (target_parameters=="") )
    {
      default_title_for_plot = bquote("Scatter plot of"~.(x_split_result[1])[.(x_split_result[2])]~"and"~.(y_split_result[1])[.(y_split_result[2])])
    }
    else
    {
      default_title_for_plot = bquote("Scatter plot of"~.(x_split_result[1])[.(x_split_result[2])]~"and"~.(y_split_result[1])[.(y_split_result[2])]~"("*.(target_parameters)*")")
    }
    x_parameter_for_plot = bquote(.(x_split_result[1])[.(x_split_result[2])])
    y_parameter_for_plot = bquote(.(y_split_result[1])[.(y_split_result[2])])
  }

  plot = plot +
    ggplot2::geom_point(show.legend=FALSE,alpha=alpha) +
    ggplot2::scale_size_area(max_size = max_size) +
    ggplot2::xlab(x_parameter_for_plot) +
    ggplot2::ylab(y_parameter_for_plot)

  if (default_title)
  {
    plot = plot + ggplot2::labs(title=default_title_for_plot)
  }

  if ( (!is.null(ylimits)) && (is.numeric(ylimits)) && (is.vector(ylimits)) )
  {
    plot = plot + ggplot2::ylim(ylimits[1],ylimits[2])
  }

  if ( (!is.null(xlimits)) && (is.numeric(xlimits)) && (is.vector(xlimits)) )
  {
    plot = plot + ggplot2::xlim(xlimits[1],xlimits[2])
  }

  return(plot)
}


#' A histogram of a single variable from a single target.
#'
#' @param output Output from the SMC or EnK algorithm.
#' @param x_parameter The parameter indexed by the x-axis.
#' @param x_dimension (optional) The dimension of the x-parameter we wish to histogram. (default is 1)
#' @param y_parameter The parameter indexed by the y-axis.
#' @param y_dimension (optional) The dimension of the y-parameter we wish to histogram. (default is 1)
#' @param target (optionaL) If specified, will fix to this target, and animate over ExternalTarget (if present in output).
#' @param external_target (optionaL) If specified, will fix to this external_target, and animate over Target.
#' @param use_initial_points (optional) If target is not specified and this argument is TRUE, will add the initial unweighted proposed points to the output to be plotted. (default is TRUE)
#' @param use_weights (optional) If FALSE, will ignore particle weights in the scatter plot. If TRUE, will use the particle weights. (defaults to TRUE)
#' @param max_size (optional) The maximum size of the points in the plot. (default=1)
#' @param alpha (optional) The transparency of the points in the plot. (default=0.1)
#' @param xlimits (optional) Input of the form c(start,end), which specifies the ends of the x-axis.
#' @param ylimits (optional) Input of the form c(start,end), which specifies the ends of the y-axis.
#' @param default_title (optional) If TRUE, will provide a default title for the figure. If FALSE, no title is used. (defaults to FALSE)
#' @param view_follow (optional) If TRUE, the view will follow the particles. (default FALSE)
#' @param shadow_mark_proportion_of_max_size (optional) If set, the animation will leave behind shadow points, of a size (and transparency) specified by this proportion. (default to not set)
#' @param shadow_wake_length (optional) If set, the animation will leave a shadow wake behind each point, of a duration given by this parameter (proportion of the entire animation length). (default to not set)
#' @param duration (optional) The duration of the animation. (defaults to producing an animation that uses 10 frames per second)
#' @param animate_plot (optiional) If TRUE, will return an animation. If FALSE, returns a gganim object that can be furher modified before animating. (defaults to FALSE)
#' @param save_filename (optional) If specified, the animation will be saved to a gif with this filename. (default is not to save)
#' @param save_path (optional) If specified along with save_filename, will save the gif to save_path/save_filename. (defaults to working directory)
#' @return A scatter plot in a ggplot figure.
#' @export
animate_scatter = function(output,
                                 x_parameter,
                                 x_dimension,
                                 y_parameter,
                                 y_dimension,
                                 target=NULL,
                                 external_target=NULL,
                                 use_initial_points=TRUE,
                                 use_weights=TRUE,
                                 max_size=1,
                                 alpha=0.1,
                                 xlimits=NULL,
                                 ylimits=NULL,
                                 default_title=FALSE,
                                 view_follow=FALSE,
                                 shadow_mark_proportion_of_max_size=NULL,
                                 shadow_wake_length=NULL,
                                 duration=NULL,
                                 animate_plot=TRUE,
                                 save_filename=NULL,
                                 save_path=NULL)
{
  if ("Value" %in% names(output))
  {
    new_variable_names = mapply(FUN = function(a,b) { paste(a,"_",b,sep="") },output$ParameterName,output$Dimension)
    output = subset(output,select = -c(ParameterName,Dimension))
    output$Parameter = new_variable_names
    output = poorman::distinct(output)

    #output = poorman::pivot_wider(output, names_from = "Parameter", values_from = "Value")
    output_to_use = poorman::pivot_wider(output,names_from=Parameter,values_from=Value)
  }
  else
  {
    output_to_use = output
  }

  p = plot_scatter(output = output_to_use,
                   x_parameter = x_parameter,
                   x_dimension = x_dimension,
                   y_parameter = y_parameter,
                   y_dimension = y_dimension,
                   target = target,
                   external_target = external_target,
                   use_initial_points = use_initial_points,
                   use_weights = use_weights,
                   max_size=max_size,
                   alpha=alpha,
                   xlimits=xlimits,
                   ylimits=ylimits,
                   default_title=default_title)

  if (view_follow)
  {
    p = p + gganimate::view_follow()
  }

  if (!is.null(shadow_mark_proportion_of_max_size))
  {
    if ( (shadow_mark_proportion_of_max_size>=0) && (shadow_mark_proportion_of_max_size<=1) )
    {
      p = p + gganimate::shadow_mark(alpha = alpha*shadow_mark_proportion_of_max_size, size = max_size*shadow_mark_proportion_of_max_size)
    }
    else
    {
      stop("shadow_mark_proportion_of_max_size must be between 0 and 1")
    }
  }

  if (!is.null(shadow_wake_length))
  {
    if ( (shadow_wake_length>=0) && (shadow_wake_length<=1) )
    {
      p = p + gganimate::shadow_wake(wake_length = shadow_wake_length, alpha = TRUE, wrap=FALSE)
    }
    else
    {
      stop("shadow_wake_length must be between 0 and 1")
    }
  }

  if (!is.null(external_target) && is.null(target))
  {
    to_animate = p + gganimate::transition_time(Target)

    nframes = length(unique(output_to_use$Target))
  }
  else if (is.null(external_target) && !is.null(target))
  {
    if ("ExternalTarget" %in% names(output))
    {
      to_animate = p + gganimate::transition_time(ExternalTarget)

      nframes = length(unique(output_to_use$ExternalTarget))
    }
    else
    {
      stop("ExternalTarget not specified in output, so cannot animate over it.")
    }
  }
  else if (!is.null(external_target) && !is.null(target))
  {
    stop("Both target and external_target are specified, so no animation to be done.")
  }
  else
  {
    if ("ExternalTarget" %in% names(output))
    {
      stop("Neither target nor external_target are specified: must specify one or the other when ExternalTarget is present in output.")
    }
    else
    {
      to_animate = p + gganimate::transition_time(Target)
      nframes = length(unique(output$Target))
    }
  }

  if (animate_plot)
  {
    if (is.null(duration))
    {
      animated = gganimate::animate(to_animate,nframes=nframes)
    }
    else
    {
      animated = gganimate::animate(to_animate,nframes=nframes,duration=duration)
    }

    if (!is.null(save_filename))
    {
      if (is.null(save_path))
      {
        gganimate::anim_save(filename=save_filename,animation=animated)
      }
      else
      {
        gganimate::anim_save(filename=save_filename,animation=animated,path=save_path)
      }
    }

    return(animated)
  }
  else
  {
    if (!is.null(save_filename))
    {
      stop("Cannot save, since plot not animated.")
    }
    else
    {
      return(to_animate)
    }
  }
}
