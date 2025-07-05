AncestorValue = Target = ExternalTarget = ParameterName = Parameter = Dimension = Value = Target = LogWeight = Iteration = ExternalIndex = Particle = Chain = NULL

#' Plot line graph showing parameter value vs dimension from algorithm output.
#'
#' @param output Output from the SMC or EnK algorithm.
#' @param parameters The parameters we wish to be on the y-axis of the line graph.
#' @param target (optional) The target to plot. (default is to use all targets)
#' @param external_target (optional) The external target to plot. (default is to use all external targets, or to ignore if the column is not present)
#' @param use_initial_points (optional) If target is not specified and this argument is TRUE, will add the initial unweighted proposed points to the output to be plotted. (default is TRUE)
#' @param use_weights (optional) If FALSE, will ignore particle weights in the line graph. If TRUE, will use the particle weights. (defaults to TRUE)
#' @param max_line_width (optional) The maximum size of the points in the plot. (default=1)
#' @param alpha (optional) The transparency of the lines in the plot. (default=0.1)
#' @param xlimits (optional) Input of the form c(start,end), which specifies the ends of the x-axis.
#' @param ylimits (optional) Input of the form c(start,end), which specifies the ends of the y-axis.
#' @return A line graph in a ggplot figure.
#' @export
plot_time_series = function(output,
                                  parameters,
                                  target=NULL,
                                  external_target=NULL,
                                  use_initial_points=TRUE,
                                  use_weights=TRUE,
                                  max_line_width=1,
                                  alpha=0.1,
                                  xlimits=NULL,
                                  ylimits=NULL)
{
  if (!is.null(target) && !(target %in% output$Target))
  {
    stop('target not found in output.')
  }

  if (!is.null(external_target) && !("ExternalTarget" %in% names(output)))
  {
    stop("ExternalTarget column not found in output.")
  }

  target_data = extract_target_data(output,target,external_target,use_initial_points)
  output_to_use = target_data[[1]]
  target_parameters = target_data[[2]]

  output_to_use = poorman::filter(output_to_use,(ParameterName %in% parameters))

  #browser()

  if ( ("LogWeight" %in% names(output)) && (use_weights==TRUE) )
  {
    if ("ExternalIndex" %in% names(output_to_use))
    {
      if ("ExternalTarget" %in% names(output_to_use))
      {
        plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Dimension,
                                                           y=Value,
                                                           group=interaction(Iteration, Particle, ExternalIndex, ExternalTarget, Target, ParameterName),
                                                           colour=ParameterName,
                                                           linewidth=exp(LogWeight)))
      }
      else
      {
        plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Dimension,
                                                           y=Value,
                                                           group=interaction(Iteration, Particle, ExternalIndex, Target, ParameterName),
                                                           colour=ParameterName,
                                                           linewidth=exp(LogWeight)))
      }
    }
    else
    {
      if ("ExternalTarget" %in% names(output_to_use))
      {
        plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Dimension,
                                                           y=Value,
                                                           group=interaction(Iteration, Particle, ExternalTarget, Target, ParameterName),
                                                           colour=ParameterName,
                                                           linewidth=exp(LogWeight)))
      }
      else
      {
        plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Dimension,
                                                           y=Value,
                                                           group=interaction(Iteration, Particle, Target, ParameterName),
                                                           colour=ParameterName,
                                                           linewidth=exp(LogWeight)))
      }
    }
  }
  else
  {
    if ("Particle" %in% names(output_to_use))
    {
      if ("ExternalIndex" %in% names(output_to_use))
      {
        if ("ExternalTarget" %in% names(output_to_use))
        {
          plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Dimension,
                                                             y=Value,
                                                             group=interaction(Iteration, Particle, ExternalIndex, Target, ExternalTarget, ParameterName),
                                                             colour=ParameterName))
        }
        else
        {
          plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Dimension,
                                                             y=Value,
                                                             group=interaction(Iteration, Particle, ExternalIndex, Target, ParameterName),
                                                             colour=ParameterName))
        }
      }
      else
      {
        if ("ExternalTarget" %in% names(output_to_use))
        {
          plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Dimension,
                                                             y=Value,
                                                             group=interaction(Iteration, Particle, ExternalTarget, Target, ParameterName),
                                                             colour=ParameterName))
        }
        else
        {
          plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Dimension,
                                                             y=Value,
                                                             group=interaction(Iteration, Particle, Target, ParameterName),
                                                             colour=ParameterName))
        }
      }
    }
    else if ("Chain" %in% names(output_to_use))
    {
      if ("ExternalIndex" %in% names(output_to_use))
      {
        if ("ExternalTarget" %in% names(output_to_use))
        {
          plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Dimension,
                                                             y=Value,
                                                             group=interaction(Iteration, Chain, ExternalIndex, ExternalTarget, ParameterName),
                                                             colour=ParameterName))
        }
        else
        {
          plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Dimension,
                                                             y=Value,
                                                             group=interaction(Iteration, Chain, ExternalIndex, ParameterName),
                                                             colour=ParameterName))
        }
      }
      else
      {
        if ("ExternalTarget" %in% names(output_to_use))
        {
          plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Dimension,
                                                             y=Value,
                                                             group=interaction(Iteration, Chain, ExternalTarget, ParameterName),
                                                             colour=ParameterName))
        }
        else
        {
          plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Dimension,
                                                             y=Value,
                                                             group=interaction(Iteration, Chain, ParameterName),
                                                             colour=ParameterName))
        }
      }
    }
    else
    {
      stop('output must contain a column named either "Particle" or "Chain".')
    }
  }

  # if (is.null(target) || (target_parameters=="") )
  # {
  #   default_title_for_plot = bquote("The evolution of"~.(parameter))
  # }
  # else
  # {
  #   default_title_for_plot = bquote("The evolution of"~.(parameter)~"("*.(target_parameters)*")")
  # }

  plot = plot +
    ggplot2::geom_line(show.legend=TRUE,alpha=alpha) +
    ggplot2::scale_linewidth(range = c(0,max_line_width)) +
    ggplot2::xlab("Index") #+
    #ggplot2::ylab(parameter_for_plot)

  # if (default_title)
  # {
  #   plot = plot + ggplot2::labs(title=default_title_for_plot)
  # }

  if ( (!is.null(xlimits)) && (is.numeric(xlimits)) && (is.vector(xlimits)) )
  {
    plot = plot + ggplot2::xlim(xlimits[1],xlimits[2])
  }

  if ( (!is.null(ylimits)) && (is.numeric(ylimits)) && (is.vector(ylimits)) )
  {
    plot = plot + ggplot2::ylim(ylimits[1],ylimits[2])
  }

  plot = plot +
    ggplot2::scale_color_discrete(name = "Parameter", labels = sort(parameters)) +
    ggplot2::guides(linewidth = "none") +
    ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1,linewidth=1)))

  return(plot)
}

#' Plot animated line graph showing parameter value vs dimension (revealed in the animation) from algorithm output.
#'
#' @param output Output from the SMC or EnK algorithm.
#' @param parameters The parameters we wish to be on the y-axis of the line graph.
#' @param target (optional) The target to plot. (default is to use all targets)
#' @param external_target (optional) The external target to plot. (default is to use all external targets, or to ignore if the column is not present)
#' @param use_initial_points (optional) If target is not specified and this argument is TRUE, will add the initial unweighted proposed points to the output to be plotted. (default is TRUE)
#' @param use_weights (optional) If FALSE, will ignore particle weights in the line graph. If TRUE, will use the particle weights. (defaults to TRUE)
#' @param max_line_width (optional) The maximum size of the points in the plot. (default=1)
#' @param alpha (optional) The transparency of the lines in the plot. (default=0.1)
#' @param xlimits (optional) Input of the form c(start,end), which specifies the ends of the x-axis.
#' @param ylimits (optional) Input of the form c(start,end), which specifies the ends of the y-axis.
#' @param duration (optional) The duration of the animation. (defaults to producing an animation that uses 10 frames per second)
#' @param animate_plot (optiional) If TRUE, will return an animation. If FALSE, returns a gganim object that can be furher modified before animating. (defaults to FALSE)
#' @param save_filename (optional) If specified, the animation will be saved to a gif with this filename. (default is not to save)
#' @param save_path (optional) If specified along with save_filename, will save the gif to save_path/save_filename. (defaults to working directory)
#' @return An animated line graph, which successively adds points along the time axis.
#' @export
animate_reveal_time_series = function(output,
                                                  parameters,
                                                  target=NULL,
                                                  external_target=NULL,
                                                  use_initial_points=TRUE,
                                                  use_weights=TRUE,
                                                  max_line_width=1,
                                                  alpha=0.1,
                                                  xlimits=NULL,
                                                  ylimits=NULL,
                                                  duration=NULL,
                                                  animate_plot=TRUE,
                                                  save_filename=NULL,
                                                  save_path=NULL)
{
  p = plot_time_series(output = output,
                             parameters = parameters,
                             target = target,
                             external_target = external_target,
                             use_initial_points = use_initial_points,
                             use_weights = use_weights,
                             max_line_width = max_line_width,
                             alpha = alpha,
                             xlimits = xlimits,
                             ylimits = ylimits)
  to_animate = p + gganimate::transition_reveal(Dimension)

  nframes = length(unique(output$Dimension))

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


#' Plot animated line graph showing parameter value vs dimension across targets from algorithm output.
#'
#' @param output Output from the SMC or EnK algorithm.
#' @param parameters The parameters we wish to be on the y-axis of the line graph.
#' @param target (optional) The target to plot. (default is to use all targets)
#' @param external_target (optional) The external target to plot. (default is to use all external targets, or to ignore if the column is not present)
#' @param use_initial_points (optional) If target is not specified and this argument is TRUE, will add the initial unweighted proposed points to the output to be plotted. (default is TRUE)
#' @param use_weights (optional) If FALSE, will ignore particle weights in the line graph. If TRUE, will use the particle weights. (defaults to TRUE)
#' @param max_line_width (optional) The maximum size of the points in the plot. (default=1)
#' @param alpha (optional) The transparency of the lines in the plot. (default=0.1)
#' @param xlimits (optional) Input of the form c(start,end), which specifies the ends of the x-axis.
#' @param ylimits (optional) Input of the form c(start,end), which specifies the ends of the y-axis.
#' @param duration (optional) The duration of the animation. (defaults to producing an animation that uses 10 frames per second)
#' @param animate_plot (optiional) If TRUE, will return an animation. If FALSE, returns a gganim object that can be furher modified before animating. (defaults to FALSE)
#' @param save_filename (optional) If specified, the animation will be saved to a gif with this filename. (default is not to save)
#' @param save_path (optional) If specified along with save_filename, will save the gif to save_path/save_filename. (defaults to working directory)
#' @return An animated line graph, showing how the lines evolve through the sequence of targets.
#' @export
animate_time_series = function(output,
                                           parameters,
                                           target=NULL,
                                           external_target=NULL,
                                           use_initial_points=TRUE,
                                           use_weights=TRUE,
                                           max_line_width=1,
                                           alpha=0.1,
                                           xlimits=NULL,
                                           ylimits=NULL,
                                           duration=NULL,
                                           animate_plot=TRUE,
                                           save_filename=NULL,
                                           save_path=NULL)
{
  p = plot_time_series(output = output,
                             parameters = parameters,
                             target = target,
                             external_target = external_target,
                             use_initial_points = use_initial_points,
                             use_weights = use_weights,
                             max_line_width = max_line_width,
                             alpha = alpha,
                             xlimits = xlimits,
                             ylimits = ylimits)

  if (!is.null(external_target) && is.null(target))
  {
    to_animate = p + gganimate::transition_manual(Target)
    nframes = length(unique(output$Target))
  }
  else if (is.null(external_target) && !is.null(target))
  {
    if ("ExternalTarget" %in% names(output))
    {
      to_animate = p + gganimate::transition_manual(ExternalTarget)
      nframes = length(unique(output$ExternalTarget))
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
      to_animate = p + gganimate::transition_manual(Target)
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
