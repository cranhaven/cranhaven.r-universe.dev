AncestorValue = Target = ExternalTarget = ParameterName = Parameter = Dimension = Value = Target = LogWeight = Iteration = ExternalIndex = Particle = Chain = NULL

#' Plot an SMC or EnK genealogy from algorithm output.
#'
#' @param output Output from the SMC or EnK algorithm.
#' @param parameter The parameter we wish to see the evolution of.
#' @param dimension (optional) The dimension of the parameter we wish to see the evolution of. (default is 1)
#' @param target (optional) The target to plot. (default is to use all targets)
#' @param external_target (optional) The external target to plot. (default is to use all external targets, or to ignore if the column is not present)
#' @param use_initial_points (optional) If target is not specified and this argument is TRUE, will add the initial unweighted proposed points to the output to be plotted. (default is TRUE)
#' @param use_weights (optional) If FALSE, will ignore particle weights in the line graph. If TRUE, will use the particle weights. (defaults to TRUE)
#' @param alpha_points (optional) The transparency of the points in the plot. (default=0.1)
#' @param alpha_lines (optional) The transparency of the lines in the plot. (default=0.1)
#' @param axis_limits (optional) Input of the form c(start,end), which specifies the ends of the parameter axis.
#' @param vertical (optional) If TRUE (default), plots a genealogy vertically. If FALSE, plots horizontally.
#' @param arrows (optional) If TRUE (default), includes arrowheads. If FALSE, arrowheads are omitted.
#' @param default_title (optional) If TRUE, will provide a default title for the figure. If FALSE, no title is used. (defaults to FALSE)
#' @return A particle genealogy in a ggplot figure.
#' @export
plot_genealogy = function(output,
                     parameter,
                     dimension=1,
                     target=NULL,
                     external_target=NULL,
                     use_initial_points=TRUE,
                     use_weights=TRUE,
                     alpha_points=0.1,
                     alpha_lines=0.1,
                     axis_limits=NULL,
                     vertical=TRUE,
                     arrows=TRUE,
                     default_title=FALSE)
{
  output_to_use = output

  # Either we will iterate over target or external_target

  if (!is.null(target))
  {
    # In this case we will iterate over external_target, so chosen target variable must be present in data, and external_target must be in the data.

    if ( (!("Target" %in% names(output))) || (!(target %in% output$Target)) )
    {
      stop("To show genealogy for specified target, target variable must be present in data.")
    }

    if (!("ExternalTarget" %in% names(output)))
    {
      stop("To show genealogy over external_target, external_target variable must be present in data.")
    }
  }
  else
  {
    # In this case we will iterate over target, so external_target variable must either not be present in data, or specified as fixed.

    if ("ExternalTarget" %in% names(output))
    {
      if ( (is.null(external_target)) || (!(external_target %in% output$ExternalTarget)) )
      {
        stop("To show genealogy over targets, if external target is found in data, you must choose a fixed value for it.")
      }
    }

  }

  if ( ("ExternalIndex" %in% names(output)) && (length(unique(output$ExternalIndex))>1) )
  {
    stop("If ExternalIndex is in data, it can only take one value.")
  }

  target_data = extract_target_data(output,target,external_target,use_initial_points)
  output_to_use = target_data[[1]]
  target_parameters = target_data[[2]]

  if (length(unique(output$Dimension))==1)
  {
    parameter_for_plot = parameter
    if (is.null(target) || (target_parameters=="") )
    {
      default_title_for_plot = bquote("The sampling genealogy of"~.(parameter))
    }
    else
    {
      default_title_for_plot = bquote("The sampling genealogy of"~.(parameter)~"("*.(target_parameters)*")")
    }
  }
  else
  {
    if (is.null(target) || (target_parameters=="") )
    {
      default_title_for_plot = bquote("The sampling genealogy of"~.(parameter)[.(dimension)])
    }
    else
    {
      default_title_for_plot = bquote("The sampling genealogy of"~.(parameter)[.(dimension)]~"("*.(target_parameters)*")")
    }
    parameter_for_plot = bquote(.(parameter)[.(dimension)])
  }

  output_to_use = poorman::filter(poorman::filter(output_to_use,ParameterName==parameter),Dimension==dimension)

  if (!("AncestorIndex" %in% names(output_to_use)))
  {
    output_to_use$AncestorIndex = output_to_use$Particle
  }

  if (is.null(target))
  {
    # Iterating over target.
    output_to_use$AncestorValue = unlist(lapply(unique(output_to_use$Target),FUN = function(i) { if (i==min(output_to_use$Target)) { return(poorman::filter(output_to_use,Target==i)$Value) } else { previous_target = poorman::filter(output_to_use,Target==(i-1)); current_target = poorman::filter(output_to_use,Target==i); return(previous_target$Value[current_target$AncestorIndex]) } } ))

  }

  if ("Iteration" %in% names(output_to_use))
  {
    if ( ("LogWeight" %in% names(output)) && (use_weights==TRUE) )
    {
      if ("ExternalIndex" %in% names(output_to_use))
      {
        if ("ExternalTarget" %in% names(output_to_use))
        {
          if (vertical)
          {
            plot = ggplot2::ggplot(output_to_use, ggplot2::aes(y=Target,
                                                               x=Value,
                                                               group=interaction(Iteration, Particle, ExternalIndex, ExternalTarget, Target, ParameterName),
                                                               size=exp(LogWeight),
                                                               stroke=0))
          }
          else
          {
            plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Target,
                                                               y=Value,
                                                               group=interaction(Iteration, Particle, ExternalIndex, ExternalTarget, Target, ParameterName),
                                                               size=exp(LogWeight),
                                                               stroke=0))
          }
        }
        else
        {
          if (vertical)
          {
            plot = ggplot2::ggplot(output_to_use, ggplot2::aes(y=Target,
                                                               x=Value,
                                                               group=interaction(Iteration, Particle, ExternalIndex, Target, ParameterName),
                                                               size=exp(LogWeight),
                                                               stroke=0))
          }
          else
          {
            plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Target,
                                                               y=Value,
                                                               group=interaction(Iteration, Particle, ExternalIndex, Target, ParameterName),
                                                               size=exp(LogWeight),
                                                               stroke=0))
          }
        }
      }
      else
      {
        if ("ExternalTarget" %in% names(output_to_use))
        {
          if (vertical)
          {
            plot = ggplot2::ggplot(output_to_use, ggplot2::aes(y=Target,
                                                               x=Value,
                                                               group=interaction(Iteration, Particle, ExternalTarget, Target, ParameterName),
                                                               size=exp(LogWeight),
                                                               stroke=0))
          }
          else
          {
            plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Target,
                                                               y=Value,
                                                               group=interaction(Iteration, Particle, ExternalTarget, Target, ParameterName),
                                                               size=exp(LogWeight),
                                                               stroke=0))
          }
        }
        else
        {
          if (vertical)
          {
            plot = ggplot2::ggplot(output_to_use, ggplot2::aes(y=Target,
                                                               x=Value,
                                                               group=interaction(Iteration, Particle, Target, ParameterName),
                                                               size=exp(LogWeight),
                                                               stroke=0))
          }
          else
          {
            plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Target,
                                                               y=Value,
                                                               group=interaction(Iteration, Particle, Target, ParameterName),
                                                               size=exp(LogWeight),
                                                               stroke=0))
          }
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
            if (vertical)
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(y=Target,
                                                                 x=Value,
                                                                 group=interaction(Iteration, Particle, ExternalIndex, Target, ExternalTarget, ParameterName)))
            }
            else
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Target,
                                                                 y=Value,
                                                                 group=interaction(Iteration, Particle, ExternalIndex, Target, ExternalTarget, ParameterName)))
            }
          }
          else
          {
            if (vertical)
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(y=Target,
                                                                 x=Value,
                                                                 group=interaction(Iteration, Particle, ExternalIndex, Target, ParameterName)))
            }
            else
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Target,
                                                                 y=Value,
                                                                 group=interaction(Iteration, Particle, ExternalIndex, Target, ParameterName)))
            }

            #plot + ggplot2::geom_point() + ggplot2::geom_segment(data=poorman::filter(output_to_use,Target!=min(Target)),ggplot2::aes(x = Target-1, y = AncestorValue, xend = Target, yend = Value))
          }
        }
        else
        {
          if ("ExternalTarget" %in% names(output_to_use))
          {
            if (vertical)
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(y=Target,
                                                                 x=Value,
                                                                 group=interaction(Iteration, Particle, Target, ExternalTarget, ParameterName)))
            }
            else
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Target,
                                                                 y=Value,
                                                                 group=interaction(Iteration, Particle, Target, ExternalTarget, ParameterName)))
            }
          }
          else
          {
            if (vertical)
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(y=Target,
                                                                 x=Value,
                                                                 group=interaction(Iteration, Particle, Target, ParameterName)))
            }
            else
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Target,
                                                                 y=Value,
                                                                 group=interaction(Iteration, Particle, Target, ParameterName)))
            }

            #plot + ggplot2::geom_point() + ggplot2::geom_segment(data=poorman::filter(output_to_use,Target!=min(Target)),ggplot2::aes(x = Target-1, y = AncestorValue, xend = Target, yend = Value))
          }
        }
      }
      else if ("Chain" %in% names(output_to_use))
      {
        if ("ExternalIndex" %in% names(output_to_use))
        {
          if ("ExternalTarget" %in% names(output_to_use))
          {
            if (vertical)
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(y=Target,
                                                                 x=Value,
                                                                 group=interaction(Iteration, Chain, ExternalIndex, Target, ExternalTarget, ParameterName)))
            }
            else
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Target,
                                                                 y=Value,
                                                                 group=interaction(Iteration, Chain, ExternalIndex, Target, ExternalTarget, ParameterName)))
            }
          }
          else
          {
            if (vertical)
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(y=Target,
                                                                 x=Value,
                                                                 group=interaction(Iteration, Chain, ExternalIndex, Target, ParameterName)))
            }
            else
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Target,
                                                                 y=Value,
                                                                 group=interaction(Iteration, Chain, ExternalIndex, Target, ParameterName)))
            }

            #plot + ggplot2::geom_point() + ggplot2::geom_segment(data=poorman::filter(output_to_use,Target!=min(Target)),ggplot2::aes(x = Target-1, y = AncestorValue, xend = Target, yend = Value))
          }
        }
        else
        {
          if ("ExternalTarget" %in% names(output_to_use))
          {
            if (vertical)
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(y=Target,
                                                                 x=Value,
                                                                 group=interaction(Iteration, Chain, Target, ExternalTarget, ParameterName)))
            }
            else
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Target,
                                                                 y=Value,
                                                                 group=interaction(Iteration, Chain, Target, ExternalTarget, ParameterName)))
            }
          }
          else
          {
            if (vertical)
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(y=Target,
                                                                 x=Value,
                                                                 group=interaction(Iteration, Chain, Target, ParameterName)))
            }
            else
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Target,
                                                                 y=Value,
                                                                 group=interaction(Iteration, Chain, Target, ParameterName)))
            }

            #plot + ggplot2::geom_point() + ggplot2::geom_segment(data=poorman::filter(output_to_use,Target!=min(Target)),ggplot2::aes(x = Target-1, y = AncestorValue, xend = Target, yend = Value))
          }
        }
      }
      else
      {
        stop('output must contain a column named either "Particle" or "Chain".')
      }
    }
  }
  else
  {
    if ( ("LogWeight" %in% names(output)) && (use_weights==TRUE) )
    {
      if ("ExternalIndex" %in% names(output_to_use))
      {
        if ("ExternalTarget" %in% names(output_to_use))
        {
          if (vertical)
          {
            plot = ggplot2::ggplot(output_to_use, ggplot2::aes(y=Target,
                                                               x=Value,
                                                               group=interaction(Particle, ExternalIndex, ExternalTarget, Target, ParameterName),
                                                               size=exp(LogWeight),
                                                               stroke=0))
          }
          else
          {
            plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Target,
                                                               y=Value,
                                                               group=interaction(Particle, ExternalIndex, ExternalTarget, Target, ParameterName),
                                                               size=exp(LogWeight),
                                                               stroke=0))
          }
        }
        else
        {
          if (vertical)
          {
            plot = ggplot2::ggplot(output_to_use, ggplot2::aes(y=Target,
                                                               x=Value,
                                                               group=interaction(Particle, ExternalIndex, Target, ParameterName),
                                                               size=exp(LogWeight),
                                                               stroke=0))
          }
          else
          {
            plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Target,
                                                               y=Value,
                                                               group=interaction(Particle, ExternalIndex, Target, ParameterName),
                                                               size=exp(LogWeight),
                                                               stroke=0))
          }
        }
      }
      else
      {
        if ("ExternalTarget" %in% names(output_to_use))
        {
          if (vertical)
          {
            plot = ggplot2::ggplot(output_to_use, ggplot2::aes(y=Target,
                                                               x=Value,
                                                               group=interaction(Particle, ExternalTarget, Target, ParameterName),
                                                               size=exp(LogWeight),
                                                               stroke=0))
          }
          else
          {
            plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Target,
                                                               y=Value,
                                                               group=interaction(Particle, ExternalTarget, Target, ParameterName),
                                                               size=exp(LogWeight),
                                                               stroke=0))
          }
        }
        else
        {
          if (vertical)
          {
            plot = ggplot2::ggplot(output_to_use, ggplot2::aes(y=Target,
                                                               x=Value,
                                                               group=interaction(Particle, Target, ParameterName),
                                                               size=exp(LogWeight),
                                                               stroke=0))
          }
          else
          {
            plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Target,
                                                               y=Value,
                                                               group=interaction(Particle, Target, ParameterName),
                                                               size=exp(LogWeight),
                                                               stroke=0))
          }
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
            if (vertical)
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(y=Target,
                                                                 x=Value,
                                                                 group=interaction(Particle, ExternalIndex, Target, ExternalTarget, ParameterName)))
            }
            else
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Target,
                                                                 y=Value,
                                                                 group=interaction(Particle, ExternalIndex, Target, ExternalTarget, ParameterName)))
            }
          }
          else
          {
            if (vertical)
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(y=Target,
                                                                 x=Value,
                                                                 group=interaction(Particle, ExternalIndex, Target, ParameterName)))
            }
            else
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Target,
                                                                 y=Value,
                                                                 group=interaction(Particle, ExternalIndex, Target, ParameterName)))
            }

            #plot + ggplot2::geom_point() + ggplot2::geom_segment(data=poorman::filter(output_to_use,Target!=min(Target)),ggplot2::aes(x = Target-1, y = AncestorValue, xend = Target, yend = Value))
          }
        }
        else
        {
          if ("ExternalTarget" %in% names(output_to_use))
          {
            if (vertical)
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(y=Target,
                                                                 x=Value,
                                                                 group=interaction(Particle, Target, ExternalTarget, ParameterName)))
            }
            else
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Target,
                                                                 y=Value,
                                                                 group=interaction(Particle, Target, ExternalTarget, ParameterName)))
            }
          }
          else
          {
            if (vertical)
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(y=Target,
                                                                 x=Value,
                                                                 group=interaction(Particle, Target, ParameterName)))
            }
            else
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Target,
                                                                 y=Value,
                                                                 group=interaction(Particle, Target, ParameterName)))
            }

            #plot + ggplot2::geom_point() + ggplot2::geom_segment(data=poorman::filter(output_to_use,Target!=min(Target)),ggplot2::aes(x = Target-1, y = AncestorValue, xend = Target, yend = Value))
          }
        }
      }
      else if ("Chain" %in% names(output_to_use))
      {
        if ("ExternalIndex" %in% names(output_to_use))
        {
          if ("ExternalTarget" %in% names(output_to_use))
          {
            if (vertical)
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(y=Target,
                                                                 x=Value,
                                                                 group=interaction(Chain, ExternalIndex, Target, ExternalTarget, ParameterName)))
            }
            else
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Target,
                                                                 y=Value,
                                                                 group=interaction(Chain, ExternalIndex, Target, ExternalTarget, ParameterName)))
            }
          }
          else
          {
            if (vertical)
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(y=Target,
                                                                 x=Value,
                                                                 group=interaction(Chain, ExternalIndex, Target, ParameterName)))
            }
            else
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Target,
                                                                 y=Value,
                                                                 group=interaction(Chain, ExternalIndex, Target, ParameterName)))
            }

            #plot + ggplot2::geom_point() + ggplot2::geom_segment(data=poorman::filter(output_to_use,Target!=min(Target)),ggplot2::aes(x = Target-1, y = AncestorValue, xend = Target, yend = Value))
          }
        }
        else
        {
          if ("ExternalTarget" %in% names(output_to_use))
          {
            if (vertical)
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(y=Target,
                                                                 x=Value,
                                                                 group=interaction(Chain, Target, ExternalTarget, ParameterName)))
            }
            else
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Target,
                                                                 y=Value,
                                                                 group=interaction(Chain, Target, ExternalTarget, ParameterName)))
            }
          }
          else
          {
            if (vertical)
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(y=Target,
                                                                 x=Value,
                                                                 group=interaction(Chain, Target, ParameterName)))
            }
            else
            {
              plot = ggplot2::ggplot(output_to_use, ggplot2::aes(x=Target,
                                                                 y=Value,
                                                                 group=interaction(Chain, Target, ParameterName)))
            }

            #plot + ggplot2::geom_point() + ggplot2::geom_segment(data=poorman::filter(output_to_use,Target!=min(Target)),ggplot2::aes(x = Target-1, y = AncestorValue, xend = Target, yend = Value))
          }
        }
      }
      else
      {
        stop('output must contain a column named either "Particle" or "Chain".')
      }
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

  if (arrows)
  {
    if (vertical)
    {
      plot = plot +
        ggplot2::geom_point(alpha=alpha_points) + ggplot2::geom_segment(data=poorman::filter(output_to_use,Target!=min(Target)),inherit.aes=FALSE,ggplot2::aes(y = Target-1, x = AncestorValue, yend = Target, xend = Value),alpha=alpha_lines,arrow=grid::arrow(type="closed",length = grid::unit(.1, "inches"))) +
        ggplot2::xlab(parameter_for_plot)
    }
    else
    {
      plot = plot +
        ggplot2::geom_point(alpha=alpha_points) + ggplot2::geom_segment(data=poorman::filter(output_to_use,Target!=min(Target)),inherit.aes=FALSE,ggplot2::aes(x = Target-1, y = AncestorValue, xend = Target, yend = Value),alpha=alpha_lines,arrow=grid::arrow(type="closed",length = grid::unit(.1, "inches"))) +
        ggplot2::ylab(parameter_for_plot)
    }
  }
  else
  {
    if (vertical)
    {
      plot = plot +
        ggplot2::geom_point(alpha=alpha_points) + ggplot2::geom_segment(data=poorman::filter(output_to_use,Target!=min(Target)),inherit.aes=FALSE,ggplot2::aes(y = Target-1, x = AncestorValue, yend = Target, xend = Value),alpha=alpha_lines) +
        ggplot2::xlab(parameter_for_plot)
    }
    else
    {
      plot = plot +
        ggplot2::geom_point(alpha=alpha_points) + ggplot2::geom_segment(data=poorman::filter(output_to_use,Target!=min(Target)),inherit.aes=FALSE,ggplot2::aes(x = Target-1, y = AncestorValue, xend = Target, yend = Value),alpha=alpha_lines) +
        ggplot2::ylab(parameter_for_plot)
    }
  }

  if (default_title)
  {
    plot = plot + ggplot2::labs(title=default_title_for_plot)
  }

  if ( (!is.null(axis_limits)) && (is.numeric(axis_limits) && (is.vector(axis_limits)) ) )
  {
    if (vertical)
    {
      plot = plot + ggplot2::xlim(axis_limits[1],axis_limits[2])
      plot = plot + ggplot2::scale_y_continuous(breaks = integer_breaks())
    }
    else
    {
      plot = plot + ggplot2::ylim(axis_limits[1],axis_limits[2])
      plot = plot + ggplot2::scale_x_continuous(breaks = integer_breaks())
    }
  }

  if (vertical)
  {
    plot = plot + ggplot2::scale_y_reverse()
  }

  plot = plot + ggplot2::theme(legend.position = "none")

  # plot = plot +
  #   ggplot2::guides(linewidth = "none") +
  #   ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(alpha = 1,linewidth=1)))

  return(plot)
}

#' @param output Output from the SMC or EnK algorithm.
#' @param parameter The parameter we wish to see the evolution of.
#' @param dimension (optional) The dimension of the parameter we wish to see the evolution of. (default is 1)
#' @param target (optional) The target to plot. (default is to use all targets)
#' @param external_target (optional) The external target to plot. (default is to use all external targets, or to ignore if the column is not present)
#' @param use_initial_points (optional) If target is not specified and this argument is TRUE, will add the initial unweighted proposed points to the output to be plotted. (default is TRUE)
#' @param use_weights (optional) If FALSE, will ignore particle weights in the line graph. If TRUE, will use the particle weights. (defaults to TRUE)
#' @param alpha_points (optional) The transparency of the points in the plot. (default=0.1)
#' @param alpha_lines (optional) The transparency of the lines in the plot. (default=0.1)
#' @param axis_limits (optional) Input of the form c(start,end), which specifies the ends of the parameter axis.
#' @param vertical (optional) If TRUE (default), plots a genealogy vertically. If FALSE, plots horizontally.
#' @param arrows (optional) If TRUE (default), includes arrowheads. If FALSE, arrowheads are omitted.
#' @param default_title (optional) If TRUE, will provide a default title for the figure. If FALSE, no title is used. (defaults to FALSE)
#' @param duration (optional) The duration of the animation. (defaults to producing an animation that uses 10 frames per second)
#' @param animate_plot (optional) If TRUE, will return an animation. If FALSE, returns a gganim object that can be furher modified before animating. (defaults to FALSE)
#' @param save_filename (optional) If specified, the animation will be saved to a gif with this filename. (default is not to save)
#' @param save_path (optional) If specified along with save_filename, will save the gif to save_path/save_filename. (defaults to working directory)
#' @return An animated line graph, which successively adds points along the time axis.
#' @export
# animated_reveal_genealogy = function(output,
#                                      parameter,
#                                      dimension=1,
#                                      target=NULL,
#                                      external_target=NULL,
#                                      use_initial_points=TRUE,
#                                      use_weights=TRUE,
#                                      max_line_width=1,
#                                      alpha=0.1,
#                                      axis_limits=NULL,
#                                      vertical=TRUE,
#                                      arrows=TRUE,
#                                      default_title=FALSE,
#                                      duration=NULL,
#                                      animate_plot=TRUE,
#                                      save_filename=NULL,
#                                      save_path=NULL)
# {
#   p = genealogy(output = output,
#                 parameter = parameter,
#                 target = target,
#                 external_target = external_target,
#                 use_initial_points = use_initial_points,
#                 use_weights = use_weights,
#                 alpha_points = alpha_points,
#                 alpha_lines = alpha_lines,
#                 axis_limits = axis_limits,
#                 vertical = vertical,
#                 arrows = arrows,
#                 default_title = default_title)
#   to_animate = p + gganimate::transition_reveal(Target)
#
#   nframes = length(unique(output$Target))
#
#   if (animate_plot)
#   {
#     if (is.null(duration))
#     {
#       animated = gganimate::animate(to_animate,nframes=nframes)
#     }
#     else
#     {
#       animated = gganimate::animate(to_animate,nframes=nframes,duration=duration)
#     }
#
#     if (!is.null(save_filename))
#     {
#       if (is.null(save_path))
#       {
#         gganimate::anim_save(filename=save_filename,animation=animated)
#       }
#       else
#       {
#         gganimate::anim_save(filename=save_filename,animation=animated,path=save_path)
#       }
#     }
#
#     return(animated)
#   }
#   else
#   {
#     if (!is.null(save_filename))
#     {
#       stop("Cannot save, since plot not animated.")
#     }
#     else
#     {
#       return(to_animate)
#     }
#   }
# }
