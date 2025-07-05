AncestorValue = Target = ExternalTarget = ParameterName = Parameter = Dimension = Value = Target = LogWeight = Iteration = ExternalIndex = Particle = Chain = NULL

add_proposed_points <- function(output)
{
  min_target = min(output$Target)
  proposed_points = poorman::filter(output,Target==min_target)
  if ("LogWeight" %in% names(output))
  {
    proposed_points$LogWeight = matrix(-log(nrow(proposed_points)),nrow(proposed_points))
  }
  proposed_points$Target = as.integer(matrix(min_target-1,nrow(proposed_points)))
  if ("TargetParameters" %in% names(output))
  {
    proposed_points$TargetParameters = matrix("proposal",nrow(proposed_points))
  }
  return(rbind(proposed_points,output))
}

extract_target_data = function(output,
                               target,
                               external_target,
                               use_initial_points)
{
  output_to_use = output

  if (!is.null(external_target))
  {
    if ("ExternalTargetParameters" %in% names(output))
    {
      target_parameters = poorman::filter(output,ExternalTarget==external_target)$ExternalTargetParameters[1]
    }
    else
    {
      target_parameters = ""
    }
    output_to_use = poorman::filter(output,ExternalTarget==external_target)
  }
  else
  {
    target_parameters = ""
  }

  if (!is.null(target))
  {
    if ("TargetParameters" %in% names(output_to_use))
    {
      if (target_parameters!="")
      {
        target_parameters = paste(target_parameters,",",sep="")
      }

      target_parameters = paste(target_parameters,poorman::filter(output_to_use,Target==target)$TargetParameters[1],sep="")
    }
    else
    {
      target_parameters = paste(target_parameters,"",sep="")
    }
    output_to_use = poorman::filter(output_to_use,Target==target)
  }
  else
  {
    if (use_initial_points)
    {
      if ("Target" %in% names(output))
        output_to_use = add_proposed_points(output_to_use)
      else
        stop("extract_target_data - Target column must be in the data in order to add proposed points to the plot.")
    }
  }

  return(list(output_to_use,target_parameters))
}


# From https://joshuacook.netlify.app/post/integer-values-ggplot-axis/
integer_breaks <- function(n = 5, ...)
{
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}
