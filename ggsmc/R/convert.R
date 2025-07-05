Target = ExternalTarget = ParameterName = Parameter = Dimension = Value = Target = LogWeight = Iteration = ExternalIndex = Particle = Chain = NULL

#' Convert IS, SMC or EnK output stored as a matrix to tidy format.
#'
#' @param output Matrix output (one point per row) from an IS algorithm, or one target from a SMC or EnK algorithm.
#' @param parameter The name to assign the parameter in the tidy output.
#' @param target (optional) The target index to use in the tidy output (default 1).
#' @param log_weights (optional) The log_weights to use in the tidy output (default all equal).
#' @return The output in tidy format.
#' @export
matrix2tidy = function(output,
                       parameter,
                       target=1,
                       log_weights=NULL)
{
  n = nrow(output)
  d = ncol(output)

  if (is.null(log_weights))
  {
    log_weights = rep(-log(n),n*d)
  }
  else
  {
    if (length(log_weights)!=n)
    {
      stop('"log_weights" needs to be of the same length as the number of rows in "output"')
    }
    log_weights = unlist(lapply(1:n,FUN=function(i) { rep(log_weights[i],d) }))
  }

  targets = rep(target,n*d)

  particles = unlist(lapply(1:n,FUN=function(i) { rep(i,d) }))

  parameters = rep(parameter,n*d)

  dimensions = unlist(lapply(1:n,FUN=function(i) { 1:d }))

  values = unlist(lapply(1:n,FUN=function(i) { output[i,] }))

  tidy_output = as.data.frame(cbind(targets,log_weights,particles,parameters,dimensions,values))

  names(tidy_output) = c("Target","LogWeight","Particle","ParameterName","Dimension","Value")

  tidy_output$Target = as.integer(tidy_output$Target)
  tidy_output$LogWeight = as.numeric(tidy_output$LogWeight)
  tidy_output$Particle = as.integer(tidy_output$Particle)
  tidy_output$Dimension = as.integer(tidy_output$Dimension)
  tidy_output$Value = as.numeric(tidy_output$Value)

  return(tidy_output)
}
