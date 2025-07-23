param_unconvert  <- function(x, param)
{
  parameter_definitions <- NULL
  rm(parameter_definitions)
  data('parameter_definitions',envir=environment())
  
  i <- which(parameter_definitions$param == param)
  if (length(i) < 1) stop("selected parameter not found! please select valid parameter label:\n\n",paste(paste(parameter_definitions$param,"\t",parameter_definitions$name),collapse='\n'))
  
  param_def <- parameter_definitions[i,]  
#   param_def
  x[x > param_def$max] <- NA # clouds
  x[x > param_def$maxv & !is.na(x)] <- param_def$maxv
  x[x < param_def$minv & !is.na(x)] <- param_def$minv
  x2 <- switch(param_def$log+1, (x-param_def$b)/param_def$a, 10^(x*param_def$a +param_def$b)-param_def$c)
  x2[is.nan(x2)] <- parameter_definitions$land_dc[parameter_definitions$param == param] # set landmask
  x2[is.na(x2)] <- parameter_definitions$no_data_dc[parameter_definitions$param == param] # set clouds
  return(x2)
}