param_convert  <- function(x, param)
{
  parameter_definitions <- NULL
  rm(parameter_definitions)
  data('parameter_definitions',envir=environment())
  
  i <- which(parameter_definitions$param == param)
  if (length(i) < 1) stop("selected parameter not found! please select valid parameter label:\n\n",paste(paste(parameter_definitions$param,"\t",parameter_definitions$name),collapse='\n'))
  
  param_def <- parameter_definitions[i,]  

  x[x == param_def$land_dc] <- NaN # set landmask
  x[x > param_def$max] <- NA # set clouds
  x2 <- switch(param_def$log+1, param_def$a*x +param_def$b, 10^(x*param_def$a +param_def$b)+param_def$c)
  return(x2)
}