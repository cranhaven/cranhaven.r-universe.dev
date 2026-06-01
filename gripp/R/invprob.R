# Function to solve the Inverse Problem
#
#' @export
invprob <- function(parm_init)
#' @importFrom utils write.table read.csv write.csv
#' @importFrom GenSA GenSA
#' @importFrom GA ga
#' @importFrom stats rnorm
{
 if (.GlobalEnv$solver=='GenSA'){
  out_inv <- GenSA(par = parm_init, lower = .GlobalEnv$parm_min, upper = .GlobalEnv$parm_max, fn=cost, control = .GlobalEnv$control)
  return(out_inv[c('value','par','counts')])
 }
 if (.GlobalEnv$solver=='GA')
 {
  out_inv <- ga(type = 'real-valued',fitness = function (parm) - cost(parm), lower = .GlobalEnv$parm_min, upper = .GlobalEnv$parm_max,popSize = as.numeric(.GlobalEnv$control['popSize']),maxiter = as.numeric(.GlobalEnv$control['maxiter']), run = as.numeric(.GlobalEnv$control['run']))
  return(out_inv)
 }
}

