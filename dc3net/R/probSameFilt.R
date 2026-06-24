# Additional step to C3NET to work on probe level. This function assign zero to the probes of the same gene.

probSameFilt <- function(mim,genes)
{
  ngene <- length(genes)
  
  for(i in 1:ngene){
    indx <- which(genes==genes[i])
    if(length(indx)>1){mim[i, indx] <- 0} 
  } 
  return(mim)
}