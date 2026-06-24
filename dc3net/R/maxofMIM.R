# Find max partner for each gene.

maxofMIM <- function(mim)
{
  numprobs <- nrow(mim)
  maxM <- c()
  for(i in 1: numprobs){
    j <- which.max(mim[i,]) 
    tmp <- cbind(mim[i,j], i, j)
    maxM <- rbind(maxM,tmp)
  }
  return(maxM)
}