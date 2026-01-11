#'Build Expected Distribution of Numbers of Heterozygous Loci
#'@details This function build the expected distribution of numbers of heterozygous loci for known heterozygosity of each loci.
#'@usage DistHetero(H)
#'@param H a vector of average heterozygosity of each locus
#'@return a dataframe of expected density on each possible total number of heterozygous loci.
#'@references Chakraborty, R. (1981, ISSN:0016-6731)
#'@export
#'@examples
#'DistHetero(runif(10))
#'
DistHetero <- function(H)
{
  N <- length(H)
  H_ <- 1-H

  P1 <- c(H_[1],H[1])

  P <- mat.or.vec(N,N+1)
  P[1,1] <- P1[1]
  P[1,2] <- P1[2]


  for (r in 2:N)
  {
    P[r,1] <- P[r-1,1]*H_[r]
    for (k in 2:N)
    {
      P[r,k] <- P[r-1,k]*H_[r]+P[r-1,k-1]*H[r]
    }
    P[r,N+1] <-P[r-1,N]*H[r]
  }
  return(data.frame("K"=c(0:N),"Density"=(P[N,])))
}

