#'Build Expected Distribution of Numbers of Shared Alleles
#'@details This function build the expected distribution of numbers of shared alleles for known shared alleles of each pair of individuals.
#'@usage DistAlleleShare(e)
#'@param e a matrix/dataframe of probability of shared alleles; outcome of "ExpProAlleleShare" or "RealProAlleleShare". Each row denotes each locus. The first column is the case of 0 shared alleles, the second column is the case of 1 shared alleles, the third column is the case of 2 shared alleles.
#'@return a dataframe of probabilities of each number of shared alleles(from 0 to 2*loci); the first column is No. of Shared Alleles; the Second Column is Expected Density
#'@export
#'@references Chakraborty, R., Stivers, D. N., Su, B., Zhong, Y., & Budowle, B. (1999) <doi:10.1002/(SICI)1522-2683(19990101)20:8<1682::AID-ELPS1682>3.0.CO;2-Z>
#'@examples
#'e0<-data.frame("P0"=runif(5,min = 0,max = 0.5),"P1"=runif(5,0,0.5))
#'e<-data.frame(e0,"P2"=1-rowSums(e0))
#'DistAlleleShare(e)

DistAlleleShare <- function(e)
{
  N <- nrow(e)
  P <- mat.or.vec(N,2*N+1)###generate the vetor/matrix for product####

  P[1,1] <- e[1,1]
  P[1,2] <- e[1,2]
  P[1,3] <- e[1,3]

  for (r in 2:N)
  {
    P[r,1] <- P[r-1,1]*e[r,1]
    P[r,2] <- P[r-1,1]*e[r,2]+P[r-1,2]*e[r,1]

    m <- 2*r-1

    for (k in 3:m)
    {
      P[r,k] <- P[r-1,k-2]*e[r,3]+P[r-1,k-1]*e[r,2]+P[r-1,k]*e[r,1]
    }
    P[r,2*r] <-P[r-1,2*r-2]*e[r,3]+P[r-1,2*r-1]*e[r,2]
    P[r,2*r+1] <- P[r-1,2*r-1]*e[r,3]
  }
  colnames(P)<-0:(ncol(P)-1)
  return(data.frame("X"=c(0:(2*N)),"Density"=P[N,]))
}
