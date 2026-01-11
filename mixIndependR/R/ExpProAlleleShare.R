#'Calculate the Expected Probability of 0,1 and 2 Shared Alleles###
#'@details This function Calculates the Expected Probability of 0,1 and 2 Shared Alleles for a set of loci. Usually followed by write.csv(as.data.frame(y),file = "~/*.csv") to export the result of a n x3 matrix.
#'@usage ExpProAlleleShare(p)
#'@param p a matrix/double of frequency of alleles; Outcome of "AlleleFreq". Each column denotes each locus. Different alleles is ordered in different rows such as 11,11.3,12,12.2,13... and so on
#'@return a matrix/double of expected probabilities of 0,1 and 2 shared alleles for each locus. Each row denotes each locus. The first column denotes the probability of 0 shared alleles, the second denotes 1 shared allele, the third denotes 2 shared alleles.
#'@references Weir, B. S. (2004, ISSN:0022-1198)
#'@export
#'@examples
#'a0<-matrix(runif(20),nrow=5)
#'a1<-colSums(a0)
#'a<-data.frame(STR1=a0[,1]/a1[1],STR2=a0[,2]/a1[2],STR3=a0[,3]/a1[3],STR4=a0[,4]/a1[4])
#'ExpProAlleleShare(a)
#'

ExpProAlleleShare <- function(p)

{
  p1 <- colSums(p,na.rm = TRUE,dims =1)
  p2 <- colSums(p*p,na.rm = TRUE,dims =1)
  p3 <- colSums(p*p*p,na.rm = TRUE,dims =1)
  p4 <- colSums(p*p*p*p,na.rm = TRUE,dims =1)

  P0 <- 1-4*p2+4*p3+2*p2*p2-3*p4
  P1 <- 4*p2-4*p3-4*p2*p2+4*p4
  P2 <- 2*p2*p2-p4

  return(cbind(P0,P1,P2))
  print(cbind(P0,P1,P2))
}
