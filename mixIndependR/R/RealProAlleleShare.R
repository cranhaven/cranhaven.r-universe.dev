#'Calculate the Real Probability of 0,1 and 2 Shared Alleles###
#'@details This function Calculates the density of 0,1 and 2 Shared Alleles for a set of loci. Usually followed by write.csv(as.data.frame(y),file = "~/*.csv") to export the result of a n x3 matrix.
#'@usage RealProAlleleShare(AS)
#'@param AS a matrix/double of no. of Shared alleles, made up with 0,1 and 2; Outcome of "AlleleShare_Table". Each column denotes each locus. Each row denotes each individual.
#'@return a matrix/double of real density of 0,1 and 2 shared alleles for each locus. Each row denotes each locus. The first column denotes the probability of 0 shared alleles, the second denotes 1 shared allele, the third denotes 2 shared alleles.
#'@export
#'@examples
#'AS<-matrix(sample(c(0:2),20,replace=TRUE,prob=c(0.3,0.3,0.4)),nrow=5)
#'RealProAlleleShare(AS)
RealProAlleleShare <-function(AS){
  pr<-t(rbind(sapply(AS,counta,y=0),sapply(AS,counta,y=1),sapply(AS,counta,y=2)))
  output <-pr/rowSums(pr)
  colnames(output) <- c("P0","P1","P2")
  return(output)
}
