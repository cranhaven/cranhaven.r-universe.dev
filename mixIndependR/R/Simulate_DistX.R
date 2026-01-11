#'Build a simulated distribution for No. of Shared Alleles
#'@details This function generates multinomial distribution for loci known the Allele Frequency and Expected Probability of Shared 2,1 or 0 alleles
#'@usage Simulate_DistX(e,m,t)
#'@param e a matrix of Probability of Sharing 2,1 or 0 alleles at each loci. Each row denotes each locus. Three columns denote sharing 0,1 or 2 alleles.
#'@param m the sample size you want, usually similar to the real sample size.
#'@param t the number of samples you want to build/ the times to generate a sample
#'@return a matrix of frequencies of No. of shared alleles. Each row denotes each simulated sample; Each column denotes each No. of shared alleles, from 0 to 2e length of e.
#'@export
#'@examples
#'e0<-data.frame("P0"=runif(5,min = 0,max = 0.5),"P1"=runif(5,0,0.5))
#'e<-data.frame(e0,"P2"=1-rowSums(e0))
#'Simulate_DistX(e,500,10)
#'

Simulate_DistX <- function(e,m,t){
  OneDist <- function(e,m){
    OneSample <- sapply(data.frame(t(e)),function(x){sample(c(0,1,2),m,replace = T,prob = x)})
    s<-2*nrow(e)
    return(sapply(c(0:s), counta,z=rowSums(OneSample)))
  }
  output<-t(replicate(t,OneDist(e,m)))
  s<-2*nrow(e)
  colnames(output) <- c(0:s)
  return(output)
}
