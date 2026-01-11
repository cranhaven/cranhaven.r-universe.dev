#'Generate a Bundle of Simulated distributions for No. of heterozygous loci with known heterozygosites
#'@details This function generates multinomial distribution for loci known the heterozygosity and build the simulated distribution for no. of heterozygous loci.
#'@importFrom stats rbinom
#'@usage Simulate_DistK(H,m,t)
#'@param H a vector of average heterozygosity of each loci. Length of H is the number of loci.
#'@param m the sample size you want, usually similar to the real sample size.
#'@param t the number of samples you want to build
#'@return a matrix of frequencies of No. of Heterozygous Loci. Each row denotes each simulated sample; Each column denotes each No. of Heterozygous loci, from 0 to length of H.
#'@export
#'@examples
#'Simulate_DistK(runif(10),500,100)
#'

Simulate_DistK <- function(H,m,t){
  OneDist <- function(H,m){
    n<- length(H)
    onetrial<-rowSums(t(replicate(m,rbinom(n,1,H))))
    return(sapply(c(0:n), counta,z=onetrial))
  }
  output <-t(replicate(t,OneDist(H,m)))
  colnames(output) <- c(0:length(H))
  return(output)
}
