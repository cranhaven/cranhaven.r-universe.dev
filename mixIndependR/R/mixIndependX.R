#'Quick pvalue of total number of shared alleles
#'@details This function is a summary of pipeline for number of shared alleles(X), and generates the p-value of K for the target dataset.
#'@usage mixIndependX(x,sep,t,B)
#'@importFrom stats ecdf
#'@param x a dataset of alleles. Each row denotes each individual.One allele in one cell.In the (2r-1)th column, there is the same locus with the 2r-th column; noted: no column for ID, make row.names=1 when importing.
#'@param sep allele separator in the imported genotype data. Note: when using the special character like "|", remember to protect it as "\\|".
#'@param t times of simulation in "Simulate_DistK" and "Simulate_DistX".
#'@param B times of bootstrapping in Chi Squares Test.
#'@return pvalue (1-cumulative probabilities) for the number of shared alleles(K)
#'@export
#'@examples
#'x <- data.frame(SNP1=c("A|A","T|T","A|T","A|T"),
#'                 STR1=c("12|12","13|14","13|13","14|15"))
#'mixIndependX(x,sep="\\|",10,10)

mixIndependX <- function(x,sep="\\|",t,B){
  ss <- nrow(x)/2
  AS <- AlleleShare(x,sep,replacement = F)
  Obs_DistAlleleShare<-FreqAlleleShare(AS)
  e <- RealProAlleleShare(AS)
  Exp_DistAlleleShare <- DistAlleleShare(e)
  prob<-Exp_DistAlleleShare$Density
  obs<-Obs_DistAlleleShare$Freq
  s <-Simulate_DistX(e,ss,t)
  x2<-Dist_SimuChisq(s,prob,B)
  idx1 <-min(which(!obs==0))
  idx2 <- max(which(!obs==0))
  x20 <-chisq.test(obs[idx1:idx2],p=prob[idx1:idx2]/sum(prob[idx1:idx2]),simulate.p.value = T,B=B)
  P <- ecdf(x2)
  return(1-P(x20$statistic))
}
