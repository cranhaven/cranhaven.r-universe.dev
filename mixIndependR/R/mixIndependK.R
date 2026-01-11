#'Quick pvalue of total number of heterozygous loci
#'@details This function is a summary of pipeline for number of heterozygous loci (K), and generates the p-value of K for the target dataset.
#'@usage mixIndependK(x,sep,t,B)
#'@param x a dataset of alleles. Each row denotes each individual.One allele in one cell.In the (2r-1)th column, there is the same locus with the 2r-th column; noted: no column for ID, make row.names=1 when importing.
#'@param sep allele separator in the imported genotype data. Note: when using the special character like "|", remember to protect it as "\\|".
#'@param t times of simulation in "Simulate_DistK" and "Simulate_DistX".
#'@param B times of bootstrapping in Chi Squares Test.
#'@return pvalue (1-cumulative probabilities) for the number of heterozygous loci(K)
#'@export
#'@examples
#'x <- data.frame(SNP1=c("A|A","T|T","A|T","A|T"),
#'                 STR1=c("12|12","13|14","13|13","14|15"))
#'mixIndependK(x,sep ="\\|",10,10)

mixIndependK<-function(x,sep="\\|",t,B){
  ss <- nrow(x)
  p <- AlleleFreq(x,sep)
  h <- Heterozygous(x,sep)
  H <- RxpHetero(h,p,HWE = F)
  Obs_DistHetero<-FreqHetero(h)
  Exp_DistHetero<-DistHetero(H)
  prob<-Exp_DistHetero$Density
  obs<-Obs_DistHetero$Freq
  s<-Simulate_DistK(H,ss,t)
  x2<-Dist_SimuChisq(s,Exp_DistHetero$Density,B)
  idx1 <-min(which(!obs==0))
  idx2 <- max(which(!obs==0))
  x20 <-chisq.test(obs[idx1:idx2],p=prob[idx1:idx2]/sum(prob[idx1:idx2]),simulate.p.value = T,B=B)
  P <- ecdf(x2)
  return(1-P(x20$statistic))
}
