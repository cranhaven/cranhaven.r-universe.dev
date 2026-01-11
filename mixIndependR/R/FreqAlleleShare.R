#'Build Observed Distribution of No. of Shared Alleles
#'@details This function build the observed distributions from observed Allele Share table, made up of 0,1 and 2.
#'@usage FreqAlleleShare(AS)
#'@param AS a matrix of number of shared alleles, made up with 0, 1 and 2, outcome of function "AlleleShare_Table". Rows for individuals, and columns for markers.
#'@return a dataframe of frequencies of each number of shared alleles(from 0 to 2*N0. of loci)
#'@export
#'@examples
#'AS<-matrix(sample(c(0:2),20,replace=TRUE,prob=c(0.3,0.3,0.4)),nrow=5)
#'FreqAlleleShare(AS)

FreqAlleleShare<- function(AS){
  n<-ncol(AS)
  X<-rowSums(AS)
  X_io <- c(0:(2*n))
  Freq<-sapply(X_io,counta,z=X)
  df <- data.frame(X_io,Freq)
  return(df)
}
