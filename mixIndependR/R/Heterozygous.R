#'Test heterozygosity at each locus
#'@details This function test the heterozygosity of each individuals at each locus.Output a table and Usually followed by write.csv(as.data.frame(y),file = "~/*.csv") to export the results.
#'@usage Heterozygous(x,sep)
#'@param x a dataset of genotypes with rownames of sample ID and column names of markers.
#'@param sep allele separator in the imported genotype data. Note: when using the special character like "|", remember to protect it as "\\|"(default).
#'@return a dataframe of heterozygosity.0 is homozygous;1 is heterozygous. Each row denotes each individual; Each column denotes each locus.
#'@export
#'@examples
#'x <- data.frame(STR1=c("12|12","13|14","13|13","14|15"),
#'                SNP1=c("A|A","T|T","A|T","A|T"))
#'Heterozygous(x,"\\|")
#'
Heterozygous <- function(x,sep="\\|"){
  g2 <- as.matrix(x)
  n <- nrow(g2)
  m <- ncol(g2)
  a1<-matrix(sapply(strsplit(g2,sep),"[",1),nrow = n,ncol=m,byrow = F)
  a2<-matrix(sapply(strsplit(g2,sep),"[",2),nrow = n,ncol=m,byrow = F)
  colnames(a1) <- colnames(g2)
  rownames(a1) <- rownames(g2)
  H<-matrix(as.numeric(!a1==a2),nrow = n,ncol = m,byrow = F)
  colnames(H) <- colnames(g2)
  rownames(H) <- rownames(g2)
  return(H)
}
