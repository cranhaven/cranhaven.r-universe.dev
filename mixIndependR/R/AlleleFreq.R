#'Calculate Allele Frequency
#'@details This function calculates the allele frequencies of one dataset.
#'@usage AlleleFreq(x,sep)
#'@param x a dataset of genotypes. Each row denotes each individual; each column contain each marker.
#'@param sep the allele separator in the imported genotype data.Note: when using the special character like "|", remember to protect it as "\\|"(default).
#'@return a matrix of allele frequencies. Each row denotes each allele; each column denotes each marker. The order of makers follows x.
#'@export
#'@examples
#'require(mixIndependR)
#'x <- data.frame(STR1=c("12|12","13|14","13|13","14|15"),
#'                SNP1=c("A|A","T|T","A|T","A|T"))
#'AlleleFreq(x,"\\|")
#'

AlleleFreq <- function(x,sep="\\|"){
  a0 <-splitGenotype(x,sep,"*",rowbind =  T)
  l <- as.data.frame(table(as.matrix(a0)))   #####All allleles included####
  c1 <- function(y,z){
    f<- sapply(y,counta,z=z)
    return(as.data.frame(f)[[1]])
  }
  Freq <-sapply(a0,c1,y=as.character(l$Var1))/(2*nrow(x))
  rownames(Freq) <- l$Var1
  return(Freq)
}
