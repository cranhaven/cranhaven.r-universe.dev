#'Calculate Genotype Frequency###
#'@details This function calculates the observed or expected genotype frequency from dataset and allele frequency.#####
#'@usage GenotypeFreq(x,sep,expect=TRUE)
#'@importFrom utils combn
#'@param x a dataframe of genotype data with rownames of sample ID and column names of markers.
#'@param sep allele separator in the imported genotype data. Note: when using the special character like "|", remember to protect it as "\\|"(default).
#'@param expect a logic variable. If expect is true, the function will calculate the expected genotype probabilities. If false, calculate the observed genotype frequencies.
#'@references Chakraborty, R., Srinivasan, M. R., & Daiger, S. P. (1993, ISSN:0002-9297).
#'@return a dataframe of genotype frequencies. Each row denotes each genotype; each column denotes each loci. The order of markers follows x; the genotypes are ordered from homozygous to heterozygous.
#'@export
#'@examples
#'require(mixIndependR)
#'x <- data.frame(SNP1=c("A|A","T|T","A|T","A|T"),
#'                 STR1=c("12|12","13|14","13|13","14|15"))
#'GenotypeFreq(x,"\\|",expect=TRUE)


GenotypeFreq <- function(x,sep="\\|",expect = TRUE){
  p <-AlleleFreq(x,sep)
  Gt_a<-as.data.frame(cbind(rbind(rownames(p),rownames(p)),combn(rownames(p),2),combn(sort(rownames(p),decreasing = T),2)))
  if (sep=="\\|"){
    Gt<-as.vector(sapply(Gt_a,function(x){paste0(x[1],"|",x[2])}))
  }else{
    Gt<-as.vector(sapply(Gt_a,function(x){paste0(x[1],sep,x[2])}))
  }
  if(expect){
    ho<-p*p
    p0<-data.frame(p)
    he<-sapply(p0,function(x){combn(x,2)[1,]*combn(x,2)[2,]})
    p_hat<-p0[sort(rownames(p),decreasing = T),]
    he_verse <- sapply(p_hat,function(x){combn(x,2)[1,]*combn(x,2)[2,]})
    output <-rbind(ho,he,he_verse)
  }else{
    output <-sapply(x,function(x){sapply(Gt,counta,z=x)})
  }
  rownames(output) <- Gt
  return(as.data.frame(output))
}
