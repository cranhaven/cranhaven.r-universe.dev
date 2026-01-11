#'Import genotype data from vcf files/
#'@details This function extract the genotypes and allele status from a vcf file.
#'@import data.table utils
#'@usage read_vcf_gt(x)
#'@param x The vcf file with its directory
#'@return a list contains the genotype and allele status.
#'@export
#'@examples
#'\dontrun{
#'df<-read_vcf_gt("~/x.vcf")
#'}

read_vcf_gt <- function(x){
  vcf <- fread(x)
  GT_DS<-vcf[,-c(1:9)]
  s1<-function(x){
    return(read.table(text = x,sep = ":",as.is = T)$V1)
  }
  GT<-sapply(GT_DS, s1)
  Genotype_example <- t(GT)
  colnames(Genotype_example) <- vcf$ID
  Allele_Stat<-vcf[,4:5]
  return(list(Genotype=as.data.frame(Genotype_example),Allele=Allele_Stat))
}
