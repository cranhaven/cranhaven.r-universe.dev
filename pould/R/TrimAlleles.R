## TrimAlleles -- Steven J. Mack April 10, 2020 v1.0
## Trims IPD-IMGT/KIR allele names to a specified number of fields using BIGDAWG::GetField()

#' Truncate allele names in haplotypes to the specified number of fields.
#' 
#' This function accepts a dataframe of tilde-delimited haplotypes and trims colon-delimited names to the number of fields specified by 'reso'. 
#' @param haplotypes Data frame of tilde-delimited haplotypes extracted from the famData provided to LDWrap()
#' @param reso An integer that specifies the number of fields to which colon-delimited allele names in famData should be truncated. The default value of 0 indicates no truncation. A value higher than the number of fields in the supplied allele data will result in no truncation.
#' @note This function is for internal POULD use only.
#' @return A data frame of two sets of tilde-delimited haplotypes. 
#' @keywords trimAllele
#' @importFrom BIGDAWG GetField
#' @export
#' @examples #

trimAlleles <- function(haplotypes,reso) {

truncAllele <- function(x) GetField(x,reso) 
haplos <- list()
for(i in 1:2) { haplos[[i]] <- as.matrix(apply(read.table(text=haplotypes[[i]],sep="~",colClasses = "character",fill=TRUE),c(1,2),truncAllele)) }
cols <- colnames(haplos[[1]])
newHap <- cbind(as.data.frame(apply(haplos[[1]][,cols],1,paste,collapse ="~"),stringsAsFactors = FALSE),as.data.frame(apply(haplos[[2]][,cols],1,paste,collapse ="~"),stringsAsFactors = FALSE))
colnames(newHap) <- c("V1","V2")
for(i in 1:2) { newHap[[i]] <- gsub("~+$","",newHap[[i]]) } ## remove trailing ~ for missing loci  
as.data.frame(lapply(newHap, gsub, pattern = ":NA", replacement = "", fixed = TRUE),stringsAsFactors = FALSE)

}
