#' Haplotype List Builder
#'
#' Builds table of haplotypes from combinations
#' @param Combn Combination of loci to extraction from genos
#' @param genos The genotype columns of the loci set being analyzed.
#' @param loci Character vector of unique loci being analyzed.
#' @param loci.ColNames Character vector of genos column names.
#' @note This function is for internal BIGDAWG use only.
buildHAPsets <- function(Combn,genos,loci,loci.ColNames) {
  
  # Range in matrix
  Set.H <- loci.ColNames %in% loci[Combn]
  return(genos[,Set.H])
  
}

#' Haplotype Name Builder
#'
#' Builds table of names for HAPsets
#' @param Combn Combination of loci to extraction from genos
#' @param loci Character vector of unique loci being analyzed.
#' @note This function is for internal BIGDAWG use only.
buildHAPnames <- function(Combn,loci) {
  
  return(paste(loci[Combn],collapse="~"))

}


#' Haplotype Table Maker
#'
#' Builds table of haplotypes
#' @param HaploEM Haplotype output object from haplo.stat::haplo.em function.
#' @param SID Index number (i.e., row number) of sample ID from genotype matrix.
#' @note This function is for internal BIGDAWG use only.
getHap <- function(SID,HaploEM) {
  
  # SID subject number
  # haplotype object from Haplo.em
  
  # Range in matrix
  Range <- which(HaploEM$indx.subj==SID)
  HapGet <- which.max(HaploEM$post[Range])
  
  # Which haplotype (when more than one possibility)
  Hap1.no <- HaploEM$hap1code[Range][HapGet]
  Hap2.no <- HaploEM$hap2code[Range][HapGet]
  
  # Combine into a haplotype string
  Hap1 <- paste(HaploEM$haplotype[Hap1.no,],collapse="~")
  Hap2 <- paste(HaploEM$haplotype[Hap2.no,],collapse="~")
  
  # Output haplotype
  return(c(Hap1,Hap2))
  
}



