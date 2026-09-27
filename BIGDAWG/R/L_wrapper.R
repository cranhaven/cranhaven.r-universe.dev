#' Locus Wrapper
#'
#' Wrapper for main L function
#' @param nloci Number of loci being analyzed.
#' @param loci Loci being analyzed.
#' @param loci.ColNames The column names of the loci being analyzed.
#' @param genos Genotype table
#' @param grp Case/Control or Phenotype groupings.
#' @param Strict.Bin Logical specify if strict rare cell binning should be used in ChiSq test
#' @param Output Data return carryover from main BIGDAWG function
#' @param Verbose Summary display carryover from main BIGDAWG function
#' @note This function is for internal BIGDAWG use only.
L.wrapper <- function(nloci,loci,loci.ColNames,genos,grp,Strict.Bin,Output,Verbose) {

  cat("\n>>>> STARTING LOCUS LEVEL ANALYSIS...\n")

  Allele.binned <- list() # Alleles binned during chi-square test
  Allele.freq <- list() # Alleles Frequencies
  overall.chisq <- list() # Chi-Square Table
  ORtable <- list() # Odds Ratio Table
  Final_binned <- list() # Contingency Table

  for(j in 1:nloci) {

    # Get Locus
    Locus <- loci[j]

    # Run Locus Level Analysis
    L.list <- L(loci.ColNames,Locus,genos,grp,Strict.Bin)

    # Build Output Lists
    Allele.binned[[Locus]] <- L.list[['binned']]
    Allele.freq[[Locus]] <- L.list[['freq']]
    overall.chisq[[Locus]] <- L.list[['chisq']]
    ORtable[[Locus]] <- L.list[['OR']]
    Final_binned[[Locus]] <- L.list[['table']]

  }# END locus loop

  Out <- list()
  Out[['AB']] <- do.call(rbind,Allele.binned)
  Out[['AF']] <- do.call(rbind,Allele.freq)
  Out[['CS']] <- do.call(rbind,overall.chisq)
  Out[['OR']] <- do.call(rbind,ORtable)
  Out[['FB']] <- do.call(rbind,Final_binned)

  if(Output) {
    ## write to file
    write.table(Out[['AF']], file = paste("Locus_freqs.txt",sep=""), sep="\t", row.names = F, col.names=T, quote = F)
    write.table(Out[['FB']], file = paste("Locus_table.txt",sep=""), sep="\t", row.names = F, col.names=T, quote = F)
    write.table(Out[['AB']], file = paste("Locus_binned.txt",sep=""), sep="\t", row.names = F, col.names=T, quote = F)
    write.table(Out[['OR']], file = paste("Locus_OR.txt",sep=""), sep="\t", row.names = F, col.names=T, quote = F)
    write.table(Out[['CS']], file = paste("Locus_chisq.txt",sep=""), sep="\t", row.names = F, col.names=T, quote = F)
  }

    cat("> LOCUS LEVEL ANALYSIS COMPLETED","\n")
  if(Verbose) {
    print(as.data.frame(Out[['CS']]),row.names=F)
    cat("\n")
  }

  return(Out)

}
