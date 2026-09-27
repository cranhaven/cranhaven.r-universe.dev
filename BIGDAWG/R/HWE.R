#' Hardy Weinbergy Equilibrium Function
#'
#' This is the main function for the HWE analysis.
#' @param Tab data frame of genotype files post processing.
#' @note This function is for internal BIGDAWG use only.
HWE <- function(Tab) {

  HWE.out <- list()

  All.ColNames <- colnames(Tab)
  loci <- as.list(unique(All.ColNames[3:length(All.ColNames)]))
  nloci <- length(loci)

  #HWE Controls / Group 0
  genos.sub <- Tab[which(Tab[,2]==0),3:ncol(Tab)]
  HWE.out[["controls"]] <- HWE.ChiSq(genos.sub,loci,nloci)

  #HWE Cases / Group 1
  genos.sub <- Tab[which(Tab[,2]==1),3:ncol(Tab)]
  HWE.out[["cases"]] <- HWE.ChiSq(genos.sub,loci,nloci)

  return(HWE.out)

}
