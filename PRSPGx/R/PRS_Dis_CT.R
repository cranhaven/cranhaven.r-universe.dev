#' Construct disease PRS unadjusted or using clumping and thresholding
#'
#' Shrink prognostic effect sizes by p-value cutoff (PRS-Dis-CT turns out to be PRS-Dis-Unadj when setting p-value cutoff = 1)
#' @param DIS_GWAS a numeric matrix containing disease GWAS summary statistics, including SNP ID, position, \eqn{\beta}, SE(\eqn{\beta}), p-value, N, and MAF
#' @param G_reference a numeric matrix containing the individual-level genotype information from the reference panel (e.g., 1KG)
#' @param pcutoff a numeric value indicating the p-value cutoff
#' @param clumping a logical flag indicating should clumping be performed
#' @param p1 a numeric value indicating p-value threshold to decide flag SNPs in clumping
#' @param d1 a numeric value indicating window size in clumping
#' @param r1 a numeric value indicating correlation in clumping
#' @details PRS-Dis-CT automatically sets predictive effect sizes equivalent to the prognostic effect sizes; and only need disease GWAS summary statistics
#' @return A numeric list, the first sublist contains estimated prognostic effect sizes, the second sublist contains estimated predictive effect sizes
#' @references Euesden, J., Lewis, C.M. & O'Reilly, P.F. PRSice: Polygenic Risk Score software. Bioinformatics 564, 1466-1468 (2015).
#' @references Zhai, S., Zhang, H., Mehrotra, D.V. & Shen, J. Paradigm Shift from Disease PRS to PGx PRS for Drug Response Prediction using PRS-PGx Methods (submitted).
#' @author Song Zhai
#' @export
#' @examples
#' data(PRSPGx.example); attach(PRSPGx.example)
#' coef_est <- PRS_Dis_CT(DIS_GWAS, G_reference, pcutoff = 0.01, clumping = TRUE)
#' summary(coef_est$coef.G)
#' summary(coef_est$coef.TG)
#'
PRS_Dis_CT <- function(DIS_GWAS, G_reference, pcutoff=1e-05, clumping = TRUE, p1=0.0001, d1=250000, r1=0.8){

  pvalue <- DIS_GWAS$pvalue; coef.G <- DIS_GWAS$beta_hat
  names(coef.G) <- names(pvalue) <- DIS_GWAS$ID

  index.rm.thresholding <- which(pvalue > pcutoff) # if pcutoff = 1, then PRS-PGx-Unadj
  if(clumping == FALSE){
    index.rm <- index.rm.thresholding
  }
  if(clumping == TRUE){
    index.rm.clumping <- run_clumping(DIS_GWAS, G_reference, p1, d1, r1)
    index.rm <- unique(c(index.rm.clumping, index.rm.thresholding))
  }

  coef.G[index.rm] <- 0

  re <- list(coef.G = coef.G, coef.TG = coef.G)
  return(re)
}

