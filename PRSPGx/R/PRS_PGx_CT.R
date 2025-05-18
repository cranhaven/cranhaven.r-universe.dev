#' Construct PGx PRS unadjusted or using clumping and thresholding
#'
#' Shrink prognostic and predictive effect sizes simutaneously by 2-df (main and interaction) p-value cutoff (PRS-PGx-CT turns out to be PRS-PGx-Unadj when setting p-value cutoff = 1)
#' @param PGx_GWAS a numeric matrix containing PGx GWAS summary statistics, including SNP ID, MAF, position, \eqn{\beta}, \eqn{\alpha}, 2-df p-value, and N
#' @param G_reference a numeric matrix containing the individual-level genotype information from the reference panel (e.g., 1KG)
#' @param pcutoff a numeric value indicating the p-value cutoff
#' @param clumping a logical flag indicating should clumping be performed
#' @param p1 a numeric value indicating p-value threshold to decide flag SNPs in clumping
#' @param d1 a numeric value indicating window size in clumping
#' @param r1 a numeric value indicating correlation in clumping
#' @details PRS-PGx-CT only needs PGx summary statistics
#' @return A numeric list, the first sublist contains estimated prognostic effect sizes, the second sublist contains estimated predictive effect sizes, the third sublist contains 2-df p-values
#' @references Zhai, S., Zhang, H., Mehrotra, D.V. & Shen, J. Paradigm Shift from Disease PRS to PGx PRS for Drug Response Prediction using PRS-PGx Methods (submitted).
#' @author Song Zhai
#' @export
#' @examples
#' data(PRSPGx.example); attach(PRSPGx.example)
#' coef_est <- PRS_PGx_CT(PGx_GWAS, G_reference, pcutoff = 0.01, clumping = TRUE)
#' summary(coef_est$coef.G)
#' summary(coef_est$coef.TG)
#'
PRS_PGx_CT <- function(PGx_GWAS, G_reference, pcutoff=0.0001, clumping = TRUE, p1=0.0001, d1=250000, r1=0.8){
  PGx_GWAS <- PGx_GWAS$G_INFO
  pvalue <- PGx_GWAS$pvalue; coef.G <- PGx_GWAS$beta_hat; coef.TG <- PGx_GWAS$alpha_hat

  index.rm.thresholding <- which(pvalue > pcutoff) # if pcutoff = 1, then PRS-PGx-Unadj
  if(clumping == FALSE){
    index.rm <- index.rm.thresholding
  }
  if(clumping == TRUE){
    index.rm.clumping <- run_clumping(PGx_GWAS, G_reference, p1, d1, r1)
    index.rm <- unique(c(index.rm.clumping, index.rm.thresholding))
  }

  coef.G[index.rm] <- 0; coef.TG[index.rm] <- 0

  names(coef.G) <- names(coef.TG) <- names(pvalue) <- PGx_GWAS$ID

  re <- list(coef.G = coef.G, coef.TG = coef.TG, pvalue=pvalue)
  return(re)
}

run_clumping <- function(GWAS, G_reference, p1=0.0001, d1=250000, r1=0.5){
  pvalue <- GWAS$pvalue; pos <- GWAS$POS

  indexSNP <- which(pvalue < p1)

  if(length(indexSNP) == 0){
    index.rm <- double()
  }

  if(length(indexSNP) > 0){
    pvalue.cand <- pvalue[indexSNP]
    or <- order(pvalue.cand)
    indexSNP <- indexSNP[or]

    index.rm <- double()
    while (length(indexSNP) > 0) {
      target.bp <- pos[indexSNP[1]]
      index.dis <- which(abs(pos - target.bp) < d1)

      r2 <- cor(G_reference[,indexSNP[1]], G_reference[,index.dis])
      r2 <- as.vector(r2)
      index.r2 <- which(abs(r2) > r1)

      index.inter0 <- index.dis[index.r2]
      index.inter1 <- index.inter0[-which(index.inter0 == indexSNP[1])]

      index.rm <- c(index.rm, index.inter1)

      label <- which(indexSNP%in%index.dis == FALSE)
      indexSNP <- indexSNP[label]
    }
  }

  return(unique(index.rm))
}
